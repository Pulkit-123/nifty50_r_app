##############################
# app.R — NIFTY 50 Dashboard #
##############################

# ---- Auto-install guard ----------------------------------------------
# Set this to FALSE in your R session before runGitHub() to skip installs:
if (isTRUE(getOption("nifty50.autoinstall", TRUE))) {
  req_pkgs <- c(
    "shiny","bslib","quantmod","xts","dplyr","lubridate","plotly","TTR",
    "memoise","shinycssloaders","scales","purrr","stringr","nser","DT"
  )
  to_install <- setdiff(req_pkgs, rownames(installed.packages()))
  if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")
}

# --- Libraries ----------------------------------------------------------
library(shiny)
library(bslib)
library(quantmod)
library(xts)
library(dplyr)
library(lubridate)
library(plotly)
library(TTR)
library(memoise)
library(shinycssloaders)
library(scales)
library(purrr)
library(stringr)
library(DT)
suppressPackageStartupMessages(try(library(nser), silent = TRUE))

# --- Config -------------------------------------------------------------
options(warn = -1)
Sys.setenv(TZ = "Asia/Kolkata")
NIFTY_YH <- "^NSEI"                 # Yahoo symbol for NIFTY 50 index
default_from <- Sys.Date() - 365
default_to   <- Sys.Date()

# NIFTY 50 map (edit as needed; Yahoo tickers)
nifty_map <- c(
  "Reliance Industries"="RELIANCE.NS","HDFC Bank"="HDFCBANK.NS","ICICI Bank"="ICICIBANK.NS",
  "Infosys"="INFY.NS","TCS"="TCS.NS","Hindustan Unilever"="HINDUNILVR.NS","ITC"="ITC.NS",
  "Larsen & Toubro"="LT.NS","State Bank of India"="SBIN.NS","Axis Bank"="AXISBANK.NS",
  "Kotak Mahindra Bank"="KOTAKBANK.NS","Bharti Airtel"="BHARTIARTL.NS","Asian Paints"="ASIANPAINT.NS",
  "HCL Technologies"="HCLTECH.NS","Maruti Suzuki"="MARUTI.NS","Titan"="TITAN.NS",
  "UltraTech Cement"="ULTRACEMCO.NS","Wipro"="WIPRO.NS","NTPC"="NTPC.NS","Power Grid"="POWERGRID.NS",
  "Nestle India"="NESTLEIND.NS","Sun Pharma"="SUNPHARMA.NS","Tata Steel"="TATASTEEL.NS",
  "JSW Steel"="JSWSTEEL.NS","Tata Motors"="TATAMOTORS.NS","Mahindra & Mahindra"="M&M.NS",
  "Bajaj Finance"="BAJFINANCE.NS","Bajaj Finserv"="BAJAJFINSV.NS","HDFC Life"="HDFCLIFE.NS",
  "SBI Life"="SBILIFE.NS","Adani Enterprises"="ADANIENT.NS","Adani Ports"="ADANIPORTS.NS",
  "Coal India"="COALINDIA.NS","Hindalco"="HINDALCO.NS","Tata Consumer"="TATACONSUM.NS",
  "Divi's Labs"="DIVISLAB.NS","Dr Reddy's"="DRREDDY.NS","Cipla"="CIPLA.NS","Grasim"="GRASIM.NS",
  "Tech Mahindra"="TECHM.NS","Eicher Motors"="EICHERMOT.NS","Hero MotoCorp"="HEROMOTOCO.NS",
  "Britannia"="BRITANNIA.NS","UPL"="UPL.NS","ONGC"="ONGC.NS","BPCL"="BPCL.NS",
  "IndusInd Bank"="INDUSINDBK.NS","Bajaj Auto"="Bajaj-AUTO.NS","Apollo Hospitals"="APOLLOHOSP.NS",
  "LTI Mindtree"="LTIM.NS","Shriram Finance"="SHRIRAMFIN.NS"
)

# --- Helpers: safe theme toggle ----------------------------------------
theme_toggle_safe <- function() {
  if (requireNamespace("bslib", quietly = TRUE)) {
    ns <- asNamespace("bslib")
    if (exists("theme_toggle", envir = ns, inherits = FALSE)) {
      return(bslib::theme_toggle())
    }
  }
  NULL
}

# --- Helpers: fetchers, math, caching ----------------------------------
.fetch_yahoo <- function(symbol, from, to) {
  suppressWarnings(
    getSymbols(Symbols = symbol, src = "yahoo",
               from = as.Date(from), to = as.Date(to), auto.assign = FALSE)
  )
}
fetch_yahoo <- memoise::memoise(.fetch_yahoo)

safe_last_first_ret <- function(x) {
  if (NROW(x) < 2) return(NA_real_)
  as.numeric(last(x) / first(x) - 1)
}

daily_log_returns <- function(px) {
  px <- na.locf(px, na.rm = FALSE)
  px <- na.omit(px)
  if (NROW(px) < 2) return(xts())
  diff(log(px))
}

max_drawdown_pct <- function(px) {
  if (NROW(px) < 2) return(NA_real_)
  cum <- as.numeric(px)
  run_max <- cummax(cum)
  draw <- (cum / run_max) - 1
  min(draw, na.rm = TRUE) * 100
}

fetch_nifty_fallback <- function(from, to) {
  tryCatch({ fetch_yahoo(NIFTY_YH, from, to) }, error = function(e) NULL)
}

# --- README content -----------------------------------------------------
readme_md <- paste(
  "# NIFTY 50 Shiny Dashboard",
  "",
  "Interactive market app for Indian equities using **quantmod** (Yahoo Finance) and **plotly**.",
  "",
  "## Features",
  "- Interactive **candlestick** + volume (green up / red down), log-scale toggle, configurable SMAs.",
  "- **Real-time quote** badge (auto-refresh + manual refresh).",
  "- **Stock vs NIFTY** comparison: dual-axis price vs price, or normalized % view.",
  "- **Risk metrics** (β, corr, R², vol, max DD, tracking error, info ratio).",
  "- **Watchlist** small-multiples (normalize toggle) with CSV export.",
  "- **Data Table** tab for last N days with OHLCV, Avg Price, OC% and CC% returns.",
  "- **Alerts**, **caching**, **debounced inputs**, themed UI, and CSV downloads.",
  "",
  "## Run Locally",
  "```r",
  "shiny::runApp()",
  "```",
  sep = "\n"
)

# --- UI -----------------------------------------------------------------
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly", base_font = font_google("Inter")),
  tags$head(tags$style(HTML("
    .small-note{color:#666;font-size:12px;}
    .neg {color:#c0392b;}
    .pos {color:#27ae60;}
    .control-row {display:flex; gap:10px; flex-wrap:wrap;}
    .form-group{margin-bottom:10px;}
  "))),
  pageTitle = "NIFTY 50 — Real-Time & Historical",
  titlePanel(tagList(
    "NIFTY 50 — Real-Time & Historical (quantmod + Yahoo, interactive)",
    theme_toggle_safe()
  )),
  sidebarLayout(
    sidebarPanel(
      div(class = "control-row",
          selectizeInput("company","Select company", choices = nifty_map,
                         selected = "HDFCBANK.NS", multiple = FALSE,
                         options = list(placeholder = 'Type to search…')),
          actionButton("refresh_now", "Refresh quote", icon = icon("rotate"))
      ),
      dateRangeInput("daterange","Date range",
                     start = default_from, end = default_to,
                     min = as.Date("2000-01-01"), max = Sys.Date(),
                     format = "dd M yyyy"),
      checkboxInput("autorefresh","Auto-refresh quote (every 30s)", TRUE),
      hr(),
      div(class="control-row",
          numericInput("sma_fast","SMA fast", value = 20, min = 2, max = 200, step = 1, width = "110px"),
          numericInput("sma_slow","SMA slow", value = 50, min = 2, max = 400, step = 1, width = "110px"),
          checkboxInput("log_scale","Log scale", value = FALSE)
      ),
      checkboxInput("use_adjusted","Use Adjusted Close for returns", TRUE),
      hr(),
      strong("Alerts"),
      div(class="control-row",
          numericInput("alert_pct","Alert if |%Δ| ≥", value = 2, min = 0.1, step = 0.1, width = "140px"),
          numericInput("alert_px","Alert if Last ≥", value = NA, min = 0, step = 1, width = "160px")
      ),
      checkboxInput("alerts_on","Enable alerts", TRUE),
      hr(),
      downloadButton("dl_prices","Download CSV (Historical)"),
      br(), br(),
      span(class="small-note",
           "Data: Yahoo Finance via quantmod · Times shown in IST · Hover, zoom, pan enabled.")
    ),
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel(
          title = "Overview",
          br(),
          fluidRow(
            column(12, h4(textOutput("title_lbl")),
                   uiOutput("quote_badge") %>% withSpinner(type = 6, color = "#1b6ec2"),
                   tableOutput("last_row_tbl") %>% withSpinner(type = 5))
          ),
          hr(),
          fluidRow(
            column(12, h4("Price chart (hover for details)"),
                   plotlyOutput("price_plotly", height = "560px") %>%
                     withSpinner(type = 6, color = "#1b6ec2"))
          ),
          hr(),
          fluidRow(
            column(6, h4("Return summary (period)"),
                   tableOutput("ret_tbl") %>% withSpinner(type = 5)),
            column(6, h4("Session stats"),
                   tableOutput("session_tbl") %>% withSpinner(type = 5))
          ),
          hr(),
          fluidRow(
            column(12, h4("Risk metrics vs NIFTY"),
                   tableOutput("risk_tbl") %>% withSpinner(type = 5))
          )
        ),
        tabPanel(
          title = "Stock vs Nifty",
          br(),
          div(class="control-row",
              checkboxInput("normalize_pct", "Normalize to % change (same left Y-axis)", FALSE)
          ),
          plotlyOutput("compare_plot", height = "520px") %>% withSpinner(type = 6, color = "#1b6ec2"),
          br(),
          downloadButton("dl_compare", "Download CSV (Stock & NIFTY series)")
        ),
        tabPanel(
          title = "Watchlist",
          br(),
          selectizeInput("watchlist","Add tickers", choices = nifty_map,
                         multiple = TRUE, options = list(placeholder = 'Pick 1–8 tickers…')),
          div(class="control-row",
              checkboxInput("wl_normalize","Normalize to % change", TRUE),
              numericInput("wl_max","Max panels", value = 6, min = 2, max = 12, step = 1, width = "130px")
          ),
          plotlyOutput("watch_plot", height = "700px") %>% withSpinner(type = 6, color = "#1b6ec2"),
          br(),
          downloadButton("dl_watchlist", "Download CSV (Watchlist panel)")
        ),
        tabPanel(
          title = "Data Table",
          br(),
          div(class="control-row",
              numericInput("tbl_days", "Show last N days", value = 15, min = 10, max = 60, step = 1, width = "160px")
          ),
          DTOutput("data_tbl") %>% withSpinner(type = 6, color = "#1b6ec2"),
          br(),
          downloadButton("dl_table", "Download CSV (Data Table)")
        ),
        tabPanel(
          title = "README",
          br(),
          tags$pre(style="white-space:pre-wrap; font-family: ui-monospace, SFMono-Regular, Menlo;",
                   readme_md),
          br(),
          downloadButton("dl_readme","Download README.md")
        )
      )
    )
  )
)

# --- SERVER -------------------------------------------------------------
server <- function(input, output, session){

  # Debounce heavy triggers
  debounced_inputs <- reactive({
    list(sym = input$company, dr = input$daterange,
         sma_fast = input$sma_fast, sma_slow = input$sma_slow,
         use_adj = input$use_adjusted, log_scale = input$log_scale)
  }) %>% debounce(250)

  # Title label
  output$title_lbl <- renderText({
    picked <- input$company
    name <- names(nifty_map)[nifty_map == picked]
    sprintf("%s   (%s)", ifelse(length(name)==0, picked, name), picked)
  })

  # Historical (stock) via Yahoo (cached)
  prices_xts <- reactive({
    pars <- debounced_inputs()
    req(pars$dr[1], pars$dr[2], input$company)
    validate(need(!is.na(pars$dr[1]) && !is.na(pars$dr[2]) && pars$dr[1] <= pars$dr[2],
                  "Select a valid date range."))
    tryCatch(
      fetch_yahoo(input$company, pars$dr[1], pars$dr[2]),
      error = function(e){
        showNotification(paste("Historical fetch error:", e$message), type="error"); NULL
      }
    )
  })

  # Historical (NIFTY)
  nifty_xts <- reactive({
    pars <- debounced_inputs()
    req(pars$dr[1], pars$dr[2])
    out <- tryCatch(fetch_yahoo(NIFTY_YH, pars$dr[1], pars$dr[2]), error = function(e) NULL)
    if (is.null(out)) out <- fetch_nifty_fallback(pars$dr[1], pars$dr[2])
    out
  })

  # Quotes -------------------------------------------------
  rt <- reactiveTimer(30000)
  observeEvent(input$refresh_now, ignoreInit = TRUE, { quote_stamp$val <- Sys.time() })
  quote_stamp <- reactiveValues(val = Sys.time())

  latest_quote <- reactive({
    if (isTRUE(input$autorefresh)) rt()
    quote_stamp$val
    req(input$company)
    tryCatch(getQuote(input$company),
             error = function(e){ showNotification(paste("Quote error:", e$message),
                                                   type="warning"); NULL })
  })

  # Alerts (percent / price)
  observe({
    if (!isTRUE(input$alerts_on)) return()
    q <- latest_quote(); if (is.null(q) || nrow(q)==0) return()
    last <- suppressWarnings(as.numeric(q$Last))
    pct  <- suppressWarnings(as.numeric(q$`% Change`))
    if (!is.na(input$alert_pct) && is.finite(pct) && abs(pct) >= input$alert_pct) {
      showNotification(paste0("ALERT: |%Δ| = ", sprintf("%.2f", pct), "%"), type = "message")
    }
    if (!is.na(input$alert_px) && is.finite(last) && last >= input$alert_px) {
      showNotification(paste0("ALERT: Last crossed ", input$alert_px, " (Last=",
                              sprintf("%.2f", last), ")"), type = "message")
    }
  })

  # Quote badge
  output$quote_badge <- renderUI({
    q <- latest_quote(); if (is.null(q) || nrow(q)==0) return(NULL)
    last <- suppressWarnings(as.numeric(q$Last))
    open <- suppressWarnings(as.numeric(q$Open))
    chg  <- suppressWarnings(as.numeric(q$Change))
    pct  <- suppressWarnings(as.numeric(q$`% Change`))
    cls  <- ifelse(is.na(chg), "", ifelse(chg >= 0, "pos", "neg"))
    div(style="margin:6px 0 14px 0;",
        tags$span(class=paste("badge",cls),
                  style="display:inline-block;border-radius:8px;padding:8px 12px;background:#f4f6f8;",
                  strong("Last: "), ifelse(is.na(last),"—", sprintf("%.2f", last)),
                  "  ·  ", strong("Δ: "),
                  ifelse(is.na(chg), "—", sprintf("%.2f (%s)", chg,
                                                  ifelse(is.na(pct),"—", paste0(sprintf('%.2f', pct),"%")))),
                  "  ·  ", strong("Open: "),
                  ifelse(is.na(open),"—", sprintf("%.2f", open)),
                  "  ·  ", strong("Time: "),
                  if ("Trade Time" %in% names(q)) as.character(q$`Trade Time`) else "—"))
  })

  # Latest OHLCV row
  output$last_row_tbl <- renderTable({
    x <- prices_xts(); if (is.null(x)) return(NULL)
    lr <- tail(x, 1)
    data.frame(
      Date   = index(lr),
      Open   = round(as.numeric(lr[,1]),2),
      High   = round(as.numeric(lr[,2]),2),
      Low    = round(as.numeric(lr[,3]),2),
      Close  = round(as.numeric(lr[,4]),2),
      Volume = label_number_si()(as.numeric(lr[,5])),
      Adjusted = if (NCOL(lr)>=6) round(as.numeric(lr[,6]),2) else NA_real_
    )
  }, striped=TRUE, bordered=TRUE, digits=2)

  # Candlestick + Volume
  output$price_plotly <- renderPlotly({
    pars <- debounced_inputs()
    x <- prices_xts(); validate(need(!is.null(x), "No data to plot yet."))
    df <- data.frame(
      Date = as.Date(index(x)),
      Open = as.numeric(x[,1]),
      High = as.numeric(x[,2]),
      Low  = as.numeric(x[,3]),
      Close= as.numeric(x[,4]),
      Volume = as.numeric(x[,5])
    )
    sma_fast <- suppressWarnings(SMA(df$Close, n = max(2, min(400, pars$sma_fast))))
    sma_slow <- suppressWarnings(SMA(df$Close, n = max(2, min(400, pars$sma_slow))))

    p <- plot_ly(x = ~df$Date, type = "candlestick",
                 open = ~df$Open, high = ~df$High, low = ~df$Low, close = ~df$Close,
                 name = "OHLC",
                 increasing = list(line = list(color = "green"), fillcolor = "green"),
                 decreasing = list(line = list(color = "red"),   fillcolor = "red"),
                 hovertemplate = paste(
                   "<b>%{x|%d %b %Y}</b><br>",
                   "Open: %{customdata[0]:.2f}<br>",
                   "High: %{customdata[1]:.2f}<br>",
                   "Low:  %{customdata[2]:.2f}<br>",
                   "Close:%{customdata[3]:.2f}<extra></extra>"
                 ),
                 customdata = cbind(df$Open, df$High, df$Low, df$Close))

    p <- p %>%
      add_lines(x = ~df$Date, y = ~sma_fast, name = paste0("SMA ", pars$sma_fast),
                hovertemplate = "SMA: %{y:.2f}<extra></extra>") %>%
      add_lines(x = ~df$Date, y = ~sma_slow, name = paste0("SMA ", pars$sma_slow),
                hovertemplate = "SMA: %{y:.2f}<extra></extra>") %>%
      layout(
        xaxis = list(rangeslider = list(visible = FALSE)),
        yaxis = list(title = "Price", type = if (pars$log_scale) "log" else "linear"),
        hovermode = "x unified",
        legend = list(orientation = "h", x = 0, y = -0.15)
      ) %>%
      subplot(
        .,
        {
          up <- df$Close >= df$Open
          plot_ly(x = ~df$Date, y = ~df$Volume, type = "bar", name = "Volume",
                  marker = list(color = ifelse(up, "green", "red")),
                  hovertemplate = "Vol: %{y}<extra></extra>") %>%
            layout(yaxis = list(title = "Volume"))
        },
        nrows = 2, heights = c(0.75, 0.25), shareX = TRUE, titleY = TRUE
      )
    p
  })

  # Return summary
  output$ret_tbl <- renderTable({
    pars <- debounced_inputs()
    x <- prices_xts(); if (is.null(x)) return(NULL)
    cl <- if (pars$use_adj && NCOL(x) >= 6) x[,6] else Cl(x)
    if (NROW(cl) < 2) return(NULL)
    ret_total <- safe_last_first_ret(cl)
    dr <- daily_log_returns(cl); if (NROW(dr) < 2) return(NULL)
    ann_factor <- 252
    ann_ret <- mean(dr, na.rm=TRUE) * ann_factor
    ann_vol <- sd(dr, na.rm=TRUE) * sqrt(ann_factor)
    sharpe  <- if (is.finite(ann_vol) && ann_vol > 0) ann_ret / ann_vol else NA_real_
    data.frame(
      Metric = c("Total Return (period)", "Ann. Return (log)", "Ann. Volatility", "Sharpe (r_f≈0)"),
      Value  = c(sprintf("%.2f%%", 100*ret_total),
                 sprintf("%.2f%%", 100*ann_ret),
                 sprintf("%.2f%%", 100*ann_vol),
                 ifelse(is.finite(sharpe), sprintf("%.2f", sharpe), "—")),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE)

  # Session stats
  output$session_tbl <- renderTable({
    q <- latest_quote(); if (is.null(q) || nrow(q)==0) return(NULL)
    fields <- c("Open","Day's High","Day's Low","Previous Close","Volume")
    vals <- sapply(fields, function(f) if (f %in% names(q)) q[[f]][1] else NA)
    data.frame(Field = fields, Value = vals, check.names = FALSE)
  }, striped = TRUE, bordered = TRUE)

  # Risk metrics vs NIFTY
  output$risk_tbl <- renderTable({
    pars <- debounced_inputs()
    xS <- prices_xts(); xN <- nifty_xts()
    if (is.null(xS) || is.null(xN)) return(NULL)
    s <- if (pars$use_adj && NCOL(xS)>=6) xS[,6] else Cl(xS)
    n <- if (NCOL(xN)>=6) xN[,6] else Cl(xN)
    idx <- intersect(index(s), index(n))
    s <- s[idx]; n <- n[idx]
    if (NROW(s) < 20) return(NULL)
    rs <- daily_log_returns(s); rn <- daily_log_returns(n)
    idx2 <- intersect(index(rs), index(rn))
    rs <- rs[idx2]; rn <- rn[idx2]
    if (NROW(rs) < 20) return(NULL)
    ann <- 252
    volS <- sd(rs, na.rm=TRUE)*sqrt(ann)
    volN <- sd(rn, na.rm=TRUE)*sqrt(ann)
    covSN <- as.numeric(cov(rs, rn, use="complete.obs"))
    varN  <- as.numeric(var(rn, na.rm=TRUE))
    beta  <- if (varN>0) covSN/varN else NA_real_
    corr  <- as.numeric(cor(rs, rn, use="complete.obs"))
    r2    <- corr^2
    te    <- sd(rs - rn, na.rm=TRUE)*sqrt(ann)
    ir    <- (mean(rs, na.rm=TRUE)-mean(rn, na.rm=TRUE))*ann / ifelse(te>0, te, NA_real_)
    mddS  <- max_drawdown_pct(s)
    data.frame(
      Metric = c("Beta (vs NIFTY)", "Correlation", "R²", "Ann. Vol (Stock)", "Ann. Vol (NIFTY)",
                 "Max Drawdown (Stock, %)", "Tracking Error", "Information Ratio"),
      Value  = c(sprintf("%.3f", beta), sprintf("%.3f", corr), sprintf("%.3f", r2),
                 sprintf("%.2f%%", 100*volS), sprintf("%.2f%%", 100*volN),
                 sprintf("%.2f%%", mddS), sprintf("%.2f%%", 100*te),
                 ifelse(is.finite(ir), sprintf("%.2f", ir), "—")),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE)

  # Dual-axis / Normalized comparison
  output$compare_plot <- renderPlotly({
    pars <- debounced_inputs()
    xS <- prices_xts(); xN <- nifty_xts()
    validate(need(!is.null(xS) && !is.null(xN), "Need both series to compare."))

    stock_series <- if (pars$use_adj && NCOL(xS) >= 6) xS[,6] else Cl(xS)
    nifty_series <- if (NCOL(xN) >= 6) xN[,6] else Cl(xN)

    dfS <- data.frame(Date = as.Date(index(stock_series)), Stock = as.numeric(stock_series))
    dfN <- data.frame(Date = as.Date(index(nifty_series)), NIFTY = as.numeric(nifty_series))
    common <- intersect(dfS$Date, dfN$Date)
    df <- dfN[dfN$Date %in% common, ] %>% left_join(dfS, by = "Date")
    validate(need(nrow(df) > 1, "Insufficient overlapping data to compare."))

    if (isTRUE(input$normalize_pct)) {
      df <- df %>%
        mutate(NIFTY = (NIFTY/first(NIFTY) - 1)*100,
               Stock = (Stock/first(Stock) - 1)*100)
      plot_ly() %>%
        add_lines(data = df, x = ~Date, y = ~NIFTY, name = "NIFTY (%Δ)",
                  line = list(width = 2),
                  hovertemplate = "NIFTY: %{y:.2f}%%<extra></extra>") %>%
        add_lines(data = df, x = ~Date, y = ~Stock, name = "Stock (%Δ)",
                  line = list(color = "blue", width = 2),
                  hovertemplate = "Stock: %{y:.2f}%%<extra></extra>") %>%
        layout(hovermode = "x unified",
               xaxis = list(title = "Date"),
               yaxis = list(title = "% Change (since start)", rangemode = "tozero"),
               legend = list(orientation = "h", x = 0, y = 1.02))
    } else {
      plot_ly() %>%
        add_lines(data = df, x = ~Date, y = ~NIFTY, name = "NIFTY (^NSEI)",
                  line = list(width = 2),
                  hovertemplate = "NIFTY: %{y:.2f}<extra></extra>", yaxis = "y") %>%
        add_lines(data = df, x = ~Date, y = ~Stock, name = "Stock",
                  line = list(color = "blue", width = 2),
                  hovertemplate = "Stock: %{y:.2f}<extra></extra>", yaxis = "y2") %>%
        layout(hovermode = "x unified",
               xaxis = list(title = "Date"),
               yaxis = list(title = "NIFTY Price (left)", rangemode = "tozero"),
               yaxis2 = list(title = "Stock Price (right)", overlaying = "y", side = "right",
                             rangemode = "tozero"),
               legend = list(orientation = "h", x = 0, y = 1.02))
    }
  })

  # Watchlist small-multiples
  output$watch_plot <- renderPlotly({
    syms <- input$watchlist
    if (length(syms) == 0) return(NULL)
    syms <- syms[seq_len(min(length(syms), input$wl_max))]
    dr <- input$daterange

    series <- purrr::map(syms, function(s) {
      x <- try(fetch_yahoo(s, dr[1], dr[2]), silent = TRUE)
      if (inherits(x, "try-error") || is.null(x)) return(NULL)
      cl <- Cl(x)
      tibble(Symbol = s, Date = as.Date(index(cl)), Close = as.numeric(cl))
    })

    series <- purrr::compact(series)
    if (!length(series)) return(NULL)
    df <- bind_rows(series)

    if (isTRUE(input$wl_normalize)) {
      df <- df %>% group_by(Symbol) %>%
        arrange(Date) %>%
        mutate(Value = (Close/first(Close) - 1)*100) %>%
        ungroup()
      y_title <- "% Change (since start)"
      fmt <- "%{y:.2f}%%"
    } else {
      df <- df %>% rename(Value = Close)
      y_title <- "Price"
      fmt <- "%{y:.2f}"
    }

    plots <- lapply(split(df, df$Symbol), function(d) {
      plot_ly(d, x = ~Date, y = ~Value, type = "scatter", mode = "lines",
              name = unique(d$Symbol),
              hovertemplate = paste0(unique(d$Symbol), ": ", fmt, "<extra></extra>")) %>%
        layout(yaxis = list(title = y_title))
    })
    subplot(plots, nrows = ceiling(length(plots)/2), shareX = TRUE, titleY = TRUE) %>%
      layout(showlegend = FALSE)
  })

  # ---------------- Data Table (last N days) ----------------
  table_df <- reactive({
    pars <- debounced_inputs()
    x <- prices_xts()
    req(x)

    # choose series for CC return per setting
    close_series <- if (pars$use_adj && NCOL(x) >= 6) x[,6] else Cl(x)

    df <- data.frame(
      Date    = as.Date(index(x)),
      Open    = as.numeric(x[,1]),
      High    = as.numeric(x[,2]),
      Low     = as.numeric(x[,3]),
      Close   = as.numeric(x[,4]),
      Volume  = as.numeric(x[,5]),
      Adjusted= if (NCOL(x) >= 6) as.numeric(x[,6]) else NA_real_
    )

    # typical price (H+L+C)/3
    df <- df %>%
      mutate(`Avg Price` = (High + Low + Close)/3)

    # open->close daily return
    df <- df %>%
      mutate(`OC Return %` = ifelse(Open > 0, (Close / Open - 1) * 100, NA_real_))

    # close->close return based on chosen series (Close or Adjusted)
    cc <- dailyReturn(close_series, type = "arithmetic")
    cc_df <- data.frame(Date = as.Date(index(cc)), `CC Return %` = as.numeric(cc) * 100)
    df <- df %>%
      left_join(cc_df, by = "Date")

    # keep last N days only
    n_days <- max(10, min(60, ifelse(is.null(input$tbl_days), 15, input$tbl_days)))
    tail(df, n_days) %>% arrange(desc(Date))
  })

  output$data_tbl <- renderDT({
    d <- table_df()
    validate(need(nrow(d) > 0, "No rows to display."))
    datatable(
      d,
      rownames = FALSE,
      options = list(
        pageLength = 15,
        lengthMenu = c(10, 15, 30, 60),
        order = list(list(0, 'desc')),
        scrollX = TRUE
      )
    ) %>%
      formatRound(c("Open","High","Low","Close","Adjusted","Avg Price","OC Return %","CC Return %"), 2) %>%
      formatCurrency("Volume", currency = "", interval = 3, mark = ",")
  })

  # ----------------- Downloads -----------------
  output$dl_prices <- downloadHandler(
    filename = function(){
      paste0(gsub("\\.NS$","", input$company), "_prices_",
             format(input$daterange[1], "%Y%m%d"), "_",
             format(input$daterange[2], "%Y%m%d"), ".csv")
    },
    content = function(file){
      x <- prices_xts(); validate(need(!is.null(x), "No data to download."))
      df <- data.frame(
        Date = index(x),
        Open = as.numeric(x[,1]),
        High = as.numeric(x[,2]),
        Low  = as.numeric(x[,3]),
        Close= as.numeric(x[,4]),
        Volume = as.numeric(x[,5]),
        Adjusted = if (NCOL(x)>=6) as.numeric(x[,6]) else NA_real_
      )
      write.csv(df, file, row.names = FALSE)
    }
  )

  output$dl_compare <- downloadHandler(
    filename = function(){
      paste0("compare_", gsub("\\.NS$","", input$company), "_vs_NIFTY_",
             format(input$daterange[1], "%Y%m%d"), "_",
             format(input$daterange[2], "%Y%m%d"), ".csv")
    },
    content = function(file){
      pars <- debounced_inputs()
      xS <- prices_xts(); xN <- nifty_xts(); validate(need(!is.null(xS) && !is.null(xN),
                                                           "No data to download."))
      stock_series <- if (pars$use_adj && NCOL(xS) >= 6) xS[,6] else Cl(xS)
      nifty_series <- if (NCOL(xN) >= 6) xN[,6] else Cl(xN)
      dfS <- data.frame(Date = as.Date(index(stock_series)), Stock = as.numeric(stock_series))
      dfN <- data.frame(Date = as.Date(index(nifty_series)), NIFTY = as.numeric(nifty_series))
      df <- dfN %>% left_join(dfS, by = "Date")
      if (isTRUE(input$normalize_pct)) {
        df <- df %>%
          mutate(NIFTY_pct = (NIFTY / first(NIFTY) - 1) * 100,
                 Stock_pct = (Stock / first(Stock) - 1) * 100)
      }
      write.csv(df, file, row.names = FALSE)
    }
  )

  output$dl_watchlist <- downloadHandler(
    filename = function(){
      paste0("watchlist_", format(input$daterange[1], "%Y%m%d"), "_",
             format(input$daterange[2], "%Y%m%d"), ".csv")
    },
    content = function(file){
      syms <- input$watchlist
      if (length(syms) == 0) stop("No symbols selected.")
      dr <- input$daterange
      series <- purrr::map(syms, function(s) {
        x <- try(fetch_yahoo(s, dr[1], dr[2]), silent = TRUE)
        if (inherits(x, "try-error") || is.null(x)) return(NULL)
        cl <- Cl(x)
        tibble(Symbol = s, Date = as.Date(index(cl)), Close = as.numeric(cl))
      })
      series <- purrr::compact(series)
      if (!length(series)) stop("No series available.")
      df <- bind_rows(series)
      write.csv(df, file, row.names = FALSE)
    }
  )

  output$dl_table <- downloadHandler(
    filename = function(){
      paste0(gsub("\\.NS$","", input$company), "_table_last_", input$tbl_days %||% 15, "d.csv")
    },
    content = function(file){
      d <- table_df(); validate(need(nrow(d) > 0, "No data to download."))
      write.csv(d, file, row.names = FALSE)
    }
  )

  output$dl_readme <- downloadHandler(
    filename = function(){ "README.md" },
    content = function(file){ writeLines(readme_md, con = file) }
  )
}

shinyApp(ui, server)
