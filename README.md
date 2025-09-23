Here’s a clean, copy-paste **README.md** for your repo.

````markdown
# NIFTY 50 — Real-Time & Historical (quantmod + Yahoo, interactive)

A Shiny app for Indian equities (NIFTY 50) that fetches real-time quotes and historical data from Yahoo Finance via **quantmod** and renders interactive charts with **plotly**.

## 🚀 Quick start

Run directly from GitHub in R / RStudio:

```r
options(nifty50.autoinstall = FALSE)           # skip auto-install block (recommended)
shiny::runGitHub("nifty50_r_app", "Pulkit-123")
````

> If you want the app to auto-install missing packages, omit the `options(...)` line.

## 💡 Features

* Interactive candlesticks with green (up) / red (down) bars, volume panel, zoom/hover
* Real-time quote badge with auto-refresh and manual refresh
* Dual-axis **Stock vs NIFTY** view or normalized %-change view
  (baseline = start / N days ago / specific date)
* Watchlist: **overlay** or **stacked** small multiples (shared X axis in stacked)
* Risk metrics vs NIFTY: β, correlation, R², annualized vol, max drawdown, tracking error, info ratio
* Data Table with rich filters: weekday/month, up days, price/volume/ATR/gap ranges, inside/outside bars, column picker
* CSV downloads for historical prices, comparisons, watchlist, and data table
* Theming (bslib), caching, debounced inputs, alerting rules

## 🧰 Requirements

* R ≥ 4.1
* Packages: `shiny`, `bslib`, `quantmod`, `xts`, `dplyr`, `lubridate`, `plotly`, `TTR`,
  `memoise`, `shinycssloaders`, `scales`, `purrr`, `stringr`, `DT` (and optionally `nser`)

Install manually (if you don’t use auto-install):

```r
install.packages(c(
  "shiny","bslib","quantmod","xts","dplyr","lubridate","plotly","TTR",
  "memoise","shinycssloaders","scales","purrr","stringr","DT"
))
```

## 🏃‍♀️ Run locally (clone)

```r
# In RStudio: File -> New Project -> From Version Control -> Git
# URL: https://github.com/Pulkit-123/nifty50_r_app.git
# Then:
options(nifty50.autoinstall = FALSE)   # or TRUE to auto-install on first run
shiny::runApp("path/to/nifty50_r_app")
```

## ⚙️ Config

* **Auto-install packages** (default `TRUE` inside the app). To skip at runtime:

```r
options(nifty50.autoinstall = FALSE)
```

* Timezone set to **Asia/Kolkata** by default.

## 📦 Data source & disclaimer

* Prices via **Yahoo Finance** through `quantmod::getSymbols("SYMBOL", src = "yahoo")`.
* Data/quotes may be delayed or adjusted. This app is for **educational/demo** purposes and is **not investment advice**.

## 🖼️ Screenshots

(Add screenshots of *Overview*, *Stock vs Nifty*, *Watchlist*, and *Data Table* tabs here.)

---

**Author:** Pulkit
**License:** MIT (or add your preferred license)

```
::contentReference[oaicite:0]{index=0}
```
