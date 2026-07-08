# qpcr-tidy-app

Interactive Shiny app that converts raw qPCR machine output (Format 2 TXT) into a tidy CSV ready for downstream R analysis.

## What it does

1. Upload the TXT file exported from the qPCR machine (Format 2)
2. Click wells on the plate layout to select replicates
3. Type a sample name → **グループに追加** (or **⌘+Enter**)
4. Download a tidy CSV with columns `Cycles, SampleName_1, SampleName_2, ...`

## Installation

```r
install.packages(c("shiny", "dplyr", "tidyr", "readr", "stringr"))
```

```bash
git clone https://github.com/Ribo-builder/qpcr-tidy-app.git
cd qpcr-tidy-app
```

## Usage

```r
setwd("path/to/qpcr-tidy-app")
shiny::runApp("qpcr_tidy_app", launch.browser = TRUE)
```

Or open `run_app.R` in RStudio and click **Run**. The app launches at `http://127.0.0.1:5815/`.

## Supported file formats

| Format | Example first line |
|--------|--------------------|
| Format 2 with header | `Amplification Plots` → wells on line 3 |
| Format 2 without header (quoted) | `"B3, SYBR"  "B4, SYBR"  ...` |

Both are detected automatically.
