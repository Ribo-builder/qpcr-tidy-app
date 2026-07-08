library(shiny)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(ggplot2)
library(plotly)
library(openxlsx)

# ── parser ───────────────────────────────────────────────────────────────────
parse_qpcr_txt <- function(path) {
  lines <- readLines(path, encoding = "UTF-8")
  well_line_idx <- which(str_detect(lines, "^\\t+[A-H]\\d+,"))[1]
  if (is.na(well_line_idx))
    well_line_idx <- which(str_detect(lines, ", SYBR"))[1]

  well_raw <- str_split(lines[well_line_idx], "\t")[[1]]
  wells <- well_raw[well_raw != ""] %>%
    str_remove_all('"') %>%
    str_extract("^[A-H]\\d+") %>%
    .[!is.na(.)]

  data_start <- well_line_idx + 2
  data_lines <- lines[data_start:length(lines)]
  data_lines <- data_lines[data_lines != ""]

  mat <- lapply(data_lines, function(l) {
    vals <- str_split(l, "\t")[[1]]
    as.numeric(vals[vals != ""])
  })
  mat <- do.call(rbind, mat)

  n_cols    <- ncol(mat)
  cycle_col <- which(sapply(1:min(3, n_cols), function(i) {
    !any(is.na(mat[1:5, i])) && mat[1, i] == 1
  }))[1]
  if (is.na(cycle_col)) cycle_col <- 1

  cycles    <- mat[, cycle_col]
  fluor_mat <- mat[, setdiff(seq_len(n_cols), cycle_col), drop = FALSE]
  if (ncol(fluor_mat) > length(wells))
    fluor_mat <- fluor_mat[, seq_along(wells), drop = FALSE]

  df <- as.data.frame(fluor_mat)
  colnames(df) <- wells
  df <- cbind(Cycles = cycles, df)
  df <- df[!is.na(df$Cycles), ]
  list(df = df, wells = wells)
}

# ── palette ──────────────────────────────────────────────────────────────────
COLORS <- c("#3b82f6","#ef4444","#10b981","#f59e0b","#8b5cf6",
            "#06b6d4","#ec4899","#84cc16","#f97316","#6366f1",
            "#14b8a6","#e11d48","#0ea5e9","#a3e635","#d946ef")

# ── Excel chart injection ─────────────────────────────────────────────────────
# Embeds a native line chart (±SD error bars) into a regular worksheet
# so it appears as a chart object sitting on top of cells — like normal Excel.
excel_col <- function(n) {
  if (n <= 26) return(LETTERS[n])
  paste0(LETTERS[(n - 1L) %/% 26L], LETTERS[(n - 1L) %% 26L + 1L])
}
xml_esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;",  x, fixed = TRUE)
  x <- gsub(">", "&gt;",  x, fixed = TRUE)
  x
}

inject_excel_chart <- function(xlsx_path, groups, n_rows) {
  tmpdir <- tempfile("xlchart_")
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  utils::unzip(xlsx_path, exdir = tmpdir)

  xl <- file.path(tmpdir, "xl")
  n  <- length(groups)
  nr <- n_rows + 1L

  # ── series XML ──────────────────────────────────────────────────────────────
  ser_xml <- paste(sapply(seq_len(n), function(i) {
    g   <- groups[[i]]
    mc  <- excel_col(1L + (i - 1L) * 2L + 1L)
    sc  <- excel_col(1L + (i - 1L) * 2L + 2L)
    hex <- toupper(gsub("^#", "", g$color))
    sprintf(
      '<c:ser>
        <c:idx val="%d"/><c:order val="%d"/>
        <c:tx><c:strRef><c:f>Data!$%s$1</c:f>
          <c:strCache><c:ptCount val="1"/>
            <c:pt idx="0"><c:v>%s</c:v></c:pt>
          </c:strCache>
        </c:strRef></c:tx>
        <c:spPr><a:ln w="25400">
          <a:solidFill><a:srgbClr val="%s"/></a:solidFill>
        </a:ln></c:spPr>
        <c:marker><c:symbol val="none"/></c:marker>
        <c:cat><c:numRef><c:f>Data!$A$2:$A$%d</c:f></c:numRef></c:cat>
        <c:val><c:numRef><c:f>Data!$%s$2:$%s$%d</c:f></c:numRef></c:val>
        <c:errBars>
          <c:errDir val="y"/><c:errBarType val="both"/>
          <c:errValType val="cust"/><c:noEndCap val="0"/>
          <c:plus><c:numRef><c:f>Data!$%s$2:$%s$%d</c:f></c:numRef></c:plus>
          <c:minus><c:numRef><c:f>Data!$%s$2:$%s$%d</c:f></c:numRef></c:minus>
        </c:errBars>
        <c:smooth val="0"/>
      </c:ser>',
      i-1L, i-1L, mc, xml_esc(g$name), hex, nr,
      mc, mc, nr, sc, sc, nr, sc, sc, nr
    )
  }), collapse = "\n")

  # ── chart1.xml ──────────────────────────────────────────────────────────────
  chart_xml <- sprintf(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<c:chartSpace xmlns:c="http://schemas.openxmlformats.org/drawingml/2006/chart"
              xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"
              xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">
  <c:lang val="en-US"/>
  <c:chart>
    <c:autoTitleDeleted val="1"/>
    <c:plotArea>
      <c:layout/>
      <c:lineChart>
        <c:grouping val="standard"/>
        <c:varyColors val="0"/>
        %s
        <c:axId val="100"/><c:axId val="200"/>
      </c:lineChart>
      <c:catAx>
        <c:axId val="100"/>
        <c:scaling><c:orientation val="minMax"/></c:scaling>
        <c:delete val="0"/><c:axPos val="b"/>
        <c:title><c:tx><c:rich><a:bodyPr/><a:lstStyle/>
          <a:p><a:r><a:rPr lang="en-US" b="1"/><a:t>Cycle</a:t></a:r></a:p>
        </c:rich></c:tx><c:overlay val="0"/></c:title>
        <c:numFmt formatCode="General" sourceLinked="1"/>
        <c:tickLblPos val="nextTo"/><c:crossAx val="200"/>
      </c:catAx>
      <c:valAx>
        <c:axId val="200"/>
        <c:scaling><c:orientation val="minMax"/></c:scaling>
        <c:delete val="0"/><c:axPos val="l"/>
        <c:title><c:tx><c:rich><a:bodyPr/><a:lstStyle/>
          <a:p><a:r><a:rPr lang="en-US" b="1"/><a:t>Fluorescence (R)</a:t></a:r></a:p>
        </c:rich></c:tx><c:overlay val="0"/></c:title>
        <c:numFmt formatCode="General" sourceLinked="1"/>
        <c:tickLblPos val="nextTo"/><c:crossAx val="100"/>
      </c:valAx>
    </c:plotArea>
    <c:legend><c:legendPos val="r"/><c:overlay val="0"/></c:legend>
    <c:plotVisOnly val="1"/>
  </c:chart>
</c:chartSpace>', ser_xml)

  # ── drawing1.xml — twoCellAnchor so chart sits on cells ────────────────────
  drawing_xml <-
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<xdr:wsDr xmlns:xdr="http://schemas.openxmlformats.org/drawingml/2006/spreadsheetDrawing"
          xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"
          xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
          xmlns:c="http://schemas.openxmlformats.org/drawingml/2006/chart">
  <xdr:twoCellAnchor moveWithCells="0" sizeWithCells="0">
    <xdr:from><xdr:col>0</xdr:col><xdr:colOff>152400</xdr:colOff>
              <xdr:row>0</xdr:row><xdr:rowOff>152400</xdr:rowOff></xdr:from>
    <xdr:to>  <xdr:col>14</xdr:col><xdr:colOff>0</xdr:colOff>
              <xdr:row>28</xdr:row><xdr:rowOff>0</xdr:rowOff></xdr:to>
    <xdr:graphicFrame macro="">
      <xdr:nvGraphicFramePr>
        <xdr:cNvPr id="2" name="Chart 1"/>
        <xdr:cNvGraphicFramePr/>
      </xdr:nvGraphicFramePr>
      <xdr:xfrm><a:off x="0" y="0"/><a:ext cx="0" cy="0"/></xdr:xfrm>
      <a:graphic>
        <a:graphicData uri="http://schemas.openxmlformats.org/drawingml/2006/chart">
          <c:chart r:id="rId1"/>
        </a:graphicData>
      </a:graphic>
    </xdr:graphicFrame>
    <xdr:clientData/>
  </xdr:twoCellAnchor>
</xdr:wsDr>'

  # ── Plot worksheet (regular sheet, chart sits on top of cells) ──────────────
  ws_dir   <- file.path(xl, "worksheets")
  existing <- list.files(ws_dir, pattern = "^sheet\\d+\\.xml$")
  next_num <- max(as.integer(str_extract(existing, "\\d+")), na.rm = TRUE) + 1L
  ws_name  <- sprintf("sheet%d.xml", next_num)

  plot_ws_xml <-
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main"
           xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">
  <sheetViews><sheetView workbookViewId="0"/></sheetViews>
  <sheetData/>
  <drawing r:id="rId1"/>
</worksheet>'

  # ── write files ─────────────────────────────────────────────────────────────
  for (d in c(
    file.path(xl, "charts"),
    file.path(xl, "drawings"),  file.path(xl, "drawings",  "_rels"),
    file.path(ws_dir, "_rels")
  )) dir.create(d, showWarnings = FALSE, recursive = TRUE)

  writeLines(chart_xml,    file.path(xl, "charts",   "chart1.xml"))
  writeLines(drawing_xml,  file.path(xl, "drawings", "drawing1.xml"))
  writeLines(plot_ws_xml,  file.path(ws_dir, ws_name))

  # drawing → chart
  writeLines(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">
  <Relationship Id="rId1"
    Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/chart"
    Target="../charts/chart1.xml"/>
</Relationships>',
    file.path(xl, "drawings", "_rels", "drawing1.xml.rels"))

  # Plot worksheet → drawing
  writeLines(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">
  <Relationship Id="rId1"
    Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/drawing"
    Target="../drawings/drawing1.xml"/>
</Relationships>',
    file.path(ws_dir, "_rels", paste0(ws_name, ".rels")))

  # ── [Content_Types].xml ─────────────────────────────────────────────────────
  ct_path <- file.path(tmpdir, "[Content_Types].xml")
  ct <- paste(readLines(ct_path, warn = FALSE), collapse = "\n")
  ct <- sub("</Types>", paste0(
    sprintf('<Override PartName="/xl/worksheets/%s"', ws_name),
    ' ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/>',
    '<Override PartName="/xl/charts/chart1.xml"',
    ' ContentType="application/vnd.openxmlformats-officedocument.drawingml.chart+xml"/>',
    '<Override PartName="/xl/drawings/drawing1.xml"',
    ' ContentType="application/vnd.openxmlformats-officedocument.drawing+xml"/>',
    "</Types>"
  ), ct, fixed = TRUE)
  writeLines(ct, ct_path)

  # ── workbook.xml ─────────────────────────────────────────────────────────────
  wb_path <- file.path(xl, "workbook.xml")
  wb <- paste(readLines(wb_path, warn = FALSE), collapse = "\n")
  wb <- sub("</sheets>",
    '<sheet name="Plot" sheetId="99" r:id="rId99"/></sheets>', wb, fixed = TRUE)
  writeLines(wb, wb_path)

  # ── workbook.xml.rels ────────────────────────────────────────────────────────
  rels_path <- file.path(xl, "_rels", "workbook.xml.rels")
  rels <- paste(readLines(rels_path, warn = FALSE), collapse = "\n")
  rels <- sub("</Relationships>", paste0(
    '<Relationship Id="rId99"',
    ' Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet"',
    sprintf(' Target="worksheets/%s"/>', ws_name),
    "</Relationships>"
  ), rels, fixed = TRUE)
  writeLines(rels, rels_path)

  # ── rezip ────────────────────────────────────────────────────────────────────
  out <- tempfile(fileext = ".xlsx")
  owd <- getwd(); setwd(tmpdir)
  utils::zip(out, list.files(".", recursive = TRUE, all.files = TRUE), flags = "-r9Xq")
  setwd(owd)
  file.copy(out, xlsx_path, overwrite = TRUE)
  invisible(xlsx_path)
}

# ── UI ───────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600&display=swap');
      *, body { font-family: 'Inter', Arial, sans-serif; }
      body { background: #f1f5f9; color: #1e293b; margin: 0; }

      .app-header {
        background: white; border-bottom: 1px solid #e2e8f0;
        padding: 18px 28px 14px; margin-bottom: 20px;
      }
      .app-header h2 { margin:0; font-size:20px; font-weight:600; color:#0f172a; }
      .app-header .subtitle { font-size:12px; color:#94a3b8; margin-top:3px; }

      .card {
        background: white; border: 1px solid #e2e8f0;
        border-radius: 10px; padding: 18px; margin-bottom: 14px;
      }
      .card-title {
        font-size:11px; font-weight:600; letter-spacing:0.8px;
        text-transform:uppercase; color:#94a3b8; margin:0 0 12px 0;
      }

      .shiny-input-container { margin-bottom: 0 !important; }
      .btn-file { background:#f8fafc !important; border-color:#cbd5e1 !important;
                  color:#475569 !important; font-size:13px !important; }
      input[type=text].form-control {
        background:white; border:1px solid #cbd5e1; color:#0f172a;
        border-radius:6px; font-size:13px; padding:7px 11px;
      }
      input[type=text].form-control:focus {
        border-color:#3b82f6; box-shadow:0 0 0 2px rgba(59,130,246,0.2); outline:none;
      }
      label { font-size:12px; color:#64748b; margin-bottom:4px; }

      .btn-assign {
        width:100%; background:#2563eb; border:none; color:white;
        font-weight:600; font-size:13px; border-radius:7px; padding:9px;
      }
      .btn-assign:hover { background:#1d4ed8; color:white; }

      .btn-ghost {
        width:100%; background:transparent; border:1px solid #e2e8f0;
        color:#94a3b8; font-size:12px; border-radius:7px; padding:7px;
        margin-top:6px;
      }
      .btn-ghost:hover { background:#f8fafc; color:#475569; }

      .btn-danger-ghost {
        width:100%; background:transparent; border:1px solid #fecaca;
        color:#f87171; font-size:12px; border-radius:7px; padding:7px; margin-top:6px;
      }
      .btn-danger-ghost:hover { background:#fef2f2; color:#ef4444; }

      .export-row { display:flex; gap:8px; margin-top:4px; }
      .btn-export-csv {
        flex:1; background:#0f172a; border:none; color:white;
        font-weight:600; font-size:12px; border-radius:7px; padding:8px 4px;
      }
      .btn-export-csv:hover { background:#1e293b; color:white; }
      .btn-export-xlsx {
        flex:1; background:#059669; border:none; color:white;
        font-weight:600; font-size:12px; border-radius:7px; padding:8px 4px;
      }
      .btn-export-xlsx:hover { background:#047857; color:white; }

      .kbd-hint { font-size:11px; color:#94a3b8; margin-top:5px; text-align:center; }
      kbd { background:#f1f5f9; border:1px solid #cbd5e1; border-radius:3px;
            padding:1px 5px; font-size:10px; color:#64748b; }

      .plate-row { display:flex; align-items:center; margin-bottom:3px; }
      .row-label { width:18px; font-size:12px; font-weight:600; color:#64748b; }
      .col-labels { display:flex; margin-left:20px; }
      .col-label { width:48px; text-align:center; font-size:10px; color:#94a3b8; margin:0 2px; }

      .well-btn {
        width:48px; height:38px; margin:2px; font-size:10px; font-weight:500;
        border-radius:6px; border:1.5px solid #e2e8f0; background:white; color:#94a3b8;
        cursor:pointer; transition:background 0.1s, border-color 0.1s, color 0.1s;
        user-select:none;
      }
      .well-btn:hover { border-color:#3b82f6 !important; }

      .group-tag {
        display:inline-flex; align-items:center; gap:6px;
        padding:4px 12px; margin:3px; border-radius:20px; font-size:12px; font-weight:500;
      }
      .group-tag .wells-sub { font-size:10px; opacity:0.75; }

      .nav-tabs { border-bottom: 1px solid #e2e8f0; margin-bottom:14px; }
      .nav-tabs > li > a {
        font-size:12px; font-weight:500; color:#64748b;
        border:none !important; border-bottom:2px solid transparent !important;
        padding:8px 14px; border-radius:0;
      }
      .nav-tabs > li.active > a, .nav-tabs > li > a:hover {
        color:#1e293b !important; background:transparent !important;
        border-bottom-color:#3b82f6 !important;
      }
      .plot-controls { display:flex; gap:16px; align-items:center; margin-bottom:10px; }
      .plot-controls label { margin:0; }

      table.shiny-table { border-collapse:collapse; font-size:12px; width:100%; color:#334155; }
      table.shiny-table th {
        background:#f8fafc; color:#64748b; font-weight:600;
        padding:7px 10px; border-bottom:1px solid #e2e8f0; font-size:11px;
      }
      table.shiny-table td { padding:5px 10px; border-bottom:1px solid #f8fafc; }
    ")),
    tags$script(HTML("
      $(document).on('keydown', '#name_input', function(e) {
        if ((e.metaKey || e.ctrlKey) && e.key === 'Enter') {
          e.preventDefault(); $('#assign_btn').click();
        }
      });
      $(document).on('click', '.well-btn', function() {
        Shiny.setInputValue('well_click', $(this).data('well'), {priority: 'event'});
      });
      Shiny.addCustomMessageHandler('well_state', function(msg) {
        var btn = document.getElementById('well_' + msg.well);
        if (!btn) return;
        if (msg.state === 'selected') {
          btn.style.background = '#2563eb'; btn.style.borderColor = '#60a5fa'; btn.style.color = 'white';
        } else if (msg.state === 'assigned') {
          btn.style.background = msg.color; btn.style.borderColor = msg.color; btn.style.color = 'white';
        } else {
          btn.style.background = 'white'; btn.style.borderColor = '#e2e8f0'; btn.style.color = '#94a3b8';
        }
      });
    "))
  ),

  div(class = "app-header",
    h2("qPCR → Tidy CSV"),
    div(class = "subtitle", "Format 2 txt → well grouping → CSV / Excel")
  ),

  fluidRow(
    column(3,
      div(class = "card",
        p(class = "card-title", "1 · Load"),
        fileInput("file", NULL, accept = ".txt",
          buttonLabel = "Select TXT", placeholder = "no file selected")
      ),
      div(class = "card",
        p(class = "card-title", "2 · Name wells"),
        p(style = "font-size:11px;color:#94a3b8;margin-bottom:10px;",
          "Click wells → type name → Add"),
        textInput("name_input", NULL, placeholder = "e.g. ND-Amils_modified"),
        actionButton("assign_btn", "グループに追加", class = "btn-assign"),
        div(class = "kbd-hint", tags$kbd("⌘"), "+", tags$kbd("↵"), " でも追加"),
        actionButton("clear_sel_btn", "選択解除",         class = "btn-ghost"),
        actionButton("undo_btn",      "⟵ 最後を取り消す", class = "btn-danger-ghost")
      ),
      div(class = "card",
        p(class = "card-title", "3 · Export"),
        p(style = "font-size:11px;color:#94a3b8;margin-bottom:10px;",
          "未割り当てウェルは除外されます"),
        div(class = "export-row",
          downloadButton("download_csv",  "CSV",   class = "btn-export-csv"),
          downloadButton("download_xlsx", "Excel", class = "btn-export-xlsx")
        )
      )
    ),

    column(9,
      div(class = "card",
        tabsetPanel(
          tabPanel("Plate", br(), uiOutput("plate_ui")),
          tabPanel("Plot",
            br(),
            div(class = "plot-controls",
              checkboxInput("show_mean", "平均線",      value = TRUE),
              checkboxInput("show_rep",  "個別rep",     value = TRUE),
              checkboxInput("show_sd",   "エラーバー",  value = TRUE),
              checkboxInput("log_y",     "Y軸 log10",   value = FALSE)
            ),
            plotlyOutput("fluor_plot", height = "430px")
          ),
          tabPanel("Preview", br(), tableOutput("preview_table"))
        )
      ),
      div(class = "card",
        p(class = "card-title", "Groups"),
        uiOutput("groups_ui")
      )
    )
  )
)

# ── server ───────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  rv <- reactiveValues(
    df = NULL, wells = character(0),
    selected = character(0), groups = list(), assigned = character(0)
  )

  well_msg <- function(w) {
    if (w %in% rv$selected) {
      session$sendCustomMessage("well_state", list(well=w, state="selected", color=NULL))
    } else if (w %in% rv$assigned) {
      color <- "#94a3b8"
      for (g in rv$groups) if (w %in% g$wells) { color <- g$color; break }
      session$sendCustomMessage("well_state", list(well=w, state="assigned", color=color))
    } else {
      session$sendCustomMessage("well_state", list(well=w, state="unassigned", color=NULL))
    }
  }

  observeEvent(input$file, {
    req(input$file)
    tryCatch({
      p <- parse_qpcr_txt(input$file$datapath)
      rv$df <- p$df; rv$wells <- p$wells
      rv$selected <- character(0); rv$groups <- list(); rv$assigned <- character(0)
      showNotification(paste0(length(p$wells), " wells loaded"), type="message", duration=2)
    }, error = function(e) showNotification(paste("Error:", e$message), type="error"))
  })

  observeEvent(input$well_click, {
    w <- input$well_click
    req(w %in% rv$wells)
    if (w %in% rv$assigned) return()
    if (w %in% rv$selected) rv$selected <- setdiff(rv$selected, w)
    else                     rv$selected <- c(rv$selected, w)
    well_msg(w)
  })

  observeEvent(input$clear_sel_btn, {
    prev <- rv$selected; rv$selected <- character(0)
    for (w in prev) well_msg(w)
  })

  observeEvent(input$assign_btn, {
    req(length(rv$selected) > 0, nchar(trimws(input$name_input)) > 0)
    name  <- trimws(input$name_input)
    color <- COLORS[((length(rv$groups)) %% length(COLORS)) + 1]
    wtd   <- rv$selected
    rv$groups   <- c(rv$groups, list(list(name=name, wells=wtd, color=color)))
    rv$assigned <- c(rv$assigned, wtd)
    rv$selected <- character(0)
    for (w in wtd) well_msg(w)
    updateTextInput(session, "name_input", value = "")
  })

  observeEvent(input$undo_btn, {
    req(length(rv$groups) > 0)
    last <- rv$groups[[length(rv$groups)]]
    rv$groups   <- rv$groups[-length(rv$groups)]
    rv$assigned <- setdiff(rv$assigned, last$wells)
    for (w in last$wells) well_msg(w)
  })

  output$plate_ui <- renderUI({
    req(rv$wells)
    wells    <- rv$wells
    rows     <- sort(unique(str_extract(wells, "^[A-H]")))
    cols_all <- sort(unique(as.integer(str_extract(wells, "\\d+"))))
    col_labels <- div(class="col-labels",
      lapply(cols_all, function(c) div(class="col-label", as.character(c))))
    tagList(col_labels, lapply(rows, function(r) {
      div(class="plate-row", div(class="row-label", r),
        lapply(cols_all, function(c) {
          wname <- paste0(r, c)
          if (wname %in% wells)
            tags$button(wname, id=paste0("well_",wname), class="well-btn", `data-well`=wname)
          else
            div(class="well-btn", style="visibility:hidden;")
        })
      )
    }))
  })

  output$groups_ui <- renderUI({
    if (length(rv$groups) == 0)
      return(p(style="color:#94a3b8;font-size:13px;","No groups yet"))
    do.call(tagList, lapply(rv$groups, function(g) {
      span(class="group-tag",
        style=sprintf("background:%s22;border:1px solid %s;color:%s;",g$color,g$color,g$color),
        g$name, span(class="wells-sub", paste0("(", paste(g$wells, collapse=", "), ")"))
      )
    }))
  })

  # ── data reactives ──────────────────────────────────────────────────────────
  make_long_df <- reactive({
    req(rv$df, length(rv$groups) > 0)
    bind_rows(lapply(rv$groups, function(g) {
      bind_rows(lapply(seq_along(g$wells), function(i)
        data.frame(Cycles=rv$df$Cycles, fluor=rv$df[[g$wells[i]]],
                   condition=g$name, rep=i, color=g$color, stringsAsFactors=FALSE)
      ))
    }))
  })

  make_summ_df <- reactive({
    req(make_long_df())
    df   <- make_long_df()
    summ <- df %>%
      group_by(condition, Cycles) %>%
      summarise(mean_fluor=mean(fluor,na.rm=TRUE), sd_fluor=sd(fluor,na.rm=TRUE),
                .groups="drop") %>%
      mutate(sd_fluor=ifelse(is.na(sd_fluor), 0, sd_fluor))

    result <- data.frame(Cycles = sort(unique(df$Cycles)))
    for (g in rv$groups) {
      sub <- summ %>% filter(condition == g$name) %>% arrange(Cycles)
      result[[paste0(g$name, "_mean")]] <- sub$mean_fluor
      result[[paste0(g$name, "_sd")]]   <- sub$sd_fluor
    }
    result
  })

  make_wide_df <- reactive({
    req(rv$df, length(rv$groups) > 0)
    result <- data.frame(Cycles = rv$df$Cycles)
    for (g in rv$groups)
      for (i in seq_along(g$wells))
        result[[paste0(g$name, "_", i)]] <- rv$df[[g$wells[i]]]
    result
  })

  # ── Plotly preview (Excel-like) ─────────────────────────────────────────────
  output$fluor_plot <- renderPlotly({
    req(make_long_df())
    df      <- make_long_df()
    summ    <- make_summ_df()
    cmap_df <- df %>% distinct(condition, color)
    cmap    <- setNames(cmap_df$color, cmap_df$condition)

    # rebuild summ in long form for plotly
    summ_long <- df %>%
      group_by(condition, Cycles) %>%
      summarise(mean_fluor=mean(fluor,na.rm=TRUE), sd_fluor=sd(fluor,na.rm=TRUE),
                .groups="drop") %>%
      mutate(sd_fluor=ifelse(is.na(sd_fluor),0,sd_fluor))

    p <- plot_ly()

    for (cond in sapply(rv$groups, `[[`, "name")) {
      col   <- cmap[[cond]]
      col_a <- paste0(col, "66")  # semi-transparent for reps

      # individual rep lines
      if (input$show_rep) {
        rep_data <- df %>% filter(condition == cond)
        for (r in unique(rep_data$rep)) {
          rd <- rep_data %>% filter(rep == r)
          p <- add_trace(p, data=rd, x=~Cycles, y=~fluor,
                         type="scatter", mode="lines",
                         line=list(color=col_a, width=1),
                         showlegend=FALSE, legendgroup=cond,
                         hoverinfo="skip")
        }
      }

      # mean line ± error bars
      s <- summ_long %>% filter(condition == cond)
      eb <- if (input$show_sd) list(type="data", symmetric=TRUE,
                                     array=s$sd_fluor, color=col,
                                     thickness=1.5, width=4, visible=TRUE)
             else               list(visible=FALSE)

      if (input$show_mean)
        p <- add_trace(p, data=s, x=~Cycles, y=~mean_fluor,
                       type="scatter", mode="lines",
                       line=list(color=col, width=2.5),
                       error_y=eb,
                       name=cond, legendgroup=cond, showlegend=TRUE)
    }

    p %>% layout(
      xaxis  = list(title="Cycle", gridcolor="#e2e8f0", linecolor="#cbd5e1",
                    zeroline=FALSE),
      yaxis  = list(title="Fluorescence (R)", gridcolor="#e2e8f0",
                    linecolor="#cbd5e1", zeroline=FALSE,
                    type=if(input$log_y) "log" else "linear"),
      paper_bgcolor = "white", plot_bgcolor = "white",
      legend = list(orientation="v", x=1.02, y=0.5,
                    font=list(size=11), bgcolor="rgba(0,0,0,0)"),
      margin = list(l=60, r=20, t=20, b=50),
      hovermode = "x unified"
    ) %>% config(displayModeBar=FALSE)
  })

  output$preview_table <- renderTable({
    req(make_wide_df()); head(make_wide_df(), 5)
  }, digits=2)

  # ── CSV export ──────────────────────────────────────────────────────────────
  output$download_csv <- downloadHandler(
    filename = function() paste0(tools::file_path_sans_ext(input$file$name), "_tidy.csv"),
    content  = function(file) write_csv(make_wide_df(), file)
  )

  # ── Excel export: Data sheet (mean±SD) + native chart on worksheet ──────────
  output$download_xlsx <- downloadHandler(
    filename = function() paste0(tools::file_path_sans_ext(input$file$name), "_tidy.xlsx"),
    content  = function(file) {
      summ            <- make_summ_df()
      groups_snapshot <- rv$groups

      wb <- createWorkbook()
      addWorksheet(wb, "Data")
      writeData(wb, "Data", summ)
      addStyle(wb, "Data",
               createStyle(fontColour="#FFFFFF", bgFill="#1e293b", fontName="Arial",
                           fontSize=10, textDecoration="bold", halign="center",
                           border="Bottom", borderColour="#3b82f6"),
               rows=1, cols=1:ncol(summ), gridExpand=TRUE)
      addStyle(wb, "Data",
               createStyle(fontName="Arial", fontSize=10, numFmt="0.00"),
               rows=2:(nrow(summ)+1), cols=1:ncol(summ), gridExpand=TRUE)
      setColWidths(wb, "Data", cols=1:ncol(summ), widths="auto")
      saveWorkbook(wb, file, overwrite=TRUE)

      tryCatch(
        inject_excel_chart(file, groups_snapshot, nrow(summ)),
        error = function(e)
          showNotification(paste("Chart injection failed:", e$message), type="warning")
      )
    }
  )
}

shinyApp(ui, server)
