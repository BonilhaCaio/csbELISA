# csbELISA ----
#' Analyse and visualize ELISA data
#'
#' @description
#' The csbELISA R package is an open-source project that provides a
#' user-friendly, interactive UI for automated ELISA data processing,
#' standard curve fitting, concentration estimation, visualization,
#' and statistical analysis.
#'
#' @docType package
#' @name csbELISA
NULL

#' Run the csbELISA application
#'
#' @description
#' runCsbELISA() launches the csbELISA Shiny application.
#'
#' @details
#' Use runCsbELISA() at the R console to initiate the csbELISA UI.
#'
#' @export
#'
runCsbELISA <- function() {
  shinyApp(
    ui = ui,
    server = server,
    onStart = function() {
      shinyjs::useShinyjs()
    }
  )
}

# Imports ----------------------------------------------------------------
#' @import shiny
#' @import ggplot2
#' @import DT
#' @import jsonlite
#' @import fontawesome

#' @importFrom shinyjs useShinyjs
#' @importFrom shinyjs enable
#' @importFrom shinyjs disable
#' @importFrom shinyjs runjs
#' @importFrom shinyjs addClass
#' @importFrom shinyjs removeClass
#' @importFrom colourpicker colourInput
#' @importFrom colourpicker updateColourInput
#' @importFrom stats aov
#' @importFrom stats kruskal.test
#' @importFrom stats pairwise.wilcox.test
#' @importFrom stats shapiro.test
#' @importFrom stats t.test
#' @importFrom stats lm
#' @importFrom stats sd

#' @importFrom utils read.csv
#' @importFrom utils write.csv

#' @importFrom grDevices png
#' @importFrom grDevices dev.off
#' @importFrom base64enc dataURI
#' @importFrom stats TukeyHSD aggregate coefficients setNames wilcox.test
#' @importFrom utils combn
NULL

NULL

utils::globalVariables(c(
  "Group",
  "Concentration",
  "Concentration.mean",
  "Concentration.sd",
  "Concentration.sem",
  "log_od",
  "log_conc",
  "x1",
  "x2",
  "y",
  "label"
))

css_tabs <- "
body { background-color: #ffffff; }
.nav-tabs { border-bottom: 1px solid #000000; }
.nav-tabs > li > a { color: #000000 !important; border: 1px solid transparent; }
.nav-tabs > li.active > a,
.nav-tabs > li.active > a:hover,
.nav-tabs > li.active > a:focus {
  color: #000000 !important;
  border: 1px solid #000000;
  border-bottom-color: transparent;
}
.colourpicker input {
  border: 1px solid #000000 !important;
}
.btn-primary {
  background-color: #000000 !important;
  border-color: #000000 !important;
  color: #ffffff !important;
}
.nav-tabs > li > a.disabled {
  pointer-events: none;
  color: #bdbdbd !important;
  cursor: not-allowed;
}
"
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML(css_tabs)),
    tags$style(HTML("
    #about {
      border-color: white !important;
      box-shadow: none !important;
      font-size: 12px;
    }
    .modal-content {
      border: none !important;
      box-shadow: none !important;
      font-size: 12px;
    }
  "))
  ),

  headerPanel(
    fluidRow(
      column(
        width = 3,
        tags$div(
          style = "display:flex; align-items:center; gap:8px;",
          tags$img(
            src = base64enc::dataURI(
              file = system.file("icons", "Logo.png", package = "csbELISA"),
              mime = "image/png"
            ),
            height = "50px"
          ),
          tags$span(
            "csbELISA",
            style = "font-weight:600; font-size:30px;"
          )
        )
      ),
      column(
        width=2, offset=7, align="right",
        actionButton(inputId="about", label="About csbELISA")
      )
    ),
  ),

  tabsetPanel(
    id = "main_tabs",

    #### TAB 1 - FILE
    tabPanel(
      title = tagList(icon("cloud-upload-alt"), "File"),
      value = "file_tab",
      div(
        style="display:flex;flex-direction:column;justify-content:center;align-items:center;height:70vh;",
        fileInput("csv_file", NULL,
                  buttonLabel = tagList(icon("file-upload"), "Load excel file"),
                  accept = ".csv"),
        br(),
        textOutput("file_status")
      )
    ),

    #### TAB 2 - STANDARD SELECTION
    tabPanel(
      title = tagList(icon("table"), "Standard selection"),
      value = "standard_tab",
      br(),
      actionButton("standard_step_btn", "Assign blank", class = "btn-default"),
      br(), br(),
      DTOutput("standard_table")
    ),

    #### TAB 3 - STANDARD CURVE
    tabPanel(
      title = tagList(icon("chart-line"), "Standard curve"),
      value = "curve_tab",
      br(),
      fluidRow(
        column(
          3,
          div(
            style="border:1px solid #000000;padding:10px;",
            numericInput("neat_conc","Neat standard concentration", NA),
            numericInput("dilution_factor","Dilution factor", NA),
            br(),
            actionButton("build_curve","Build curve",class="btn-primary"),
            br(), br(),
            downloadButton("save_curve","Save plot"),
            br(), br(), br(),
            actionButton("go_samples","Proceed",class="btn-primary")
          )
        ),
        column(9, plotOutput("curve_plot"))
      ),
    ),

    #### TAB 4 - SAMPLE SELECTION
    tabPanel(
      title = tagList(icon("table"), "Sample selection"),
      value = "sample_tab",
      br(),
      fluidRow(
        column(4, uiOutput("sample_group_name_ui")),
        column(4, actionButton("add_sample_group","Assign group")),
        column(4, actionButton("finalize_samples","Proceed",class="btn-primary"))
      ),
      br(),
      div(
        class = "group-box",
        div(
          icon("list-ul"),
          "Defined groups",
          class = "tab-title-text"
        ),
        uiOutput("sample_group_list")
      ),
      br(),
      DTOutput("sample_table")
    ),

    #### TAB 5 - RESULTS
    tabPanel(
      title = tagList(icon("calculator"), "Results"),
      value = "results_tab",
      br(),
      verbatimTextOutput("results_text"),
      br(),
      fluidRow(
        column(
          6,
          downloadButton("export_results","Export CSV",class="btn-primary")
        ),
        column(
          6,
          div(
            style = "text-align: right;",
            actionButton("go_plot", "Proceed", class = "btn-primary")
          )
        )
      )
    ),

    #### TAB 6 - PLOT
    tabPanel(
      title = tagList(icon("chart-bar"), "Plot"),
      value = "plot_tab",
      br(),
      fluidRow(
        ## LEFT COLUMN - CONTROLS
        column(
          3,
          div(
            style = "border: 1px solid #000000; padding: 10px;",
            selectInput(
              "plot_type",
              tagList(icon("chart-column"), "Type"),
              c("Bar plot", "Box plot")
            ),
            selectInput(
              "error_type",
              tagList(icon("ruler-vertical"), "Error bars"),
              c("SD", "SEM")
            ),
            checkboxInput(
              "show_points",
              tagList("Show individual points"),
              TRUE
            ),
            br(),
            selectInput(
              "x_label_angle",
              "Group name orientation",
              choices = c("Horizontal" = 0, "Inclined" = 45, "Vertical" = 90),
              selected = 0
            ),
            br(),
            uiOutput("plot_group_selector"),
            br(),
            uiOutput("plot_colour_selectors"),
            br(),
            actionButton(
              "go_stats",
              "Proceed to Stats",
              class = "btn-primary"
            )
          )
        ),
        ## MIDDLE COLUMN
        column(
          6,
          div(
            style = "display:flex;justify-content:center;align-items:center;",
            plotOutput("plot_output")
          )
        ),
        ## RIGHT COLUMN
        column(
          3,
          div(
            style = "border: 1px solid #000000; padding: 10px;",
            sliderInput("plot_width", tagList(icon("arrows-left-right")), 250, 600, 350),
            sliderInput("plot_height", tagList(icon("arrows-up-down")), 250, 600, 400),
            sliderInput("line_width", tagList(icon("pen-ruler")), 0.5, 2, 1),
            sliderInput("bar_width", tagList(icon("grip-lines-vertical")), 0.6, 1, 0.7),
            sliderInput("dot_size", tagList(icon("circle")), 3, 8, 4),
            sliderInput("text_size", tagList(icon("text-height")), 10, 30, 16)
          )
        )
      )
    ),

    #### TAB 7 - STATS
    tabPanel(
      title = tagList(icon("calculator"), "Stats"),
      value = "stats_tab",
      br(),
      fluidRow(
        ## LEFT COLUMN
        column(
          3,
          div(
            style = "border: 1px solid #000000; padding: 10px;",
            uiOutput("comparison_selector"),
            br(),
            actionButton(
              "back_to_plot",
              tagList(icon("chart-bar"), "Return to Plot")
            ),
            br(), br(),
            downloadButton("save_stats_plot", "Save plot", class = "btn-primary")
          )
        ),
        ## MIDDLE COLUMN
        column(
          6,
          div(
            style = "
          display: flex;
          justify-content: center;
          align-items: center;
          height: 100%;
        ",
            div(
              style = "width: 100%;",
              plotOutput("stats_plot")
            )
          )
        ),
        ## RIGHT COLUMN
        column(
          3,
          div(
            style = "border: 1px solid #000000; padding: 10px;",
            sliderInput(
              "sig_bar_offset",
              tagList(icon("arrows-up-down"), "Plot to bar"),
              min = 0, max = 0.2, value = 0.06, step = 0.01
            ),
            sliderInput(
              "sig_text_offset",
              tagList(icon("arrows-up-down"), "Bar to symbol"),
              min = 0, max = 0.1, value = 0.03, step = 0.005
            ),
            sliderInput(
              "sig_text_size",
              "Significance symbol size",
              min = 15, max = 50, value = 20
            )
          )
        )
      ),
      br(),
      fluidRow(
        column(
          12,
          verbatimTextOutput("stats_text")
        )
      )
    )
  )
)

server <- function(input, output, session){

  observeEvent(input$about, {
    showModal(
      modalDialog(
        title = "csbELISA",
        "This is an open-source project that provides an user-friendly UI for
      automated ELISA data analysis in R.",
        br(),
        br(),
        strong("Project website:"),
        "https://github.com/BonilhaCaio/csbELISA",
        footer = NULL,
        size = "m",
        easyClose = TRUE
      )
    )
  })

  rv <- reactiveValues(
    data_raw = NULL,
    data_work = NULL,
    blank_cells = NULL,
    standard_cells = NULL,
    locked_cells = NULL,
    step = "blank",
    curve_model = NULL,
    curve_df = NULL,
    sample_groups = list()
  )

  disable(c("standard_tab","curve_tab","sample_tab","results_tab"))
  disable(c("build_curve","save_curve","go_samples"))
  disable("finalize_samples")
  disable(c("plot_tab", "stats_tab"))
  disable("add_sample_group")

  observeEvent(input$csv_file,{
    raw <- read.csv(input$csv_file$datapath,
                    check.names = FALSE,
                    row.names = 1)
    raw[] <- lapply(raw, function(x) {
      x <- as.character(x)
      x <- gsub(",", ".", x)
      as.numeric(x)
    })
    rv$data_raw  <- raw
    rv$data_work <- raw
    output$file_status <- renderText("File loaded successfully.")
    enable("standard_tab")
    updateTabsetPanel(session,"main_tabs","standard_tab")
  })

  output$standard_table <- renderDT({
    req(rv$data_work)
    locked_js <- if (is.null(rv$locked_cells)) "[]" else
      toJSON(lapply(seq_len(nrow(rv$locked_cells)),
                    function(i) as.numeric(rv$locked_cells[i, ])),
             keep_vec_names = FALSE)
    datatable(
      rv$data_work,
      rownames = TRUE,
      selection = list(mode="multiple", target="cell"),
      options = list(
        dom = "t",
        pageLength = nrow(rv$data_work),
        ordering = FALSE,
        rowCallback = JS(sprintf(
          "function(row, data, displayIndex) {
       var locked = %s;
       locked.forEach(function(cell){
         if(cell[0] === displayIndex + 1){
           $('td:eq(' + cell[1] + ')', row).css('color','#bdbdbd');
         }
       });
     }", locked_js))
      )
    )
  })

  #### STANDARD ASSIGNMENT
  observeEvent(input$standard_step_btn,{
    if(rv$step == "blank"){
      rv$blank_cells <- input$standard_table_cells_selected
      blank_vals <- apply(rv$blank_cells,1,function(x) rv$data_work[x[1],x[2]])
      blank_mean <- mean(blank_vals, na.rm = TRUE)
      rv$data_work[,] <- round(rv$data_work[,] - blank_mean, 4)
      rv$locked_cells <- rv$blank_cells
      rv$step <- "standard"
      updateActionButton(session,"standard_step_btn","Assign standard")
    } else if(rv$step == "standard"){
      cells <- input$standard_table_cells_selected
      vals <- apply(cells,1,function(x) rv$data_work[x[1],x[2]])
      rv$standard_cells <- cells[order(vals, decreasing = TRUE), , drop = FALSE]
      rv$locked_cells <- rbind(rv$locked_cells, rv$standard_cells)
      rv$step <- "proceed"
      updateActionButton(session,"standard_step_btn","Proceed")
      addClass("standard_step_btn","btn-primary")
      removeClass("standard_step_btn","btn-default")
    } else {
      enable("curve_tab")
      updateTabsetPanel(session,"main_tabs","curve_tab")
    }
  })

  observe({
    sel <- input$standard_table_cells_selected
    if (is.null(sel)) {
      disable("standard_step_btn")
      return()
    }
    n_sel <- nrow(sel)
    if (rv$step == "blank") {
      if (n_sel >= 1) {
        enable("standard_step_btn")
      } else {
        disable("standard_step_btn")
      }
    } else if (rv$step == "standard") {
      if (n_sel >= 3) {
        enable("standard_step_btn")
      } else {
        disable("standard_step_btn")
      }
    } else {
      enable("standard_step_btn")
    }
  })

  observe({
    if (!is.na(input$neat_conc) && !is.na(input$dilution_factor)) {
      enable("build_curve")
    } else {
      disable("build_curve")
    }
  })

  observeEvent(input$build_curve,{
    st_vals <- apply(rv$standard_cells,1,function(x) rv$data_work[x[1],x[2]])
    conc <- input$neat_conc /
      (input$dilution_factor^(seq_along(st_vals)-1))
    rv$curve_df <- data.frame(
      log_od = log(st_vals),
      log_conc = log(conc)
    )
    rv$curve_model <- lm(log_conc ~ log_od, rv$curve_df)
    enable("save_curve")
    enable("go_samples")
  })

  observe({
    if (is.na(input$neat_conc) || is.na(input$dilution_factor)) {
      disable("build_curve")
      disable("save_curve")
      disable("go_samples")
    }
  })

  output$curve_plot <- renderPlot({
    req(rv$curve_model)
    co <- coefficients(rv$curve_model)
    eq <- paste0("y = ", round(co[2],4),"x + ", round(co[1],4))
    ggplot(rv$curve_df,aes(log_od,log_conc))+
      geom_point(size=4)+
      geom_smooth(method="lm",se=FALSE)+
      annotate("text",
               x=min(rv$curve_df$log_od),
               y=max(rv$curve_df$log_conc),
               label=eq,
               hjust=0)+
      theme_classic()
  })

  observeEvent(input$go_samples,{
    enable("sample_tab")
    updateTabsetPanel(session,"main_tabs","sample_tab")
  })

  output$sample_table <- renderDT({
    locked_js <- toJSON(lapply(seq_len(nrow(rv$locked_cells)),
                               function(i) as.numeric(rv$locked_cells[i, ])),
                        keep_vec_names = FALSE)
    datatable(
      rv$data_work,
      rownames = TRUE,
      selection=list(mode="multiple", target="cell"),
      options = list(
        dom = "t",
        pageLength = nrow(rv$data_work),
        ordering = FALSE,
        rowCallback = JS(sprintf(
          "function(row, data, displayIndex) {
       var locked = %s;
       locked.forEach(function(cell){
         if(cell[0] === displayIndex + 1){
           $('td:eq(' + cell[1] + ')', row).css('color','#bdbdbd');
         }
       });
     }", locked_js))
      )
    )
  })

  output$sample_group_name_ui <- renderUI({
    textInput(
      "sample_group_name",
      label = NULL,
      placeholder = paste0("Group ", length(rv$sample_groups) + 1)
    )
  })

  observeEvent(input$add_sample_group,{
    cells <- input$sample_table_cells_selected
    rv$sample_groups[[input$sample_group_name]] <- cells
    rv$locked_cells <- rbind(rv$locked_cells, cells)
    updateTextInput(
      session,
      "sample_group_name",
      value = "",
      placeholder = paste0("Group ", length(rv$sample_groups) + 1)
    )
  })

  observe({
    if(length(rv$sample_groups)>=2) enable("finalize_samples")
  })

  output$sample_group_list <- renderUI({
    tagList(lapply(names(rv$sample_groups), function(g) {
      cells <- rv$sample_groups[[g]]
      values <- as.numeric(apply(cells, 1, function(x)
        rv$data_work[x[1], x[2]]
      ))
      div(paste0(
        "",
        g,
        ". N=",
        length(values),
        " (",
        paste(values, collapse = ", "),
        ")"
      ))
    }))
  })

  observeEvent(input$finalize_samples,{
    enable(c("results_tab", "plot_tab", "stats_tab"))
    updateTabsetPanel(session,"main_tabs","results_tab")
  })

  results_df <- reactive({
    co <- coefficients(rv$curve_model)
    do.call(rbind, lapply(names(rv$sample_groups), function(g) {
      vals <- apply(rv$sample_groups[[g]], 1,
                    function(x) rv$data_work[x[1], x[2]])
      log_vals <- log(vals)
      pred_log_conc <- co[2] * log_vals + co[1]
      final_conc <- exp(pred_log_conc)
      m  <- mean(final_conc, na.rm = TRUE)
      sdv <- sd(final_conc, na.rm = TRUE)
      sem <- sdv / sqrt(sum(!is.na(final_conc)))
      data.frame(
        Group = g,
        Concentration = final_conc,
        Mean = c(m, rep(NA, length(final_conc) - 1)),
        SD = c(sdv, rep(NA, length(final_conc) - 1)),
        SEM = c(sem, rep(NA, length(final_conc) - 1))
      )
    }))
  })

  observeEvent(input$go_stats, {
    enable("stats_tab")
    updateTabsetPanel(session, "main_tabs", "stats_tab")
  })

  output$results_text <- renderPrint({ print(results_df()) })

  output$export_results <- downloadHandler(
    filename=function() "csbELISA_results.csv",
    content=function(file){
      write.csv(results_df(),file,row.names=FALSE)
    }
  )

  observe({
    tabs <- c(
      "file_tab",
      "standard_tab",
      "curve_tab",
      "sample_tab",
      "results_tab",
      "plot_tab",
      "stats_tab"
    )
    lapply(tabs, function(tb) {
      runjs(sprintf(
        "$('a[data-value=\"%s\"]').addClass('disabled');",
        tb
      ))
    })
    runjs(sprintf(
      "$('a[data-value=\"%s\"]').removeClass('disabled');",
      input$main_tabs
    ))
  })

  observe({
    sel <- input$sample_table_cells_selected
    if (is.null(sel)) {
      disable("add_sample_group")
      return()
    }
    if (nrow(sel) >= 3) {
      enable("add_sample_group")
    } else {
      disable("add_sample_group")
    }
  })

  output$save_curve <- downloadHandler(
    filename = function() {
      "standard_curve.png"
    },
    content = function(file) {
      req(rv$curve_model)
      co <- coefficients(rv$curve_model)
      eq <- paste0("y = ", round(co[2],4), "x + ", round(co[1],4))
      p <- ggplot(rv$curve_df, aes(log_od, log_conc)) +
        geom_point(size = 4) +
        geom_smooth(method = "lm", se = FALSE) +
        annotate(
          "text",
          x = min(rv$curve_df$log_od),
          y = max(rv$curve_df$log_conc),
          label = eq,
          hjust = 0
        ) +
        theme_classic()
      ggsave(file, plot = p, width = 6, height = 5, dpi = 300)
    }
  )

  observeEvent(input$go_plot, {
    enable("plot_tab")
    updateTabsetPanel(session, "main_tabs", "plot_tab")
  })

  output$plot_group_selector <- renderUI({
    req(results_df())
    selectInput(
      "plot_groups",
      "Groups",
      choices = unique(results_df()$Group),
      selected = unique(results_df()$Group),
      multiple = TRUE
    )
  })

  output$plot_colour_selectors <- renderUI({
    req(results_df(), input$plot_groups)
    groups <- input$plot_groups
    div(
      style = "display:flex;flex-wrap:wrap;gap:12px;",
      lapply(groups, function(g) {
        div(
          style = "display:flex;flex-direction:column;align-items:center;",
          div(
            g,
            style = "font-size:12px;margin-bottom:4px;text-align:center;"
          ),
          colourpicker::colourInput(
            inputId = paste0("col_", make.names(g)),
            label = NULL,
            value = "#BDBDBD",
            showColour = "background",
            width = "40px"
          )
        )
      })
    )
  })

  output$plot_output <- renderPlot(
    {
      req(results_df(), input$plot_groups)
      df <- results_df()
      df <- df[df$Group %in% input$plot_groups, ]
      p <- ggplot(df, aes(x = Group, y = Concentration)) +
        theme_classic() +
        theme(
          axis.title.x = element_blank(),
          axis.text.x = element_text(
            angle = as.numeric(input$x_label_angle),
            hjust = ifelse(as.numeric(input$x_label_angle) == 0, 0.5, 1),
            size = input$text_size
          ),
          axis.text.y = element_text(size = input$text_size),
          axis.title.y = element_text(size = input$text_size),
          legend.text = element_text(size = input$text_size),
          legend.title = element_blank()
        )
      if (input$plot_type == "Box plot") {
        p <- p + geom_boxplot(
          aes(fill = Group),
          width = input$bar_width,
          size = input$line_width,
          color = "black"
        )
      } else {
        p <- p +
          stat_summary(
            fun = mean,
            geom = "bar",
            aes(fill = Group),
            width = input$bar_width,
            size = input$line_width,
            color = "black"
          )
      }
      df <- df[!is.na(df$Concentration), ]
      summary_df <- aggregate(
        Concentration ~ Group,
        data = df,
        FUN = function(x) {
          c(
            mean = mean(x, na.rm = TRUE),
            sd   = sd(x, na.rm = TRUE),
            sem  = sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
          )
        }
      )
      summary_df <- do.call(data.frame, summary_df)
      if (input$error_type == "SD") {
        p <- p +
          geom_errorbar(
            data = summary_df,
            inherit.aes = FALSE,
            aes(
              x = Group,
              ymin = Concentration.mean - Concentration.sd,
              ymax = Concentration.mean + Concentration.sd
            ),
            width = 0.2,
            size = input$line_width
          )
      } else {
        p <- p +
          geom_errorbar(
            data = summary_df,
            inherit.aes = FALSE,
            aes(
              x = Group,
              ymin = Concentration.mean - Concentration.sem,
              ymax = Concentration.mean + Concentration.sem
            ),
            width = 0.2,
            size = input$line_width
          )
      }
      if (input$show_points) {
        p <- p +
          geom_jitter(
            aes(fill = Group),
            shape = 21,
            color = "black",
            stroke = input$line_width,
            width = 0.1,
            size = input$dot_size
          )
      }
      col_map <- setNames(
        sapply(input$plot_groups, function(g) {
          col <- input[[paste0("col_", make.names(g))]]
          if (is.null(col) || is.na(col) || col == "") "#BDBDBD" else col
        }),
        input$plot_groups
      )
      p +
        scale_fill_manual(values = col_map) +
        guides(
          fill  = guide_legend(override.aes = list(shape = NA)),
          color = guide_legend(override.aes = list(shape = NA))
        )
    },
    width  = function() input$plot_width,
    height = function() input$plot_height
  )

  output$comparison_selector <- renderUI({
    req(results_df())
    df <- results_df()
    groups <- unique(df$Group)
    req(length(groups) >= 2)
    cmb <- combn(groups, 2, simplify = FALSE)
    checkboxGroupInput(
      "shown_comparisons",
      tagList(icon("grip-lines-vertical"), "Comparisons"),
      choices  = sapply(cmb, paste, collapse = " vs "),
      selected = sapply(cmb, paste, collapse = " vs ")
    )
  })

  output$stats_plot <- renderPlot(
    {
      req(results_df(), input$shown_comparisons)
      df <- results_df()
      df <- df[df$Group %in% input$plot_groups, ]
      p <- ggplot(df, aes(x = Group, y = Concentration)) +
        theme_classic() +
        theme(
          axis.title.x = element_blank(),
          axis.text.x = element_text(
            angle = as.numeric(input$x_label_angle),
            hjust = ifelse(as.numeric(input$x_label_angle) == 0, 0.5, 1),
            size = input$text_size
          ),
          axis.text.y = element_text(size = input$text_size),
          axis.title.y = element_text(size = input$text_size),
          legend.text = element_text(size = input$text_size),
          legend.title = element_blank()
        )
      if (input$plot_type == "Box plot") {
        p <- p + geom_boxplot(
          aes(fill = Group),
          width = input$bar_width,
          size = input$line_width,
          color = "black"
        )
      } else {
        p <- p +
          stat_summary(
            fun = mean,
            geom = "bar",
            aes(fill = Group),
            width = input$bar_width,
            size = input$line_width,
            color = "black"
          )
      }
      df <- df[!is.na(df$Concentration), ]
      summary_df <- aggregate(
        Concentration ~ Group,
        data = df,
        FUN = function(x) {
          c(
            mean = mean(x, na.rm = TRUE),
            sd   = sd(x, na.rm = TRUE),
            sem  = sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
          )
        }
      )
      summary_df <- do.call(data.frame, summary_df)
      if (input$error_type == "SD") {
        p <- p +
          geom_errorbar(
            data = summary_df,
            inherit.aes = FALSE,
            aes(
              x = Group,
              ymin = Concentration.mean - Concentration.sd,
              ymax = Concentration.mean + Concentration.sd
            ),
            width = 0.2,
            size = input$line_width
          )
      } else {
        p <- p +
          geom_errorbar(
            data = summary_df,
            inherit.aes = FALSE,
            aes(
              x = Group,
              ymin = Concentration.mean - Concentration.sem,
              ymax = Concentration.mean + Concentration.sem
            ),
            width = 0.2,
            size = input$line_width
          )
      }
      if (input$show_points) {
        p <- p +
          geom_jitter(
            aes(fill = Group),
            shape = 21,
            color = "black",
            stroke = input$line_width,
            width = 0.1,
            size = input$dot_size
          )
      }
      col_map <- setNames(
        sapply(input$plot_groups, function(g) {
          col <- input[[paste0("col_", make.names(g))]]
          if (is.null(col) || is.na(col) || col == "") "#BDBDBD" else col
        }),
        input$plot_groups
      )
      p <- p +
        scale_fill_manual(values = col_map) +
        guides(
          fill  = guide_legend(override.aes = list(shape = NA)),
          color = guide_legend(override.aes = list(shape = NA))
        )
      groups <- unique(df$Group)
      range_y <- diff(range(df$Concentration, na.rm = TRUE))
      comps <- strsplit(input$shown_comparisons, " vs ")
      bars <- do.call(rbind, lapply(seq_along(comps), function(i) {
        data.frame(
          x1 = match(comps[[i]][1], groups),
          x2 = match(comps[[i]][2], groups),
          base_y = max(df$Concentration[df$Group %in% comps[[i]]], na.rm = TRUE)
        )
      }))
      bars$span <- abs(bars$x2 - bars$x1)
      bars <- bars[order(bars$span), ]
      bars$y <- bars$base_y +
        range_y * input$sig_bar_offset * seq_len(nrow(bars))
      # determine normality globally (same logic as stats_text)
      groups_all <- unique(df$Group)
      sh <- sapply(groups_all, function(g)
        shapiro.test(df$Concentration[df$Group == g])$p.value
      )
      normal <- all(sh > 0.05)

      # compute adjusted p-values once
      if (normal) {
        aov_model <- aov(Concentration ~ Group, data = df)
        pw <- TukeyHSD(aov_model)$Group[, "p adj"]
      } else {
        pw <- pairwise.wilcox.test(
          df$Concentration,
          df$Group,
          p.adjust.method = "bonferroni"
        )$p.value
      }

      # assign symbols to bars using the SAME adjusted p-values
      bars$label <- apply(bars, 1, function(b) {
        g1 <- groups[b["x1"]]
        g2 <- groups[b["x2"]]

        if (normal) {
          nm1 <- paste(g1, g2, sep = "-")
          nm2 <- paste(g2, g1, sep = "-")
          pval <- if (nm1 %in% names(pw)) pw[nm1] else pw[nm2]
        } else {
          pval <- if (!is.na(pw[g1, g2])) pw[g1, g2] else pw[g2, g1]
        }

        if (is.na(pval)) "ns"
        else if (pval < 0.0001) "****"
        else if (pval < 0.001) "***"
        else if (pval < 0.01) "**"
        else if (pval < 0.05) "*"
        else "ns"
      })
      p <- p +
        geom_segment(
          data = bars,
          aes(x = x1, xend = x2, y = y, yend = y),
          inherit.aes = FALSE,
          linewidth = input$line_width
        ) +
        geom_text(
          data = bars,
          aes(
            x = (x1 + x2) / 2,
            y = y + range_y * input$sig_text_offset,
            label = label
          ),
          inherit.aes = FALSE,
          fontface = "bold",
          vjust = 0,
          size = input$sig_text_size / 3
        ) +
        expand_limits(
          y = max(bars$y + range_y * (input$sig_text_offset + 0.05))
        )
      p
    },
    width  = function() input$plot_width,
    height = function() input$plot_height
  )

  output$stats_text <- renderPrint({
    req(results_df(), input$plot_groups)
    df <- results_df()
    df <- df[df$Group %in% input$plot_groups, ]
    groups <- unique(df$Group)
    sh <- sapply(groups, function(g)
      shapiro.test(df$Concentration[df$Group == g])$p.value
    )
    normal <- all(sh > 0.05)
    cat(
      "Normality (Shapiro-Wilk): ",
      ifelse(normal, "normal distribution\n", "non-normal distribution\n")
    )
    if (length(groups) == 2) {
      test <- if (normal)
        t.test(Concentration ~ Group, df)
      else
        wilcox.test(Concentration ~ Group, df)
      cat(
        "\nOmnibus test: ",
        ifelse(normal, "t-test", "Wilcoxon test"),
        "\np-value: ", signif(test$p.value, 3),
        "\nSignificant: ",
        ifelse(test$p.value < 0.05, "YES", "NO"), "\n"
      )
    } else {
      omni <- if (normal)
        aov(Concentration ~ Group, df)
      else
        kruskal.test(Concentration ~ Group, df)
      p.omni <- if (normal)
        summary(omni)[[1]][["Pr(>F)"]][1]
      else
        omni$p.value
      cat(
        "\nOmnibus test: ",
        ifelse(normal, "One-way ANOVA", "Kruskal-Wallis"),
        "\np-value: ", signif(p.omni, 3),
        "\nSignificant: ",
        ifelse(p.omni < 0.05, "YES", "NO"), "\n"
      )
      if (p.omni < 0.05) {
        cat("\nMultiple comparisons:\n")
        if (normal) {
          pw <- TukeyHSD(omni)$Group[, "p adj"]
          for (nm in names(pw)) {
            comps <- strsplit(nm, "-")[[1]]
            p <- pw[nm]
            sym <- if (p < 0.0001) "****"
            else if (p < 0.001) "***"
            else if (p < 0.01) "**"
            else if (p < 0.05) "*"
            else "ns"
            cat(
              comps[1], "vs", comps[2],
              ": p =",
              ifelse(p < 0.0001, "< 0.0001", formatC(p, digits = 4)),
              "(", sym, ")\n"
            )
          }
        } else {
          pw <- pairwise.wilcox.test(
            df$Concentration,
            df$Group,
            p.adjust.method = "bonferroni"
          )$p.value
          for (i in rownames(pw)) for (j in colnames(pw)) {
            if (!is.na(pw[i, j])) {
              p <- pw[i, j]
              sym <- if (p < 0.0001) "****"
              else if (p < 0.001) "***"
              else if (p < 0.01) "**"
              else if (p < 0.05) "*"
              else "ns"
              cat(
                i, "vs", j,
                ": p =",
                ifelse(p < 0.0001, "< 0.0001", formatC(p, digits = 4)),
                "(", sym, ")\n"
              )
            }
          }
        }
      }
    }
  })

  observeEvent(input$back_to_plot, {
    updateTabsetPanel(session, "main_tabs", "plot_tab")
  })

  output$save_stats_plot <- downloadHandler(
    filename = function() "stats_plot.png",
    content = function(file) {
      req(results_df(), input$shown_comparisons)
      df <- results_df()
      df <- df[df$Group %in% input$plot_groups, ]
      p <- ggplot(df, aes(x = Group, y = Concentration)) +
        theme_classic() +
        theme(
          axis.title.x = element_blank(),
          axis.text.x = element_text(
            angle = as.numeric(input$x_label_angle),
            hjust = ifelse(as.numeric(input$x_label_angle) == 0, 0.5, 1),
            size = input$text_size
          ),
          axis.text.y = element_text(size = input$text_size),
          axis.title.y = element_text(size = input$text_size),
          legend.text = element_text(size = input$text_size),
          legend.title = element_blank()
        )
      if (input$plot_type == "Box plot") {
        p <- p + geom_boxplot(
          aes(fill = Group),
          width = input$bar_width,
          size = input$line_width,
          color = "black"
        )
      } else {
        p <- p +
          stat_summary(
            fun = mean,
            geom = "bar",
            aes(fill = Group),
            width = input$bar_width,
            size = input$line_width,
            color = "black"
          )
      }
      df <- df[!is.na(df$Concentration), ]
      summary_df <- aggregate(
        Concentration ~ Group,
        data = df,
        FUN = function(x) {
          c(
            mean = mean(x, na.rm = TRUE),
            sd   = sd(x, na.rm = TRUE),
            sem  = sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
          )
        }
      )
      summary_df <- do.call(data.frame, summary_df)
      if (input$error_type == "SD") {
        p <- p +
          geom_errorbar(
            data = summary_df,
            inherit.aes = FALSE,
            aes(
              x = Group,
              ymin = Concentration.mean - Concentration.sd,
              ymax = Concentration.mean + Concentration.sd
            ),
            width = 0.2,
            size = input$line_width
          )
      } else {
        p <- p +
          geom_errorbar(
            data = summary_df,
            inherit.aes = FALSE,
            aes(
              x = Group,
              ymin = Concentration.mean - Concentration.sem,
              ymax = Concentration.mean + Concentration.sem
            ),
            width = 0.2,
            size = input$line_width
          )
      }
      if (input$show_points) {
        p <- p +
          geom_jitter(
            aes(fill = Group),
            shape = 21,
            color = "black",
            stroke = input$line_width,
            width = 0.1,
            size = input$dot_size
          )
      }
      col_map <- setNames(
        sapply(input$plot_groups, function(g) {
          col <- input[[paste0("col_", make.names(g))]]
          if (is.null(col) || is.na(col) || col == "") "#BDBDBD" else col
        }),
        input$plot_groups
      )
      p <- p +
        scale_fill_manual(values = col_map) +
        guides(
          fill  = guide_legend(override.aes = list(shape = NA)),
          color = guide_legend(override.aes = list(shape = NA))
        )
      combs <- combn(unique(df$Group), 2, simplify = FALSE)
      y_max <- max(df$Concentration, na.rm = TRUE)
      for (i in seq_along(combs)) {
        g1 <- combs[[i]][1]
        g2 <- combs[[i]][2]
        if (!paste(g1, g2, sep = " vs ") %in% input$shown_comparisons &&
            !paste(g2, g1, sep = " vs ") %in% input$shown_comparisons) {
          next
        }
        v1 <- df$Concentration[df$Group == g1]
        v2 <- df$Concentration[df$Group == g2]
        all_groups <- unique(df$Group)
        normality_p <- sapply(all_groups, function(g) {
          shapiro.test(df$Concentration[df$Group == g])$p.value
        })
        all_normal <- all(normality_p > 0.05)
        if (all_normal) {
          aov_model <- aov(Concentration ~ Group, data = df)
          posthoc <- TukeyHSD(aov_model)$Group
          comp_name <- paste(g1, g2, sep = "-")
          comp_rev  <- paste(g2, g1, sep = "-")
          pval <- if (comp_name %in% rownames(posthoc)) {
            posthoc[comp_name, "p adj"]
          } else {
            posthoc[comp_rev, "p adj"]
          }
        } else {
          pw <- pairwise.wilcox.test(
            df$Concentration,
            df$Group,
            p.adjust.method = "bonferroni"
          )
          if (!is.na(pw$p.value[g1, g2])) {
            pval <- pw$p.value[g1, g2]
          } else {
            pval <- pw$p.value[g2, g1]
          }
        }
        symb <- ifelse(
          pval < 0.0001, "****",
          ifelse(
            pval < 0.001, "***",
            ifelse(
              pval < 0.01, "**",
              ifelse(pval < 0.05, "*", "ns")
            )
          )
        )
        x1 <- match(g1, unique(df$Group))
        x2 <- match(g2, unique(df$Group))
        y_bar <- y_max + input$sig_bar_offset * y_max +
          i * input$sig_text_offset * y_max
        p <- p +
          annotate(
            "segment",
            x = x1,
            xend = x2,
            y = y_bar,
            yend = y_bar,
            size = input$line_width * 0.7
          ) +
          annotate(
            "text",
            x = mean(c(x1, x2)),
            y = y_bar + input$sig_text_offset * y_max,
            label = symb,
            size = input$sig_text_size / 5,
            fontface = "bold"
          )
      }
      ggsave(
        file,
        plot   = p,
        width  = input$plot_width  / 72,
        height = input$plot_height / 72,
        dpi    = 300,
        units  = "in"
      )
    }
  )
}
