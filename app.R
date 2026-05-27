options(shiny.maxRequestSize = 100 * 1024^2)

suppressPackageStartupMessages({
  library(shiny)
})

source(file.path("functions", "shiny_data.R"))
source(file.path("functions", "circleplot.R"))
source(file.path("functions", "oncoprint_data.R"))
source(file.path("functions", "oncoprint_plot.R"))

is_processed_case <- function(x) {
  is.list(x) && is.null(x$error)
}

is_processed_oncoprint <- function(x) {
  is.list(x) && is.null(x$error) && !is.null(x$complete_matrix)
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML(
      "
      .app-note {
        color: #555;
        margin-bottom: 1rem;
      }
      .status-box {
        border-radius: 6px;
        margin-bottom: 1rem;
        padding: 0.8rem 1rem;
      }
      .status-info {
        background: #eef6ff;
        border: 1px solid #b7d7f2;
      }
      .status-ok {
        background: #eef8f0;
        border: 1px solid #b8dfc0;
      }
      .status-error {
        background: #fff0f0;
        border: 1px solid #e4b3b3;
      }
      "
    ))
  ),
  titlePanel("BioCalc"),
  tabsetPanel(
    tabPanel(
      "Información"
    ),
    tabPanel(
      "CirclePlot",
      sidebarLayout(
        sidebarPanel(
          p(
            class = "app-note",
            "Sube un archivo Classified_Variants y un archivo Aneuploidy del mismo caso. ",
            "Ambos nombres deben empezar por el mismo numero de muestra."
          ),
          fileInput(
            "classified_file",
            "Archivo Classified_Variants",
            accept = c(".txt", "text/plain")
          ),
          fileInput(
            "aneuploidy_file",
            "Archivo Aneuploidy",
            accept = c(".txt", "text/plain")
          ),
          tags$hr(),
          checkboxGroupInput(
            "classification_filter",
            "Variantes a incluir",
            choices = c(
              "Pathogenic",
              "Likely pathogenic",
              "Uncertain significance"
            ),
            selected = c(
              "Pathogenic",
              "Likely pathogenic",
              "Uncertain significance"
            )
          ),
          sliderInput(
            "min_molecule_count",
            "Moleculas minimas",
            value = 10,
            min = 1,
            max = 100,
            step = 1
          ),
          sliderInput(
            "min_vaf",
            "VAF minima (%)",
            value = 5,
            min = 0,
            max = 100,
            step = 1,
            post = "%"
          ),
          tags$hr(),
          tags$h4("Resumen"),
          tableOutput("summary"),
          tags$hr(),
          downloadButton("download_pdf", "Descargar PDF"),
          downloadButton("download_png", "Descargar PNG"),
          downloadButton("download_csv", "Descargar variantes filtradas (CSV)")
        ),
        mainPanel(
          uiOutput("status"),
          plotOutput("circle_plot", height = "760px")
        )
      )
    ),
    tabPanel(
      "Oncoprint",
      sidebarLayout(
        sidebarPanel(
          p(
            class = "app-note",
            "Sube varios Classified_Variants y varios Aneuploidy (un par por paciente, ",
            "mismo numero al inicio del nombre). Tambien es obligatorio subir un archivo .bed ",
            "con las regiones a evaluar (archivo .bed con columnas chrom, start, end y name)."
          ),
          fileInput(
            "onco_classified_files",
            "Archivos Classified_Variants",
            accept = c(".txt", "text/plain"),
            multiple = TRUE
          ),
          fileInput(
            "onco_aneuploidy_files",
            "Archivos Aneuploidy",
            accept = c(".txt", "text/plain"),
            multiple = TRUE
          ),
          fileInput(
            "onco_bed_file",
            "Archivo BED de regiones",
            accept = c(".bed"),
            multiple = FALSE
          ),
          tags$hr(),
          checkboxGroupInput(
            "onco_classification_filter",
            "Variantes a incluir",
            choices = c(
              "Pathogenic",
              "Likely pathogenic",
              "Uncertain significance"
            ),
            selected = c(
              "Pathogenic",
              "Likely pathogenic",
              "Uncertain significance"
            )
          ),
          sliderInput(
            "onco_min_molecule_count",
            "Moleculas minimas",
            value = 10,
            min = 1,
            max = 100,
            step = 1
          ),
          sliderInput(
            "onco_min_vaf",
            "VAF minima (%)",
            value = 5,
            min = 0,
            max = 100,
            step = 1,
            post = "%"
          ),
          tags$hr(),
          numericInput(
            "onco_cnv_padding",
            "Padding CNV (bp)",
            value = 500000,
            min = 0,
            step = 1000
          ),
          numericInput(
            "onco_sv_padding",
            "Padding SV (bp)",
            value = 0,
            min = 0,
            step = 1000
          ),
          tags$hr(),
          tags$h4("Resumen"),
          tableOutput("onco_summary"),
          tags$hr(),
          downloadButton("onco_download_pdf", "Descargar PDF")
        ),
        mainPanel(
          uiOutput("onco_status"),
          plotOutput("onco_plot", height = "900px")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  processed_case <- reactive({
    req(input$classified_file, input$aneuploidy_file)

    tryCatch(
      prepare_circleplot_case(
        classified_path = input$classified_file$datapath,
        classified_name = input$classified_file$name,
        aneuploidy_path = input$aneuploidy_file$datapath,
        aneuploidy_name = input$aneuploidy_file$name,
        classifications = input$classification_filter,
        min_molecule_count = input$min_molecule_count,
        min_vaf = input$min_vaf / 100
      ),
      error = function(error) {
        list(error = conditionMessage(error))
      }
    )
  })

  output$status <- renderUI({
    if (is.null(input$classified_file) || is.null(input$aneuploidy_file)) {
      return(
        div(
          class = "status-box status-info",
          "Sube los dos archivos para generar el circle plot."
        )
      )
    }

    case <- processed_case()

    if (!is_processed_case(case)) {
      return(
        div(
          class = "status-box status-error",
          strong("No se puede generar el gráfico: "),
          case$error
        )
      )
    }

    div(
      class = "status-box status-ok",
      strong("Muestra validada: "),
      case$id,
      ". El gráfico se ha generado con los filtros del workflow."
    )
  })

  output$circle_plot <- renderPlot(
    {
      if (is.null(input$classified_file) || is.null(input$aneuploidy_file)) {
        validate(need(FALSE, "Sube los dos archivos para generar el circle plot."))
      }

      case <- processed_case()
      validate(need(is_processed_case(case), case$error))

      circleplot(case$id, case$dbase)
    },
    res = 96
  )

  output$summary <- renderTable({
    if (is.null(input$classified_file) || is.null(input$aneuploidy_file)) {
      return(NULL)
    }

    case <- processed_case()
    validate(need(is_processed_case(case), case$error))

    summarise_circleplot_case(case)
  })

  output$download_pdf <- downloadHandler(
    filename = function() {
      case <- processed_case()
      sample_id <- if (is_processed_case(case)) case$id else "circleplot"
      paste0(sample_id, "_circleplot.pdf")
    },
    content = function(file) {
      case <- processed_case()
      req(is_processed_case(case))

      pdf(file, width = 9, height = 9)
      on.exit(dev.off(), add = TRUE)
      circleplot(case$id, case$dbase)
    }
  )

  output$download_png <- downloadHandler(
    filename = function() {
      case <- processed_case()
      sample_id <- if (is_processed_case(case)) case$id else "circleplot"
      paste0(sample_id, "_circleplot.png")
    },
    content = function(file) {
      case <- processed_case()
      req(is_processed_case(case))

      png(file, width = 2400, height = 2400, res = 300)
      on.exit(dev.off(), add = TRUE)
      circleplot(case$id, case$dbase)
    }
  )

  output$download_csv <- downloadHandler(
    filename = function() {
      case <- processed_case()
      sample_id <- if (is_processed_case(case)) case$id else "variantes_filtradas"
      paste0(sample_id, "_variantes_filtradas.csv")
    },
    content = function(file) {
      case <- processed_case()
      req(is_processed_case(case))

      readr::write_csv(case$filtered_variants, file)
    }
  )

  processed_oncoprint <- reactive({
    req(
      input$onco_classified_files,
      input$onco_aneuploidy_files,
      input$onco_bed_file
    )

    tryCatch(
      prepare_oncoprint_cohort(
        classified_paths = input$onco_classified_files$datapath,
        classified_names = input$onco_classified_files$name,
        aneuploidy_paths = input$onco_aneuploidy_files$datapath,
        aneuploidy_names = input$onco_aneuploidy_files$name,
        bed_path = input$onco_bed_file$datapath,
        cnv_overlap = input$onco_cnv_padding,
        sv_overlap = input$onco_sv_padding,
        classifications = input$onco_classification_filter,
        min_molecule_count = input$onco_min_molecule_count,
        min_vaf = input$onco_min_vaf / 100
      ),
      error = function(error) {
        list(error = conditionMessage(error))
      }
    )
  })

  output$onco_status <- renderUI({
    if (
      is.null(input$onco_classified_files) ||
        is.null(input$onco_aneuploidy_files) ||
        is.null(input$onco_bed_file)
    ) {
      return(
        div(
          class = "status-box status-info",
          "Sube los archivos Classified_Variants, Aneuploidy y el archivo .bed para generar el oncoprint."
        )
      )
    }

    cohort <- processed_oncoprint()

    if (!is_processed_oncoprint(cohort)) {
      return(
        div(
          class = "status-box status-error",
          strong("No se puede generar el oncoprint: "),
          cohort$error
        )
      )
    }

    div(
      class = "status-box status-ok",
      strong("Cohorte validada: "),
      cohort$n_patients,
      " paciente(s) (",
      paste(cohort$patient_ids, collapse = ", "),
      "). Regiones BED: ",
      cohort$n_regions,
      "."
    )
  })

  output$onco_plot <- renderPlot(
    {
      if (
        is.null(input$onco_classified_files) ||
          is.null(input$onco_aneuploidy_files) ||
          is.null(input$onco_bed_file)
      ) {
        validate(need(FALSE, "Sube los archivos necesarios para generar el oncoprint."))
      }

      cohort <- processed_oncoprint()
      validate(need(is_processed_oncoprint(cohort), cohort$error))

      draw_oncoprint_detailed(
        cohort$complete_matrix,
        cohort$annotation_data,
        n_patients = cohort$n_patients
      )
    },
    res = 96
  )

  output$onco_summary <- renderTable({
    if (
      is.null(input$onco_classified_files) ||
        is.null(input$onco_aneuploidy_files) ||
        is.null(input$onco_bed_file)
    ) {
      return(NULL)
    }

    cohort <- processed_oncoprint()
    validate(need(is_processed_oncoprint(cohort), cohort$error))

    summarise_oncoprint_cohort(cohort)
  })

  output$onco_download_pdf <- downloadHandler(
    filename = function() {
      "oncoprint_ogm_detallado.pdf"
    },
    content = function(file) {
      cohort <- processed_oncoprint()
      req(is_processed_oncoprint(cohort))

      pdf(file, width = 14, height = 10)
      on.exit(dev.off(), add = TRUE)
      draw_oncoprint_detailed(
        cohort$complete_matrix,
        cohort$annotation_data,
        n_patients = cohort$n_patients
      )
    }
  )
}

shinyApp(ui, server)
