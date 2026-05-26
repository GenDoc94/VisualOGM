options(shiny.maxRequestSize = 100 * 1024^2)

suppressPackageStartupMessages({
  library(shiny)
})

source(file.path("functions", "shiny_data.R"))
source(file.path("functions", "circleplot.R"))

is_processed_case <- function(x) {
  is.list(x) && is.null(x$error)
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
  titlePanel("BioCalc Circle Plot"),
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
      downloadButton("download_png", "Descargar PNG")
    ),
    mainPanel(
      uiOutput("status"),
      plotOutput("circle_plot", height = "760px")
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
}

shinyApp(ui, server)
