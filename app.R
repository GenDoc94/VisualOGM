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

APP_NAME <- "BioCalc"
APP_VERSION <- "1.0.0"
APP_REPO_URL <- "https://github.com/GenDoc94/BioCalc"
APP_LICENSE <- "MIT"

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
      .info-panel {
        line-height: 1.6;
        padding-bottom: 1.5rem;
      }
      .info-sidebar {
        text-align: center;
      }
      .info-sidebar-logo {
        max-width: 5rem;
        margin: 0.25rem auto 0.75rem;
        display: block;
      }
      .info-sidebar-version {
        color: #666;
        font-size: 0.95rem;
        margin-bottom: 1rem;
      }
      .info-sidebar-section {
        text-align: left;
        margin-bottom: 1rem;
      }
      .info-sidebar-section h4 {
        font-size: 1rem;
        margin: 0 0 0.4rem;
      }
      .info-sidebar-section ul {
        padding-left: 1.2rem;
        margin: 0;
        font-size: 0.92rem;
      }
      .info-sidebar-links {
        text-align: left;
        font-size: 0.92rem;
        margin-top: 1rem;
      }
      .info-sidebar-links p {
        margin: 0.35rem 0;
      }
      .info-sidebar .app-author {
        margin-top: 1rem;
        padding-top: 0.75rem;
        border-top: 1px solid #ddd;
        text-align: center;
        font-size: 0.88rem;
      }
      .info-panel h3 {
        margin-top: 1.5rem;
        margin-bottom: 0.75rem;
      }
      .info-panel h4 {
        margin-top: 1.25rem;
        margin-bottom: 0.5rem;
      }
      .info-panel ul {
        margin-bottom: 1rem;
      }
      .info-panel code {
        background: #f4f4f4;
        padding: 0.1rem 0.35rem;
        border-radius: 3px;
        font-size: 0.92em;
      }
      .info-screenshots {
        display: flex;
        flex-wrap: wrap;
        gap: 1.5rem;
        margin: 1rem 0 1.5rem;
      }
      .info-screenshot {
        flex: 1 1 18rem;
        max-width: 100%;
      }
      .info-screenshot img {
        width: 100%;
        height: auto;
        border: 1px solid #ddd;
        border-radius: 6px;
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.08);
      }
      .info-screenshot figcaption {
        margin-top: 0.5rem;
        font-size: 0.9rem;
        color: #555;
        text-align: center;
      }
      .app-author {
        margin-top: 2rem;
        padding-top: 1rem;
        border-top: 1px solid #ddd;
        text-align: center;
        color: #555;
        font-size: 0.95rem;
      }
      .app-author a {
        color: inherit;
        text-decoration: none;
        display: inline-flex;
        align-items: center;
        gap: 0.25rem;
      }
      .app-author a:hover {
        text-decoration: underline;
      }
      .app-author img {
        height: 1em;
        vertical-align: middle;
      }
      .app-author-sep {
        margin: 0 0.5rem;
        color: #aaa;
      }
      "
    ))
  ),
  titlePanel("BioCalc"),
  tabsetPanel(
    tabPanel(
      "Información",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          class = "info-sidebar",
          tags$img(
            src = "logo_hem.png",
            class = "info-sidebar-logo",
            alt = paste(APP_NAME, "logo")
          ),
          tags$h3(APP_NAME, style = "margin-top: 0;"),
          tags$p(class = "info-sidebar-version", paste("Versión", APP_VERSION)),
          tags$hr(),
          div(
            class = "info-sidebar-section",
            tags$h4("Funcionalidades"),
            tags$ul(
              tags$li("CirclePlot por muestra"),
              tags$li("Oncoprint de cohortes"),
              tags$li("Filtros de clasificación, moléculas y VAF"),
              tags$li("Exportación PDF, PNG y CSV")
            )
          ),
          div(
            class = "info-sidebar-section",
            tags$h4("Archivos de entrada"),
            tags$ul(
              tags$li(tags$code("*_Classified_Variants*.txt")),
              tags$li(tags$code("*_Aneuploidy.txt")),
              tags$li(tags$code(".bed"), " (solo Oncoprint)")
            )
          ),
          div(
            class = "info-sidebar-links",
            tags$p(
              tags$strong("Repositorio: "),
              tags$a(href = APP_REPO_URL, target = "_blank", rel = "noreferrer", "GitHub")
            ),
            tags$p(
              tags$strong("Licencia: "),
              tags$a(
                href = paste0(APP_REPO_URL, "/blob/main/LICENSE"),
                target = "_blank",
                rel = "noreferrer",
                APP_LICENSE
              )
            ),
            tags$p(
              tags$strong("Datos: "),
              "en uso local, los archivos se procesan en tu equipo."
            )
          ),
          div(
            class = "app-author",
            tags$span("Created by "),
            tags$a(
              href = "https://github.com/GenDoc94",
              target = "_blank",
              rel = "noreferrer",
              "GenDoc94",
              tags$img(src = "logo_hem.png", alt = "GenDoc94 logo")
            ),
            tags$br(),
            tags$a(
              href = "https://buymeacoffee.com/gendoc94",
              target = "_blank",
              rel = "noreferrer",
              "Buy me a coffee",
              tags$img(
                src = "https://cdn.buymeacoffee.com/buttons/v2/default-yellow.png",
                alt = "Buy me a coffee"
              )
            )
          )
        ),
        mainPanel(
          width = 9,
          div(
            class = "info-panel",
            h3("¿Para qué sirve esta aplicación?"),
        p(
          "Esta aplicación está pensada para procesar los archivos de variantes clasificadas ",
          tags$code("XXX_Classified_Variants_.txt"),
          " y los archivos de aneuploidías ",
          tags$code("XXX_Aneuploidy.txt"),
          " de vuestras muestras de Bionano que hayáis descargado, y poder hacer ",
          strong("circleplots personalizados"),
          " y vuestros propios ",
          strong("oncoprints"),
          "."
        ),
        h4("Descarga de archivos desde OGM"),
        p(
          "Para descargar los archivos necesarios de OGM, hacerlo a través de la plataforma de análisis ",
          "en el botón de ",
          strong("descargar"),
          "."
        ),
        tags$ul(
          tags$li(
            tags$strong("Classified_Variants: "),
            "se recomienda descargarlos una vez se hayan categorizado las variantes ",
            "(porque luego puedes filtrar las que quieras en la app)."
          ),
          tags$li(
            tags$strong("Aneuploidy: "),
            "se recomienda descargarlos tal cual, para que se descarguen todas las aneuploidías."
          )
        ),
        h4("Capturas de la plataforma"),
        p(
          "Para descargar los ficheros desde la plataforma, pulsa el botón ",
          strong("Download"),
          ":"
        ),
        div(
          class = "info-screenshots",
          tags$figure(
            class = "info-screenshot",
            tags$img(
              src = "download_button.png",
              alt = "Botón Download en la plataforma OGM"
            ),
            tags$figcaption("Botón Download")
          )
        ),
        p("Ejemplo de cómo descargar cada tipo de archivo:"),
        div(
          class = "info-screenshots",
          tags$figure(
            class = "info-screenshot",
            tags$img(
              src = "ogm_download_aneuploidy.png",
              alt = "Captura: descarga del archivo Aneuploidy"
            ),
            tags$figcaption("Aneuploidy — Descargar tal cual (todas las aneuploidías)")
          ),
          tags$figure(
            class = "info-screenshot",
            tags$img(
              src = "ogm_download_classified_variants.png",
              alt = "Captura: descarga del archivo Classified_Variants"
            ),
            tags$figcaption("Classified_Variants — Descargar tras categorizar (para poder filtrar)")
          )
        ),
        tags$p(
          class = "app-note",
          em(
            "Herramienta de apoyo a la investigación. No sustituye la interpretación clínica ",
            "ni la validación en la plataforma OGM."
          )
        )
          )
        )
      )
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
