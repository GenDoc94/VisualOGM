options(shiny.maxRequestSize = 100 * 1024^2)

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
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

APP_NAME <- "VisualOGM"
APP_VERSION <- "1.0.0"
APP_REPO_URL <- "https://github.com/GenDoc94/VisualOGM"
APP_LICENSE <- "MIT"
APP_COLOR_PRIMARY <- "#007BFF"
APP_COLOR_DARK <- "#004085"

app_theme <- bs_theme(
  version = 5,
  primary = APP_COLOR_PRIMARY,
  secondary = APP_COLOR_DARK,
  info = APP_COLOR_DARK,
  link_color = APP_COLOR_PRIMARY,
  heading_color = APP_COLOR_DARK,
  "nav-tabs-link-active-color" = APP_COLOR_DARK,
  "nav-tabs-link-active-border-color" = APP_COLOR_PRIMARY,
  base_font = font_collection(
    "system-ui",
    "-apple-system",
    "Segoe UI",
    "Roboto",
    "Helvetica Neue",
    "Arial",
    "sans-serif"
  )
)

ui <- page_fluid(
  theme = app_theme,
  title = APP_NAME,
  tags$head(
    tags$style(HTML(
      "
      .app-header {
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 1rem;
        margin: 0.25rem 0 1rem;
        padding-bottom: 0.85rem;
        border-bottom: 1px solid var(--bs-border-color);
      }
      .app-header-brand {
        flex: 1 1 auto;
        min-width: 0;
      }
      .app-title-logo {
        max-width: min(22rem, 100%);
        height: auto;
        display: block;
      }
      .app-header-actions {
        flex: 0 0 auto;
      }
      .app-header-actions .form-group {
        margin-bottom: 0;
      }
      .nav-tabs .nav-link {
        font-weight: 500;
      }
      [data-bs-theme='dark'] .nav-tabs .nav-link.active,
      [data-bs-theme='dark'] .nav-tabs .nav-item.show .nav-link {
        color: #fff;
        --bs-nav-tabs-link-active-color: #fff;
        border-bottom-color: #007BFF;
      }
      .app-note {
        color: var(--bs-secondary-color);
        margin-bottom: 1rem;
      }
      .status-box {
        border-radius: 0.5rem;
        margin-bottom: 1rem;
        padding: 0.8rem 1rem;
      }
      .status-info {
        background: rgba(0, 123, 255, 0.08);
        border: 1px solid rgba(0, 123, 255, 0.28);
        color: var(--bs-body-color);
      }
      .status-ok {
        background: rgba(25, 135, 84, 0.08);
        border: 1px solid rgba(25, 135, 84, 0.28);
      }
      .status-error {
        background: rgba(220, 53, 69, 0.08);
        border: 1px solid rgba(220, 53, 69, 0.28);
      }
      [data-bs-theme='dark'] .status-info {
        background: rgba(0, 123, 255, 0.16);
        border-color: rgba(0, 123, 255, 0.4);
      }
      [data-bs-theme='dark'] .status-ok {
        background: rgba(25, 135, 84, 0.16);
      }
      [data-bs-theme='dark'] .status-error {
        background: rgba(220, 53, 69, 0.16);
      }
      .info-panel {
        line-height: 1.6;
        padding-bottom: 1.5rem;
      }
      .info-sidebar {
        text-align: center;
      }
      .info-sidebar-brand {
        display: flex;
        align-items: center;
        justify-content: center;
        gap: 0.6rem;
        margin: 0.15rem 0 0.65rem;
        flex-wrap: wrap;
      }
      .info-sidebar-brand .logo-vo {
        height: 3.25rem;
        width: auto;
        flex: 0 0 auto;
      }
      .info-sidebar-brand .logo-wordmark {
        height: 2.15rem;
        width: auto;
        max-width: 9.5rem;
        flex: 0 1 auto;
      }
      .info-sidebar-version {
        color: var(--bs-secondary-color);
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
        border-top: 1px solid var(--bs-border-color);
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
        background: var(--bs-tertiary-bg);
        color: var(--bs-body-color);
        padding: 0.1rem 0.35rem;
        border-radius: 0.25rem;
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
        border: 1px solid var(--bs-border-color);
        border-radius: 0.5rem;
        box-shadow: 0 2px 10px rgba(0, 64, 133, 0.08);
      }
      [data-bs-theme='dark'] .info-screenshot img {
        box-shadow: 0 2px 12px rgba(0, 0, 0, 0.35);
      }
      .info-screenshot figcaption {
        margin-top: 0.5rem;
        font-size: 0.9rem;
        color: var(--bs-secondary-color);
        text-align: center;
      }
      .info-screenshot--compact {
        flex: 0 0 auto;
        max-width: 100%;
      }
      .info-screenshot--compact img {
        width: auto;
        max-width: min(12rem, 100%);
        height: auto;
        image-rendering: -webkit-optimize-contrast;
      }
      .info-bed-download {
        margin: 1rem 0 1.25rem;
      }
      .info-bed-table {
        font-size: 0.95rem;
        margin: 0.75rem 0 1rem;
      }
      .info-bed-table th,
      .info-bed-table td {
        padding: 0.35rem 0.75rem 0.35rem 0;
        vertical-align: top;
      }
      .app-author {
        margin-top: 2rem;
        padding-top: 1rem;
        border-top: 1px solid var(--bs-border-color);
        text-align: center;
        color: var(--bs-secondary-color);
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
        color: var(--bs-border-color);
      }
      .btn-primary {
        --bs-btn-bg: #007BFF;
        --bs-btn-border-color: #007BFF;
        --bs-btn-hover-bg: #0069d9;
        --bs-btn-hover-border-color: #0062cc;
        --bs-btn-active-bg: #004085;
        --bs-btn-active-border-color: #004085;
      }
      .irs--shiny .irs-single,
      .irs--shiny .irs-from,
      .irs--shiny .irs-to {
        color: #fff;
      }
      .filters-row {
        margin-left: 0;
        margin-right: 0;
      }
      .filters-row > [class*='col-'] {
        padding-left: 0.35rem;
        padding-right: 0.35rem;
      }
      .filters-row > [class*='col-']:first-child {
        padding-left: 0;
      }
      .filters-row > [class*='col-']:last-child {
        padding-right: 0;
      }
      .filters-row .shiny-input-container {
        margin-bottom: 0.65rem;
      }
      .filters-row .shiny-input-container:last-child {
        margin-bottom: 0;
      }
      "
    ))
  ),
  div(
    class = "app-header",
    div(
      class = "app-header-brand",
      tags$img(
        src = "VisualOGM.png",
        class = "app-title-logo",
        alt = APP_NAME
      )
    ),
    div(
      class = "app-header-actions",
      input_dark_mode(
        id = "dark_mode",
        mode = "light"
      )
    )
  ),
  tabsetPanel(
    tabPanel(
      "Información",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          class = "info-sidebar",
          div(
            class = "info-sidebar-brand",
            tags$img(
              src = "VO.png",
              class = "logo-vo",
              alt = "VisualOGM icon"
            ),
            tags$img(
              src = "VisualOGM.png",
              class = "logo-wordmark",
              alt = APP_NAME
            )
          ),
          tags$p(
            class = "info-sidebar-version",
            tags$em(paste("Versión", APP_VERSION))
          ),
          tags$hr(),
          div(
            class = "info-sidebar-section",
            tags$h4(tags$strong("Funcionalidades")),
            tags$ul(
              tags$li("CirclePlot por muestra"),
              tags$li("Oncoprint de cohortes"),
              tags$li("Filtros de clasificación, moléculas y VAF"),
              tags$li("Exportación PDF, PNG y CSV")
            )
          ),
          div(
            class = "info-sidebar-section",
            tags$h4(tags$strong("Archivos de entrada")),
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
            class = "info-screenshot info-screenshot--compact",
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
        h4("Archivo .bed para Oncoprint"),
        p(
          "El oncoprint requiere un archivo ",
          tags$code(".bed"),
          " con las regiones que quieras evaluar en la cohorte. La primera fila es la ",
          strong("cabecera informativa del BED"),
          " (línea ",
          tags$code("track"),
          "); la aplicación la omite al leer el fichero. A partir de la segunda fila, ",
          "cada línea define una región de interés. Las columnas van ",
          strong("separadas por tabuladores"),
          " (no por espacios)."
        ),
        tags$table(
          class = "info-bed-table",
          tags$thead(
            tags$tr(
              tags$th("Columna"),
              tags$th("Contenido")
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td("1"),
              tags$td(
                "Número de cromosoma (1–22). Cromosoma X = ",
                tags$code("23"),
                ", cromosoma Y = ",
                tags$code("24"),
                ", igual que en los archivos ",
                tags$code("Classified_Variants"),
                " (columnas ",
                tags$code("RefcontigID1"),
                " / ",
                tags$code("RefcontigID2"),
                ")."
              )
            ),
            tags$tr(
              tags$td("2"),
              tags$td("Inicio de la región de interés (posición en el genoma de referencia).")
            ),
            tags$tr(
              tags$td("3"),
              tags$td("Fin de la región de interés.")
            ),
            tags$tr(
              tags$td("4"),
              tags$td(
                "Nombre de la región. Si indicas solo el gen (p. ej. ",
                tags$code("PDGFRA"),
                "), se buscarán ",
                strong("todas"),
                " las alteraciones estructurales que solapen esa región. Si añades ",
                tags$code("_"),
                " seguido del tipo de alteración (p. ej. ",
                tags$code("TP53_deletion"),
                "), solo se buscará esa alteración concreta."
              )
            )
          )
        ),
        p("Tipos de alteración que puedes indicar tras el guion bajo en la cuarta columna:"),
        tags$ul(
          tags$li(tags$code("insertion")),
          tags$li(tags$code("deletion")),
          tags$li(tags$code("inversion")),
          tags$li(tags$code("translocation")),
          tags$li(tags$code("rearrangement"), " (se trata como translocación/reordenamiento)"),
          tags$li(tags$code("duplication")),
          tags$li(tags$code("gain")),
          tags$li(tags$code("loss"))
        ),
        p(
          "Ejemplos de nombres en la cuarta columna: ",
          tags$code("CDKN2C_deletion"),
          ", ",
          tags$code("CKS1B_gain"),
          ", ",
          tags$code("MYC_rearrangement"),
          ", ",
          tags$code("CCND1_t(11;14)_CCND1::IGH_translocation"),
          ", o solo el gen ",
          tags$code("BIRC3"),
          " para cualquier variante en esa zona."
        ),
        p(
          "La primera fila del ejemplo tiene el formato habitual de cabecera BED, por ejemplo: ",
          tags$code('track db="hg38" name="filter_example" description="..."')
        ),
        h4("Padding CNV y SV (solapamiento)"),
        p(
          "En la pestaña ",
          strong("Oncoprint"),
          ", los campos ",
          tags$strong("Padding CNV (bp)"),
          " y ",
          tags$strong("Padding SV (bp)"),
          " no amplían la región del BED en el fichero: definen el ",
          strong("solapamiento"),
          " que estás dispuesto a aceptar para considerar que una variante estructural ",
          strong('"toca"'),
          " el gen o la región que buscas."
        ),
        p(
          "En la práctica, a cada intervalo del BED se le suma ese margen a izquierda y derecha ",
          "antes de comprobar si alguna alteración del paciente cae en esa zona ampliada. ",
          "El ",
          tags$strong("padding CNV"),
          " se aplica a ganancias y pérdidas (CNV); el ",
          tags$strong("padding SV"),
          ", al resto de variantes estructurales (deleciones puntuales, inversiones, ",
          "translocaciones, etc.)."
        ),
        p(
          "Por ejemplo, un ",
          tags$strong("Padding CNV"),
          " de ",
          tags$code("500000"),
          " significa ",
          tags$strong("500 kbp"),
          " hacia la izquierda y ",
          tags$strong("500 kbp"),
          " hacia la derecha",
          " respecto a los límites de la región del BED: se buscarán alteraciones que ",
          "solapen esa ventana ampliada, aunque el breakpoint no caiga exactamente ",
          "dentro del gen anotado."
        ),
        p(
          "Con ",
          tags$strong("Padding SV"),
          " en ",
          tags$code("0"),
          " (valor por defecto), las SV deben solaparse con la región del BED sin margen ",
          "extra; puedes subirlo si quieres ser más permisivo con breakpoints cercanos."
        ),
        div(
          class = "info-bed-download",
          downloadButton(
            "download_bed_example",
            "Descargar filter_example.bed (plantilla)",
            class = "btn-primary"
          )
        ),
        tags$p(
          class = "app-note",
          em(
            "Herramienta de apoyo a la investigación. No sustituye la interpretación clínica ",
            "ni la validación en la plataforma Bionano Access."
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
            label = tags$strong("Archivo Classified_Variants"),
            accept = c(".txt", "text/plain")
          ),
          fileInput(
            "aneuploidy_file",
            label = tags$strong("Archivo Aneuploidy"),
            accept = c(".txt", "text/plain")
          ),
          tags$hr(),
          fluidRow(
            class = "filters-row",
            column(
              width = 6,
              checkboxGroupInput(
                "classification_filter",
                label = tags$strong("Variantes a incluir"),
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
              )
            ),
            column(
              width = 6,
              sliderInput(
                "min_molecule_count",
                label = tags$strong("Moléculas mínimas"),
                value = 10,
                min = 1,
                max = 100,
                step = 1
              ),
              sliderInput(
                "min_vaf",
                label = tags$strong("VAF mínima (%)"),
                value = 5,
                min = 0,
                max = 100,
                step = 1,
                post = "%"
              )
            )
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
            "con las regiones a evaluar (consulta el formato en la pestaña ",
            strong("Información"),
            ")."
          ),
          fileInput(
            "onco_classified_files",
            label = tags$strong("Archivos Classified_Variants"),
            accept = c(".txt", "text/plain"),
            multiple = TRUE
          ),
          fileInput(
            "onco_aneuploidy_files",
            label = tags$strong("Archivos Aneuploidy"),
            accept = c(".txt", "text/plain"),
            multiple = TRUE
          ),
          fileInput(
            "onco_bed_file",
            label = tags$strong("Archivo BED de regiones a buscar"),
            accept = c(".bed"),
            multiple = FALSE
          ),
          tags$hr(),
          fluidRow(
            class = "filters-row",
            column(
              width = 6,
              checkboxGroupInput(
                "onco_classification_filter",
                label = tags$strong("Variantes a incluir"),
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
              )
            ),
            column(
              width = 6,
              sliderInput(
                "onco_min_molecule_count",
                label = tags$strong("Moléculas mínimas"),
                value = 10,
                min = 1,
                max = 100,
                step = 1
              ),
              sliderInput(
                "onco_min_vaf",
                label = tags$strong("VAF mínima (%)"),
                value = 5,
                min = 0,
                max = 100,
                step = 1,
                post = "%"
              )
            )
          ),
          tags$hr(),
          fluidRow(
            class = "filters-row",
            column(
              width = 6,
              numericInput(
                "onco_cnv_padding",
                label = tags$strong("Padding CNV (bp)"),
                value = 500000,
                min = 0,
                step = 1000
              )
            ),
            column(
              width = 6,
              numericInput(
                "onco_sv_padding",
                label = tags$strong("Padding SV (bp)"),
                value = 0,
                min = 0,
                step = 1000
              )
            )
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
  bed_example_path <- file.path("www", "filter_example.bed")

  output$download_bed_example <- downloadHandler(
    filename = "filter_example.bed",
    content = function(file) {
      if (!file.exists(bed_example_path)) {
        stop("No se encuentra el archivo de ejemplo filter_example.bed.", call. = FALSE)
      }
      file.copy(bed_example_path, file)
    }
  )

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
