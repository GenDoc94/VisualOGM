# VisualOGM

[License: MIT](https://opensource.org/licenses/MIT)

Aplicación **Shiny** para visualizar resultados de **Bionano™**: circle plots por muestra y oncoprints de varias muestras a partir de los archivos exportados por **Bionano Access™**.

Repositorio: [github.com/GenDoc94/VisualOGM](https://github.com/GenDoc94/VisualOGM)

## Funcionalidades

- **CirclePlot**: un par `*_Classified_Variants*.txt` + `*_Aneuploidy.txt` por muestra.
- **Oncoprint**: varios pares de archivos + un `.bed` de regiones de interés.
- Filtros por clasificación ACMG, número de moléculas y VAF.
- Exportación a PDF, PNG (CirclePlot) y CSV de variantes filtradas.

La pestaña **Información** de la app incluye capturas de cómo descargar los ficheros desde **Bionano Access™**.

## Requisitos

- R >= 4.1 recomendado
- Paquetes CRAN: `shiny`, `bslib`, `shinycssloaders`, `tidyverse`, `circlize`, `readr`
- Paquete Bioconductor: `ComplexHeatmap`

```r
install.packages(c("shiny", "bslib", "shinycssloaders", "tidyverse", "circlize", "readr"))

if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}
BiocManager::install("ComplexHeatmap")
```

## Instalación y uso local

```r
# Clonar el repositorio
# git clone https://github.com/GenDoc94/VisualOGM.git
# setwd("VisualOGM")

shiny::runApp()
```

Sube los archivos desde la interfaz. En ejecución local, los datos se procesan en tu equipo.

## Archivos de entrada


| Archivo                      | Uso                                                       |
| ---------------------------- | --------------------------------------------------------- |
| `*_Classified_Variants*.txt` | Variantes estructurales clasificadas                      |
| `*_Aneuploidy.txt`           | Aneuploidías                                              |
| `*.bed`                      | Regiones para Oncoprint (`chrom`, `start`, `end`, `name`) |


Los nombres de Classified_Variants y Aneuploidy de un mismo caso deben compartir el mismo identificador de muestra al inicio del nombre.

## Estructura del proyecto

- `app.R` — interfaz y servidor Shiny
- `functions/` — lectura, validación y gráficos
- `www/` — logo y capturas de la pestaña Información

## Publicada en internet

Para compartir la app sin que cada usuario instale R:

- [PENDIENTE DE AÑADIR URL](https://www.shinyapps.io/)

## Aviso legal / uso clínico

VisualOGM es una **herramienta de apoyo a la investigación y la visualización**. No sustituye la interpretación clínica, la validación en **Bionano Access™** ni ningún sistema diagnóstico certificado. El usuario es responsable del uso que haga de los resultados.

## Licencia

Este proyecto está bajo licencia **MIT** — ver [LICENSE](LICENSE).

## Cómo citar

Si usas VisualOGM en un trabajo, cita el repositorio (también disponible en [CITATION.cff](CITATION.cff)):

> GenDoc94 (2026). *VisualOGM* (v1.0.0-beta.1) [software]. Hospital Universitario Marqués de Valdecilla, Santander, Spain. [https://github.com/GenDoc94/VisualOGM](https://github.com/GenDoc94/VisualOGM)

## Contribuir

Consulta [CONTRIBUTING.md](CONTRIBUTING.md) para reportar errores o enviar mejoras.

## Autor

**GenDoc94** — [GitHub](https://github.com/GenDoc94) · [Buy me a coffee](https://buymeacoffee.com/gendoc94)

En algunas fases del desarrollo se utilizaron asistentes de IA como apoyo; el diseño, la validación y la mayor parte del código son trabajo propio del autor.