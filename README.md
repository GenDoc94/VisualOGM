# VisualOGM

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.20441643.svg)](https://doi.org/10.5281/zenodo.20441643)

Aplicación **Shiny** para visualizar resultados de **Bionano™**: circle plots por muestra y oncoprints de varias muestras a partir de los archivos exportados por **Bionano Access™**.

**Demo en línea:** [gendoc.shinyapps.io/VisualOGM](https://gendoc.shinyapps.io/VisualOGM/)

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

## Demo en línea

Puedes usar VisualOGM sin instalar R en:

**https://gendoc.shinyapps.io/VisualOGM/**

Los archivos que subas se procesan en los servidores de [shinyapps.io](https://www.shinyapps.io/). No uses datos clínicos identificables sin revisar la política de privacidad del servicio. Para que todo quede en tu equipo, usa la [instalación local](#instalación-y-uso-local).

## Aviso legal / uso clínico

VisualOGM es una **herramienta de apoyo a la investigación y la visualización**. No sustituye la interpretación clínica, la validación en **Bionano Access™** ni ningún sistema diagnóstico certificado. El usuario es responsable del uso que haga de los resultados.

## Licencia

Este proyecto está bajo licencia **MIT** — ver [LICENSE](LICENSE).

## Cómo citar

Si usas VisualOGM en un trabajo, cita el repositorio o el DOI (también en [CITATION.cff](CITATION.cff)):

> GenDoc94 (2026). *VisualOGM* (v1.0.0-beta.1) [software]. Hospital Universitario Marqués de Valdecilla, Santander, Spain. https://github.com/GenDoc94/VisualOGM — https://doi.org/10.5281/zenodo.20441643

## Contribuir

Consulta [CONTRIBUTING.md](CONTRIBUTING.md) para reportar errores o enviar mejoras.

## Autor

**GenDoc94** — [GitHub](https://github.com/GenDoc94) · [Buy me a coffee](https://buymeacoffee.com/gendoc94)

En algunas fases del desarrollo se utilizaron asistentes de IA como apoyo; el diseño, la validación y la mayor parte del código son trabajo propio del autor.