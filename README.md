# VisualOGM

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Aplicación **Shiny** para visualizar resultados de **Bionano OGM**: circle plots por muestra y oncoprints de cohortes a partir de los archivos exportados por la plataforma de análisis.

Repositorio: [github.com/GenDoc94/VisualOGM](https://github.com/GenDoc94/VisualOGM)

## Funcionalidades

- **CirclePlot**: un par `*_Classified_Variants*.txt` + `*_Aneuploidy.txt` por muestra.
- **Oncoprint**: varios pares de archivos + un `.bed` de regiones de interés.
- Filtros por clasificación ACMG, número de moléculas y VAF.
- Exportación a PDF, PNG (CirclePlot) y CSV de variantes filtradas.

La pestaña **Información** de la app incluye capturas de cómo descargar los ficheros desde OGM.

## Requisitos

- R >= 4.1 recomendado
- Paquetes CRAN: `shiny`, `tidyverse`, `circlize`, `readr`
- Paquete Bioconductor: `ComplexHeatmap`

```r
install.packages(c("shiny", "tidyverse", "circlize", "readr"))

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

| Archivo | Uso |
|---------|-----|
| `*_Classified_Variants*.txt` | Variantes estructurales clasificadas |
| `*_Aneuploidy.txt` | Aneuploidías |
| `*.bed` | Regiones para Oncoprint (`chrom`, `start`, `end`, `name`) |

Los nombres de Classified_Variants y Aneuploidy de un mismo caso deben compartir el mismo identificador de muestra al inicio del nombre.

## Estructura del proyecto

- `app.R` — interfaz y servidor Shiny
- `functions/` — lectura, validación y gráficos
- `www/` — logo y capturas de la pestaña Información

## Publicar en internet (opcional)

Para compartir la app sin que cada usuario instale R:

- [shinyapps.io](https://www.shinyapps.io/) — despliegue sencillo; revisa su política de privacidad si subes datos reales.
- Posit Connect, Shiny Server o Docker con tu propio servidor.

En despliegues públicos, indica claramente quién opera el servidor y cómo se tratan los datos clínicos.

## Aviso legal / uso clínico

VisualOGM es una **herramienta de apoyo a la investigación y la visualización**. No sustituye la interpretación clínica, la validación en la plataforma OGM ni ningún sistema diagnóstico certificado. El usuario es responsable del uso que haga de los resultados.

## Licencia

Este proyecto está bajo licencia **MIT** — ver [LICENSE](LICENSE).

## Cómo citar

Si usas VisualOGM en un trabajo, cita el repositorio (también disponible en [CITATION.cff](CITATION.cff)):

> GenDoc94 (2026). *VisualOGM* (v1.0.0). https://github.com/GenDoc94/VisualOGM

## Contribuir

Consulta [CONTRIBUTING.md](CONTRIBUTING.md) para reportar errores o enviar mejoras.

## Autor

**GenDoc94** — [GitHub](https://github.com/GenDoc94) · [Buy me a coffee](https://buymeacoffee.com/gendoc94)
