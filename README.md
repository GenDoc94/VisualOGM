BioCalc Shiny Circle Plot
=========================

Aplicacion Shiny ligera para generar un circle plot a partir de dos archivos del mismo caso:

- `*_Classified_Variants*.txt`
- `*_Aneuploidy.txt`

La app valida que ambos archivos empiecen por el mismo numero de muestra, lee los datos con la misma logica del workflow y conserva solo:

- variantes `Pathogenic`, `Likely pathogenic` y `Uncertain significance`
- variantes con `moleculeCount >= 10`, cuando el campo esta informado
- variantes con `VAF >= 0.05`, cuando el campo esta informado
- aneuploidias `gain` y `loss`

Uso local
---------

Instala las dependencias si no estan disponibles:

```r
install.packages(c("shiny", "tidyverse", "circlize"))
```

Ejecuta la aplicacion desde la raiz del proyecto:

```r
shiny::runApp()
```

Despues sube el archivo de variantes clasificadas y el archivo de aneuploidias. El grafico se actualiza automaticamente y se puede descargar en PDF o PNG.

Archivos principales
--------------------

- `app.R`: interfaz y servidor Shiny.
- `functions/shiny_data.R`: lectura, validacion, combinacion y filtrado de los archivos subidos.
- `functions/circleplot.R`: generacion del circle plot con `circlize`.

Datos sensibles
----------------

Los directorios con datos locales o de ejemplo estan ignorados por Git mediante `.gitignore` (`examples_files/`, `files/`, `bases/`, `graphs/`). Si alguno de esos archivos ya estuviera versionado, habria que retirarlo del seguimiento de Git antes de publicar el repositorio.
