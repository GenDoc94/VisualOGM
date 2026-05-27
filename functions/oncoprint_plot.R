suppressPackageStartupMessages({
  library(ComplexHeatmap)
  library(dplyr)
  library(grid)
})

oncoprint_alteration_colors <- function() {
  c(
    "Presente" = "#E41A1C",
    "DEL" = "#377EB8",
    "DUP" = "#4DAF4A",
    "TRA" = "#984EA3",
    "INS" = "#FF7F00",
    "GAIN" = "#A65628",
    "LOSS" = "#F781BF",
    "INV" = "#999999"
  )
}

map_matrix_value_to_label <- function(value) {
  dplyr::case_when(
    value == 1 ~ "Presente",
    value == 2 ~ "DEL",
    value == 3 ~ "DUP",
    value == 4 ~ "TRA",
    value == 5 ~ "INS",
    value == 6 ~ "GAIN",
    value == 7 ~ "LOSS",
    value == 8 ~ "INV",
    TRUE ~ ""
  )
}

oncoprint_alteration_fun <- function(col) {
  list(
    background = function(x, y, w, h) {
      grid.rect(
        x,
        y,
        w - unit(0.5, "mm"),
        h - unit(0.5, "mm"),
        gp = gpar(fill = "#CCCCCC", col = NA)
      )
    },
    Presente = function(x, y, w, h) {
      grid.rect(
        x,
        y,
        w - unit(0.5, "mm"),
        h - unit(0.5, "mm"),
        gp = gpar(fill = col["Presente"], col = NA)
      )
    },
    DEL = function(x, y, w, h) {
      grid.rect(
        x,
        y,
        w - unit(0.5, "mm"),
        h - unit(0.5, "mm"),
        gp = gpar(fill = col["DEL"], col = NA)
      )
    },
    DUP = function(x, y, w, h) {
      grid.rect(
        x,
        y,
        w - unit(0.5, "mm"),
        h - unit(0.5, "mm"),
        gp = gpar(fill = col["DUP"], col = NA)
      )
    },
    TRA = function(x, y, w, h) {
      grid.rect(
        x,
        y,
        w - unit(0.5, "mm"),
        h - unit(0.5, "mm"),
        gp = gpar(fill = col["TRA"], col = NA)
      )
    },
    INS = function(x, y, w, h) {
      grid.rect(
        x,
        y,
        w - unit(0.5, "mm"),
        h - unit(0.5, "mm"),
        gp = gpar(fill = col["INS"], col = NA)
      )
    },
    GAIN = function(x, y, w, h) {
      grid.rect(
        x,
        y,
        w - unit(0.5, "mm"),
        h - unit(0.5, "mm"),
        gp = gpar(fill = col["GAIN"], col = NA)
      )
    },
    LOSS = function(x, y, w, h) {
      grid.rect(
        x,
        y,
        w - unit(0.5, "mm"),
        h - unit(0.5, "mm"),
        gp = gpar(fill = col["LOSS"], col = NA)
      )
    },
    INV = function(x, y, w, h) {
      grid.rect(
        x,
        y,
        w - unit(0.5, "mm"),
        h - unit(0.5, "mm"),
        gp = gpar(fill = col["INV"], col = NA)
      )
    }
  )
}

build_oncoprint_plot_matrix <- function(complete_matrix) {
  matriz_plot <- apply(
    as.matrix(complete_matrix[, 2:ncol(complete_matrix), drop = FALSE]),
    2,
    map_matrix_value_to_label
  )
  rownames(matriz_plot) <- complete_matrix$Id
  t(matriz_plot)
}

oncoprint_column_title <- function(n_patients, base_title = "OncoPrint OGM") {
  paste0(base_title, " (n = ", n_patients, ")")
}

draw_oncoprint_detailed <- function(
  complete_matrix,
  annotation_data,
  n_patients = nrow(complete_matrix),
  column_title = oncoprint_column_title(n_patients)
) {
  matriz_plot <- build_oncoprint_plot_matrix(complete_matrix)
  col <- oncoprint_alteration_colors()
  alter_fun <- oncoprint_alteration_fun(col)

  niveles_ploidia <- levels(annotation_data$Ploidy)
  colores_ploidia <- setNames(
    c("#B2DF8A", "#FDBF6F", "#A6CEE3", "#FB9A99"),
    niveles_ploidia
  )

  anno_inferior <- HeatmapAnnotation(
    Ploidia = annotation_data$Ploidy,
    Total_Chr = anno_barplot(annotation_data$TotalChr_Stimated),
    col = list(Ploidia = colores_ploidia),
    annotation_legend_param = list(
      Ploidia = list(direction = "horizontal", nrow = 1)
    )
  )

  p <- oncoPrint(
    matriz_plot,
    alter_fun = alter_fun,
    col = col,
    bottom_annotation = anno_inferior,
    alter_fun_is_vectorized = FALSE,
    column_title = column_title,
    heatmap_legend_param = list(
      title = "Tipo de Alteración",
      at = names(col),
      labels = c(
        "Presente",
        "Deleción",
        "Duplicación",
        "Translocación",
        "Inserción",
        "Ganancia",
        "Pérdida",
        "Inversión"
      ),
      direction = "horizontal",
      nrow = 1
    )
  )

  draw(
    p,
    heatmap_legend_side = "bottom",
    annotation_legend_side = "bottom"
  )

  invisible(p)
}
