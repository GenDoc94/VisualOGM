suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(stringr)
  library(tibble)
  library(tidyr)
})

relevant_classifications <- c(
  "Uncertain significance",
  "Likely pathogenic",
  "Pathogenic"
)

extract_sample_id <- function(filename) {
  sample_id <- str_extract(basename(filename), "^[0-9]+")

  if (is.na(sample_id)) {
    return(NA_integer_)
  }

  as.integer(sample_id)
}

check_file_name <- function(filename, expected_text, file_label) {
  if (!str_detect(basename(filename), fixed(expected_text, ignore_case = TRUE))) {
    stop(
      "Comprueba que has subido un archivo \"",
      expected_text,
      "\".",
      call. = FALSE
    )
  }

  invisible(filename)
}

check_required_columns <- function(data, required_columns, file_label) {
  missing_columns <- setdiff(required_columns, names(data))

  if (length(missing_columns) > 0) {
    stop(
      file_label,
      " no contiene las columnas esperadas: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(data)
}

as_numeric_if_present <- function(data, columns) {
  present_columns <- intersect(columns, names(data))

  mutate(
    data,
    across(all_of(present_columns), ~ parse_number(as.character(.x)))
  )
}

read_classified_variants <- function(path, original_name = basename(path)) {
  sample_id <- extract_sample_id(original_name)

  if (is.na(sample_id)) {
    stop(
      "El archivo Classified_Variants debe empezar por el número de muestra.",
      call. = FALSE
    )
  }

  variants <- suppressWarnings(read_tsv(
    path,
    na = c("null", "-", "NA", ""),
    comment = "",
    show_col_types = FALSE
  )) |>
    rename_with(~ sub("^#", "", .x)) |>
    mutate(Id = sample_id, .before = 1)

  check_required_columns(
    variants,
    c(
      "Id",
      "Classification",
      "Type",
      "RefcontigID1",
      "RefcontigID2",
      "RefStartPos",
      "RefEndPos",
      "fractionalCopyNumber",
      "moleculeCount",
      "VAF"
    ),
    "El archivo Classified_Variants"
  )

  variants |>
    mutate(across(c(Classification, Type), as.character)) |>
    as_numeric_if_present(
      c(
        "RefcontigID1",
        "RefcontigID2",
        "RefStartPos",
        "RefEndPos",
        "fractionalCopyNumber",
        "moleculeCount",
        "VAF"
      )
    )
}

read_aneuploidy <- function(path, original_name = basename(path)) {
  sample_id <- extract_sample_id(original_name)

  if (is.na(sample_id)) {
    stop(
      "El archivo Aneuploidy debe empezar por el número de muestra.",
      call. = FALSE
    )
  }

  aneuploidies <- read_tsv(
    path,
    na = c("null", "-", "NA", ""),
    skip = 2,
    col_types = cols(.default = "c"),
    name_repair = "unique_quiet",
    show_col_types = FALSE
  ) |>
    rename_with(~ sub("^#", "", .x))

  check_required_columns(
    aneuploidies,
    c("chr", "types", "fractChrLen", "score", "fractCN"),
    "El archivo Aneuploidy"
  )

  aneuploidies |>
    rename(
      RefcontigID1 = chr,
      Type = types,
      copyNumber = fractChrLen,
      Confidence = score,
      fractionalCopyNumber = fractCN
    ) |>
    mutate(
      Id = sample_id,
      Type = paste0("an-", Type),
      .before = 1
    ) |>
    as_numeric_if_present(
      c("RefcontigID1", "copyNumber", "Confidence", "fractionalCopyNumber")
    )
}

filter_circleplot_variants <- function(variants) {
  variants |>
    filter(
      Classification %in% relevant_classifications |
        Type %in% c("an-gain", "an-loss"),
      moleculeCount >= 10 | is.na(moleculeCount),
      VAF >= 0.05 | is.na(VAF)
    )
}

filter_classified_variants <- function(
  variants,
  classifications = relevant_classifications,
  min_molecule_count = 10,
  min_vaf = 0.05
) {
  if (is.null(min_molecule_count) || is.na(min_molecule_count)) {
    min_molecule_count <- 10
  }

  if (is.null(min_vaf) || is.na(min_vaf)) {
    min_vaf <- 0.05
  }

  variants |>
    filter(
      Classification %in% classifications,
      moleculeCount >= min_molecule_count | is.na(moleculeCount),
      VAF >= min_vaf | is.na(VAF)
    )
}

prepare_circleplot_case <- function(
  classified_path,
  classified_name,
  aneuploidy_path,
  aneuploidy_name,
  classifications = relevant_classifications,
  min_molecule_count = 10,
  min_vaf = 0.05
) {
  check_file_name(
    classified_name,
    expected_text = "Classified_Variants",
    file_label = "Classified_Variants"
  )
  check_file_name(
    aneuploidy_name,
    expected_text = "Aneuploidy",
    file_label = "Aneuploidy"
  )

  classified_id <- extract_sample_id(classified_name)
  aneuploidy_id <- extract_sample_id(aneuploidy_name)

  if (is.na(classified_id) || is.na(aneuploidy_id)) {
    stop(
      "Ambos archivos deben empezar por el mismo número de muestra.",
      call. = FALSE
    )
  }

  if (!identical(classified_id, aneuploidy_id)) {
    stop(
      "Los archivos no pertenecen a la misma muestra: Classified_Variants empieza por ",
      classified_id,
      " y Aneuploidy empieza por ",
      aneuploidy_id,
      ".",
      call. = FALSE
    )
  }

  classified_variants <- read_classified_variants(classified_path, classified_name)
  aneuploidies <- read_aneuploidy(aneuploidy_path, aneuploidy_name)
  filtered_classified_variants <- filter_classified_variants(
    classified_variants,
    classifications = classifications,
    min_molecule_count = min_molecule_count,
    min_vaf = min_vaf
  )

  combined_variants <- bind_rows(classified_variants, aneuploidies) |>
    arrange(Id)

  filtered_variants <- bind_rows(filtered_classified_variants, aneuploidies) |>
    arrange(Id)
  dbase <- tibble(Id = classified_id, data = list(filtered_variants))

  list(
    id = classified_id,
    dbase = dbase,
    classified_variants = classified_variants,
    aneuploidies = aneuploidies,
    raw_variants = combined_variants,
    filtered_classified_variants = filtered_classified_variants,
    filtered_variants = filtered_variants
  )
}

summarise_circleplot_case <- function(processed_case) {
  tibble(
    Metrica = c(
      "Variantes totales leidas",
      "Variantes estructurales leidas",
      "Aneuploidias leidas",
      "Variantes estructurales que pasan filtro"
    ),
    Valor = c(
      nrow(processed_case$raw_variants),
      nrow(processed_case$classified_variants),
      nrow(processed_case$aneuploidies),
      nrow(processed_case$filtered_classified_variants)
    )
  )
}
