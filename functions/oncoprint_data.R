suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(stringr)
  library(tibble)
  library(tidyr)
})

matches_gene_region_variant <- function(type_values) {
  tipo_alt <- tolower(as.character(type_values))

  str_detect(tipo_alt, "deletion") |
    str_detect(tipo_alt, "duplication") |
    str_detect(tipo_alt, "insertion") |
    str_detect(tipo_alt, "translocation") |
    str_detect(tipo_alt, "inversion") |
    tipo_alt == "gain" |
    tipo_alt == "loss"
}

classify_variant_type_for_gene_region <- function(type_value) {
  tipo_alt <- tolower(as.character(type_value))

  case_when(
    tipo_alt == "deletion" ~ 2L,
    str_detect(tipo_alt, "duplication") ~ 3L,
    str_detect(tipo_alt, "translocation") ~ 4L,
    tipo_alt == "insertion" ~ 5L,
    tipo_alt == "inversion" ~ 8L,
    tipo_alt == "gain" ~ 6L,
    tipo_alt == "loss" ~ 7L,
    TRUE ~ NA_integer_
  )
}

oncoprint_ploidy_labels <- function() {
  c(
    "Hiperhaploide (24-34)",
    "No hiperdiploide (35-45)",
    "Diploide (46)",
    "Hiperdiploide (\u226547)"
  )
}

oncoprint_padding_for_type <- function(f_type, cnv_overlap, sv_overlap) {
  if (f_type %in% c(0L, 2L, 6L)) {
    return(cnv_overlap)
  }

  sv_overlap
}

bed_region_has_specific_alteration <- function(region_name) {
  str_detect(region_name, fixed("insertion", ignore_case = TRUE)) |
    str_detect(region_name, fixed("deletion", ignore_case = TRUE)) |
    str_detect(region_name, fixed("inversion", ignore_case = TRUE)) |
    str_detect(region_name, fixed("translocation", ignore_case = TRUE)) |
    str_detect(region_name, fixed("rearrangement", ignore_case = TRUE)) |
    str_detect(region_name, fixed("duplication", ignore_case = TRUE)) |
    str_detect(region_name, fixed("gain", ignore_case = TRUE)) |
    str_detect(region_name, fixed("loss", ignore_case = TRUE))
}

assign_bed_region_type <- function(region_name) {
  case_when(
    str_detect(region_name, fixed("insertion", ignore_case = TRUE)) ~ 1L,
    str_detect(region_name, fixed("deletion", ignore_case = TRUE)) ~ 2L,
    str_detect(region_name, fixed("inversion", ignore_case = TRUE)) ~ 3L,
    str_detect(region_name, fixed("translocation", ignore_case = TRUE)) |
      str_detect(region_name, fixed("rearrangement", ignore_case = TRUE)) ~ 4L,
    str_detect(region_name, fixed("duplication", ignore_case = TRUE)) ~ 5L,
    str_detect(region_name, fixed("gain", ignore_case = TRUE)) ~ 6L,
    str_detect(region_name, fixed("loss", ignore_case = TRUE)) ~ 2L,
    TRUE ~ 0L
  )
}

read_oncoprint_bed <- function(path) {
  filter_bed <- read_tsv(
    path,
    skip = 1,
    col_names = c("chrom", "start", "end", "name"),
    col_types = "iiic",
    show_col_types = FALSE
  )

  filter_bed |>
    mutate(
      type = assign_bed_region_type(name),
      detection_mode = if_else(
        bed_region_has_specific_alteration(name),
        "present",
        "typed"
      )
    )
}

add_supertype <- function(variants) {
  if ("SuperType" %in% names(variants)) {
    return(variants |> mutate(SuperType = as.numeric(SuperType)))
  }

  variants |>
    mutate(
      SuperType = case_when(
        str_detect(tolower(as.character(Type)), "insertion") ~ 1,
        str_detect(tolower(as.character(Type)), "deletion") ~ 2,
        str_detect(tolower(as.character(Type)), "inversion") ~ 3,
        str_detect(tolower(as.character(Type)), "translocation|rearrangement") ~ 4,
        str_detect(tolower(as.character(Type)), "duplication") ~ 5,
        tolower(as.character(Type)) == "gain" ~ 6,
        tolower(as.character(Type)) == "loss" ~ 2,
        TRUE ~ NA_real_
      )
    )
}

prepare_oncoprint_sample_data <- function(
  classified_path,
  classified_name,
  aneuploidy_path,
  aneuploidy_name,
  classifications = relevant_classifications,
  min_molecule_count = 10,
  min_vaf = 0.05
) {
  case <- prepare_circleplot_case(
    classified_path = classified_path,
    classified_name = classified_name,
    aneuploidy_path = aneuploidy_path,
    aneuploidy_name = aneuploidy_name,
    classifications = classifications,
    min_molecule_count = min_molecule_count,
    min_vaf = min_vaf
  )

  case$filtered_variants |> add_supertype()
}

match_cohort_uploads <- function(classified_names, aneuploidy_names) {
  if (length(classified_names) == 0 || length(aneuploidy_names) == 0) {
    stop(
      "Sube al menos un archivo Classified_Variants y uno de Aneuploidy.",
      call. = FALSE
    )
  }

  classified_tbl <- tibble(
    name = classified_names,
    id = map_int(classified_names, extract_sample_id)
  )
  aneuploidy_tbl <- tibble(
    name = aneuploidy_names,
    id = map_int(aneuploidy_names, extract_sample_id)
  )

  invalid_classified <- classified_tbl |> filter(is.na(id))
  if (nrow(invalid_classified) > 0) {
    stop(
      "Estos Classified_Variants no empiezan por numero de muestra: ",
      paste(invalid_classified$name, collapse = ", "),
      call. = FALSE
    )
  }

  invalid_aneuploidy <- aneuploidy_tbl |> filter(is.na(id))
  if (nrow(invalid_aneuploidy) > 0) {
    stop(
      "Estos Aneuploidy no empiezan por numero de muestra: ",
      paste(invalid_aneuploidy$name, collapse = ", "),
      call. = FALSE
    )
  }

  dup_classified <- classified_tbl |>
    count(id) |>
    filter(n > 1)
  if (nrow(dup_classified) > 0) {
    stop(
      "Hay mas de un Classified_Variants para la muestra ",
      paste(dup_classified$id, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  dup_aneuploidy <- aneuploidy_tbl |>
    count(id) |>
    filter(n > 1)
  if (nrow(dup_aneuploidy) > 0) {
    stop(
      "Hay mas de un Aneuploidy para la muestra ",
      paste(dup_aneuploidy$id, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  classified_ids <- classified_tbl$id
  aneuploidy_ids <- aneuploidy_tbl$id
  missing_aneuploidy <- setdiff(classified_ids, aneuploidy_ids)
  missing_classified <- setdiff(aneuploidy_ids, classified_ids)

  if (length(missing_aneuploidy) > 0 || length(missing_classified) > 0) {
    messages <- character()

    if (length(missing_aneuploidy) > 0) {
      messages <- c(
        messages,
        paste0(
          "Falta Aneuploidy para la(s) muestra(s): ",
          paste(missing_aneuploidy, collapse = ", ")
        )
      )
    }

    if (length(missing_classified) > 0) {
      messages <- c(
        messages,
        paste0(
          "Falta Classified_Variants para la(s) muestra(s): ",
          paste(missing_classified, collapse = ", ")
        )
      )
    }

    stop(paste(messages, collapse = " "), call. = FALSE)
  }

  patient_ids <- sort(classified_ids)

  list(
    patient_ids = patient_ids,
    classified_tbl = classified_tbl,
    aneuploidy_tbl = aneuploidy_tbl
  )
}

build_oncoprint_cohort_base <- function(
  classified_paths,
  classified_names,
  aneuploidy_paths,
  aneuploidy_names,
  classifications = relevant_classifications,
  min_molecule_count = 10,
  min_vaf = 0.05
) {
  match_info <- match_cohort_uploads(classified_names, aneuploidy_names)

  classified_tbl <- match_info$classified_tbl |>
    mutate(path = classified_paths[match(name, classified_names)])
  aneuploidy_tbl <- match_info$aneuploidy_tbl |>
    mutate(path = aneuploidy_paths[match(name, aneuploidy_names)])

  patient_data <- map(match_info$patient_ids, function(patient_id) {
    classified_row <- classified_tbl |> filter(id == patient_id)
    aneuploidy_row <- aneuploidy_tbl |> filter(id == patient_id)

    prepare_oncoprint_sample_data(
      classified_path = classified_row$path[[1]],
      classified_name = classified_row$name[[1]],
      aneuploidy_path = aneuploidy_row$path[[1]],
      aneuploidy_name = aneuploidy_row$name[[1]],
      classifications = classifications,
      min_molecule_count = min_molecule_count,
      min_vaf = min_vaf
    )
  })

  tibble(
    Id = match_info$patient_ids,
    data = patient_data
  )
}

check_variant <- function(
  sample_data,
  f_chrom,
  f_start,
  f_end,
  f_type,
  f_name,
  cnv_overlap = 500000,
  sv_overlap = 0
) {
  pad <- oncoprint_padding_for_type(f_type, cnv_overlap, sv_overlap)
  f_start_adj <- f_start - pad
  f_end_adj <- f_end + pad

  igh_chrom <- 14
  igh_start <- 105583731 - cnv_overlap
  igh_end <- 106879812 + cnv_overlap

  if (f_type == 4) {
    hits <- sample_data |>
      filter(
        as.character(RefcontigID1) == as.character(f_chrom) |
          as.character(RefcontigID2) == as.character(f_chrom)
      )
  } else {
    hits <- sample_data |>
      filter(as.character(RefcontigID1) == as.character(f_chrom))
  }

  if (nrow(hits) == 0) {
    return(0)
  }

  final_hits <- hits |>
    filter(
      case_when(
        f_type == 0 ~ (RefStartPos <= f_end_adj & RefEndPos >= f_start_adj) &
          matches_gene_region_variant(Type),
        f_type == 2 ~ (SuperType %in% 2 | tolower(as.character(Type)) == "loss") &
          (RefStartPos <= f_end_adj & RefEndPos >= f_start_adj),
        f_type == 4 ~ (SuperType %in% 4) & (
          (as.character(RefcontigID1) == as.character(f_chrom) &
            RefStartPos >= f_start_adj &
            RefStartPos <= f_end_adj &
            (!str_detect(f_name, "IGH_translocation") |
              (as.character(RefcontigID2) == as.character(igh_chrom) &
                RefEndPos >= igh_start &
                RefEndPos <= igh_end))) |
            (as.character(RefcontigID2) == as.character(f_chrom) &
              RefEndPos >= f_start_adj &
              RefEndPos <= f_end_adj &
              (!str_detect(f_name, "IGH_translocation") |
                (as.character(RefcontigID1) == as.character(igh_chrom) &
                  RefStartPos >= igh_start &
                  RefStartPos <= igh_end)))
        ),
        f_type == 6 ~ (tolower(as.character(Type)) == "gain") &
          (RefStartPos <= f_end_adj & RefEndPos >= f_start_adj),
        TRUE ~ (SuperType %in% f_type) &
          (RefStartPos <= f_end_adj & RefEndPos >= f_start_adj)
      )
    )

  if (nrow(final_hits) == 0) {
    return(0)
  }

  # Regiones con solo nombre de gen: color por tipo de alteracion estructural.
  if (f_type == 0L) {
    typed_hits <- final_hits |>
      mutate(
        variant_code = classify_variant_type_for_gene_region(Type)
      ) |>
      filter(!is.na(variant_code))

    if (nrow(typed_hits) == 0) {
      return(0)
    }

    return(typed_hits$variant_code[[1]])
  }

  # Regiones con alteracion indicada en el nombre del BED: presencia binaria.
  1
}

build_oncoprint_matrix <- function(
  cohort_base,
  region_filter,
  cnv_overlap = 500000,
  sv_overlap = 0
) {
  complete_matrix <- cohort_base |>
    mutate(
      presence = map(
        data,
        function(sample_df) {
          results_vector <- pmap_dbl(
            region_filter,
            function(chrom, start, end, name, type, detection_mode) {
              check_variant(
                sample_df,
                chrom,
                start,
                end,
                type,
                name,
                cnv_overlap = cnv_overlap,
                sv_overlap = sv_overlap
              )
            }
          )

          as_tibble(
            as.list(setNames(results_vector, region_filter$name)),
            .name_repair = "minimal"
          )
        }
      )
    ) |>
    select(Id, presence) |>
    unnest(presence)

  as.data.frame(complete_matrix)
}

build_aneuploidy_columns <- function(cohort_base) {
  chrom_list <- c(as.character(1:22), "X", "Y")
  chrom_names <- map(
    chrom_list,
    ~ c(paste0("Loss_", .x), paste0("Gain_", .x))
  ) |>
    unlist()

  cohort_base |>
    mutate(
      aneu_cols = map(
        data,
        function(df) {
          vec <- setNames(rep(0, length(chrom_names)), chrom_names)

          anes <- df |>
            filter(Type %in% c("an-loss", "an-gain")) |>
            mutate(
              chrom_label = case_when(
                RefcontigID1 == 23 ~ "X",
                RefcontigID1 == 24 ~ "Y",
                TRUE ~ as.character(RefcontigID1)
              ),
              valor_aneu = case_when(
                Type == "an-loss" &
                  fractionalCopyNumber >= 0.50 &
                  fractionalCopyNumber <= 1.99 ~ 1,
                Type == "an-loss" &
                  fractionalCopyNumber >= 0.00 &
                  fractionalCopyNumber <= 0.50 ~ 2,
                Type == "an-gain" &
                  fractionalCopyNumber >= 3.00 &
                  fractionalCopyNumber <= 3.99 ~ 1,
                Type == "an-gain" &
                  fractionalCopyNumber >= 4.00 &
                  fractionalCopyNumber <= 4.99 ~ 2,
                Type == "an-gain" &
                  fractionalCopyNumber >= 5.00 &
                  fractionalCopyNumber <= 5.99 ~ 3,
                Type == "an-gain" &
                  fractionalCopyNumber >= 6.00 &
                  fractionalCopyNumber <= 6.99 ~ 4,
                Type == "an-gain" &
                  fractionalCopyNumber >= 7.00 &
                  fractionalCopyNumber <= 7.99 ~ 5,
                TRUE ~ 0
              ),
              col_name = case_when(
                Type == "an-loss" ~ paste0("Loss_", chrom_label),
                Type == "an-gain" ~ paste0("Gain_", chrom_label)
              )
            ) |>
            filter(valor_aneu > 0)

          if (nrow(anes) > 0) {
            for (i in seq_len(nrow(anes))) {
              nombre_col <- anes$col_name[i]
              if (nombre_col %in% chrom_names) {
                vec[nombre_col] <- anes$valor_aneu[i]
              }
            }
          }

          as_tibble(as.list(vec))
        }
      )
    ) |>
    select(Id, aneu_cols) |>
    unnest(aneu_cols)
}

build_oncoprint_annotations <- function(complete_matrix, cohort_base) {
  aneuploidy_data <- build_aneuploidy_columns(cohort_base)

  final_matrix_extended <- complete_matrix |>
    left_join(aneuploidy_data, by = "Id")

  cols_gain <- grep("^Gain_", names(final_matrix_extended), value = TRUE)
  cols_loss <- grep("^Loss_", names(final_matrix_extended), value = TRUE)

  final_matrix_extended |>
    mutate(
      TotalChr_Stimated = 46 +
        rowSums(across(all_of(cols_gain)), na.rm = TRUE) -
        rowSums(across(all_of(cols_loss)), na.rm = TRUE),
      Ploidy = case_when(
        TotalChr_Stimated >= 24 & TotalChr_Stimated <= 34 ~ 1,
        TotalChr_Stimated >= 35 & TotalChr_Stimated <= 45 ~ 2,
        TotalChr_Stimated == 46 ~ 3,
        TotalChr_Stimated >= 47 ~ 4,
        TRUE ~ NA_real_
      ),
      Ploidy = factor(
        Ploidy,
        levels = c(1, 2, 3, 4),
        labels = oncoprint_ploidy_labels()
      )
    )
}

prepare_oncoprint_cohort <- function(
  classified_paths,
  classified_names,
  aneuploidy_paths,
  aneuploidy_names,
  bed_path,
  cnv_overlap = 500000,
  sv_overlap = 0,
  classifications = relevant_classifications,
  min_molecule_count = 10,
  min_vaf = 0.05
) {
  region_filter <- read_oncoprint_bed(bed_path)
  cohort_base <- build_oncoprint_cohort_base(
    classified_paths = classified_paths,
    classified_names = classified_names,
    aneuploidy_paths = aneuploidy_paths,
    aneuploidy_names = aneuploidy_names,
    classifications = classifications,
    min_molecule_count = min_molecule_count,
    min_vaf = min_vaf
  )
  complete_matrix <- build_oncoprint_matrix(
    cohort_base,
    region_filter,
    cnv_overlap = cnv_overlap,
    sv_overlap = sv_overlap
  )
  annotation_data <- build_oncoprint_annotations(complete_matrix, cohort_base)

  list(
    region_filter = region_filter,
    cohort_base = cohort_base,
    complete_matrix = complete_matrix,
    annotation_data = annotation_data,
    n_patients = nrow(cohort_base),
    n_regions = nrow(region_filter),
    cnv_overlap = cnv_overlap,
    sv_overlap = sv_overlap,
    patient_ids = cohort_base$Id
  )
}

summarise_oncoprint_cohort <- function(cohort) {
  tibble(
    Metrica = c(
      "Pacientes incluidos",
      "Regiones del archivo BED",
      "Padding CNV (bp)",
      "Padding SV (bp)"
    ),
    Valor = c(
      cohort$n_patients,
      cohort$n_regions,
      cohort$cnv_overlap,
      cohort$sv_overlap
    )
  )
}
