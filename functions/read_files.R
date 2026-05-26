library(tidyverse)
library(readxl)
library(jsonlite)

#QUALITY
extract_values <- function(x) {
        map_chr(x, "value", .default = NA) |> as_tibble_row()
}

process_file <- function(f) {
        json_data <- fromJSON(f, simplifyDataFrame = FALSE)
        job_data <- json_data[[1]]
        
        job_tbl <- extract_values(job_data$job$value)
        mqr_tbl <- extract_values(job_data$mqr$value)
        
        bind_cols(job_tbl, mqr_tbl, .name_repair = "unique_quiet") |> mutate(filename = basename(f))
}

quality <- list.files("files/json_files", pattern = "^report_.*\\.json$", full.names = TRUE) |>
        map_df(process_file) |> mutate(
                samplename = as.integer(samplename),
                quantity = parse_number(quantity), #Gbp
                mol_n_50 = parse_number(mol_n_50), #kbp
                coverage = as.numeric(coverage),
                map_rate = parse_number(map_rate),
                fp_rate = as.numeric(fp_rate),
                fn_rate = as.numeric(fn_rate))

message("quality created")

rm(extract_values, process_file)

#ANEUPLOIDY
#List all files that start with number
archives <- list.files(
        path = "files/aneuploidy_files",
        pattern = "^[0-9]+_.*\\.txt$",  # Empiezan con número
        full.names = TRUE
)

# Leer, procesar y combinar
aneuploidies <- archives %>%
        set_names() %>%
        map_dfr(~ {
                read_tsv(
                        .x,
                        na = c("null", "-", "NA"),
                        skip = 2,                   # Saltar las 2 primeras líneas
                        col_types = cols(.default = "c"), # Leer todo como texto para evitar conflictos
                        name_repair = "unique_quiet" 
                ) %>%
                        rename_with(~ sub("^#", "", .x)) %>%  # Eliminar el '#' del nombre de columnas
                        rename(
                                RefcontigID1 = chr,
                                Type = types,
                                fractionalCopyNumber = fractCN,
                                copyNumber = fractChrLen,
                                Confidence = score
                        ) %>%
                        mutate(
                                Id = as.integer(str_extract(basename(.x), "^[0-9]+")),
                                .before = 1
                        )
        }) %>% mutate(
                Id = as.integer(Id),
                Type = paste0("an-", Type),
                Type = as.factor(Type),
                across(c(RefcontigID1, copyNumber, Confidence, fractionalCopyNumber), as.numeric)
        )


rm(archives)
message("aneuploidies created")

#VARIANTS
#List all files that start with number
archives <- list.files(
        path = "files/variants_files",
        pattern = "^[0-9]+_.*\\.txt$",  #start with number
        full.names = TRUE
)

#Read all and add column Id
variants <- archives %>%
        set_names() %>%  #keep the name of the archive
        map_dfr(~ {
                read_tsv(.x, na = c("null", "-", "NA"), comment = "", show_col_types = FALSE) %>% #show_col_types avoid ERROR. WARNING!
                        rename_with(~ sub("^#", "", .x), .cols = 1) %>%
                        mutate(Id = as.integer(str_extract(basename(.x), "^[0-9]+")), .before = 1)
        }) %>% mutate(
                across(c(Classification, Type, Zygosity), as.factor)
        )

variants <- bind_rows(variants, aneuploidies) |> arrange(Id)

message("variants created")

#Nested variant base
n_variant <- variants |> group_by(Id) |> nest()
rm(aneuploidies)


#METADATA
source("scripts/supabase_conection.R")
rm(archives, ddx, dmuestra)
message("metadata created")


#CLINICAL
# Leer el archivo Excel
demograph <- read_excel("files/clinical_files/Basics.xlsx") %>% 
        select(-Petic) #Except Petic just because is in metadata
message("demograph created")


#Mixing data with clinical, metadata and quality (just keeping data)
complete_base <- left_join(n_variant, demograph, by = c("Id" = "NumBN")) %>% 
        left_join(metadata, by="Id") %>% 
        left_join(quality, by = c("Id" = "samplename"))

rm(n_variant)
message("complete_base created")

#Filtering P/LP/VUS variants
cb_filter <- complete_base %>%
        mutate(data = map(data, ~filter(.x, 
                                        # 1. Relevant variants filter
                                        (Classification %in% c("Uncertain significance", 
                                                               "Likely pathogenic", 
                                                               "Pathogenic") | 
                                                 Type %in% c("an-gain", "an-loss")),
                                        
                                        # 2. Quality
                                        (moleculeCount >= 10 | is.na(moleculeCount)),
                                        
                                        # 3. VAF
                                        (VAF >= 0.05 | is.na(VAF))
        )))
message("cb_filter created")