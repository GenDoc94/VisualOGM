#Code to generate the circleplot similar to Bionano
suppressPackageStartupMessages({
        library(circlize)
        library(purrr)
        library(stringr)
        library(tidyverse)
})

circleplot <- function(id_pac, dbase, show_sample_id = FALSE) {
        #Selecting patient
        case_row <- dbase |> select(Id, data) |> filter(Id == id_pac)
        if (nrow(case_row) == 0) {
                stop("No data found for patient id: ", id_pac, call. = FALSE)
        }
        case <- case_row$data[[1]]
        chromosome_index <- paste0("chr", c(1:22, "X", "Y"))

        #Generating bases for the graph
        aneuplos <- case |>
                filter(Type == "an-gain" | Type == "an-loss") |>
                select(Id, RefcontigID1, Type) |>
                rename(chr = RefcontigID1) |>
                mutate(chr = paste("chr", chr, sep = "")) |>
                ungroup() |> select(-Id) |>
                mutate(chr = case_when(
                        chr == "chr23" ~ "chrX",
                        chr == "chr24" ~ "chrY",
                        TRUE ~ chr))
        
        deletions <- case |> 
                filter(Type == "deletion") |> 
                select(Id, RefcontigID1, RefStartPos, RefEndPos) |>
                rename(chr = RefcontigID1) |>
                mutate(chr = paste("chr", chr, sep = "")) |>
                ungroup() |> select(-Id) |>
                mutate(chr = case_when(
                        chr == "chr23" ~ "chrX",
                        chr == "chr24" ~ "chrY",
                        TRUE ~ chr))
        
        insertions <- case |> 
                filter(Type == "insertion") |> 
                select(Id, RefcontigID1, RefStartPos, RefEndPos) |>
                rename(chr = RefcontigID1) |>
                mutate(chr = paste("chr", chr, sep = "")) |>
                ungroup() |> select(-Id) |>
                mutate(chr = case_when(
                        chr == "chr23" ~ "chrX",
                        chr == "chr24" ~ "chrY",
                        TRUE ~ chr))
        
        duplications <- case |> 
                filter(str_starts(Type, "duplication")) |> 
                select(Id, RefcontigID1, RefStartPos, RefEndPos) |>
                rename(chr = RefcontigID1) |>
                mutate(chr = paste("chr", chr, sep = "")) |>
                ungroup() |> select(-Id) |>
                mutate(chr = case_when(
                        chr == "chr23" ~ "chrX",
                        chr == "chr24" ~ "chrY",
                        TRUE ~ chr))
        
        inversions <- case |> 
                filter(str_starts(Type, "inversion")) |> 
                select(Id, RefcontigID1, RefStartPos, RefEndPos) |>
                rename(chr = RefcontigID1) |>
                mutate(chr = paste("chr", chr, sep = "")) |>
                ungroup() |> select(-Id) |>
                mutate(chr = case_when(
                        chr == "chr23" ~ "chrX",
                        chr == "chr24" ~ "chrY",
                        TRUE ~ chr))
        
        gains <- case |> 
                filter(Type == "gain") |> 
                select(Id, RefcontigID1, RefStartPos, RefEndPos, fractionalCopyNumber) |>
                rename(chr = RefcontigID1) |>
                mutate(chr = paste("chr", chr, sep = "")) |>
                ungroup() |> select(-Id) |>
                mutate(chr = case_when(
                        chr == "chr23" ~ "chrX",
                        chr == "chr24" ~ "chrY",
                        TRUE ~ chr))
        
        losses <- case |> 
                filter(Type == "loss") |> 
                select(Id, RefcontigID1, RefStartPos, RefEndPos, fractionalCopyNumber) |>
                rename(chr = RefcontigID1) |>
                mutate(chr = paste("chr", chr, sep = "")) |>
                ungroup() |> select(-Id) |>
                mutate(chr = case_when(
                        chr == "chr23" ~ "chrX",
                        chr == "chr24" ~ "chrY",
                        TRUE ~ chr))
        
        translocations <- case |> 
                filter(str_starts(Type, "translocation")) |> 
                select(Id, RefcontigID1, RefcontigID2, RefStartPos, RefEndPos) |>
                rename(chr1 = RefcontigID1, chr2 = RefcontigID2) |>
                mutate(chr1 = paste("chr", chr1, sep = ""), chr2 = paste("chr", chr2, sep = "")) |>
                ungroup() |> select(-Id) |>
                mutate(chr1 = case_when(
                        chr1 == "chr23" ~ "chrX",
                        chr1 == "chr24" ~ "chrY",
                        TRUE ~ chr1)) |>
                mutate(chr2 = case_when(
                        chr2 == "chr23" ~ "chrX",
                        chr2 == "chr24" ~ "chrY",
                        TRUE ~ chr2))
        
        #Starting the graph
        circos.clear()
        on.exit(circos.clear(), add = TRUE)
        if (isTRUE(show_sample_id)) {
                graphics::par(oma = graphics::par("oma") + c(0, 0, 1.3, 0))
        }
        circos.par(start.degree = 90)
        if (exists("cytobands", inherits = TRUE)) {
                circos.initializeWithIdeogram(
                        cytoband = get("cytobands", inherits = TRUE),
                        species = "hg38",
                        plotType = c("ideogram", "axis", "labels"),
                        chromosome.index = chromosome_index,
                        sort.chr = FALSE
                )
        } else {
                circos.initializeWithIdeogram(
                        species = "hg38",
                        plotType = c("ideogram", "axis", "labels"),
                        chromosome.index = chromosome_index,
                        sort.chr = FALSE
                )
        }
        
        
        #SNVs
        circos.trackPlotRegion(
                ylim = c(0, 1),
                track.height = 0.05,
                bg.border = "grey40",
                panel.fun = function(x, y) {
                        chr <- get.cell.meta.data("sector.index")
                        
                        #DELECTIONS
                        d <- deletions[deletions$chr == chr, ]
                        
                        if (nrow(d) > 0) {
                                # Dibujar puntos en el centro de cada deleción
                                mid_pos <- (d$RefStartPos + d$RefEndPos) / 2
                                circos.points(mid_pos, rep(0.5, nrow(d)), 
                                              col = "#f3794b", 
                                              pch = 16,        # tipo de punto (16 = círculo sólido)
                                              cex = 0.8)       # tamaño del punto
                        }
                        
                        #INSERTIONS
                        i <- insertions[insertions$chr == chr, ]
                        
                        if (nrow(i) > 0) {
                                # Dibujar puntos en el centro de cada deleción
                                mid_pos <- (i$RefStartPos + i$RefEndPos) / 2
                                circos.points(mid_pos, rep(0.5, nrow(i)), 
                                              col = "#2e868b", 
                                              pch = 16,        # tipo de punto (16 = círculo sólido)
                                              cex = 0.8)       # tamaño del punto
                        }
                        
                        #DUPLICATIONS
                        dp <- duplications[duplications$chr == chr, ]
                        
                        if (nrow(dp) > 0) {
                                # Dibujar puntos en el centro de cada deleción
                                mid_pos <- (dp$RefStartPos + dp$RefEndPos) / 2
                                circos.points(mid_pos, rep(0.5, nrow(dp)), 
                                              col = "#9999ff", 
                                              pch = 16,        # tipo de punto (16 = círculo sólido)
                                              cex = 0.8)       # tamaño del punto
                        }
                        
                        #INVERSIONS
                        inv <- inversions[inversions$chr == chr, ]
                        
                        if (nrow(inv) > 0) {
                                # Dibujar puntos en el centro de cada deleción
                                mid_pos <- (inv$RefStartPos + inv$RefEndPos) / 2
                                circos.points(mid_pos, rep(0.5, nrow(inv)), 
                                              col = "#6fa9db", 
                                              pch = 16,        # tipo de punto (16 = círculo sólido)
                                              cex = 0.8)       # tamaño del punto
                        }
                        
                        
                }
        )
        
        #CNVs
        circos.trackPlotRegion(
                ylim = c(min(losses$fractionalCopyNumber, gains$fractionalCopyNumber, 2) - 0.5,
                         max(losses$fractionalCopyNumber, gains$fractionalCopyNumber, 2) + 0.5),
                track.height = 0.1,
                bg.border = "grey40",
                panel.fun = function(x, y) {
                        current_chr <- get.cell.meta.data("sector.index")
                        if (!grepl("^chr", current_chr)) {
                                current_chr <- paste0("chr", current_chr)
                        }
                        
                        xlim <- get.cell.meta.data("xlim")
                        
                        # Línea base punteada en 2
                        circos.lines(x = xlim, y = rep(2, 2), col = "black", lty = 2, lwd = 1)
                        
                        # Filtrar ganancias y pérdidas del cromosoma actual
                        d_gains <- gains %>% filter(chr == current_chr)
                        d_losses <- losses %>% filter(chr == current_chr)
                        
                        # Función auxiliar para dibujar escalones
                        draw_cnvs <- function(df, color) {
                                if (nrow(df) > 0) {
                                        for (i in seq_len(nrow(df))) {
                                                xs <- c(df$RefStartPos[i], df$RefStartPos[i], df$RefEndPos[i], df$RefEndPos[i])
                                                ys <- c(2, df$fractionalCopyNumber[i], df$fractionalCopyNumber[i], 2)
                                                circos.lines(xs, ys, col = color, lwd = 2)
                                        }
                                }
                        }
                        
                        draw_cnvs(d_gains, "#9999ff")
                        draw_cnvs(d_losses, "#f3794b")
                        
                        #ANEUPLOIDIES
                        d_aneu <- aneuplos %>% filter(chr == current_chr)
                        if (nrow(d_aneu) > 0) {
                                y_min <- min(c(losses$fractionalCopyNumber, gains$fractionalCopyNumber, 2)) - 0.4
                                for (i in seq_len(nrow(d_aneu))) {
                                        color <- ifelse(d_aneu$Type[i] == "an-gain", "#9999ff", "#f3794b")
                                        circos.lines(
                                                x = xlim, 
                                                y = rep(y_min, 2), 
                                                col = color, 
                                                lwd = 3
                                        )
                                }
                        }
                }
        )
        
        #TRANSLOCATIONS
        for(i in seq_len(nrow(translocations))) {
                chr_a <- translocations$chr1[i]
                chr_b <- translocations$chr2[i]
                circos.link(
                        sector.index1 = chr_a, point1 = translocations$RefStartPos[i],
                        sector.index2 = chr_b, point2 = translocations$RefEndPos[i],
                        col = "#eb008b", lwd = 1.5
                )
        }

        if (isTRUE(show_sample_id)) {
                graphics::title(
                        main = id_pac,
                        outer = TRUE,
                        adj = 0.5,
                        line = -0.6,
                        cex.main = 1.75,
                        col.main = "#004085",
                        font.main = 2
                )
        }
}