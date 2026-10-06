# Word-ready Q-Q figures from the paired-difference data exported by index2.R.
# Run independently: Rscript supplementary_qq_plots.R
# index2.R also calls this function after exporting the diagnostic data.
suppressPackageStartupMessages({library(ggplot2); library(dplyr)})
make_supplementary_qq_figures <- function(pairs, output_dir = "figures", results_dir = "results_index2") {
  order <- c("alternaria_pecan", "canker_ripe_orange", "coffee_leaf_rust",
             "tan_spot_wheat", "botrytis_gerbera", "pecan_scab",
             "xylella_tobacco", "soybean_rust", "brown_spot")
  labels <- c("Pecan brown spot", "Citrus canker", "Coffee leaf rust",
              "Wheat tan spot", "Gerbera botrytis blight", "Pecan fruit scab",
              "Tobacco–Xylella", "Soybean rust", "Rice brown spot")
  stopifnot(all(c("study", "diff_raw", "diff_z") %in% names(pairs)))
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)
  plots <- list()
  for(scale in c("raw", "transformed")) {
    variable <- if(scale == "raw") "diff_raw" else "diff_z"
    dat <- pairs %>% filter(study %in% order, is.finite(.data[[variable]])) %>%
      mutate(study = factor(study, levels = order), difference = .data[[variable]])
    counts <- table(dat$study)
    facet_labels <- setNames(paste0(LETTERS[seq_along(order)], "  ", labels,
                                   "\nn = ", as.integer(counts), " paired raters"), order)
    p <- ggplot(dat, aes(sample = difference)) +
      stat_qq_line(color = "#D55E00", linewidth = .65) +
      stat_qq(color = "#1F2933", size = 1.35, alpha = .85) +
      facet_wrap(vars(study), ncol = 3, scales = "free_y",
                 labeller = labeller(study = facet_labels)) +
      labs(x = "Theoretical standard normal quantile",
           y = if(scale == "raw") "Paired LCCC difference (aided − unaided)" else
             "Paired atanh(LCCC) difference (aided − unaided)") +
      theme_bw(base_size = 12) +
      theme(strip.background = element_blank(),
            strip.text = element_text(hjust = 0, size = 10),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_line(color = "grey92", linewidth = .3),
            panel.spacing = grid::unit(1.3, "lines"),
            plot.margin = margin(12, 16, 12, 12))
    filename <- if(scale == "raw") "supplementary_S1_qq_raw.png" else
      "supplementary_S2_qq_transformed.png"
    ggsave(file.path(output_dir, filename), p, width = 10.5, height = 9,
           units = "in", dpi = 400, bg = "white")
    plots[[scale]] <- p
  }
  captions <- c(
    "Figure S1. Normal Q-Q plots of paired rater-level LCCC differences (aided minus unaided) for nine SAD validation studies. Points represent ordered observed differences plotted against theoretical standard normal quantiles. Reference lines pass through the first and third quartiles. Panel labels report the number of valid paired raters. Vertical scales vary among panels.",
    "Figure S2. Normal Q-Q plots of paired differences after inverse hyperbolic tangent transformation of rater-specific LCCC values: atanh(LCCC aided) minus atanh(LCCC unaided). Points represent ordered observed differences plotted against theoretical standard normal quantiles. Reference lines pass through the first and third quartiles. Panel labels report the number of valid paired raters with finite transformed differences. Vertical scales vary among panels."
  )
  writeLines(captions, file.path(results_dir, "supplementary_figure_captions.txt"))
  invisible(plots)
}
if(sys.nframe() == 0L) {
  pairs <- read.csv("results_index2/reviewer3_paired_ccc.csv")
  plots <- make_supplementary_qq_figures(pairs)
  # Check that each output contains the nine intended study panels.
  stopifnot(all(vapply(plots, function(p) nrow(ggplot_build(p)$layout$layout) == 9L, logical(1))))
  cat("Created two combined Q-Q figures, each with nine panels, at 400 dpi.\n")
}
