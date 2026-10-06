# Reference (real) severity histograms for the nine studies in the REV1 manuscript.
# In RStudio: source("real_severity_histograms.R")
# Command line: Rscript real_severity_histograms.R [all_data.xlsx] [results_directory] [figure_directory]
# Packages: readxl, dplyr, ggplot2. Optional: ragg for high-quality PNG/TIFF.
# Uses actual severity in percent; repeated rater rows are counted only once.

required <- c("readxl", "dplyr", "ggplot2")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Install required packages: install.packages(c(",
                         paste(sprintf('"%s"', missing), collapse = ", "), "))")
suppressPackageStartupMessages({library(readxl); library(dplyr); library(ggplot2)})

# Keep the default input/output beside this script, including when sourced in RStudio.
script_directory <- function() {
  for (frame in rev(sys.frames())) {
    if (!is.null(frame$ofile)) return(dirname(normalizePath(frame$ofile)))
  }
  file_arg <- grep("^--file=", commandArgs(), value = TRUE)
  if (length(file_arg)) return(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
  getwd()
}

make_real_severity_histograms <- function(
    input_file = "all_data.xlsx", output_dir = "results_index2",
    bin_width = 5, figure_width = 10.5, figure_height = 8.6, dpi = 600, figure_dir = "figures") {
  if (!file.exists(input_file)) stop("Workbook not found: ", input_file)
  if (length(bin_width) != 1L || !is.finite(bin_width) || bin_width <= 0 ||
      abs(100 / bin_width - round(100 / bin_width)) > 1e-8)
    stop("bin_width must be positive and divide 100 exactly (e.g., 2, 5, or 10).")

  # Panel order follows the existing combined figures; disease names follow Table 1.
  study_order <- c("alternaria_pecan", "canker_ripe_orange", "coffee_leaf_rust",
                   "tan_spot_wheat", "botrytis_gerbera", "pecan_scab",
                   "xylella_tobacco", "soybean_rust", "brown_spot")
  disease_names <- c("Pecan Alternaria black spot", "Orange citrus canker",
                     "Coffee leaf rust", "Wheat tan spot", "Gerbera gray mold",
                     "Pecan fruit scab", "Tobacco variegated chlorosis",
                     "Soybean Asian rust", "Rice brown spot")
  sheets <- setdiff(excel_sheets(input_file), "checklist")
  raw <- bind_rows(lapply(sheets, function(sheet) {
    d <- read_excel(input_file, sheet = sheet, col_types = "text")
    needed <- c("study", "rater", "leaf", "actual")
    if (!all(needed %in% names(d))) stop("Missing required columns in sheet: ", sheet)
    d <- d[, needed]
    d$source_sheet <- sheet
    d
  })) %>% mutate(across(c(study, rater, leaf), trimws),
                 actual = suppressWarnings(as.numeric(actual)))
  if (anyNA(raw[, c("study", "rater", "leaf", "actual")]) ||
      any(raw$study == "" | raw$rater == "" | raw$leaf == "") ||
      any(!is.finite(raw$actual))) stop("Missing or invalid identifiers/reference severities.")
  if (any(raw$actual < 0 | raw$actual > 100)) stop("actual must be percent severity in [0, 100].")
  if (!setequal(unique(raw$study), study_order)) stop("Workbook study IDs differ from the nine expected studies.")

  # Never silently choose or average conflicting actual values.
  within_rater_conflicts <- raw %>% group_by(study, rater, leaf) %>%
    summarise(n_actual = n_distinct(actual), .groups = "drop") %>% filter(n_actual > 1L)
  if (nrow(within_rater_conflicts)) stop("Conflicting reference values within a rater/specimen.")
  conflicts <- raw %>% group_by(study, leaf) %>%
    summarise(n_actual = n_distinct(actual), .groups = "drop") %>% filter(n_actual > 1L)
  if (any(conflicts$study != "botrytis_gerbera"))
    stop("Unexpected reference-value conflicts outside Gerbera; check specimen IDs.")

  # Gerbera: the same 30 images were measured independently in two laboratories.
  # Source: Melo et al. (2020), https://doi.org/10.1094/PDIS-08-19-1708-RE
  # The workbook does not label the laboratories. Recover its two reference sets
  # from each rater's full leaf-to-actual profile; do not infer Lab 1/Lab 2 names.
  # Keeping (study, reference_set, leaf) retains even equal values across sets.
  gerbera <- raw %>% filter(study == "botrytis_gerbera") %>%
    distinct(study, rater, leaf, actual)
  profiles <- gerbera %>% arrange(rater, leaf) %>% group_by(study, rater) %>%
    summarise(profile = paste(paste(leaf, sprintf("%.15g", actual), sep = "="),
                              collapse = ";"), .groups = "drop") %>%
    mutate(reference_set = match(profile, unique(profile)))
  if (n_distinct(profiles$reference_set) != 2L)
    stop("Expected two complete Gerbera reference profiles; review the workbook.")
  gerbera_values <- gerbera %>% left_join(select(profiles, study, rater, reference_set),
                                          by = c("study", "rater")) %>%
    distinct(study, reference_set, leaf, actual)
  set_counts <- gerbera_values %>% count(reference_set)
  if (any(set_counts$n != 30L) || n_distinct(gerbera_values$leaf) != 30L)
    stop("Expected 30 common Gerbera image IDs in each reference set.")
  specimens <- bind_rows(
    raw %>% filter(study != "botrytis_gerbera") %>% distinct(study, leaf, actual) %>%
      mutate(reference_set = 1L), gerbera_values
  ) %>% mutate(study = factor(study, levels = study_order)) %>%
    arrange(study, reference_set, leaf)

  summary_table <- specimens %>% group_by(study) %>%
    summarise(n_images = n_distinct(leaf), n_reference_sets = n_distinct(reference_set),
              n_reference_measurements = n(), min_severity = min(actual),
              max_severity = max(actual), median_severity = median(actual),
              mean_severity = mean(actual), .groups = "drop") %>%
    mutate(disease = disease_names[match(as.character(study), study_order)], .after = study)
  subtitles <- ifelse(summary_table$n_reference_sets == 1,
                      paste0("n = ", summary_table$n_images, " images"),
                      paste0("n = ", summary_table$n_images, " images; 2 reference sets"))
  facet_labels <- setNames(paste0(LETTERS[seq_along(study_order)], "  ",
                                 disease_names, "\n", subtitles), study_order)

  # Explicit common bins: [0, 5], (5, 10], ... (95, 100] at the default width.
  # hist() includes both severity endpoints and emits all empty bins as well.
  breaks <- seq(0, 100, by = bin_width)
  bins <- bind_rows(lapply(study_order, function(st) {
    h <- hist(specimens$actual[specimens$study == st], breaks = breaks,
              right = TRUE, include.lowest = TRUE, plot = FALSE)
    data.frame(study = st, lower = head(breaks, -1), upper = tail(breaks, -1),
               midpoint = h$mids, frequency = h$counts)
  })) %>% mutate(study = factor(study, levels = study_order))
  totals <- bins %>% group_by(study) %>% summarise(total = sum(frequency), .groups = "drop")
  stopifnot(identical(as.integer(totals$total), as.integer(summary_table$n_reference_measurements)))
  y_top <- ceiling(max(bins$frequency) / 5) * 5
  figure <- ggplot(bins, aes(x = midpoint, y = frequency)) +
    geom_col(width = bin_width, fill = "#49758C", color = "white", linewidth = 0.3) +
    facet_wrap(vars(study), ncol = 3, scales = "fixed",
               labeller = labeller(study = facet_labels)) +
    scale_x_continuous(breaks = seq(0, 100, 25), limits = c(0, 100),
                       expand = expansion(mult = c(0, 0.015))) +
    scale_y_continuous(breaks = seq(0, y_top, 5), limits = c(0, y_top),
                       expand = expansion(mult = c(0, 0.06))) +
    labs(x = "Real disease severity (%)", y = "Frequency (reference measurements)") +
    theme_bw(base_size = 13, base_family = "sans") +
    theme(strip.background = element_blank(),
          strip.text = element_text(hjust = 0, size = 11.5, lineheight = 1.12),
          panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
          panel.spacing = grid::unit(1.15, "lines"),
          axis.title = element_text(size = 13), axis.text = element_text(color = "black"),
          plot.margin = margin(10, 12, 10, 10))
  stopifnot(nrow(ggplot_build(figure)$layout$layout) == 9L)

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(figure_dir, showWarnings = FALSE, recursive = TRUE)
  stem <- file.path(output_dir, "real_severity_histograms")
  figure_stem <- file.path(figure_dir, "real_severity_histograms")
  png_device <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else "png"
  tiff_device <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_tiff else "tiff"
  ggsave(paste0(figure_stem, ".png"), figure, device = png_device, width = figure_width,
         height = figure_height, units = "in", dpi = dpi, bg = "white")
  ggsave(paste0(figure_stem, ".tiff"), figure, device = tiff_device, width = figure_width,
         height = figure_height, units = "in", dpi = dpi, compression = "lzw", bg = "white")
  ggsave(paste0(figure_stem, ".pdf"), figure, device = "pdf", width = figure_width,
         height = figure_height, units = "in", bg = "white", useDingbats = FALSE)
  write.csv(specimens, paste0(stem, "_reference_values.csv"), row.names = FALSE)
  write.csv(summary_table, paste0(stem, "_summary.csv"), row.names = FALSE)
  write.csv(bins, paste0(stem, "_bin_counts.csv"), row.names = FALSE)
  write.csv(select(profiles, study, rater, reference_set),
            paste0(stem, "_gerbera_reference_sets.csv"), row.names = FALSE)
  caption <- paste0(
    "Distribution of real (reference) disease severity for the nine pathosystems ",
    "included in the SAD validation analysis. Histograms use the actual severity ",
    "values in all_data.xlsx, with common ", bin_width, "-percentage-point bins and ",
    "identical axes across panels. Repeated observations from different raters are ",
    "counted once per image and reference set. Panel E includes both independently ",
    "measured reference sets for the same 30 Gerbera images (60 reference measurements ",
    "in total); these values are retained separately rather than averaged. Panel ",
    "labels report the number of images. Reference severities are those recorded ",
    "in the compiled workbook.")
  writeLines(caption, paste0(stem, "_caption.txt"))
  print(summary_table, width = Inf)
  message("Created one combined nine-panel figure in PNG, TIFF, and PDF: ", figure_stem)
  if (interactive()) print(figure)
  invisible(list(plot = figure, reference_values = specimens, summary = summary_table,
                 bin_counts = bins, gerbera_reference_sets = profiles))
}

project_dir <- script_directory()
args <- if (sys.nframe() == 0L) commandArgs(trailingOnly = TRUE) else character()
input_file <- if (length(args) >= 1L) args[1] else file.path(project_dir, "all_data.xlsx")
output_dir <- if (length(args) >= 2L) args[2] else file.path(project_dir, "results_index2")
figure_dir <- if (length(args) >= 3L) args[3] else file.path(project_dir, "figures")
severity_histograms <- make_real_severity_histograms(input_file, output_dir, figure_dir = figure_dir)
