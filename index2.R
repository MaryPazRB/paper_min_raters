# Main analysis for the GitHub repository; see readme.txt for the workflow.
# Reviewer 3: empirical assessment of atanh(LCCC) paired differences.
# Run from the directory containing all_data.xlsx.
if (!file.exists("all_data.xlsx")) stop("Set the working directory to the folder containing all_data.xlsx.")
dir.create("results_index2", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)
# Save automatically printed plots with the other figures.
pdf("figures/analysis_plots.pdf", width = 10, height = 8)

# ==============================================================================
# Load data
# ==============================================================================
library(readxl)
library(tidyverse)

# Read the data directly from the local Excel file, combining all data sheets
path <- "all_data.xlsx"
sheets <- excel_sheets(path)
data_sheets <- sheets[sheets != "checklist"]

# Read all columns as text to avoid type mismatch when combining, then convert types automatically
df <- map_dfr(data_sheets, ~ read_excel(path, sheet = .x, col_types = "text")) |>
  type_convert()

df |>
  distinct(study, rater) |>
  group_by(study) |>
  summarise(n_raters = n(), .groups = "drop")


# ==============================================================================
# Kmin based on CV - geral
# ==============================================================================

## ========================== SETUP ==========================
## Data: long format with columns:
## study, rater, leaf, actual, unaided, aided

library(dplyr)
library(tidyr)
library(purrr)

# ---- Lin's CCC ----
ccc_lin <- function(x, y){
  x <- as.numeric(x); y <- as.numeric(y)
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]; y <- y[ok]
  if(length(x) < 3L) return(NA_real_)
  mx <- mean(x); my <- mean(y)
  vx <- var(x); vy <- var(y); sxy <- cov(x, y)
  denom <- vx + vy + (mx - my)^2
  if(denom <= 0) return(NA_real_)
  (2 * sxy) / denom
}


# ---- Helper to pick condition column ----
.pick_cond <- function(df, condition = c("unaided","aided")){
  condition <- match.arg(condition)
  est_col <- rlang::sym(condition)
  df %>% mutate(estimate = !!est_col)
}

# ===================== PER-RATER CCCs =======================
# Returns one CCC per (study, rater) for a given condition; optionally pooled
per_rater_ccc_long <- function(df_long, condition = c("unaided","aided"),
                               pool_across_studies = TRUE){
  d <- .pick_cond(df_long, condition)
  d %>%
    group_by(study, rater) %>%
    summarise(ccc = ccc_lin(estimate, actual), .groups = "drop") %>%
    filter(is.finite(ccc)) %>%
    { if(pool_across_studies) mutate(., study = "(pooled)") else . }
}

# ===================== CV-based k_min =======================
kmin_cv <- function(mu, sd, cv_star = 0.10, k_floor = 2){
  if(!is.finite(mu) || !is.finite(sd) || mu <= 0) return(Inf)
  km <- ceiling((sd / (mu * cv_star))^2)
  max(km, k_floor)
}

kmin_from_data_long <- function(df_long, condition = c("unaided","aided"),
                                cv_star = 0.10, pool_across_studies = TRUE){
  cccs <- per_rater_ccc_long(df_long, condition, pool_across_studies)
  mu <- mean(cccs$ccc); sd <- sd(cccs$ccc)
  list(k_min = kmin_cv(mu, sd, cv_star),
       mu = mu, sd = sd,
       n_raters = nrow(cccs),
       condition = match.arg(condition),
       pooled = pool_across_studies)
}



## ===================== EXAMPLE USAGE =======================
# Suppose your long data is in df_long with the specified columns.

# 1) Point estimate of k_min for UNAIDED at CV* = 10%
k10_un <- kmin_from_data_long(df, condition = "unaided", cv_star = 0.10)
k10_un$k_min


# ==============================================================================
# Kmin based on CV - by study
# ==============================================================================

# =================================================================
# INPUT: long data frame with columns:
# study, rater, leaf, actual, unaided, aided
# Choose condition = "unaided" or "aided"
# =================================================================
library(dplyr)
library(tidyr)
library(purrr)
library(metafor)   # install.packages("metafor")

# ---- Lin's CCC (per rater vs truth) ----
ccc_lin <- function(x, y){
  x <- as.numeric(x); y <- as.numeric(y)
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]; y <- y[ok]
  if(length(x) < 3L) return(NA_real_)
  mx <- mean(x); my <- mean(y)
  vx <- var(x); vy <- var(y); sxy <- cov(x, y)
  denom <- vx + vy + (mx - my)^2
  if(denom <= 0) return(NA_real_)
  (2 * sxy) / denom
}

# ---- pick condition column and name it 'estimate' ----
pick_condition <- function(df_long, condition = c("unaided","aided")){
  condition <- match.arg(condition)
  est_col <- rlang::sym(condition)
  df_long %>% mutate(estimate = !!est_col)
}

# ---- per-study, per-rater CCCs ----
per_study_ccc <- function(df_long, condition = c("unaided","aided")){
  d <- pick_condition(df_long, condition)
  d %>%
    group_by(study, rater) %>%
    summarise(ccc = ccc_lin(estimate, actual), .groups = "drop") %>%
    filter(is.finite(ccc))
}

# ---- summarise each study: mean CCC (mu_s), SD CCC (s_s), n_raters ----
study_level_summary <- function(ccc_per_rater_df){
  ccc_per_rater_df %>%
    group_by(study) %>%
    summarise(mu = mean(ccc),
              sd = sd(ccc),
              n_raters = n(),
              .groups = "drop") %>%
    filter(is.finite(mu), is.finite(sd), n_raters >= 4)  # need >=4 for Fisher-z var
}

# ---- compute kmin by CV rule ----
kmin_cv <- function(mu, sd, cv_star = 0.10, k_floor = 2){
  if(!is.finite(mu) || !is.finite(sd) || mu <= 0) return(Inf)
  km <- ceiling((sd / (mu * cv_star))^2)
  max(km, k_floor)
}

# ==================== (1) k_min per study ====================
# --- corrected version of kmin_per_study() ---
kmin_per_study <- function(df_long, condition = c("unaided","aided"), cv_star = 0.10){
  condition <- match.arg(condition)

  # get per-rater CCCs
  prs <- per_study_ccc(df_long, condition)
  # summarize per study
  summ <- study_level_summary(prs)

  # compute k_min rowwise (one per study)
  summ %>%
    rowwise() %>%
    mutate(k_min = kmin_cv(mu, sd, cv_star)) %>%
    ungroup() %>%
    mutate(condition = condition, cv_target = cv_star)
}


# ============================= EXAMPLE =============================
# Choose condition and CV target:

# (1) Study-specific k_min
condition_use <- "unaided"
cv_star <- 0.10

k_by_study <- kmin_per_study(df, condition = condition_use, cv_star = cv_star)
print(k_by_study)
k_by_study



# ==============================================================================
# Power e k min
# ==============================================================================

# =========================================================
# Minimum Raters Analysis (CV rule + Power rule)
# =========================================================
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)

# =========================================================
# 1. Helper Functions
# =========================================================

# Lin's CCC (robust to types / small quirks)
ccc_lin <- function(x, y){
  x <- suppressWarnings(as.numeric(x))
  y <- suppressWarnings(as.numeric(y))
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]; y <- y[ok]
  if(length(x) < 3) return(NA_real_)
  mx <- mean(x); my <- mean(y)
  vx <- var(x);  vy <- var(y)
  sxy <- cov(x, y)
  denom <- vx + vy + (mx - my)^2
  if(!is.finite(denom) || denom <= 0) return(NA_real_)
  (2 * sxy) / denom
}

# CCC por avaliador DENTRO de estudo (mais seguro)
per_rater_ccc <- function(df_long, cond = c("unaided","aided")){
  cond <- match.arg(cond)
  df_long %>%
    group_by(study, rater) %>%
    summarise(ccc = ccc_lin(.data[[cond]], actual), .groups = "drop") %>%
    filter(is.finite(ccc))
}

# k_min (regra do CV) por estudo
kmin_per_study <- function(df_long, condition = "unaided", cv_star = 0.10, k_floor = 2){
  studies <- sort(unique(df_long$study))
  purrr::map_dfr(studies, function(st){
    ds  <- df_long %>% filter(study == st)
    tab <- per_rater_ccc(ds, condition)
    if(nrow(tab) < 2 || !any(is.finite(tab$ccc))) {
      return(tibble(study = st, mu = NA_real_, sd = NA_real_, k_min = NA_integer_))
    }
    mu <- mean(tab$ccc, na.rm = TRUE)
    sd <- sd(tab$ccc,   na.rm = TRUE)
    if(!is.finite(mu) || !is.finite(sd) || mu <= 0) {
      return(tibble(study = st, mu = NA_real_, sd = NA_real_, k_min = NA_integer_))
    }
    k_min <- max(k_floor, ceiling((sd/(mu*cv_star))^2))
    tibble(study = st, mu = mu, sd = sd, k_min = k_min)
  })
}

# Exact effect conversion at the mean UNAIDED CCC of complete rater pairs.
# This is a planning contrast at a representative baseline, not the mean
# of rater-specific transformed effects (atanh is nonlinear).
exact_ccc_effect <- function(baseline_ccc, delta_ccc) {
  if(length(baseline_ccc) != 1L || !is.finite(baseline_ccc) ||
     abs(baseline_ccc) >= 1 || !is.finite(delta_ccc) || delta_ccc <= 0)
    return(tibble(target_ccc = NA_real_, feasible = FALSE,
                  delta_z = NA_real_, effect_status = "invalid_baseline_or_effect"))
  target <- baseline_ccc + delta_ccc
  feasible <- target < 1 && target > -1
  tibble(target_ccc = target, feasible = feasible,
         delta_z = if(feasible) atanh(target) - atanh(baseline_ccc) else NA_real_,
         effect_status = if(feasible) "feasible" else "infeasible_target_at_or_above_1")
}

required_k_power_study <- function(df_study, alpha = 0.05, power = 0.80, delta_ccc = 0.10){
  un <- per_rater_ccc(df_study, "unaided") %>% rename(ccc_u = ccc)
  ai <- per_rater_ccc(df_study, "aided") %>% rename(ccc_a = ccc)
  tab <- inner_join(un, ai, by = c("study", "rater"))
  n_complete <- nrow(tab)
  # Boundary CCCs are excluded explicitly, never silently clipped.
  tab <- filter(tab, abs(ccc_u) < 1, abs(ccc_a) < 1)
  n_pairs <- nrow(tab)
  baseline <- if(n_pairs) mean(tab$ccc_u) else NA_real_
  effect <- exact_ccc_effect(baseline, delta_ccc)
  sd_d <- if(n_pairs >= 2) sd(atanh(tab$ccc_a) - atanh(tab$ccc_u)) else NA_real_
  status <- effect$effect_status
  if(effect$feasible && n_pairs < 4) status <- "insufficient_pairs"
  if(effect$feasible && n_pairs >= 4 && (!is.finite(sd_d) || sd_d <= 0))
    status <- "invalid_difference_sd"
  k <- NA_integer_
  if(status == "feasible") {
    out <- power.t.test(delta = effect$delta_z, sd = sd_d, sig.level = alpha,
                        power = power, type = "one.sample",
                        alternative = "two.sided", strict = TRUE)
    k <- as.integer(ceiling(out$n))
  }
  bind_cols(tibble(k_power = k, sd_diff_z = sd_d, n_pairs = n_pairs,
                   boundary_pairs_excluded = n_complete - n_pairs,
                   baseline_ccc = baseline, available_improvement = 1 - baseline),
            effect, tibble(power_status = status))
}

k_power_by_study <- function(df_long, alpha = 0.05, power = 0.80, delta_ccc = 0.10){
  studies <- sort(unique(df_long$study))
  purrr::map_dfr(studies, function(st){
    ds <- df_long %>% filter(study == st)
    bind_cols(tibble(study = st),
              required_k_power_study(ds, alpha = alpha, power = power, delta_ccc = delta_ccc))
  })
}


# =========================================================
# 2. Main Analysis
# =========================================================

cv_alvo   <- 0.1
alpha     <- 0.05
power_tgt <- 0.80
delta_ccc <- 0.10
# Sensitivity grid includes the original code and manuscript alternatives.
sensitivity_deltas <- c(0.05, 0.10, 0.15, 0.20)

k_cv_tbl <- kmin_per_study(df, condition = "unaided", cv_star = cv_alvo) %>%
  dplyr::select(study, mu, sd, k_min_cv = k_min)

k_pow_tbl <- k_power_by_study(df, alpha = alpha, power = power_tgt, delta_ccc = delta_ccc)

table_cv_k <- left_join(k_cv_tbl, k_pow_tbl)
table_cv_k

library(writexl)
write_xlsx(table_cv_k, "results_index2/table_cv_power.xlsx")
design_tbl <- k_cv_tbl %>%
  left_join(k_pow_tbl, by = "study") %>%
  mutate(
    # No combined recommendation when either criterion cannot be estimated.
    k_final = if_else(is.finite(k_min_cv) & is.finite(k_power),
                      pmax(k_min_cv, k_power), NA_real_),
    cv_target = cv_alvo,
    alpha     = alpha,
    power_tgt = power_tgt,
    delta_ccc = delta_ccc
  )

# CSVs for updating manuscript values. Infeasible effects retain NA sample sizes.
write.csv(design_tbl, "results_index2/table_cv_power.csv", row.names = FALSE)
sensitivity_tbl <- map_dfr(sensitivity_deltas, function(delta) {
  k_power_by_study(df, alpha = alpha, power = power_tgt, delta_ccc = delta) %>%
    left_join(k_cv_tbl, by = "study") %>%
    mutate(delta_ccc = delta, alpha = alpha, power_tgt = power_tgt,
           cv_target = cv_alvo,
           k_final = if_else(is.finite(k_min_cv) & is.finite(k_power),
                             pmax(k_min_cv, k_power), NA_real_))
})
write.csv(sensitivity_tbl, "results_index2/table_effect_sensitivity.csv", row.names = FALSE)
write.csv(sensitivity_tbl %>%
  select(study, delta_ccc, k_power, k_final, power_status) %>%
  pivot_wider(names_from = delta_ccc,
              values_from = c(k_power, k_final, power_status), names_prefix = "delta_"),
  "results_index2/table_effect_sensitivity_wide.csv", row.names = FALSE)

# Same effect conversion and two-sided t calculation for every power curve.
power_curve_data <- function(st, ks, effects = sensitivity_deltas) {
  rows <- sensitivity_tbl %>% filter(study == st, delta_ccc %in% effects,
                                     power_status == "feasible")
  map_dfr(seq_len(nrow(rows)), function(i) {
    row <- rows[i, ]
    tibble(study = st, k = ks, delta_ccc = row$delta_ccc,
           baseline_ccc = row$baseline_ccc, target_ccc = row$target_ccc,
           power = map_dbl(ks, ~ power.t.test(n = .x, delta = row$delta_z,
             sd = row$sd_diff_z, sig.level = row$alpha,
             type = "one.sample", alternative = "two.sided", strict = TRUE)$power))
  })
}
all_curve_data <- map_dfr(sort(unique(df$study)), ~ power_curve_data(.x, 2:max(100, sensitivity_tbl$k_power, na.rm = TRUE)))
write.csv(all_curve_data, "results_index2/power_curve_data.csv", row.names = FALSE)

# =========================================================
# 3. Plot 1 – Barplot (CV vs Power vs Final k)
# =========================================================
plot_tab <- design_tbl %>%
  mutate(study = factor(study, levels = study[order(k_final, decreasing = TRUE)])) %>%
  pivot_longer(cols = c(k_min_cv, k_power),
               names_to = "criterion", values_to = "k") %>%
  mutate(criterion = recode(criterion,
                            k_min_cv = "CV (precision)",
                            k_power  = "Power (paired)"))
plot_tab

p1 <- ggplot() +
  geom_col(data = plot_tab, aes(x = study, y = k, fill = criterion),
           position = position_dodge(width = 0.7), width = 0.6) +
  geom_point(data = distinct(design_tbl, study, k_final),
             aes(x = study, y = k_final), color = "red", size = 3) +
  geom_text(data = distinct(design_tbl, study, k_final),
            aes(x = study, y = k_final, label = k_final),
            color = "red", vjust = -0.7, size = 3) +
  scale_fill_manual(values = c("CV (precision)" = "#4E79A7",
                               "Power (paired)" = "#F28E2B")) +
  labs(title = "Minimum number of raters by criterion and final design",
       subtitle = paste0("CV*=", cv_alvo,
                         " | α=", alpha,
                         " | power=", power_tgt,
                         " | ΔCCC=", delta_ccc),
       x = "Study", y = "k (number of raters)", fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 20, hjust = 1))
p1

# =========================================================
# 4. Plot 2 – Contour (μ, σ, k_min)
# =========================================================
cv_star <- cv_alvo
grid_k <- expand.grid(
  mu = seq(0.4, 0.95, length.out = 80),
  sd = seq(0.02, 0.25, length.out = 80)
) %>% mutate(k_min = (sd / (mu * cv_star))^2)

p2 <- ggplot() +
  #geom_contour_filled(data = grid_k,
  #                    aes(x = mu, y = sd, z = k_min),
  #                    breaks = c(1, 2, 4, 6, 8, 10, 15, 20, 30, 40, 50, 60, 70)) +

  geom_point(data = design_tbl,
             aes(x = mu, y = sd, size = k_final, color = k_final),
             #color = "red", 
             alpha = 0.8) +
  geom_text(data = design_tbl,
            aes(x = mu, y = sd, label = study),
            vjust = -0.8, size = 3, color = "black") +
  scale_color_viridis_c(option = "C", direction = -1,
                       name = expression(k[final])) +
    labs(title = "Precision-based k_min as a function of mean (μ) and variability (σ)",
       subtitle = paste("Isolines for CV*", cv_star),
       x = "Mean CCC (μ)", y = "Standard deviation (σ)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right")
p2

# =========================================================
# 5. Plot 3 – Power Curves vs k for ΔCCC values
# =========================================================
deltas <- sensitivity_deltas
st <- unique(design_tbl$study)[4]  # choose one study

sd_d <- design_tbl %>% filter(study == st) %>% pull(sd_diff_z)
mu_st <- design_tbl %>% filter(study == st) %>% pull(mu)
alpha <- 0.05
ks <- 2:100
pow_df <- power_curve_data(st, ks, deltas)

library(ggthemes)
p3 <- ggplot(pow_df, aes(k, power, color = factor(delta_ccc))) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  geom_hline(yintercept = 0.80, linetype = 2) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  labs(title = paste("Power vs. k (paired aided–unaided) —", st),
       subtitle = paste0("α=0.05, sd_diff_z=", round(sd_d,3), ", μ=", round(mu_st,2)),
       x = "Number of raters (pairs)", y = "Power", color = "ΔCCC") +
  scale_color_colorblind()+
  theme_minimal(base_size = 12)
p3


# ==============================================================================
# Plot 4 - Power Curve by Disease
# ==============================================================================
library(dplyr)
library(tibble)
library(purrr)
library(ggplot2)
library(ggthemes)
library(scales)
library(patchwork)

# =========================
# Parameters
# =========================
deltas <- sensitivity_deltas
alpha  <- 0.05
z_alpha <- qnorm(1 - alpha/2)
ks <- 2:max(80, sensitivity_tbl$k_power, na.rm = TRUE)

# =========================
# Study labels (NEW)
# =========================
study_labels <- c(
  alternaria_pecan    = "Pecan brown spot",
  botrytis_gerbera    = "Gerbera botrytis blight",
  brown_spot          = "Rice brown spot",
  canker_ripe_orange  = "Citrus canker",
  coffee_leaf_rust    = "Coffee leaf rust",
  pecan_scab          = "Pecan fruit scab",
  soybean_rust        = "Soybean rust",
  tan_spot_wheat      = "Wheat tan spot",
  xylella_tobacco     = "Tobacco-Xylella"
)

# =========================
# Export the publication table from the same current sensitivity results.
write.csv(sensitivity_tbl %>%
  mutate(disease = unname(study_labels[study])) %>%
  select(disease, study, mu, sd, n_pairs, baseline_ccc, sd_diff_z,
         delta_ccc, target_ccc, delta_z, k_min_cv, k_power, k_final, power_status),
  "results_index2/supplementary_table_S1.csv", row.names = FALSE)

# Studies to plot
# =========================
studies_to_plot <- c(
  "alternaria_pecan",     # Pecan brown spot
  "canker_ripe_orange",   # Citrus canker
  "coffee_leaf_rust",     # Coffee leaf rust
  "tan_spot_wheat",       # Wheat tan spot
  "botrytis_gerbera",     # Gerbera botrytis blight
  "pecan_scab",           # Pecan fruit scab
  "xylella_tobacco",      # Tobacco-Xylella
  "soybean_rust",         # Soybean rust
  "brown_spot"            # Rice brown spot
)

# =========================
# Plot function
# =========================
make_power_plot <- function(st){

  st_row <- design_tbl %>%
    filter(study == st) %>%
    slice(1)

  sd_d  <- st_row$sd_diff_z
  mu_st <- st_row$mu

  # Handle cases with missing data
  if(!is.finite(sd_d) || !is.finite(mu_st) || sd_d == 0){
    return(
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = paste0(study_labels[st], "\n(Not estimable)"),
                 size = 4) +
        theme_void()
    )
  }

  pow_df <- power_curve_data(st, ks, deltas)
  omitted <- sensitivity_tbl %>% filter(study == st, delta_ccc %in% deltas,
                                         power_status != "feasible") %>% pull(delta_ccc)
  omitted_note <- if(length(omitted)) paste0("\nInfeasible Delta: ", paste(omitted, collapse = ", ")) else ""

  ggplot(pow_df, aes(k, power, color = factor(delta_ccc))) +
    geom_line(linewidth = 1) +
    geom_hline(yintercept = 0.80, linetype = 2) +
    scale_color_colorblind(name = expression(Delta*CCC),
                           limits = as.character(deltas), drop = FALSE) +
    labs(
      title = ifelse(st %in% names(study_labels), study_labels[st], st),
      subtitle = paste0("sd_diff_z=", round(sd_d, 3),
                        ", baseline=", round(st_row$baseline_ccc, 2), omitted_note),
      x = "Number of raters",
      y = "Power"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(size = 10, face = "bold"),
      plot.subtitle = element_text(size = 8)
    )
}

# =========================
# Generate plots
# =========================
# A single faceted ggplot guarantees one shared color scale and legend.
# Panel labels retain study order, baseline and unavailable effects.
facet_labels <- setNames(map_chr(seq_along(studies_to_plot), function(i) {
  st <- studies_to_plot[i]
  row <- filter(design_tbl, study == st)
  omitted <- sensitivity_tbl %>% filter(study == st, power_status != "feasible") %>%
    pull(delta_ccc)
  paste0(LETTERS[i], "  ", study_labels[st],
         "\nSD(diff z) = ", sprintf("%.3f", row$sd_diff_z),
         "; baseline = ", sprintf("%.3f", row$baseline_ccc),
         "\n", if(length(omitted)) paste0("Infeasible Delta: ",
                     paste(sprintf("%.2f", omitted), collapse = ", ")) else " ")
}), studies_to_plot)
figure_curve_data <- all_curve_data %>%
  filter(study %in% studies_to_plot, k %in% ks) %>%
  mutate(study = factor(study, levels = studies_to_plot),
         effect = factor(delta_ccc, levels = sensitivity_deltas,
                         labels = sprintf("%.2f", sensitivity_deltas)))
p_fig <- ggplot(figure_curve_data, aes(k, power, color = effect)) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = power_tgt, linetype = 2, color = "grey35") +
  facet_wrap(vars(study), ncol = 3, labeller = labeller(study = facet_labels)) +
  scale_color_manual(name = expression(Delta*LCCC),
                     values = c("0.05" = "#000000", "0.10" = "#E69F00",
                                "0.15" = "#56B4E9", "0.20" = "#009E73"),
                     breaks = sprintf("%.2f", sensitivity_deltas), drop = FALSE) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  labs(x = "Number of raters", y = "Power") +
  guides(color = guide_legend(nrow = 1, byrow = TRUE)) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.box.margin = margin(t = 8),
        strip.text = element_text(hjust = 0, size = 8.5),
        panel.spacing = unit(1.2, "lines"),
        plot.margin = margin(12, 16, 12, 12))
p_fig

ggsave("figures/power_curve_by_disease.png", p_fig,
       bg = "white", width = 10.5, height = 9, dpi = 300)


# ==============================================================================
# Reviewer 3: distribution diagnostics and empirical resampling
# ==============================================================================
# Lin (1989), Biometrics 45:255-268, DOI: 10.2307/2532051, considers
# inverse hyperbolic tangent transformation for CCC inference.
# Delta method: Var[atanh(CCC)] ~ Var[CCC]/(1-CCC^2)^2.
# This does NOT imply the Pearson variance 1/(n-3), nor normality of
# differences across heterogeneous raters. We estimate SD from paired raters.
# Bootstrap unit is the whole rater pair, conditional on the observed leaves.
# It assesses between-rater sampling, not uncertainty from sampling new leaves.
# A normal bootstrap mean does NOT establish normal individual differences.

set.seed(20261005)
B_review3 <- 5000L

# Avoid silently clipping all CCCs at 0.999. Flag exact boundary values.
review3_pairs <- inner_join(
  per_rater_ccc(df, "unaided") %>% rename(ccc_u = ccc),
  per_rater_ccc(df, "aided") %>% rename(ccc_a = ccc),
  by = c("study", "rater")
) %>% mutate(
  boundary = abs(ccc_u) >= 1 | abs(ccc_a) >= 1,
  diff_raw = ccc_a - ccc_u,
  diff_z = ifelse(boundary, NA_real_,
                  atanh(ccc_a) - atanh(ccc_u))
)
write.csv(review3_pairs, "results_index2/reviewer3_paired_ccc.csv", row.names = FALSE)

shape_summary <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  s <- sd(x)
  if(n < 3 || !is.finite(s) || s == 0)
    return(tibble(n = n, mean = mean(x), sd = s, skewness = NA_real_,
                  excess_kurtosis = NA_real_, shapiro_p = NA_real_))
  # Descriptive moment coefficients (not bias-corrected estimators).
  m2 <- mean((x - mean(x))^2)
  tibble(n = n, mean = mean(x), sd = s,
         skewness = mean((x - mean(x))^3)/m2^1.5,
         excess_kurtosis = mean((x - mean(x))^4)/m2^2 - 3,
         shapiro_p = if(n <= 5000) shapiro.test(x)$p.value else NA_real_)
}
review3_normality <- map_dfr(sort(unique(review3_pairs$study)), function(st) {
  tab <- filter(review3_pairs, study == st)
  bind_rows(
    bind_cols(tibble(study = st, scale = "raw difference"), shape_summary(tab$diff_raw)),
    bind_cols(tibble(study = st, scale = "atanh difference"), shape_summary(tab$diff_z))
  ) %>% mutate(boundary_pairs = sum(tab$boundary))
})
write.csv(review3_normality, "results_index2/reviewer3_normality.csv", row.names = FALSE)

pdf("figures/reviewer3_qq_differences.pdf", width = 10, height = 5)
for(st in sort(unique(review3_pairs$study))) {
  tab <- filter(review3_pairs, study == st)
  par(mfrow = c(1,2))
  for(nm in c("diff_raw", "diff_z")) {
    x <- tab[[nm]][is.finite(tab[[nm]])]
    qqnorm(x, main = paste(st, nm, sep = "\n")); qqline(x, col = "red")
  }
}
dev.off()

# Word-ready combined Q-Q figures (raw and transformed differences).
source("supplementary_qq_plots.R")
make_supplementary_qq_figures(review3_pairs, "figures", "results_index2")

# Empirical t-test calibration under a centered null, and power under a
# location-shift alternative. This preserves observed distribution shape.
# Each feasible sensitivity scenario uses the exact baseline-to-target contrast.
# The observed SD is held fixed across effects as a planning assumption.
# Simulated rejection rates are conditional sensitivity analyses, not proof
# of population normality or guaranteed prospective power.
review3_bootstrap <- map_dfr(which(sensitivity_tbl$power_status == "feasible"), function(i) {
  row <- sensitivity_tbl[i, ]
  st <- row$study
  x <- filter(review3_pairs, study == st)$diff_z
  x <- x[is.finite(x)]
  if(length(x) < 4 || sd(x) == 0 || !is.finite(row$delta_z)) return(tibble())
  n <- length(x)
  centered <- x - mean(x)
  # Match sample SD of the empirical resampling distribution to original SD.
  centered <- centered * sqrt(n/(n-1))
  kvals <- sort(unique(c(5L, 10L, 20L, n, as.integer(row$k_power))))
  map_dfr(kvals, function(k) {
    draws <- matrix(sample(centered, k * B_review3, replace = TRUE), nrow = k)
    bm <- colMeans(draws)
    bs <- sqrt(pmax(0, (colSums(draws^2) - k*bm^2)/(k-1)))
    se <- bs/sqrt(k)
    valid <- is.finite(se) & se > 0
    critical <- qt(1 - row$alpha/2, df = k-1)
    null_reject <- abs(bm[valid]/se[valid]) > critical
    alt_reject <- abs((bm[valid] + row$delta_z)/se[valid]) > critical
    pnull <- mean(null_reject); palt <- mean(alt_reject)
    bind_cols(
      tibble(study = st, k = k, observed_pairs = n,
             replicates = B_review3, valid_replicates = sum(valid),
             alpha = row$alpha, delta_ccc = row$delta_ccc,
             baseline_ccc = row$baseline_ccc, target_ccc = row$target_ccc, delta_z = row$delta_z,
             analytic_t_power = power.t.test(n = k, delta = row$delta_z,
               sd = row$sd_diff_z, sig.level = row$alpha,
               type = "one.sample", alternative = "two.sided", strict = TRUE)$power,
             empirical_type1 = pnull,
             type1_mcse = sqrt(pnull*(1-pnull)/sum(valid)),
             empirical_power = palt,
             power_mcse = sqrt(palt*(1-palt)/sum(valid)),
             boot_mean_ci_low = unname(quantile(bm + mean(x), .025)),
             boot_mean_ci_high = unname(quantile(bm + mean(x), .975))),
      shape_summary(bm) %>% select(bootstrap_mean_skewness = skewness,
                                  bootstrap_mean_excess_kurtosis = excess_kurtosis)
    )
  })
})
write.csv(review3_bootstrap, "results_index2/reviewer3_bootstrap_sensitivity.csv", row.names = FALSE)
cat("\nReviewer 3: observed paired-difference diagnostics\n")
print(review3_normality, n = Inf)
cat("\nReviewer 3: sensitivity at the analytic recommended power sample size\n")
print(review3_bootstrap %>% inner_join(
  sensitivity_tbl %>% select(study, delta_ccc, k_power), by = c("study", "delta_ccc")) %>%
  filter(k == k_power) %>% select(study, delta_ccc, k, analytic_t_power,
                                empirical_type1, empirical_power, power_mcse), n = Inf)

# Close the automatic plot device opened at the top of this script.
dev.off()

# Record the R and package versions used for this analysis.
writeLines(capture.output(sessionInfo()), "results_index2/session_info.txt")
