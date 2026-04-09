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

# Tamanho amostral por poder (pareado aided–unaided) por estudo
required_k_power_study <- function(df_study, alpha = 0.05, power = 0.80, delta_ccc = 0.10){
  un <- per_rater_ccc(df_study, "unaided") %>% rename(ccc_u = ccc)
  ai <- per_rater_ccc(df_study, "aided")   %>% rename(ccc_a = ccc)

  tab <- inner_join(un, ai, by = c("study","rater"))
  if(nrow(tab) < 4) {
    return(tibble(k_power = NA_integer_, sd_diff_z = NA_real_, delta_z = NA_real_, n_pairs = nrow(tab)))
  }

  # Fisher z + diferença pareada
  z_u <- atanh(pmin(pmax(tab$ccc_u, -0.999), 0.999))
  z_a <- atanh(pmin(pmax(tab$ccc_a, -0.999), 0.999))
  d   <- z_a - z_u
  sd_d <- sd(d, na.rm = TRUE)
  if(!is.finite(sd_d) || sd_d == 0) {
    return(tibble(k_power = NA_integer_, sd_diff_z = sd_d, delta_z = NA_real_, n_pairs = nrow(tab)))
  }

  ccc_mid <- mean(c(tab$ccc_u, tab$ccc_a), na.rm = TRUE)
  if(!is.finite(ccc_mid)) return(tibble(k_power = NA_integer_, sd_diff_z = sd_d, delta_z = NA_real_, n_pairs = nrow(tab)))
  delta_z <- delta_ccc / (1 - ccc_mid^2)

  out <- power.t.test(delta = delta_z, sd = sd_d, sig.level = alpha,
                      power = power, type = "one.sample", alternative = "two.sided")
  tibble(k_power = ceiling(out$n), sd_diff_z = sd_d, delta_z = delta_z, n_pairs = nrow(tab))
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
delta_ccc <- 0.1

k_cv_tbl <- kmin_per_study(df, condition = "unaided", cv_star = cv_alvo) %>%
  dplyr::select(study, mu, sd, k_min_cv = k_min)

k_pow_tbl <- k_power_by_study(df, alpha = alpha, power = power_tgt, delta_ccc = delta_ccc)

table_cv_k <- left_join(k_cv_tbl, k_pow_tbl)
table_cv_k

library(writexl)
write_xlsx(table_cv_k, "table_0.05_0.05.xlsx")
design_tbl <- k_cv_tbl %>%
  left_join(k_pow_tbl, by = "study") %>%
  mutate(
    k_final   = pmax(k_min_cv, k_power, na.rm = TRUE),
    cv_target = cv_alvo,
    alpha     = alpha,
    power_tgt = power_tgt,
    delta_ccc = delta_ccc
  )

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
                       name = expression(k[min]~"(CV-rule)")) +
    labs(title = "Precision-based k_min as a function of mean (μ) and variability (σ)",
       subtitle = paste("Isolines for CV*", cv_star),
       x = "Mean CCC (μ)", y = "Standard deviation (σ)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right")
p2

# =========================================================
# 5. Plot 3 – Power Curves vs k for ΔCCC values
# =========================================================
deltas <- c(0.05, 0.1, 0.15)
st <- unique(design_tbl$study)[4]  # choose one study

sd_d <- design_tbl %>% filter(study == st) %>% pull(sd_diff_z)
mu_st <- design_tbl %>% filter(study == st) %>% pull(mu)

alpha <- 0.05
z_alpha <- qnorm(1 - alpha/2)
ks <- 2:100
delta_z_vec <- deltas / (1 - mu_st^2)

pow_df <- purrr::map_dfr(seq_along(deltas), function(i){
  dz <- delta_z_vec[i]
  tibble(
    k = ks,
    power = pnorm(sqrt(ks)*dz/sd_d - z_alpha) + pnorm(-sqrt(ks)*dz/sd_d - z_alpha),
    delta_ccc = deltas[i]
  )
})
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
deltas <- c(0.05, 0.10, 0.15)
alpha  <- 0.05
z_alpha <- qnorm(1 - alpha/2)
ks <- 2:80

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
# Studies to plot
# =========================
studies_to_plot <- c(
  "alternaria_pecan",
  "botrytis_gerbera",
  "brown_spot",
  "canker_ripe_orange",
  "coffee_leaf_rust",
  "pecan_scab",
  "soybean_rust",
  "tan_spot_wheat",
  "xylella_tobacco"
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

  delta_z_vec <- deltas / (1 - mu_st^2)

  pow_df <- map_dfr(seq_along(deltas), function(i){
    dz <- delta_z_vec[i]
    tibble(
      k = ks,
      power = pnorm(sqrt(ks) * dz / sd_d - z_alpha) +
              pnorm(-sqrt(ks) * dz / sd_d - z_alpha),
      delta_ccc = deltas[i]
    )
  })

  ggplot(pow_df, aes(k, power, color = factor(delta_ccc))) +
    geom_line(linewidth = 1) +
    geom_hline(yintercept = 0.80, linetype = 2) +
    scale_color_colorblind(name = expression(Delta*CCC)) +
    labs(
      title = ifelse(st %in% names(study_labels), study_labels[st], st),
      subtitle = paste0("sd_diff_z=", round(sd_d, 3),
                        ", μ=", round(mu_st, 2)),
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
p_list <- map(studies_to_plot, make_power_plot)

p_fig <- wrap_plots(p_list, ncol = 3, nrow = 3, guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 9, face = "bold"),
    plot.subtitle = element_text(size = 7)
  )

# Show figure
p_fig

# Save
ggsave("power_curve_by_disease.png",
       p_fig,
       bg = "white",
       width = 9,
       height = 8)
