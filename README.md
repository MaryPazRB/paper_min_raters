# Minimum number of raters in SAD validation studies

This repository provides the data, R scripts, figures, and an interactive application used to examine **how many raters are needed to validate standard area diagrams (SADs) for visual assessment of plant disease severity**.

The observations pair reference disease severity with estimates made by raters **without SADs (unaided)** and **with SADs (aided)**. The analysis calculates rater-level Lin's concordance correlation coefficient (LCCC, also called CCC in the scripts) against the reference values, then uses the variation among raters and their paired aided–unaided differences to estimate sample-size requirements.

The repository supports three uses:

- **Reproduce the study analysis:** calculate precision and power requirements across nine plant disease studies.
- **Inspect the data and assumptions:** examine reference-severity distributions, paired differences, and bootstrap sensitivity results.
- **Explore sequential planning:** use the Shiny application to see how estimated requirements change as raters are added.

## What question does the analysis answer?

A SAD validation study needs enough raters to support both a precise estimate of agreement and a meaningful comparison between aided and unaided assessments. The scripts evaluate two complementary criteria.

| Criterion | Question | Main output |
| --- | --- | --- |
| Precision | How many raters are needed to estimate mean unaided CCC at the chosen relative standard error? | `k_min_cv` |
| Statistical power | How many paired raters are needed to detect the specified absolute improvement in CCC? | `k_power` |
| Combined requirement | How many raters meet both criteria, when both can be estimated? | `k_final` |

The precision calculation targets the **relative standard error (RSE) of the estimated mean**. Some historical variable and output names use `cv`; the application labels this criterion RSE.

For a mean unaided CCC `mu`, between-rater standard deviation `sd`, and target `rse_target`, the planning rule is:

```text
RSE of the mean = sd / (mu * sqrt(k))
k_precision = max(operational_floor, ceiling((sd / (mu * rse_target))^2))
```

The power calculation uses complete aided–unaided rater pairs. Its planning contrast is an exact transformation at a representative unaided baseline:

```text
target_ccc = baseline_ccc + delta_ccc
delta_z = atanh(target_ccc) - atanh(baseline_ccc)
```

The observed standard deviation of the paired transformed differences supplies the variability estimate for the t-test power calculation. The main analysis uses two-sided power.

**An improvement targeting CCC at or above 1 is infeasible.** In that case, the power and combined requirements remain `NA`; a precision-only estimate cannot stand in for the joint requirement.

## Repository layout

```text
.
├── README.md                     Explanation and workflow
├── readme.txt                    Plain-text guide
├── all_data.xlsx                 Compiled observations for nine studies
├── clr.csv                       Coffee-leaf-rust application example
├── index2.R                      Main analysis
├── real_severity_histograms.R    Reference-severity figures and summaries
├── supplementary_qq_plots.R      Supplementary diagnostic figures
├── app.R                         Interactive Shiny application
├── tests/
│   └── test_app.R                Statistical and Shiny server checks
├── figures/                      Plots and example screenshots
└── results/                  Tables, numeric results, captions, and run details
```

> **Output-folder note:** The saved results currently reside in `results/`. The commands below follow those existing script paths; all generated figures go to `figures/`.

Keep the input files at the repository root. Run commands from that folder so relative input and script paths resolve correctly.

## What each script does

### `index2.R`: reproduce the main analysis

[index2.R](index2.R) reads the study sheets in `all_data.xlsx`, excluding the `checklist` sheet, and combines the observations for analysis. It:

1. Calculates unaided and aided CCCs for each rater within each study.
2. Estimates the precision-based and paired-power sample sizes.
3. Combines the two requirements when both are estimable.
4. Repeats the power calculation for absolute CCC improvements of **0.05, 0.10, 0.15, and 0.20**.
5. Generates power curves and exports their underlying values.
6. Examines raw and transformed paired differences using Q-Q plots and distribution summaries.
7. Runs empirical bootstrap sensitivity analyses for feasible effects.
8. Exports the supplementary table and records the R session information.

It also calls `supplementary_qq_plots.R`, so you do not need to run that script separately during a full analysis.

### `real_severity_histograms.R`: describe the reference severities

[real_severity_histograms.R](real_severity_histograms.R) creates one nine-panel histogram showing the distribution of **reference disease severity** for the nine studies. It exports the figure in PNG, TIFF, and PDF formats, plus reference values, summary statistics, bin counts, and a caption.

Repeated rows from different raters are counted once per image and reference set. The Gerbera study contains **30 images with two reference sets**; both sets are retained as 60 reference measurements rather than averaged. This script describes the specimens used in the studies and can run independently of the sample-size analysis.

### `supplementary_qq_plots.R`: inspect paired-difference distributions

[supplementary_qq_plots.R](supplementary_qq_plots.R) reads the paired CCC output from the main analysis and creates two combined diagnostic figures:

- **Figure S1:** aided–unaided differences on the original CCC scale.
- **Figure S2:** differences after transforming each CCC with `atanh()`.

Each figure contains nine study panels. These plots help readers inspect the observed distributions used in the power analysis.

### `app.R`: explore sequential rater planning

[app.R](app.R) provides a Shiny interface for one study at a time. Readers can load the example or upload their own data, choose planning settings, and add raters sequentially. The app displays current precision, estimated power requirements, feasibility status, and the sampling history.

Its stopping indicator requires both precision and power criteria to qualify for the specified number of consecutive additions. Applying settings starts a new history; unapplied changes do not alter an active run. The history can be downloaded as CSV.

## Data included

[all_data.xlsx](all_data.xlsx) contains observations from nine studies covering pecan Alternaria black spot, citrus canker, coffee leaf rust, wheat tan spot, Gerbera gray mold, pecan fruit scab, tobacco variegated chlorosis, soybean rust, and rice brown spot. Study identifiers in the data connect observations to the output tables and figure panels.

[clr.csv](clr.csv) is the application's coffee-leaf-rust example, containing **21 raters and 40 specimens**.

| Column | Meaning |
| --- | --- |
| `study` | Study identifier; optional for app uploads, but must identify a single study if included. |
| `rater` | Rater identifier. |
| `leaf` | Specimen or image identifier. |
| `actual` | Reference severity, expressed as a percentage from 0 to 100. |
| `unaided` | Severity estimate made without SADs, as a percentage from 0 to 100. |
| `aided` | Severity estimate made with SADs, as a percentage from 0 to 100. |

Use one row per rater–specimen pair, comma-separated CSV fields, and decimal points. The app permits missing aided estimates for precision calculations, but such data cannot satisfy the combined precision/power rule. It rejects duplicate pairs, invalid values, and inconsistent specimen references.

For app uploads, reference severity must be consistent for a given specimen within the uploaded study. The histogram script explicitly handles the two Gerbera reference sets in the compiled workbook.

## Installation

The workflow was checked with **R 4.5.2 on Windows**. The scripts use the native `|>` pipe, which requires R 4.1 or later; individual packages may have additional version requirements. RStudio is optional.

Install the analysis packages in an R console:

```r
install.packages(c(
  "readxl", "tidyverse", "metafor", "writexl",
  "ggthemes", "patchwork"
))
```

For the application and its checks:

```r
install.packages(c(
  "shiny", "bslib", "bsicons", "dplyr",
  "echarts4r", "scales", "DT"
))
```

Optionally, install `ragg` for histogram PNG/TIFF rendering:

```r
install.packages("ragg")
```

Package installation requires internet access.

## Reproduce the results

Download the repository and open a terminal in its root folder. With `Rscript` on your PATH, run:

```sh
Rscript index2.R
Rscript real_severity_histograms.R
```

Alternatively, set the RStudio working directory to the repository root and run:

```r
source("index2.R", echo = TRUE)
source("real_severity_histograms.R")
```

The scripts create `results_index2/` for numeric results and captions and write plots to `figures/`. The supplied tables remain available in `results/`.

Successful runs overwrite outputs with the same names. The main analysis uses **5,000 bootstrap resamples per evaluated scenario**, with seed `20261005`, and takes longer than the figure-only scripts. R and package versions are written to `results_index2/session_info.txt`.

To recreate only the supplementary Q-Q figures after the paired CCC CSV exists:

```sh
Rscript supplementary_qq_plots.R
```

To specify the histogram input and output directories explicitly:

```sh
Rscript real_severity_histograms.R all_data.xlsx results_index2 figures
```

The second directory receives numeric results and the caption; the third receives the figure files.

### Default planning settings

| Setting | Main analysis | Shiny app |
| --- | --- | --- |
| Precision target | RSE = 0.10 | RSE = 0.10 |
| Significance level | 0.05 | 0.05 |
| Target power | 0.80 | 0.80 |
| Absolute CCC improvement | 0.10 | 0.10 |
| Test sidedness | Two-sided | Two-sided |
| Operational minimum | 2 raters | 3 raters |
| Consecutive qualifying additions | Not a sequential stopping analysis | 2 |

The main analysis evaluates all four sensitivity effects in addition to the primary 0.10 scenario. To explore other settings, change the main planning parameters in `index2.R` or use the app's controls.

## Run the application

From an R console in the repository root:

```r
shiny::runApp("app.R")
```

1. Select **Use Example (clr.csv)** or upload a CSV for one study.
2. Review the target parameters and sequential-control settings.
3. Select **Apply Settings & Start**.
4. Add raters and inspect the requirements and history.
5. Download the history from the **Download** tab.

By default, the app estimates power inputs from the raters added so far. Its optional full-dataset mode uses all available raters to estimate planning inputs, while still requiring enough valid collected pairs to meet the power requirement.

The example's stopping step depends on the settings and rater order. The stopping indicator is a planning heuristic; it does not establish prospective power for a new study.

## Find and interpret the outputs

### Tables and numeric results

The saved outputs are in [results/](results/). Start with the main planning table, then examine effect sensitivity and diagnostic results.

| File | What it contains |
| --- | --- |
| [`table_cv_power.csv`](results/table_cv_power.csv) | Main scenario, including component requirements, combined `k_final`, settings, and feasibility fields. |
| [`table_cv_power.xlsx`](results/table_cv_power.xlsx) | Excel version of the component estimates; it has fewer fields than the CSV. |
| [`table_effect_sensitivity.csv`](results/table_effect_sensitivity.csv) | Results for every study and specified improvement. |
| [`table_effect_sensitivity_wide.csv`](results/table_effect_sensitivity_wide.csv) | Compact comparison of requirements and status across improvements. |
| [`supplementary_table_S1.csv`](results/supplementary_table_S1.csv) | Publication table generated from the sensitivity results. |
| [`power_curve_data.csv`](results/power_curve_data.csv) | Values underlying the power-curve figure. |
| [`reviewer3_paired_ccc.csv`](results/reviewer3_paired_ccc.csv) | Paired rater-level CCCs, raw/transformed differences, and boundary flags. |
| [`reviewer3_normality.csv`](results/reviewer3_normality.csv) | Distribution summaries and exploratory normality diagnostics. |
| [`reviewer3_bootstrap_sensitivity.csv`](results/reviewer3_bootstrap_sensitivity.csv) | Empirical type-I error and power estimates by study, effect, and sample size. |
| [`app_example_history.csv`](results/app_example_history.csv) | Worked sequential history generated by the app checks. |
| `real_severity_histograms_*.csv` | Reference measurements, severity summaries, bin counts, and Gerbera reference-set mapping. |
| `*_caption.txt` / `*_captions.txt` | Captions accompanying the generated figures. |
| [`analysis_notes.txt`](results/analysis_notes.txt) | Further interpretation, assumptions, and output provenance. |
| [`session_info.txt`](results/session_info.txt) | R and package versions recorded during the analysis. |

The `reviewer3_` filenames identify diagnostic analyses added during manuscript revision. They are part of the current workflow.

The existing [formatted supplementary table](results/supplementary_table_S1.docx) is provided in Word format. The scripts regenerate its CSV source but do not rewrite the Word document; compare the formatted table with the new CSV when inputs or settings change.

### Figures

All image and plot outputs are in [figures/](figures/).

| File | Purpose |
| --- | --- |
| [`power_curve_by_disease.png`](figures/power_curve_by_disease.png) | Nine-panel comparison of power across rater numbers and feasible effects. |
| [`analysis_plots.pdf`](figures/analysis_plots.pdf) | Additional plots from the main analysis. |
| [`reviewer3_qq_differences.pdf`](figures/reviewer3_qq_differences.pdf) | Study-level raw and transformed Q-Q plots. |
| [`supplementary_S1_qq_raw.png`](figures/supplementary_S1_qq_raw.png) | Combined raw paired-difference Q-Q figure. |
| [`supplementary_S2_qq_transformed.png`](figures/supplementary_S2_qq_transformed.png) | Combined transformed paired-difference Q-Q figure. |
| [`real_severity_histograms.png`](figures/real_severity_histograms.png) | Reference-severity histograms; also supplied as TIFF and PDF. |
| `app_example_run.jpg` / `app_example_settings.jpg` | Previously captured illustrations of the application. |
