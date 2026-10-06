MINIMUM NUMBER OF RATERS IN SAD VALIDATION STUDIES

This repository contains the data, R analysis, figures, and interactive Shiny
application for planning the number of raters in studies validating standard
area diagrams (SADs). Start with index2.R; app.R is the interactive application.

1. FOLDER GUIDE

  all_data.xlsx                 Compiled observations for nine studies.
  clr.csv                       Coffee-leaf-rust example for the application.
  index2.R                      Main analysis, sensitivity tables, and diagnostics.
  real_severity_histograms.R    Nine-panel reference-severity histograms.
  supplementary_qq_plots.R      Supplementary Q-Q figures; also called by index2.R.
  app.R                         Shiny application for sequential rater planning.
  tests/test_app.R              Statistical, input, and Shiny server checks.
  figures/                      All plots, PDFs, TIFFs, and example screenshots.
  results_index2/               Numeric results, tables, captions, and run details.

The workbook and example CSV remain at the repository root so the scripts and
application can find them. Run every command below from this root folder.

2. REQUIREMENTS AND INSTALLATION

Use R 4.1 or later (the scripts use the native |> pipe); RStudio is optional.
The cleanup was checked with R 4.5.2. Package versions for the analysis are
recorded in results_index2/session_info.txt after each complete analysis run.

In an R console, install the analysis packages:

  install.packages(c("readxl", "tidyverse", "metafor", "writexl",
                     "ggthemes", "patchwork"))

For the application and its checks, also install:

  install.packages(c("shiny", "bslib", "bsicons", "dplyr",
                     "echarts4r", "scales", "DT"))

Optional: install.packages("ragg") for histogram PNG/TIFF rendering.
These commands install packages from CRAN and need an internet connection.

3. REPRODUCE THE ANALYSIS AND FIGURES

Download the repository and set your working directory to its root folder.
In RStudio, open this folder as a project or use Session > Set Working Directory.

From a terminal with Rscript on PATH, run these commands in order:

  Rscript index2.R
  Rscript real_severity_histograms.R

The main analysis generates the CV/precision and power tables, effect sensitivity
results, power curves, paired-CCC diagnostics, and bootstrap sensitivity results.
It also generates the supplementary Q-Q figures and supplementary_table_S1.csv.
The bootstrap uses 5,000 resamples per evaluated scenario and seed 20261005,
so it takes longer than the figure-only scripts. Successful runs overwrite
outputs with the same names and create output folders when needed.

To recreate only the supplementary Q-Q plots after the paired CCC CSV exists:

  Rscript supplementary_qq_plots.R

Equivalent RStudio console commands (in this order):

  source("index2.R", echo = TRUE)
  source("real_severity_histograms.R")

Standalone histogram command with custom locations:

  Rscript real_severity_histograms.R all_data.xlsx results_index2 figures

Its second directory receives CSVs and the caption; its third receives figures.

4. RUN THE INTERACTIVE APPLICATION

From an R console in the root folder:

  shiny::runApp("app.R")

Select "Use Example (clr.csv)", apply the desired settings, and add raters to
follow the sequential history. Applying settings starts a new history; pending
changes to settings do not change the active run. The example has 21 raters and
40 specimens. Default settings use a 10% precision target, alpha 0.05, power 0.80,
a 0.10 absolute CCC improvement, a floor of three raters, and two consecutive
qualifying additions. The analysis uses a floor of two raters.

CSV uploads must contain one study and one row per rater/specimen pair, with:

  rater    Rater identifier.
  leaf     Specimen/image identifier.
  actual   Reference severity as a percentage from 0 to 100.
  unaided  Unaided severity estimate as a percentage from 0 to 100.
  aided    Optional aided estimate as a percentage from 0 to 100.
  study    Optional single study identifier.

Use comma separators and decimal points. Missing aided estimates allow precision
estimation but cannot meet the combined precision/power stopping criterion.
Duplicate pairs, invalid values, and inconsistent specimen references are rejected.
The compiled workbook uses the same observation columns; the analysis combines
its study sheets and excludes the checklist sheet. Gerbera has two reference sets:
the histogram retains them separately, while app uploads require a consistent
reference value for each specimen within the uploaded study.

5. WHICH OUTPUTS TO READ

Start in results_index2/ with:

  table_cv_power.csv / .xlsx          Main planning scenario (CCC improvement 0.10).
  table_effect_sensitivity.csv        All study/effect combinations.
  table_effect_sensitivity_wide.csv   Compact comparison across improvements.
  supplementary_table_S1.csv          Publication table from current results.
  supplementary_table_S1.docx         Previously prepared formatted table.
  power_curve_data.csv               Values underlying the power curves.
  reviewer3_paired_ccc.csv            Rater-level paired CCCs and differences.
  reviewer3_normality.csv             Raw/transformed difference diagnostics.
  reviewer3_bootstrap_sensitivity.csv Bootstrap results by study, effect, and k.
  app_example_history.csv            Worked application history from the tests.
  real_severity_histograms_*.csv      Reference values, summaries, and bin counts.
  *_caption.txt / *_captions.txt      Figure captions.
  analysis_notes.txt                 Interpretation and output provenance.
  session_info.txt                   R and package versions for the analysis run.

The XLSX main table contains the component estimates; the CSV additionally has
k_final, settings, and status fields. The formatted DOCX is supplied as an existing
publication artifact; scripts regenerate its CSV source but do not rewrite the
Word document. Check the DOCX against the regenerated CSV after changing inputs.

In figures/:

  power_curve_by_disease.png          Combined power curves for the nine studies.
  analysis_plots.pdf                 Additional analysis plots.
  reviewer3_qq_differences.pdf        Study-level paired-difference Q-Q plots.
  supplementary_S1_qq_raw.png         Combined raw-difference Q-Q figure.
  supplementary_S2_qq_transformed.png Combined transformed-difference Q-Q figure.
  real_severity_histograms.png        Combined reference-severity histogram.
  real_severity_histograms.tiff/.pdf  Alternative formats of that histogram.
  app_example_run.jpg                Previously captured example application run.
  app_example_settings.jpg           Previously captured application settings.

6. INTERPRETING THE RESULTS

k_min_cv estimates the precision-based minimum using variability of unaided CCCs.
k_power estimates the paired power requirement. k_final is their maximum only
when both can be estimated. NA is not zero and must not be replaced with the
precision-only minimum when an effect is infeasible.

Power uses the mean unaided CCC of complete interior pairs as its baseline and
an exact transformed contrast: atanh(baseline + delta) - atanh(baseline).
Improvements targeting CCC >= 1 are infeasible. The sensitivity grid is 0.05,
0.10, 0.15, and 0.20. Match both study and delta_ccc when comparing bootstrap
outputs. Read analysis_notes.txt for the assumptions and limitations.

7. CHECK THE APPLICATION

After creating results_index2/table_cv_power.csv:

  Rscript tests/test_app.R

These checks compare the example against the analysis and exercise boundary
cases, invalid inputs, sequential stopping, and the Shiny server. They write
results_index2/app_example_history.csv. The JPG screenshots are historical
illustrations and are not recreated by this test.

8. FILES TO UPLOAD TO GITHUB

Upload the files and folders listed in section 1, this readme.txt, README.md,
and .gitignore. README.md provides a short GitHub landing page linking this guide.

_local_archive/ holds a verified copy of all files before the cleanup, including
older analyses and figures, the manuscript draft, revision notes, deployment
metadata, R history, and Windows metadata. It is for local recovery and is excluded
by .gitignore. Do not select it when uploading through the GitHub web interface:
.gitignore applies to Git commands, not to files manually selected for upload.

All active figure outputs belong in figures/. Numeric outputs and captions belong
in results_index2/. Retain the input datasets and scripts to make the results
reproducible. This cleanup prepares the folder; it does not publish or deploy it.
