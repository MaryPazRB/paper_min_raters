# Minimum number of raters in SAD validation studies

Data, R scripts, results, figures, and a Shiny application for planning rater
numbers in standard area diagram (SAD) validation studies.

**Start with [readme.txt](readme.txt)** for package installation, input formats,
run commands, output descriptions, and interpretation.

- Run `Rscript index2.R` and then `Rscript real_severity_histograms.R` from this folder.
- Launch the application in R with `shiny::runApp("app.R")`.
- Find plots in [figures/](figures/) and tables, data, and captions in [results_index2/](results_index2/).
- After running the analysis, check the app with `Rscript tests/test_app.R`.
