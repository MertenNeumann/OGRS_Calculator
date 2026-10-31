# Run from the repository root: Rscript tests/run.R
if (!file.exists("OGRS_Calculator.R")) stop("Please run from the repository root.")
required <- c("testthat", "shiny", "bslib", "ggplot2", "haven", "readxl", "readr", "digest")
if (!all(vapply(required, requireNamespace, logical(1), quietly = TRUE))) {
  stop("Test dependencies are missing. Run scripts/setup.R first.")
}
results <- testthat::test_dir("tests/testthat", reporter = "summary", stop_on_failure = TRUE,
                             stop_on_warning = TRUE)
summary <- as.data.frame(results)
if (any(summary$skipped)) stop("Required checks were skipped.")
cat("Tests:", nrow(summary), "· Assertions:", sum(summary$passed), "· no skipped checks\n")
