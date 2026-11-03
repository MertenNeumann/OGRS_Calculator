# Run from the repository root: Rscript --vanilla scripts/setup.R
if (getRversion() < "4.6.2") stop("Install R 4.6.2 or later first.")
if (!file.exists("renv.lock")) stop("Run from the repository root.")

# Reuse a configured CA bundle without changing proxy or TLS verification settings.
ca <- Sys.getenv("SSL_CERT_FILE")
if (nzchar(ca) && !nzchar(Sys.getenv("CURL_CA_BUNDLE"))) Sys.setenv(CURL_CA_BUNDLE = ca)
options(repos = c(CRAN = "https://cloud.r-project.org"), timeout = 300)
if (!requireNamespace("renv", quietly = TRUE)) {
  # Bootstrap in a user-writable cache instead of requiring system-library access.
  bootstrap <- file.path(tools::R_user_dir("renv", "cache"), "bootstrap")
  dir.create(bootstrap, recursive = TRUE, showWarnings = FALSE)
  if (!requireNamespace("renv", lib.loc = bootstrap, quietly = TRUE)) {
    install.packages("renv", lib = bootstrap)
    loadNamespace("renv", lib.loc = bootstrap)
  }
}
project <- normalizePath(".")
# --vanilla skips project activation, so select the project library explicitly.
renv::restore(project = project, library = renv::paths$library(project = project),
              prompt = FALSE)
