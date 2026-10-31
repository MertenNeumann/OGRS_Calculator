# Run from the repository root: Rscript --vanilla scripts/setup.R
if (getRversion() < "4.6.2") stop("Please install R 4.6.2 or later first.")
if (!file.exists("renv.lock")) stop("Please run from the repository root.")
# Use the managed environment CA bundle for R/libcurl as well.
# Preserve proxy settings and keep TLS verification enabled.
ca <- Sys.getenv("SSL_CERT_FILE")
if (nzchar(ca) && !nzchar(Sys.getenv("CURL_CA_BUNDLE"))) Sys.setenv(CURL_CA_BUNDLE = ca)
options(repos = c(CRAN = "https://cran.r-project.org"), timeout = 180)
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
renv::restore(prompt = FALSE)
