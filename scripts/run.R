# Run from the repository root: Rscript scripts/run.R
if (getRversion() < "4.6.2") stop("Install R 4.6.2 or later first.")
if (!file.exists("app.R")) stop("Run from the repository root.")
port_value <- Sys.getenv("OGRS_PORT", "3838")
port <- suppressWarnings(as.integer(port_value))
if (!grepl("^[0-9]+$", port_value) || is.na(port) || port < 1024L || port > 65535L) {
  stop("OGRS_PORT must be a whole number between 1024 and 65535.")
}
shiny::runApp(".", host = Sys.getenv("OGRS_HOST", "127.0.0.1"), port = port,
              launch.browser = interactive())
