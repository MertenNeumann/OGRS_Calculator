if (getRversion() < "4.6.2") stop("This version of the app requires R 4.6.2 or later.")
if (!file.exists("app.R")) stop("Please run from the repository root.")
port <- suppressWarnings(as.integer(Sys.getenv("OGRS_PORT", "3838")))
if (is.na(port) || port < 1L || port > 65535L) stop("Invalid OGRS_PORT.")
shiny::runApp(".", host = Sys.getenv("OGRS_HOST", "127.0.0.1"), port = port,
              launch.browser = interactive())
