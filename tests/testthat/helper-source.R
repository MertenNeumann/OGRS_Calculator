root <- normalizePath(file.path("..", ".."))
source(file.path(root, "OGRS_Calculator.R"), local = TRUE)
source(file.path(root, "R", "workflow.R"), local = TRUE)
valid_case <- function(...) {
  data <- data.frame(Item1 = "m", Item2 = 20, Item3 = 20, Item4 = 0,
                     Item5 = 20, Item6 = "Violence", stringsAsFactors = FALSE)
  for (name in names(list(...))) data[[name]] <- list(...)[[name]]
  data
}
.app_root <- root
i18n_environment <- environment()
withr::with_dir(root, sys.source(file.path(root, "R", "i18n.R"), envir = i18n_environment))
