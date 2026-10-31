# File import, mappings and export status using the shared scoring rules.
read_ogrs_file <- function(path, format = "auto", filename = basename(path), sheet = 1L) {
  if (!file.exists(path)) stop("The uploaded file is no longer available.", call. = FALSE)
  if (file.size(path) > 10 * 1024^2) stop("The file must not exceed 10 MiB.", call. = FALSE)
  if (format == "auto") {
    extension <- tolower(tools::file_ext(filename))
    format <- switch(extension, xlsx = "xlsx", sav = "sav", csv = "delimited", txt = "delimited", "")
  }
  data <- switch(format,
    xlsx = readxl::read_excel(path, sheet = sheet, .name_repair = "minimal"),
    sav = haven::read_sav(path),
    csv = readr::read_delim(path, delim = ",", col_types = readr::cols(.default = "c"),
      name_repair = "minimal", show_col_types = FALSE, progress = FALSE, skip_empty_rows = FALSE),
    csv2 = readr::read_delim(path, delim = ";", col_types = readr::cols(.default = "c"),
      name_repair = "minimal", show_col_types = FALSE, progress = FALSE, skip_empty_rows = FALSE),
    txt = readr::read_delim(path, delim = "\t", col_types = readr::cols(.default = "c"),
      name_repair = "minimal", show_col_types = FALSE, progress = FALSE, skip_empty_rows = FALSE),
    delimited = readr::read_delim(path, delim = NULL, col_types = readr::cols(.default = "c"),
      name_repair = "minimal", show_col_types = FALSE, progress = FALSE, skip_empty_rows = FALSE),
    stop("Supported formats are XLSX, CSV, TXT and SAV. Please select the file format.", call. = FALSE)
  )
  if (format %in% c("csv", "csv2", "txt", "delimited") && nrow(readr::problems(data))) {
    stop("The table structure is inconsistent. Check the separator and column count.", call. = FALSE)
  }
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  if (!nrow(data)) stop("The file contains no data rows.", call. = FALSE)
  if (nrow(data) > 100000L) stop("Please use no more than 100,000 data rows per file.", call. = FALSE)
  if (anyNA(names(data)) || any(!nzchar(trimws(names(data)))) || anyDuplicated(names(data))) {
    stop("All columns must have a unique, non-empty name.", call. = FALSE)
  }
  data
}

observed_values <- function(x) {
  values <- trimws(as.character(x))
  sort(unique(values[!is.na(values) & nzchar(values)]))
}

# Keep each observed value identifiable when categories change order after an
# upload. Previous widget values must not be reassigned to different categories.
mapping_input_id <- function(kind, column, value) {
  paste0("map_", kind, "_", digest::digest(c(column, value), algo = "xxhash64"))
}

default_category <- function(value, kind, labels = NULL) {
  key <- tolower(trimws(value))
  if (kind == "sex") {
    if (key %in% c("m", "male", "männlich")) return("m")
    if (key %in% c("f", "w", "female", "weiblich")) return("f")
  } else {
    position <- match(key, tolower(ogrs_offences$code))
    if (is.na(position)) position <- match(key, tolower(ogrs_offences$label))
    if (is.na(position)) position <- match(key, tolower(ogrs_offences$german))
    if (!is.na(position)) return(ogrs_offences$code[position])
  }
  # SPSS value labels only provide a visible mapping suggestion.
  if (!is.null(labels)) {
    label <- names(labels)[match(value, as.character(unname(labels)))]
    if (length(label) == 1L && !is.na(label)) return(default_category(label, kind))
  }
  ""
}

prepare_ogrs_data <- function(raw, columns, sex_map, offence_map, decimal_mark = ".") {
  if (length(columns) != 6L || any(!columns %in% names(raw)) || anyDuplicated(columns)) {
    stop("Map each item to a distinct existing column.", call. = FALSE)
  }
  data <- raw[unname(columns)]
  names(data) <- names(ogrs_items)
  for (field in c("Item2", "Item3", "Item4", "Item5")) {
    x <- data[[field]]
    if (is.character(x) || is.factor(x)) {
      x <- trimws(as.character(x))
      if (identical(decimal_mark, ",")) x <- gsub(",", ".", x, fixed = TRUE)
      data[[field]] <- x
    }
  }
  convert <- function(x, map) {
    values <- trimws(as.character(x))
    result <- unname(map[match(values, names(map))])
    result[is.na(result) | !nzchar(result)] <- "__unmapped__"
    result[is.na(x) | values == ""] <- NA_character_
    result
  }
  data$Item1 <- convert(data$Item1, sex_map)
  data$Item6 <- convert(data$Item6, offence_map)
  data
}

make_ogrs_export <- function(data, scores, issues, ids = NULL,
                             timestamp = Sys.time(), filename = "") {
  n <- nrow(data)
  if (!is.null(ids)) {
    ids <- as.character(ids)
    if (length(ids) != n || anyNA(ids) || any(!nzchar(trimws(ids))) || anyDuplicated(ids)) {
      stop("The optional case ID must be present and unique for every row.", call. = FALSE)
    }
  }
  reasons <- vapply(seq_len(n), function(i) {
    rows <- issues[issues$row == i, , drop = FALSE]
    paste(paste0(unname(ogrs_items[rows$field]), ": ", rows$message), collapse = " | ")
  }, character(1))
  data.frame(Row = seq_len(n), Case_ID = if (is.null(ids)) rep("", n) else ids,
    Status = ifelse(seq_len(n) %in% issues$row, "not_calculable", "calculated"),
    Error = reasons, Prob_one_year = scores$Prob_one_year, Prob_two_years = scores$Prob_two_years,
    Copas_rate = scores$Copas_rate, App_version = rep(OGRS_APP_VERSION, n),
    Model_version = rep(OGRS_MODEL_VERSION, n), Source_file = rep(filename, n),
    Calculated_at_UTC = rep(format(timestamp, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), n),
    stringsAsFactors = FALSE, check.names = FALSE)
}

same_batch_spec <- function(saved, current) !is.null(saved) && identical(saved, current)

# Use the calculation time for result filenames, matching Calculated_at_UTC.
ogrs_export_filename <- function(label, timestamp) {
  paste0("OGRS3_", label, "_", format(timestamp, "%Y-%m-%d_%H-%M-%S_UTC", tz = "UTC"), ".csv")
}

make_explore_data <- function(axis, range, sexes, offences, values) {
  if (!axis %in% c("Item2", "Item3", "Item4", "Item5")) stop("Please select a valid axis.", call. = FALSE)
  if (!length(sexes) || !all(sexes %in% c("m", "f"))) stop("Please select at least one sex.", call. = FALSE)
  if (!length(offences) || length(offences) > 5L || !all(offences %in% ogrs_offences$code)) {
    stop("Please select one to five offence groups.", call. = FALSE)
  }
  upper <- if (axis == "Item4") 1000 else 120
  lower <- if (axis == "Item4") 0 else 10
  if (length(range) != 2L || any(!is.finite(range)) || range[1] > range[2] ||
      any(range != floor(range)) || any(range < lower | range > upper)) {
    stop("Please select a valid whole-number axis range.", call. = FALSE)
  }
  grid <- as.list(values[c("Item2", "Item3", "Item4", "Item5")])
  if (length(grid) != 4L || any(lengths(grid) != 1L)) stop("Please complete all fixed inputs.", call. = FALSE)
  grid$Item1 <- sexes
  grid$Item6 <- offences
  grid[[axis]] <- seq.int(range[1], range[2])
  grid <- expand.grid(grid[names(ogrs_items)], stringsAsFactors = FALSE)
  issues <- validate_ogrs(grid)
  invalid <- unique(issues$row)
  good <- grid[setdiff(seq_len(nrow(grid)), invalid), , drop = FALSE]
  scores <- ogrs(Data = good)
  list(data = cbind(good, scores), excluded = length(invalid), total = nrow(grid))
}

# Display formatting is session-local; exports always use unrounded values.
format_probability <- function(x, language = "en") {
  mark <- if (identical(language, "de")) "," else "."
  ifelse(is.finite(x), paste0(formatC(100 * x, format = "f", digits = 1, decimal.mark = mark), " %"), "–")
}
format_copas <- function(x, language = "en") {
  formatC(x, format = "f", digits = 3, decimal.mark = if (identical(language, "de")) "," else ".")
}
format_local_time <- function(x, language = "en") {
  format(x, if (identical(language, "de")) "%d.%m.%Y, %H:%M %Z" else "%Y-%m-%d, %H:%M %Z", tz = "Europe/Berlin")
}
