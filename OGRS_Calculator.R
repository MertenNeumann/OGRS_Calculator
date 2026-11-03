# OGRS 3: shared, dependency-free scoring and input validation.
# Copyright (c) 2026 Merten Neumann. MIT license; see LICENSE.
# Model: Howard, Francis, Soothill & Humphreys (2009), appendix.
# German coding: Breiling, Dahle, Oberlader & Rettenberger (2022), chapter 5.

OGRS_APP_VERSION <- "2.1.6"
OGRS_MODEL_VERSION <- "OGRS 3 / German coding 2022 / model 2009"
OGRS_MANUAL_URL <- "https://krimpub.krimz.de/files/223/bm-online30.pdf"

ogrs_items <- c(
  Item1 = "Sex",
  Item2 = "Age at the current conviction",
  Item3 = "Age at the start of the at-risk period",
  Item4 = "Number of previous convictions",
  Item5 = "Age at the first conviction",
  Item6 = "Principal current offence"
)

ogrs_items_de <- c(
  Item1 = "Geschlecht",
  Item2 = "Alter bei der letzten Verurteilung",
  Item3 = "Alter zum Beginn des Prognosezeitraumes",
  Item4 = "Anzahl der Vorstrafen",
  Item5 = "Alter bei der ersten Verurteilung",
  Item6 = "Aktuelles Hauptdelikt"
)

# Stable API codes, original English labels and German manual terminology.
.ogrs_offence_labels <- c("Violence", "Robbery", "Public order",
    "Sexual (not against child)", "Sexual (against child)",
    "Soliciting or prostitution", "Burglary (domestic)", "Burglary (other)", "Theft (non-motor)",
    "Handling stolen goods", "Fraud and forgery", "Absconding or bail offences", "Taking & driving away and related offences",
    "Theft from vehicles", "Other motoring", "Drink driving", "Criminal damage",
    "Drug import/export/production", "Drug possession/supply", "Other offence")

ogrs_offences <- data.frame(
  code = c("Violence", "Robbery", "Public_order", "Sexual_no_child", "Sexual_child",
    "Soliciting_Prostitution", "Burglary_domestic", "Burglary_other", "Theft_non_motor",
    "Handling_Stolen_Goods", "Fraud_Forgery", "Absconding_Bail_Offences", "Taking_Driving",
    "Theft_from_Vehicle", "Other_motoring", "Drink_Driving", "Criminal_Damage",
    "Drug_import_export_production", "Drug_possession_supply", "Other"),
  label = .ogrs_offence_labels,
  german = c("Gewalt", "Raub", "Straftaten gegen die öffentliche Ordnung",
    "Sexualdelikte (Opfer ist kein Kind)", "Sexualdelikte (Opfer ist ein Kind)",
    "Zuhälterei / Prostitution", "Hausfriedensbruch", "Sonstige Einbrüche", "Diebstahl",
    "Hehlerei", "Betrug/Fälschung", "Flucht/Weisungsverstöße", "KFZ-Diebstahl",
    "KFZ-Aufbruch", "Verkehrsdelikte", "Trunkenheit am Steuer", "Sachbeschädigung",
    "Drogenhandel/-produktion", "Drogenbesitz", "Sonstige Delikte"),
  english = .ogrs_offence_labels,
  weight = c(0, -0.634795912, 0.181917975, 0.003276327, -0.653434071,
    0.760608858, -0.123943520, 0.240604429, 0.661244321, 0.351866973,
    0.159910192, 0.733378677, 0.380059431, 0.427225615, 0.262228428,
    -0.121439408, 0.204960477, -0.795556373, 0.077165871, -0.060667525),
  stringsAsFactors = FALSE
)

.ogrs_frame <- function(Data) {
  if (!is.data.frame(Data)) stop("Data must be a data.frame.", call. = FALSE)
  if (anyDuplicated(names(Data))) stop("Column names must be unique.", call. = FALSE)
  missing <- setdiff(names(ogrs_items), names(Data))
  if (length(missing)) stop("Missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  Data[names(ogrs_items)]
}

.ogrs_number <- function(x) {
  if (is.numeric(x) && !inherits(x, c("Date", "POSIXt"))) return(as.numeric(x))
  if (is.character(x) || is.factor(x)) return(suppressWarnings(as.numeric(trimws(as.character(x)))))
  rep(NA_real_, length(x))
}

# One record per issue; row is the one-based data row, excluding the header.
validate_ogrs <- function(Data) {
  Data <- .ogrs_frame(Data)
  issues <- data.frame(row = integer(), field = character(), code = character(),
                       message = character(), stringsAsFactors = FALSE)
  add <- function(mask, field, code, message) {
    rows <- which(!is.na(mask) & mask)
    if (length(rows)) issues <<- rbind(issues, data.frame(row = rows, field = field,
      code = code, message = message, stringsAsFactors = FALSE))
  }
  missing <- lapply(Data, function(x) is.na(x) | trimws(as.character(x)) == "")
  for (field in names(Data)) add(missing[[field]], field, "missing", "A value is required.")
  add(!missing$Item1 & !as.character(Data$Item1) %in% c("m", "f"), "Item1",
      "category", "Sex is not mapped to a model category.")
  add(!missing$Item6 & !as.character(Data$Item6) %in% ogrs_offences$code, "Item6",
      "category", "The principal offence is not mapped to a model category.")
  numeric_fields <- c("Item2", "Item3", "Item4", "Item5")
  numbers <- lapply(Data[numeric_fields], .ogrs_number)
  for (field in numeric_fields) {
    x <- numbers[[field]]
    add(!missing[[field]] & !is.finite(x), field, "number", "A finite number is required.")
    add(is.finite(x) & x != floor(x), field, "integer", "Please enter a whole number.")
    if (field == "Item4") {
      add(is.finite(x) & x < 0, field, "range", "The number of previous convictions must not be negative.")
    } else {
      # 10: original model minimum; 120: a software plausibility cap, not a norm range.
      add(is.finite(x) & (x < 10 | x > 120), field, "range", "Age must be between 10 and 120 years.")
    }
  }
  add(numbers$Item5 > numbers$Item2, "Item5", "chronology",
      "The first conviction must not be later than the current conviction.")
  add(numbers$Item2 > numbers$Item3, "Item3", "chronology",
      "The at-risk period must not start before the current conviction.")
  add(numbers$Item4 == 0 & numbers$Item5 != numbers$Item2, "Item5", "first_conviction",
      "With no previous convictions, the ages at first and current conviction must match.")
  issues[order(issues$row, match(issues$field, names(ogrs_items))), , drop = FALSE]
}

#' Score a single case or a data frame with named OGRS items.
#'
#' Item1: "m" or "f". Item2/3/5: age in completed years. Item4: number of
#' previous convictions, excluding the current conviction. Item6: stable code
#' from ogrs_offences$code. Data requires named columns Item1 through Item6;
#' column order is arbitrary and additional columns are ignored.
#' strict=FALSE retains invalid rows as NA; attr(result, "validation") contains
#' the reasons. Invalid inputs raise a structured error by default.
#' Probabilities are in [0, 1]; the Copas rate is logarithmic.
#' This function does not depend on Shiny or any other package.

ogrs <- function(Data = NULL, Item1 = NULL, Item2 = NULL, Item3 = NULL,
                 Item4 = NULL, Item5 = NULL, Item6 = NULL, strict = TRUE) {
  single <- is.null(Data)
  if (single) {
    args <- list(Item1 = Item1, Item2 = Item2, Item3 = Item3, Item4 = Item4,
                 Item5 = Item5, Item6 = Item6)
    if (any(lengths(args) != 1L)) {
      stop("A single case requires exactly one value for each item.", call. = FALSE)
    }
    Data <- as.data.frame(args, stringsAsFactors = FALSE)
  }
  Data <- .ogrs_frame(Data)
  issues <- validate_ogrs(Data)
  if (isTRUE(strict) && nrow(issues)) {
    condition <- structure(list(message = paste0("Invalid OGRS inputs: ",
      paste(unique(paste0("Row ", issues$row, ", ", ogrs_items[issues$field], ": ", issues$message)), collapse = " ")),
      call = NULL, issues = issues), class = c("ogrs_validation_error", "error", "condition"))
    stop(condition)
  }
  result <- data.frame(Prob_one_year = rep(NA_real_, nrow(Data)),
                        Prob_two_years = rep(NA_real_, nrow(Data)),
                        Copas_rate = rep(NA_real_, nrow(Data)))
  valid <- setdiff(seq_len(nrow(Data)), issues$row)
  if (length(valid)) {
    d <- Data[valid, , drop = FALSE]
    x <- lapply(d[c("Item2", "Item3", "Item4", "Item5")], .ogrs_number)
    copas <- log((1 + x$Item4) / (10 + x$Item2 - x$Item5))
    band <- findInterval(x$Item3, c(10, 12, 14, 16, 18, 21, 25, 30, 35, 40, 50))
    male <- c(0, 0.083922902, 0.075775765, -0.061594199, -0.625103618,
      -1.051515067, -1.166679288, -1.325976554, -1.368045933, -1.499690953, -2.025261458)
    female <- c(-0.785038489, -0.613852078, -0.669521331, -0.959179629, -0.897480934,
      -1.028488454, -1.052777806, -1.129127959, -1.421874940, -1.524652221, -2.449837160)
    age_sex <- ifelse(as.character(d$Item1) == "m", male[band], female[band])
    history <- ifelse(x$Item4 == 0, 0.126142106, 0.463062792)
    offence <- ogrs_offences$weight[match(as.character(d$Item6), ogrs_offences$code)]
    z <- 1.251124464 * copas + history + age_sex + offence
    result[valid, ] <- data.frame(Prob_one_year = plogis(1.402562384 + z),
                                  Prob_two_years = plogis(2.121705678 + z), Copas_rate = copas)
  }
  if (!isTRUE(strict)) attr(result, "validation") <- issues
  if (single) {
    out <- setNames(as.numeric(result[1, ]), names(result))
    if (!isTRUE(strict)) attr(out, "validation") <- issues
    return(out)
  }
  result
}

# Generate illustrative inputs for examples and software tests. These arbitrary
# distributions are not a calibrated population or independent scoring reference.
# Use set.seed() before calling to reproduce a dataset; no seed is set internally.
sim_ogrs_data <- function(N, prob_missing = 0.1) {
  numeric_scalar <- function(x) {
    typeof(x) %in% c("integer", "double") && !is.object(x) &&
      is.null(dim(x)) && length(x) == 1L && is.finite(x)
  }
  if (!numeric_scalar(N) || N < 0 || N != floor(N) || N > .Machine$integer.max) {
    stop("N must be a non-negative whole number no greater than .Machine$integer.max.", call. = FALSE)
  }
  if (!numeric_scalar(prob_missing) || prob_missing < 0 || prob_missing > 1) {
    stop("prob_missing must be a finite number between 0 and 1.", call. = FALSE)
  }
  N <- as.integer(N)
  current_age <- sample.int(81L, N, replace = TRUE) + 9L
  previous_convictions <- stats::rnbinom(N, size = 2, mu = 2)
  first_age <- 10L + floor(stats::runif(N) * (current_age - 9L))
  first_age[previous_convictions == 0] <- current_age[previous_convictions == 0]
  data <- data.frame(
    Item1 = sample(c("m", "f"), N, replace = TRUE),
    Item2 = current_age,
    Item3 = pmin(120L, current_age + stats::rpois(N, lambda = 2)),
    Item4 = previous_convictions,
    Item5 = first_age,
    Item6 = sample(ogrs_offences$code, N, replace = TRUE),
    stringsAsFactors = FALSE
  )
  # Mask fields after constructing valid rows so missingness introduces no
  # contradictory chronology among the values that remain observed.
  if (prob_missing > 0) {
    for (field in names(data)) {
      data[[field]][stats::runif(N) < prob_missing] <- NA
    }
  }
  data
}

# Return one case without changing global variables. An explicitly supplied
# environment receives the six item bindings for interactive scoring examples.
sim_ogrs_case <- function(prob_missing = 0, envir = NULL) {
  if (!is.null(envir) && !is.environment(envir)) {
    stop("envir must be NULL or an environment.", call. = FALSE)
  }
  data <- sim_ogrs_data(1L, prob_missing = prob_missing)
  if (!is.null(envir)) list2env(as.list(data), envir = envir)
  data
}
