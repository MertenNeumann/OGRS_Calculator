test_that("simulated complete cases obey the input schema and chronology", {
  set.seed(721)
  data <- sim_ogrs_data(10000L, prob_missing = 0)
  expect_identical(names(data), names(ogrs_items))
  expect_identical(nrow(data), 10000L)
  expect_false(anyNA(data))
  expect_true(all(data$Item1 %in% c("m", "f")))
  expect_true(all(data$Item6 %in% ogrs_offences$code))
  expect_true(all(data$Item5 <= data$Item2 & data$Item2 <= data$Item3))
  expect_true(all(data$Item5[data$Item4 == 0] == data$Item2[data$Item4 == 0]))
  expect_true(any(data$Item4 == 0) && any(data$Item4 > 0))
  expect_identical(nrow(validate_ogrs(data)), 0L)
  scores <- ogrs(Data = data)
  expect_true(all(is.finite(as.matrix(scores))))
  expect_true(all(scores$Prob_one_year <= scores$Prob_two_years))
})

test_that("simulation has reproducible missingness and defined empty outputs", {
  set.seed(918)
  first <- sim_ogrs_data(100L, prob_missing = 0.35)
  set.seed(918)
  expect_identical(sim_ogrs_data(100L, prob_missing = 0.35), first)
  expect_true(anyNA(first))
  expect_true(any(!is.na(as.matrix(first))))
  issues <- validate_ogrs(first)
  expect_true(all(issues$code == "missing"))
  expect_true(all(is.na(as.matrix(sim_ogrs_data(3L, prob_missing = 1)))))
  for (probability in c(0, 0.5, 1)) {
    empty <- sim_ogrs_data(0L, prob_missing = probability)
    expect_identical(dim(empty), c(0L, 6L))
    expect_identical(names(empty), names(ogrs_items))
    expect_identical(nrow(ogrs(Data = empty)), 0L)
  }
})

test_that("invalid simulation parameters are rejected before drawing values", {
  invalid_counts <- list(NULL, numeric(), c(1, 2), -1, 0.5, NA, NaN, Inf,
                         "1", TRUE, 1i, matrix(1), as.Date("2026-01-01"),
                         .Machine$integer.max + 1)
  invalid_probabilities <- list(NULL, numeric(), c(0, 1), -0.1, 1.1, NA,
                                NaN, Inf, "0.1", TRUE, 1i, matrix(0.1))
  set.seed(32)
  seed <- .Random.seed
  for (count in invalid_counts) expect_error(sim_ogrs_data(count), "N must")
  for (probability in invalid_probabilities) {
    expect_error(sim_ogrs_data(1L, probability), "prob_missing must")
  }
  expect_error(sim_ogrs_case(envir = list()), "envir must")
  expect_identical(.Random.seed, seed)
})

test_that("single-case simulation returns a case and assigns only when requested", {
  items_before <- lapply(names(ogrs_items), function(field) {
    if (exists(field, envir = .GlobalEnv, inherits = FALSE)) {
      get(field, envir = .GlobalEnv, inherits = FALSE)
    } else NULL
  })
  case <- sim_ogrs_case()
  expect_identical(dim(case), c(1L, 6L))
  expect_identical(nrow(validate_ogrs(case)), 0L)
  items_after <- lapply(names(ogrs_items), function(field) {
    if (exists(field, envir = .GlobalEnv, inherits = FALSE)) {
      get(field, envir = .GlobalEnv, inherits = FALSE)
    } else NULL
  })
  expect_identical(items_after, items_before)
  target <- new.env(parent = emptyenv())
  assigned <- sim_ogrs_case(envir = target)
  expect_setequal(ls(target), names(ogrs_items))
  expect_identical(as.list(target)[names(ogrs_items)], as.list(assigned))
  expect_true(all(is.na(as.matrix(sim_ogrs_case(prob_missing = 1)))))
})
