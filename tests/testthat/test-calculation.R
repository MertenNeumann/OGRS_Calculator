test_that("all coefficients and age boundaries match independent reference cases", {
  ref <- read.csv(file.path(root, "tests", "fixtures", "howard-reference.csv"))
  scores <- ogrs(Data = ref[names(ogrs_items)])
  expect_equal(as.matrix(scores), as.matrix(ref[c("Prob_one_year", "Prob_two_years", "Copas_rate")]), tolerance = 1e-12)
  expect_equal(nrow(scores), 1840L)
  expect_true(all(scores$Prob_two_years > scores$Prob_one_year))
})

test_that("single cases retain the established public result format", {
  result <- do.call(ogrs, as.list(valid_case()[1, ]))
  expect_equal(names(result), c("Prob_one_year", "Prob_two_years", "Copas_rate"))
  expect_equal(unname(result), c(.12161553504714573, .2213052114051131, -2.3025850929940455), tolerance = 1e-12)
  female <- do.call(ogrs, as.list(valid_case(Item1="f", Item2=35, Item3=35, Item4=3, Item5=18, Item6="Robbery")[1, ]))
  expect_equal(unname(female), c(.07042880754352777, .1345891308526054, -1.9095425048844386), tolerance = 1e-12)
})

test_that("missing, invalid and implausible inputs do not produce risk values", {
  bad <- list(valid_case(Item4=-1), valid_case(Item4=.5), valid_case(Item3=9),
    valid_case(Item2=200,Item3=200), valid_case(Item5=25), valid_case(Item3=19),
    valid_case(Item4=0, Item5=18), valid_case(Item6="unknown"), valid_case(Item1="divers"),
    valid_case(Item2=Inf), valid_case(Item2="zwanzig"), valid_case(Item2=NA))
  for (data in bad) {
    expect_error(ogrs(Data=data), class="ogrs_validation_error")
    out <- ogrs(Data=data, strict=FALSE)
    expect_true(all(is.na(out)))
    expect_gt(nrow(attr(out,"validation")),0)
  }
  expect_error(ogrs(Item2=20, Item3=20, Item4=0, Item5=20), "exactly one value")
})

test_that("empty and entirely missing tables retain their row structure", {
  empty <- valid_case()[FALSE, ]
  expect_identical(dim(ogrs(Data=empty)), c(0L,3L))
  missing <- valid_case()[c(1,1), ];missing[,] <- NA
  expect_error(ogrs(Data=missing), class="ogrs_validation_error")
  out <- ogrs(Data=missing, strict=FALSE)
  expect_identical(dim(out),c(2L,3L));expect_true(all(is.na(out)))
  expect_error(ogrs(Data=data.frame()),"Missing columns")
  expect_error(ogrs(Data=list()),"data.frame")
})

test_that("columns are matched by name and large values remain numerically stable", {
  data <- valid_case(Item4=1e300)
  expect_equal(ogrs(Data=data)$Prob_one_year,1)
  expect_true(all(is.finite(as.matrix(ogrs(Data=data)))))
  expect_equal(ogrs(Data=valid_case()[rev(names(ogrs_items))]),ogrs(Data=valid_case()))
  expect_equal(ogrs(Data=valid_case(Item2="20",Item3="20")),ogrs(Data=valid_case()))
})
