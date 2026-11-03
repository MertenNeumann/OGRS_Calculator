test_that("English is the default and German labels follow the manual metadata", {
  expect_identical(translate("Sex"), "Sex")
  expect_identical(translate("Sex", "de"), "Geschlecht")
  expect_identical(unname(translate(ogrs_items, "de")), unname(ogrs_items_de))
  expect_identical(unname(translate(ogrs_offences$label, "de")), ogrs_offences$german)
  expect_identical(format_probability(.12161553504714573), "12.2 %")
  expect_identical(format_probability(.12161553504714573, "de"), "12,2 %")
  expect_identical(format_copas(-2.302585093), "-2.303")
  expect_identical(format_copas(-2.302585093, "de"), "-2,303")
  expect_identical(default_category("Raub", "offence"), "Robbery")
  expect_identical(default_category("Robbery", "offence"), "Robbery")
})

test_that("translation markers distinguish authored text from uploaded values", {
  authored <- localize_ui(htmltools::p("Male"), "de")
  raw <- localize_ui(htmltools::span("Male", `data-i18n-raw` = "true"), "de")
  expect_match(as.character(authored), "Männlich", fixed = TRUE)
  expect_false(grepl("Männlich", as.character(raw), fixed = TRUE))
  expect_match(as.character(raw), "Male", fixed = TRUE)
  control <- i18n_select_input("column_1", "Sex", c("Please select" = "", "Sex" = "Sex"))
  attributes <- htmltools::tagQuery(control)$find("select")$selectedTags()[[1]]$attribs
  options <- jsonlite::fromJSON(attributes[["data-i18n-options"]])
  expect_identical(options$value, "")
  expect_identical(options$text, "Please select")
  expect_identical(english_manual_category("Sexualdelikt (Kind)"), "Sexual (against child)")
})

test_that("page titles remain plain text before browser translation", {
  title <- htmltools::tags$title("OGRS 3 · Reoffending estimates")
  expect_identical(localize_ui(title), title)
  expect_identical(localize_ui(title, "de"), title)
})
