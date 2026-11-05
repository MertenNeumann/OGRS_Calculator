load_app <- function() {
  env <- new.env(parent = globalenv())
  withr::with_dir(root, source(file.path(root,"app.R"), local=env))
  env
}

test_that("single-case edits invalidate results and expose input errors", {
  app <- load_app()
  shiny::testServer(app$server, {
    session$setInputs(single_sex="m",single_age_last=20,single_age_risk=20,
      single_previous=0,single_age_first=20,single_offence="Violence",single_calculate=1)
    expect_true(single_ready())
    expect_equal(single_saved()$scores$Prob_one_year,.12161553504714573)
    session$setInputs(single_previous=-1)
    expect_false(single_ready())
    expect_match(output$single_validation$html,"must not be negative")
    expect_error(output$single_download,class="shiny.silent.error")
  })
})

test_that("file, category and column changes invalidate results and downloads", {
  app <- load_app()
  a <- tempfile(fileext=".csv"); b <- tempfile(fileext=".csv")
  readr::write_csv(valid_case(),a)
  readr::write_csv(valid_case(Item1="f",Item2=35,Item3=35,Item4=3,Item5=18,Item6="Robbery"),b)
  file_input <- function(path) data.frame(name=basename(path),size=file.size(path),type="text/csv",datapath=path)
  shiny::testServer(app$server, {
    args <- setNames(as.list(names(ogrs_items)),paste0("column_",1:6))
    args[[mapping_input_id("sex","Item1","m")]] <- "m"
    args[[mapping_input_id("offence","Item6","Violence")]] <- "Violence"
    do.call(session$setInputs,c(list(batch_file=file_input(a),batch_format="csv",batch_decimal=".",batch_sheet=1,batch_id=""),args))
    session$setInputs(batch_calculate=1)
    expect_true(batch_ready())
    expect_match(output$batch_status$html,basename(a),fixed=TRUE)
    expect_equal(batch_saved()$export$Prob_one_year,.12161553504714573)
    session$setInputs(batch_file=file_input(b))
    expect_false(batch_ready())
    expect_match(output$batch_status$html,"out of date")
    expect_error(output$batch_download,class="shiny.silent.error")
    args[[mapping_input_id("sex","Item1","f")]] <- "f"
    args[[mapping_input_id("offence","Item6","Robbery")]] <- "Robbery"
    do.call(session$setInputs,args)
    session$setInputs(batch_calculate=2)
    expect_true(batch_ready())
    expect_equal(batch_saved()$export$Prob_one_year,.07042880754352777)
    path <- output$batch_download
    exported <- read.csv(path)
    expect_equal(exported$Prob_one_year,.07042880754352777)
    args2 <- list();args2[[mapping_input_id("sex","Item1","f")]] <- "m"
    do.call(session$setInputs,args2)
    expect_false(batch_ready())
    session$setInputs(column_1="Item2")
    expect_false(batch_ready())
  })
})

test_that("Explore explains empty selections without internal R errors", {
  app <- load_app()
  shiny::testServer(app$server, {
    session$setInputs(explore_axis="Item4",explore_range=c(0,10),explore_sexes=character(),
      explore_offences="Violence",explore_age_last=20,explore_age_risk=20,explore_previous=0,explore_age_first=20)
    expect_match(output$explore_status$html,"at least one sex")
    expect_error(output$explore_plot,class="shiny.silent.error")
    session$setInputs(explore_sexes=c("m","f"),explore_outcome="Prob_two_years")
    expect_equal(nrow(explored()$data),22L)
    expect_type(output$explore_plot$src,"character")
  })
})

test_that("Explore starts with an age range and preserves edited inputs", {
  app <- load_app()
  defaults <- function(id) {
    input <- htmltools::tagQuery(app$ui)$find(paste0("#", id))$selectedTags()[[1]]
    as.numeric(input$attribs$value)
  }
  expect_equal(defaults("explore_previous"), 2)
  expect_equal(defaults("explore_age_risk"), 30)
  shiny::testServer(app$server, {
    session$setInputs(explore_axis = "Item2", explore_range = c(14, 90),
      explore_sexes = "m", explore_offences = "Violence", explore_age_last = defaults("explore_age_last"),
      explore_age_risk = defaults("explore_age_risk"), explore_previous = defaults("explore_previous"),
      explore_age_first = defaults("explore_age_first"), explore_outcome = "Prob_one_year")
    expect_equal(explored()$data$Item2, 20:30)
    expect_type(output$explore_plot$src, "character")
    session$setInputs(explore_previous = 0)
    expect_equal(explored()$data$Item2, 20)
    session$setInputs(language = "de", explore_axis = "Item4")
    session$setInputs(explore_axis = "Item2", explore_range = c(14, 90))
    expect_equal(input$explore_previous, 0)
    expect_equal(explored()$data$Item2, 20)
    session$setInputs(explore_previous = 4, explore_age_risk = 35, language = "en")
    expect_equal(input$explore_previous, 4)
    expect_equal(explored()$data$Item2, 20:35)
  })
})

test_that("language changes preserve results, mappings and export validity", {
  app <- load_app()
  path <- tempfile(fileext = ".csv")
  readr::write_csv(valid_case(), path)
  file_input <- data.frame(name = basename(path), size = file.size(path),
                           type = "text/csv", datapath = path)
  shiny::testServer(app$server, {
    args <- setNames(as.list(names(ogrs_items)), paste0("column_", 1:6))
    args[[mapping_input_id("sex", "Item1", "m")]] <- "m"
    # A custom mapping must survive language changes, not just default choices.
    args[[mapping_input_id("offence", "Item6", "Violence")]] <- "Robbery"
    do.call(session$setInputs, c(list(batch_file = file_input, batch_format = "csv",
      batch_decimal = ".", batch_sheet = 1, batch_id = ""), args))
    session$setInputs(single_sex = "m", single_age_last = 20, single_age_risk = 20,
      single_previous = 0, single_age_first = 20, single_offence = "Violence",
      single_calculate = 1, batch_calculate = 1)
    expect_identical(language(), "en")
    single <- single_saved(); batch <- batch_saved(); spec <- current_spec()
    session$setInputs(language = "de")
    expect_true(single_ready()); expect_true(batch_ready())
    expect_identical(single_saved(), single); expect_identical(batch_saved(), batch)
    expect_identical(current_spec(), spec)
    expect_match(output$single_results$html, "12,2 %", fixed = TRUE)
    expect_match(output$single_results$html, "Geschätzte allgemeine Rückfälligkeit", fixed = TRUE)
    expect_match(output$batch_validation_summary$html, "1 berechenbare Zeile", fixed = TRUE)
    session$setInputs(single_previous = -1)
    expect_match(output$single_validation$html, "nicht negativ", fixed = TRUE)
    session$setInputs(language = "en")
    expect_match(output$single_validation$html, "must not be negative", fixed = TRUE)
    expect_true(batch_ready()); expect_identical(batch_saved(), batch)
  })
  # Another session starts in English even after the first session used German.
  shiny::testServer(app$server, { expect_identical(language(), "en") })
})
