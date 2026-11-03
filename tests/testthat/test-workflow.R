test_that("all supported import formats work end to end", {
  raw <- valid_case(); raw$Item1 <- "Männlich";raw$Item6 <- "Gewalt"
  paths <- setNames(file.path(tempdir(),paste0("ogrs-test.",c("csv","eu.csv","txt","sav"))), c("csv","csv2","txt","sav"))
  readr::write_csv(raw, paths[["csv"]]);readr::write_csv2(raw,paths[["csv2"]])
  readr::write_tsv(raw,paths[["txt"]]);haven::write_sav(raw,paths[["sav"]])
  for (format in names(paths)) {
    data <- read_ogrs_file(paths[[format]],format)
    mapped <- prepare_ogrs_data(data,names(ogrs_items),c("Männlich"="m"),c("Gewalt"="Violence"))
    expect_equal(ogrs(Data=mapped),ogrs(Data=valid_case()))
  }
  xlsx <- read_ogrs_file(file.path(root,"tests/fixtures/cases.xlsx"),"xlsx")
  expect_equal(nrow(xlsx),3L)
  sexes <- setNames(vapply(observed_values(xlsx$Item1),default_category,character(1),kind="sex"),observed_values(xlsx$Item1))
  offences <- setNames(vapply(observed_values(xlsx$Item6),default_category,character(1),kind="offence"),observed_values(xlsx$Item6))
  mapped <- prepare_ogrs_data(xlsx,names(ogrs_items),sexes,offences)
  expect_equal(nrow(validate_ogrs(mapped)),0L)
  expect_equal(ogrs(Data=mapped)$Prob_one_year[1:2],c(.12161553504714573,.07042880754352777))
  expect_equal(nrow(read_ogrs_file(paths[["csv2"]],"auto",filename="cases.csv")),1L)
})

test_that("import rejects empty, duplicate and missing columns", {
  p <- tempfile(fileext=".csv");writeLines("Item1,Item1\nm,m",p)
  expect_error(read_ogrs_file(p,"csv"),"unique")
  writeLines("Item1,Item2",p);expect_error(read_ogrs_file(p,"csv"),"no data rows")
  expect_error(prepare_ogrs_data(valid_case(),rep("Item1",6),c(m="m"),c(Violence="Violence")),"distinct")
})

test_that("bounded SPSS reads enforce the row limit without truncating valid files", {
  path <- tempfile(fileext = ".sav")
  raw <- valid_case()[rep(1L, 100001L), ]
  haven::write_sav(raw, path)
  expect_error(read_ogrs_file(path, "sav"), "100,000")
  haven::write_sav(raw[-nrow(raw), ], path)
  expect_identical(nrow(read_ogrs_file(path, "sav")), 100000L)
})

test_that("SPSS value labels and leading zeroes in CSV IDs remain usable", {
  raw <- rbind(valid_case(),valid_case(Item1="f",Item2=35,Item3=35,Item4=3,Item5=18,Item6="Robbery"))
  raw$Item1 <- haven::labelled(c(1,2), c(Männlich=1,Weiblich=2))
  raw$Item6 <- haven::labelled(c(1,2), c(Gewalt=1,Raub=2))
  path <- tempfile(fileext=".sav");haven::write_sav(raw,path)
  imported <- read_ogrs_file(path,"sav")
  expect_equal(observed_values(imported$Item1),c("1","2"))
  expect_equal(default_category("2","sex",attr(imported$Item1,"labels")),"f")
  mapped <- prepare_ogrs_data(imported,names(ogrs_items),c("1"="m","2"="f"),c("1"="Violence","2"="Robbery"))
  expect_equal(nrow(validate_ogrs(mapped)),0L)
  expect_equal(ogrs(Data=mapped)$Prob_one_year,c(.12161553504714573,.07042880754352777))
  csv <- tempfile(fileext=".csv")
  writeLines("Case_ID,Item1,Item2,Item3,Item4,Item5,Item6\n001,m,20,20,0,20,Violence",csv)
  expect_identical(read_ogrs_file(csv,"csv")$Case_ID,"001")
})

test_that("named item mappings follow item names rather than their order", {
  raw <- valid_case()
  columns <- setNames(names(raw), names(ogrs_items))
  expect_identical(
    prepare_ogrs_data(raw, rev(columns), c(m = "m"), c(Violence = "Violence")),
    prepare_ogrs_data(raw, unname(columns), c(m = "m"), c(Violence = "Violence"))
  )
  names(columns)[1] <- "Unknown"
  expect_error(prepare_ogrs_data(raw, columns, c(m = "m"), c(Violence = "Violence")), "distinct")
})

test_that("delimited imports preserve literal NA identifiers and raw text", {
  raw <- rbind(valid_case(), valid_case())
  raw$Case_ID <- c("NA", "001")
  raw$Item1 <- c(" m ", "m")
  for (format in c("csv", "csv2", "txt")) {
    path <- tempfile(fileext = if (format == "txt") ".txt" else ".csv")
    delimiter <- switch(format, csv = ",", csv2 = ";", txt = "\t")
    readr::write_delim(raw, path, delim = delimiter)
    imported <- read_ogrs_file(path, format)
    expect_identical(imported$Case_ID, c("NA", "001"))
    expect_identical(imported$Item1, c(" m ", "m"))
    mapped <- prepare_ogrs_data(imported, names(ogrs_items), c(m = "m"), c(Violence = "Violence"))
    export <- make_ogrs_export(mapped, ogrs(Data = mapped), validate_ogrs(mapped), ids = imported$Case_ID)
    expect_identical(export$Case_ID, c("NA", "001"))
    expect_true(all(export$Status == "calculated"))
  }
})

test_that("unknown categories and decimal values produce explicit validation issues", {
  raw <- valid_case(Item1="nicht bekannt",Item4="0,5")
  d <- prepare_ogrs_data(raw,names(ogrs_items),c(m="m"),c(Violence="Violence"),decimal_mark=",")
  issues <- validate_ogrs(d)
  expect_true(all(c("category","integer") %in% issues$code))
  expect_equal(default_category("Weiblich","sex"),"f")
  expect_equal(default_category("w","sex"),"f")
  expect_equal(default_category("1","sex",c(Männlich=1)),"m")
  expect_equal(default_category("Sexual (against child)","offence"),"Sexual_child")
  expect_equal(default_category("unbekannt","offence"),"")
  expect_false(identical(mapping_input_id("sex","a","m"),mapping_input_id("sex","a","f")))
})

test_that("export retains invalid rows and preserves their identity", {
  data <- rbind(valid_case(),valid_case(Item4=-1))
  out <- make_ogrs_export(data,ogrs(Data=data,strict=FALSE),validate_ogrs(data),ids=c("001","002"))
  expect_equal(out$Row,1:2);expect_equal(out$Case_ID,c("001","002"))
  expect_equal(out$Status,c("calculated","not_calculable"))
  expect_true(is.na(out$Prob_one_year[2]));expect_match(out$Error[2],"must not be negative")
  expect_equal(out$App_version,rep(OGRS_APP_VERSION,2))
  expect_error(make_ogrs_export(data,ogrs(Data=data,strict=FALSE),validate_ogrs(data),ids=c("x","x")),"unique")
  saved <- list(file="a",mapping="m")
  expect_true(same_batch_spec(saved,saved));expect_false(same_batch_spec(saved,list(file="b",mapping="m")))
})

test_that("download filenames match the calculation's UTC metadata", {
  withr::local_envvar(TZ = "Pacific/Auckland")
  timestamp <- as.POSIXct("2026-10-31 15:30:45", tz = "Europe/Berlin")
  data <- valid_case()
  export <- make_ogrs_export(data, ogrs(Data = data), validate_ogrs(data), timestamp = timestamp)
  expect_identical(export$Calculated_at_UTC, "2026-10-31T14:30:45Z")
  expect_identical(ogrs_export_filename("Single_case", timestamp), "OGRS3_Single_case_2026-10-31_14-30-45_UTC.csv")
  expect_identical(ogrs_export_filename("Results", timestamp), "OGRS3_Results_2026-10-31_14-30-45_UTC.csv")
  expect_identical(ogrs_export_filename("Example_data", timestamp), "OGRS3_Example_data_2026-10-31_14-30-45_UTC.csv")
})

test_that("large exports retain every row and its ordered validation messages", {
  data <- valid_case()[rep(1L, 100000L), ]
  data$Item4[seq.int(2L, nrow(data), by = 2L)] <- -1
  data$Item5[2] <- 25
  issues <- validate_ogrs(data)
  result <- make_ogrs_export(data, ogrs(Data = data, strict = FALSE), issues)
  expect_identical(result$Row, seq_len(nrow(data)))
  expect_identical(sum(result$Status == "not_calculable"), 50000L)
  expect_identical(result$Error[1], "")
  expect_identical(result$Error[2], paste(
    "Number of previous convictions: The number of previous convictions must not be negative.",
    "Age at the first conviction: The first conviction must not be later than the current conviction.",
    sep = " | "
  ))
  expect_identical(result$Error[100000], "Number of previous convictions: The number of previous convictions must not be negative.")
  expect_true(all(is.na(result$Prob_one_year[result$Status == "not_calculable"])))
  expect_true(all(is.finite(result$Prob_one_year[result$Status == "calculated"])))
})

test_that("Explore excludes invalid combinations and handles empty selections", {
  values <- list(Item2=20,Item3=20,Item4=0,Item5=20)
  expect_error(make_explore_data("Item4",c(0,10),character(),"Violence",values),"sex")
  expect_error(make_explore_data("Item4",c(0,10),"m",character(),values),"offence groups")
  expect_error(make_explore_data("Item4",c(0,10),"m",head(ogrs_offences$code,6),values),"one to five")
  x <- make_explore_data("Item5",c(14,30),c("m","f"),c("Violence","Robbery"),values)
  expect_equal(nrow(x$data),4L);expect_gt(x$excluded,0)
  expect_equal(nrow(validate_ogrs(x$data)),0L)
  expect_true(all(x$data$Item5==20))
})
