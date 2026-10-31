# OGRS 3: bilingual Shiny application. Run with shiny::runApp(".").
library(shiny)
library(bslib)
library(ggplot2)
source("OGRS_Calculator.R", local = TRUE)
source("R/workflow.R", local = TRUE)
.app_root <- normalizePath(".", mustWork = TRUE)
source("R/i18n.R", local = TRUE)
options(shiny.maxRequestSize = 10 * 1024^2, shiny.sanitize.errors = TRUE)

manual_link <- function(label = "German manual (PDF)", page = NULL) {
  tags$a(label, href = paste0(OGRS_MANUAL_URL, if (!is.null(page)) paste0("#page=", page)),
         target = "_blank", rel = "noopener noreferrer")
}
notice <- function(..., type = "info") div(class = paste("notice", paste0("notice-", type)), ...)
intro <- function(kicker, title, text) div(class = "section-intro",
  p(class = "eyebrow", kicker), h1(title), p(class = "intro-copy", text))
interpretation_note <- function() notice(
  tags$strong("What is estimated? "),
  "General reoffending within one or two years from the start of the at-risk period. ",
  "The group-based estimate complements an individual assessment. ",
  manual_link("Interpretation and limitations in the manual", 18))
item_hint <- function(...) div(class = "item-hint", ...)
csv_export_note <- function(batch = FALSE) p(class = "item-hint csv-export-note",
  "CSV uses UTF-8, commas as field separators and a decimal point. Column names are in English. Probabilities are exported unrounded between 0 and 1.",
  if (batch) " Invalid rows include a status and error explanations.")
choice_offences <- setNames(ogrs_offences$code, ogrs_offences$label)
number_item <- function(id, field, value = NA_real_) numericInput(id,
  paste0(sub("Item", "Item ", field), " · ", ogrs_items[[field]]), value = value,
  min = if (field == "Item4") 0 else 10, max = if (field == "Item4") Inf else 120, step = 1)

ui <- page_navbar(
  title = div(class = "app-brand", span(class = "brand-symbol", "3"),
    span("OGRS", tags$small("Reoffending estimates"))),
  id = "main_nav", selected = "single", lang = "en", fillable = FALSE,
  window_title = "OGRS 3 · Reoffending estimates",
  theme = bs_theme(version = 5, bg = "#f5f5f5", fg = "#333333", primary = "#337ab7",
    secondary = "#60666c", base_font = "system-ui", heading_font = "system-ui"),
  header = tags$head(tags$link(rel = "stylesheet", href = "app.css"),
    tags$script(id = "i18n-catalog", type = "application/json",
      HTML(jsonlite::toJSON(as.list(.translation_catalog), auto_unbox = TRUE))),
    tags$script(src = "i18n.js", defer = NA)),
  footer = div(class = "app-footer", span(paste("OGRS 3 · App", OGRS_APP_VERSION)),
    span("German coding: Breiling et al. (2022) · Model: Howard et al. (2009)")),
  nav_panel("Single case", value = "single",
    intro("SINGLE CASE", "Estimate the risk of general reoffending.",
      "OGRS 3 uses six items. Enter the details for each item and click Calculate result below the inputs. You can then download the result as a CSV file."),
    layout_columns(col_widths = c(5, 7),
      card(class = "input-card", card_header(h2("Case details")),
        i18n_select_input("single_sex", "Item 1 · Sex",
          choices = c("Please select" = "", "Male" = "m", "Female" = "f"), selected = ""),
        number_item("single_age_last", "Item2"),
        item_hint("Use the age at sentencing, not the age when the offence was committed."),
        number_item("single_age_risk", "Item3"),
        item_hint("For custodial measures, use the age at the relevant release date. ", manual_link("Details", 14)),
        number_item("single_previous", "Item4"),
        item_hint("Count independent previous convictions; exclude the current conviction."),
        number_item("single_age_first", "Item5"),
        item_hint("With no previous convictions, this equals the age at the current conviction."),
        i18n_select_input("single_offence", "Item 6 · Principal current offence",
          choices = c("Please select" = "", choice_offences), selected = ""),
        item_hint("For multiple offences, use the one attracting the highest sentence. ", manual_link("Coding guidance in the manual", 16)),
        p(class = "item-hint", "Enter ages in completed years."),
        uiOutput("single_validation"),
        div(class = "action-row", actionButton("single_calculate", "Calculate result", class = "btn-primary"),
          actionButton("single_example", "Load example", class = "btn-outline-secondary")),
        uiOutput("example_notice")
      ),
      div(class = "result-column",
        uiOutput("single_results"), interpretation_note(),
        accordion(open = FALSE,
          accordion_panel("Measures and interpretation",
            p("The percentages estimate general reoffending resulting in a further final conviction. The two-year estimate covers both years together."),
            p("The Copas rate combines the extent and time span of the criminal history on a logarithmic scale. It is not a probability and can be negative."),
            p("The model coefficients were derived from a British development sample. The German manual published in 2022 did not recalibrate them using German data. Interpret absolute percentages with appropriate caution."),
            manual_link("Method, application and limitations", 17)))
      )
    )
  ),
  nav_panel("Dataset", value = "batch",
    intro("DATASET CALCULATION", "From a dataset to checked results.",
      "Upload a dataset, assign its columns to the six OGRS 3 items and check the mappings for sex and principal offence. Review the data checks, calculate the results and download them as CSV. An example dataset is available below."),
    notice(tags$strong("Minimise the data you upload. "),
      "Upload only the required items and, if needed, a pseudonymous case ID. Names, dates of birth and case reference numbers are not required."),
    layout_columns(col_widths = c(5, 7),
      card(card_header(h2("1 · Choose a file")),
        fileInput("batch_file", "Upload dataset (maximum 10 MiB)",
          accept = c(".xlsx", ".csv", ".txt", ".sav"), buttonLabel = "Choose file", placeholder = "No file selected"),
        i18n_select_input("batch_format", "File format", choices = c("Detect automatically" = "auto",
          "Excel (.xlsx)" = "xlsx", "CSV · Comma-separated" = "csv",
          "CSV · Semicolon-separated" = "csv2", "Text · Tab-separated" = "txt", "SPSS (.sav)" = "sav")),
        numericInput("batch_sheet", "Worksheet number (Excel only)", value = 1, min = 1, step = 1),
        i18n_select_input("batch_decimal", "Decimal mark in numeric text", choices = c("Point" = ".", "Comma" = ",")),
        p(class = "item-hint", "The first row must contain unique column names. Maximum 100,000 data rows."),
        downloadButton("example_download", "Download example dataset", class = "btn-outline-secondary"),
        uiOutput("import_message")
      ),
      card(card_header(h2("Preview")), p(class = "item-hint", "The first six data rows of your file."),
        div(class = "table-scroll", tableOutput("batch_preview")))
    ),
    card(card_header(h2("2 · Map variables")), uiOutput("column_mapping")),
    card(card_header(h2("3 · Map categories")),
      p("Check the suggested mappings for sex and principal offence. Unmapped values cannot be scored. SPSS uses the stored numeric values; value labels may provide a mapping suggestion."),
      uiOutput("category_mapping")),
    card(card_header(h2("4 · Check data and calculate")),
      uiOutput("batch_validation_summary"),
      div(class = "table-scroll", tableOutput("batch_issues")),
      actionButton("batch_calculate", "Calculate checked data", class = "btn-primary")),
    card(card_header(h2("5 · Results and export")), uiOutput("batch_status"),
      uiOutput("batch_summary"),
      i18n_select_input("batch_outcome", "Display distribution", choices = c("One year" = "Prob_one_year", "Two years" = "Prob_two_years", "Copas rate" = "Copas_rate")),
      plotOutput("batch_plot", height = "300px"), uiOutput("batch_download_control"),
      csv_export_note(batch = TRUE)),
    interpretation_note()
  ),
  nav_panel("Explore", value = "explore",
    intro("UNDERSTAND THE MODEL", "Explore the relationships.",
      "Use the plot to see how OGRS 3 results change when you vary item values. Choose the item on the x-axis, adjust the other inputs and select the measure to display. To calculate results for specific cases, use Single case or Dataset."),
    layout_columns(col_widths = c(4, 8),
      card(card_header(h2("Model combinations")),
        i18n_select_input("explore_axis", "Value on the x-axis", choices = setNames(c("Item2", "Item3", "Item4", "Item5"), unname(ogrs_items[c("Item2", "Item3", "Item4", "Item5")])), selected = "Item4"),
        sliderInput("explore_range", "Axis range", min = 0, max = 50, value = c(0, 50), step = 1),
        i18n_select_input("explore_outcome", "Measure", choices = c("Reoffending within one year" = "Prob_one_year", "Reoffending within two years" = "Prob_two_years", "Copas rate" = "Copas_rate")),
        checkboxGroupInput("explore_sexes", "Sex (select at least one)", choices = c("Male" = "m", "Female" = "f"), selected = "m"),
        i18n_selectize_input("explore_offences", "Principal current offence (one to five groups)", choices = choice_offences,
          selected = "Violence", multiple = TRUE, options = list(maxItems = 5, plugins = list("remove_button"))),
        number_item("explore_age_last", "Item2", 20), number_item("explore_age_risk", "Item3", 20),
        number_item("explore_previous", "Item4", 0), number_item("explore_age_first", "Item5", 20),
        item_hint("The selected axis value replaces the corresponding fixed input. Invalid combinations are excluded.")),
      card(card_header(h2("Model estimates")), uiOutput("explore_status"),
        plotOutput("explore_plot", height = "520px"),
        p(class = "item-hint", "Only whole years and whole conviction counts are scored. Age-band effects are shown as steps.")))
  ),
  nav_panel("Methods", value = "methods",
    intro("METHODS", "OGRS 3: instrument and calculation",
      "Find information on OGRS 3, the calculation and the interpretation of its values. Use the search below to look up principal-offence categories; the linked German manual provides the full coding guidance."),
    layout_columns(col_widths = c(7, 5),
      card(card_header(h2("What OGRS 3 estimates")),
        p("OGRS 3 uses just six static factors to estimate the statistical risk of general reoffending within one or two years. It can help inform an individual assessment, but should not be used as the sole source of information."),
        p("The German version concerns reoffending resulting in a further final conviction. Administrative offences are excluded. Item 3 defines the start of the at-risk period."),
        tags$ul(tags$li("No specific prediction of sexual or violent reoffending, or of the severity of a future offence."),
          tags$li("Unrecorded offences are not covered."),
          tags$li("Treatment progress and changes in dynamic risk factors are not captured."),
          tags$li("The German version published in 2022 did not recalibrate the British coefficients. Discrimination and the accuracy of absolute probabilities are different properties."),
          tags$li("All six items are required. The model only provides male and female categories; a missing or inapplicable category is not replaced with an assumption.")),
        manual_link("Full application, coding and limitation guidance in the manual", 10)),
      card(card_header(h2("Model and sources")),
        p("The app uses the original coefficients from Howard et al. (2009), including the age bands 18–20 and 21–24. Item names and coding guidance follow Breiling et al. (2022)."),
        p("The German coding restricts criminal history to convictions. The British original also includes cautions, reprimands and warnings. This is not a model re-estimated using German data."),
        p("The model combines the Copas rate with criminal-history, age/sex and offence terms. A logistic function converts the weighted sum into cumulative one- and two-year probabilities. Both periods start at the age specified in Item 3."),
        p(tags$a("Calculation code", href = "https://github.com/MertenNeumann/OGRS_Calculator/blob/main/OGRS_Calculator.R", target = "_blank", rel = "noopener noreferrer")),
        p(manual_link("Breiling, Dahle, Oberlader & Rettenberger (2022)"), ". BM-Online, volume 30. Kriminologische Zentralstelle."),
        p(tags$a("Howard, Francis, Soothill & Humphreys (2009)", href = "https://www.researchgate.net/publication/258365273_OGRS3_the_revised_Offender_Group_Reconviction_Score", target = "_blank", rel = "noopener noreferrer"), ". OGRS 3: The revised Offender Group Reconviction Scale. Ministry of Justice, Research Summary 7/09."),
        accordion(open = FALSE, accordion_panel("Difference from the 2022 Excel tool",
          p("The official Excel tool dated 13 January 2022 assigns age 20 to the 20–24 group. The original report uses 18–20 and 21–24; this app follows the original report. The Excel tool also slightly rounds the Copas coefficient. Details and reference cases are documented in the repository."))))
    ),
    card(card_header(h2("Principal offence coding reference")),
      p("Search by statutory provision or offence category. The table reproduces factual mappings from the German manual and its 2022 Excel tool; it does not replace case-specific coding decisions."),
      textInput("offence_search", "Search, e.g. § 242 StGB", placeholder = "Enter a statutory provision or category"),
      div(class = "table-scroll", tableOutput("offence_lookup")),
      p(class = "item-hint", "Up to 20 matches are shown. The 2022 list is incomplete and does not reflect later legislative changes. Unlisted or ambiguous cases are not assigned automatically. ", manual_link("Manual appendix", 25)))
  ),
  nav_panel("About the app", value = "about",
    intro("ABOUT", "About this app",
      "Find the source code, contact links and the separate VRAG-R calculator here. Use GitHub Issues to report bugs or suggest additions. Information on data processing is provided below."),
    layout_columns(col_widths = c(7, 5),
      card(card_header(h2("Code and contact")),
        p("The application code and documentation are available on ", tags$a("GitHub", href = "https://github.com/MertenNeumann/OGRS_Calculator", target = "_blank", rel = "noopener noreferrer"), "."),
        p("Bug reports, feature requests and suggestions for additions are welcome via ", tags$a("GitHub Issues", href = "https://github.com/MertenNeumann/OGRS_Calculator/issues", target = "_blank", rel = "noopener noreferrer"), ". Please do not post personal information or original datasets there."),
        p("Merten Neumann: ", tags$a("ResearchGate", href = "https://www.researchgate.net/profile/Merten-Neumann", target = "_blank", rel = "noopener noreferrer"), " · ",
          tags$a("Google Scholar", href = "https://scholar.google.com/citations?user=QjfiK8gAAAAJ&hl=de", target = "_blank", rel = "noopener noreferrer"))),
      card(card_header(h2("Related calculator")),
        p("The VRAG-R calculator is available as a separate app."),
        p(tags$a("VRAG-R calculator", href = "https://mertenneumann.shinyapps.io/VRAGR_Calculator/", target = "_blank", rel = "noopener noreferrer")),
        p(tags$a("VRAG-R source code", href = "https://github.com/MertenNeumann/VRAGR_Calculator", target = "_blank", rel = "noopener noreferrer")))
    ),
    card(card_header(h2("Data processing")),
      p("Calculations run on the server, not exclusively in your browser. Inputs and uploaded files are available for processing during the session. The application code does not create a permanent case database or log case details."),
      accordion(open = FALSE, accordion_panel("Hosting and temporary files",
        p("The app removes temporary uploads when a file is replaced and when the session ends, where the server can execute cleanup. A lost connection is not necessarily recognised immediately as the end of a session. Infrastructure log and backup retention depends on the hosting provider."),
        p("For deployments on shinyapps.io, Posit documents hosting in AWS us-east-1 (USA). Those responsible for a deployment must assess its suitability for their data and use case. Self-hosted deployments have their own operating conditions."),
        p(tags$a("Posit security information", href = "https://docs.posit.co/shinyapps.io/guide/security_and_compliance/index.html", target = "_blank", rel = "noopener noreferrer"))))),
    card(card_header(h2("AI-assisted development")),
      p("OpenAI Codex assisted with revising the code, interface, documentation and software tests. The app calculates results using the documented R code; no AI model is used to score cases."))
  ),
  nav_spacer(),
  nav_item(div(class = "language-control", i18n_select_input("language", "Language / Sprache",
    choices = c("English" = "en", "Deutsch" = "de"), selected = "en", selectize = FALSE)))
)

server <- function(input, output, session) {
  language <- reactive(if (identical(input$language, "de")) "de" else "en")
  tx <- function(text) translate(text, language())
  probabilities <- function(x) format_probability(x, language())
  counted <- function(n, singular, plural) paste(n, tx(if (n == 1L) singular else plural))
  render_ui <- function(expr) {
    template <- function() NULL
    body(template) <- substitute(expr); environment(template) <- parent.frame()
    shiny::renderUI({ localize_ui(template(), language()) })
  }
  # Mapping widgets must not be rebuilt when only the display language changes.
  render_inputs <- function(expr) {
    template <- function() NULL
    body(template) <- substitute(expr); environment(template) <- parent.frame()
    shiny::renderUI({ localize_ui(template()) })
  }
  single_data <- reactive(data.frame(Item1 = input$single_sex %||% "", Item2 = input$single_age_last %||% NA_real_,
    Item3 = input$single_age_risk %||% NA_real_, Item4 = input$single_previous %||% NA_real_,
    Item5 = input$single_age_first %||% NA_real_, Item6 = input$single_offence %||% "", stringsAsFactors = FALSE))
  single_saved <- reactiveVal(NULL)
  example_loaded <- reactiveVal(FALSE)
  observeEvent(input$single_example, {
    example_loaded(TRUE)
    updateSelectInput(session, "single_sex", selected = "m")
    updateNumericInput(session, "single_age_last", value = 20)
    updateNumericInput(session, "single_age_risk", value = 20)
    updateNumericInput(session, "single_previous", value = 0)
    updateNumericInput(session, "single_age_first", value = 20)
    updateSelectInput(session, "single_offence", selected = "Violence")
  })
  output$example_notice <- render_ui({
    example <- data.frame(Item1 = "m", Item2 = 20, Item3 = 20, Item4 = 0, Item5 = 20, Item6 = "Violence")
    if (example_loaded() && isTRUE(all.equal(single_data(), example))) {
      item_hint("Example loaded: male, ages 20/20/20, no previous convictions, principal offence Violence. Check before calculating.")
    }
  })
  observeEvent(input$single_calculate, {
    data <- single_data()
    if (!nrow(validate_ogrs(data))) single_saved(list(data = data, scores = ogrs(Data = data), time = Sys.time()))
  })
  output$single_validation <- render_ui({
    if (is.null(input$single_calculate) || input$single_calculate == 0L) return(NULL)
    issues <- validate_ogrs(single_data())
    if (nrow(issues)) notice(tags$strong("Please check the inputs"),
      tags$ul(lapply(seq_len(nrow(issues)), function(i) tags$li(paste0(item_labels(language())[issues$field[i]], ": ", tx(issues$message[i]))))), type = "warning")
  })
  single_ready <- reactive({
    saved <- single_saved()
    !is.null(saved) && identical(saved$data, single_data()) && !nrow(validate_ogrs(single_data()))
  })
  output$single_results <- render_ui({
    if (!single_ready()) return(card(class = "empty-result", card_header(h2("Your result")),
      h3(if (is.null(single_saved())) "Ready when your inputs are complete." else "Inputs changed."),
      p(if (is.null(single_saved())) "Complete all six items and start the calculation." else "Check the inputs and calculate again. Previous results are hidden.")))
    saved <- single_saved(); score <- saved$scores
    card(class = "result-card", card_header(h2("Estimated general reoffending")),
      div(class = "probability-grid",
        div(class = "probability", span("Within one year"), strong(id = "prob_one", probabilities(score$Prob_one_year))),
        div(class = "probability", span("Within two years"), strong(id = "prob_two", probabilities(score$Prob_two_years)))),
      p(class = "item-hint", "Both periods start at the age specified in Item 3. Displayed estimates are rounded; calculation and export use unrounded values."),
      p(class = "copas-value", "Copas rate: ", strong(id = "copas_value", format_copas(score$Copas_rate, language()))),
      div(class = "result-meta", paste(tx("Calculated:"), format_local_time(saved$time, language()))),
      accordion(open = FALSE, accordion_panel("Inputs used", tableOutput("single_input_summary"))),
      downloadButton("single_download", "Download result as CSV", class = "btn-outline-primary"),
      csv_export_note())
  })
  output$single_input_summary <- renderTable({
    req(single_ready()); data <- single_saved()$data
    values <- as.character(data[1, ])
    values[1] <- tx(if (data$Item1 == "m") "Male" else "Female")
    values[6] <- offence_labels(language())[match(data$Item6, ogrs_offences$code)]
    setNames(data.frame(Item = unname(item_labels(language())), Value = values), tx(c("Item", "Value")))
  }, rownames = FALSE)
  output$single_download <- downloadHandler(filename = function() {
    req(single_ready())
    ogrs_export_filename("Single_case", single_saved()$time)
  }, content = function(file) {
    req(single_ready())
    saved <- single_saved()
    export <- make_ogrs_export(saved$data, saved$scores, validate_ogrs(saved$data), timestamp = saved$time)
    write.csv(cbind(saved$data, export), file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  })

  uploaded_paths <- character()
  cleanup_upload <- function(paths) {
    base <- paste0(normalizePath(tempdir(), mustWork = TRUE), .Platform$file.sep)
    for (path in paths) {
      resolved <- normalizePath(path, mustWork = FALSE)
      if (startsWith(resolved, base) && file.exists(resolved) && !dir.exists(resolved)) unlink(resolved)
    }
  }
  observeEvent(input$batch_file, {
    path <- input$batch_file$datapath
    cleanup_upload(setdiff(uploaded_paths, path))
    uploaded_paths <<- union(uploaded_paths, path)
  }, ignoreNULL = FALSE)
  session$onSessionEnded(function() cleanup_upload(uploaded_paths))

  imported <- reactive({
    if (is.null(input$batch_file)) return(list(data = NULL, error = NULL))
    tryCatch(list(data = read_ogrs_file(input$batch_file$datapath, input$batch_format %||% "auto",
      input$batch_file$name, input$batch_sheet %||% 1L), error = NULL),
      error = function(e) list(data = NULL, error = "The file could not be read. Check the format, worksheet, unique column names and file size."))
  })
  output$import_message <- render_ui({
    x <- imported()
    if (!is.null(x$error)) notice(tx(x$error), type = "warning")
    else if (!is.null(x$data)) item_hint(paste(counted(nrow(x$data), "data row", "data rows"), "·", counted(ncol(x$data), "column", "columns")))
  })
  output$batch_preview <- renderTable({ req(imported()$data); head(imported()$data, 6L) }, rownames = FALSE)
  output$column_mapping <- render_inputs({
    data <- imported()$data
    if (is.null(data)) return(p("Upload a valid file first."))
    choices <- c("Please select" = "", setNames(names(data), names(data)))
    controls <- lapply(seq_along(ogrs_items), function(i) {
      key <- names(ogrs_items)[i]
      selected <- if (key %in% names(data)) key else ""
      i18n_select_input(paste0("column_", i), paste0("Item ", i, " · ", ogrs_items[i]), choices, selected)
    })
    tagList(div(class = "form-grid", controls),
      i18n_select_input("batch_id", "Optional pseudonymous case ID", choices = c("Use the sequential row number only" = "", names(data)), selected = ""))
  })
  selected_columns <- reactive(vapply(seq_along(ogrs_items), function(i) input[[paste0("column_", i)]] %||% "", character(1)))
  mapping_values <- reactive({
    data <- imported()$data; columns <- selected_columns()
    if (is.null(data) || any(!columns %in% names(data)) || anyDuplicated(columns)) return(NULL)
    list(sex = observed_values(data[[columns[1]]]), offence = observed_values(data[[columns[6]]]))
  })
  output$category_mapping <- render_inputs({
    values <- mapping_values()
    if (is.null(values)) return(p("Map each of the six variables to a distinct column first."))
    if (any(lengths(values) > 100L)) return(notice("More than 100 distinct categories: check the column mapping.", type = "warning"))
    data <- imported()$data; columns <- selected_columns()
    controls <- function(kind, choices, col) lapply(seq_along(values[[kind]]), function(i) {
      value <- values[[kind]][i]
      default <- default_category(value, kind, attr(data[[col]], "labels"))
      i18n_select_input(mapping_input_id(kind, col, value), tagList(localize_ui("Value:"), " ", span(value, `data-i18n-raw` = "true")), c("Not mapped" = "", choices), selected = default)
    })
    layout_columns(col_widths = c(4, 8),
      div(h3("Sex"), div(class = "mapping-grid", controls("sex", c("Male" = "m", "Female" = "f"), columns[1]))),
      div(h3("Principal current offence"), div(class = "mapping-grid", controls("offence", choice_offences, columns[6]))))
  })
  current_spec <- reactive({
    values <- mapping_values()
    maps <- function(kind) {
      if (is.null(values) || length(values[[kind]]) > 100L) return(character())
      column <- selected_columns()[if (kind == "sex") 1L else 6L]
      setNames(vapply(values[[kind]], function(value) input[[mapping_input_id(kind, column, value)]] %||% "", character(1)), values[[kind]])
    }
    list(file = if (is.null(input$batch_file)) NULL else as.list(input$batch_file),
      format = input$batch_format %||% "auto", sheet = input$batch_sheet %||% 1L,
      decimal = input$batch_decimal %||% ".", columns = selected_columns(),
      sex_map = maps("sex"), offence_map = maps("offence"), id = input$batch_id %||% "")
  })
  batch_prepared <- reactive({
    raw <- imported()$data; spec <- current_spec(); values <- mapping_values()
    if (is.null(raw)) return(list(data = NULL, error = "Please upload a valid file."))
    if (is.null(values) || any(lengths(values) > 100L)) return(list(data = NULL, error = "Please check the column and category mappings."))
    tryCatch({
      data <- prepare_ogrs_data(raw, spec$columns, spec$sex_map, spec$offence_map, spec$decimal)
      if (nzchar(spec$id)) {
        if (!spec$id %in% names(raw)) stop("Please select an existing ID column.")
        id <- as.character(raw[[spec$id]])
        if (anyNA(id) || any(!nzchar(trimws(id))) || anyDuplicated(id)) stop("Case IDs must be present and unique in every row.")
      }
      list(data = data, issues = validate_ogrs(data), error = NULL)
    }, error = function(e) list(data = NULL, error = conditionMessage(e)))
  })
  output$batch_validation_summary <- render_ui({
    x <- batch_prepared()
    if (is.null(x$data)) return(notice(tx(x$error)))
    bad <- length(unique(x$issues$row)); good <- nrow(x$data) - bad
    notice(tags$strong(paste(counted(good, "scoreable row", "scoreable rows"), "·", counted(bad, "unscoreable row", "unscoreable rows"))),
      if (bad) " Invalid rows are retained in the export with status and error explanations. Check the mappings and inputs." else " All data rows pass the input checks.",
      type = if (bad) "warning" else "success")
  })
  issue_table <- function(issues) {
    setNames(data.frame(Row = issues$row, Item = unname(item_labels(language())[issues$field]), Issue = tx(issues$message)), tx(c("Row", "Item", "Issue")))
  }
  output$batch_issues <- renderTable({
    x <- batch_prepared(); req(x$data)
    if (!nrow(x$issues)) return(NULL)
    head(issue_table(x$issues), 30L)
  }, rownames = FALSE)
  batch_saved <- reactiveVal(NULL)
  observeEvent(input$batch_calculate, {
    x <- batch_prepared()
    if (is.null(x$data)) return(showNotification(tx(x$error), type = "warning"))
    if (length(unique(x$issues$row)) == nrow(x$data)) {
      return(showNotification(tx("No rows can be scored. Please review the error explanations."), type = "warning"))
    }
    spec <- current_spec(); raw <- imported()$data
    scores <- ogrs(Data = x$data, strict = FALSE)
    time <- Sys.time()
    export <- make_ogrs_export(x$data, scores, x$issues,
      ids = if (nzchar(spec$id)) raw[[spec$id]] else NULL, timestamp = time, filename = input$batch_file$name)
    batch_saved(list(spec = spec, data = x$data, scores = scores, issues = x$issues, time = time, export = export))
  })
  batch_ready <- reactive(same_batch_spec(if (is.null(batch_saved())) NULL else batch_saved()$spec, current_spec()))
  output$batch_status <- render_ui({
    saved <- batch_saved()
    if (is.null(saved)) return(notice("No calculation yet. Check the file and mappings, then start the calculation."))
    if (!batch_ready()) return(notice(tags$strong("Results out of date. "),
      "The file or import settings have changed. Calculate again; downloading is disabled until then.", type = "warning"))
    notice(paste0(tx("Calculated: "), saved$spec$file$name, " · ", format_local_time(saved$time, language())), type = "success")
  })
  output$batch_summary <- render_ui({
    req(batch_ready()); x <- batch_saved()
    good <- sum(x$export$Status == "calculated")
    tagList(p(tags$strong(paste(good, tx("of"), counted(nrow(x$data), "row calculated.", "rows calculated.")))),
      div(class = "table-scroll", tableOutput("batch_result_preview")))
  })
  output$batch_result_preview <- renderTable({
    req(batch_ready()); x <- batch_saved()$export
    setNames(data.frame(Row = head(x$Row, 6), Status = tx(ifelse(head(x$Status, 6) == "calculated", "Calculated", "Not calculable")),
      One_year = probabilities(head(x$Prob_one_year, 6)),
      Two_years = probabilities(head(x$Prob_two_years, 6))), tx(c("Row", "Status", "One year", "Two years")))
  }, rownames = FALSE)
  output$batch_download_control <- render_ui({
    if (batch_ready()) downloadButton("batch_download", "Download results", class = "btn-primary")
    else tags$button("Download available after an up-to-date calculation", class = "btn btn-secondary", disabled = "disabled")
  })
  output$batch_download <- downloadHandler(filename = function() {
    req(batch_ready())
    ogrs_export_filename("Results", batch_saved()$time)
  }, content = function(file) {
    req(batch_ready())
    write.csv(batch_saved()$export, file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  })
  output$example_download <- downloadHandler(filename = function() ogrs_export_filename("Example_data", Sys.time()), content = function(file) {
    file.copy(file.path(.app_root, "examples/example-data.csv"), file, overwrite = TRUE)
  })
  output$batch_plot <- renderPlot({
    validate(need(batch_ready(), tx("The distribution appears after an up-to-date calculation.")))
    key <- input$batch_outcome %||% "Prob_one_year"
    req(key %in% c("Prob_one_year", "Prob_two_years", "Copas_rate"))
    values <- batch_saved()$scores[[key]]; values <- values[is.finite(values)]
    validate(need(length(values) > 0, tx("No scoreable values are available.")))
    data <- data.frame(Value = values)
    plot <- ggplot(data, aes(x = Value)) + geom_histogram(aes(y = after_stat(density)),
      bins = min(30L, max(1L, ceiling(sqrt(length(values))))), fill = "#d3d3d3", colour = "#60666c") +
      labs(x = tx(if (key == "Copas_rate") "Copas rate" else "Estimated general reoffending"), y = tx("Density"),
        subtitle = paste(tx("Scoreable cases:"), length(values))) + theme_minimal(base_size = 13)
    if (length(unique(values)) >= 2L) plot <- plot + geom_density(colour = "#b22222", linewidth = 0.8,
      bounds = if (key == "Copas_rate") c(-Inf, Inf) else c(0, 1))
    if (key != "Copas_rate") plot <- plot + scale_x_continuous(labels = probabilities) + coord_cartesian(xlim = c(0, 1))
    plot
  }, alt = reactive(tx("Distribution of scoreable OGRS 3 values. The subtitle gives the number of valid cases.")))

  observeEvent(input$explore_axis, {
    count <- identical(input$explore_axis, "Item4")
    updateSliderInput(session, "explore_range", min = if (count) 0 else 10,
      max = if (count) 100 else 120, value = if (count) c(0, 50) else c(14, 90), step = 1)
  })
  explored <- reactive({
    tryCatch(make_explore_data(input$explore_axis %||% "Item4", input$explore_range %||% c(0, 50),
      input$explore_sexes %||% character(), input$explore_offences %||% character(),
      list(Item2 = input$explore_age_last %||% NA_real_, Item3 = input$explore_age_risk %||% NA_real_,
        Item4 = input$explore_previous %||% NA_real_, Item5 = input$explore_age_first %||% NA_real_)),
      error = function(e) list(error = conditionMessage(e)))
  })
  output$explore_status <- render_ui({
    x <- explored()
    if (!is.null(x$error)) return(notice(tx(x$error), type = "warning"))
    if (!nrow(x$data)) return(notice("No valid combinations. Adjust the ages, conviction count or axis range.", type = "warning"))
    notice(paste(counted(nrow(x$data), "valid model combination", "valid model combinations"), "·", counted(x$excluded, "invalid combination excluded.", "invalid combinations excluded.")))
  })
  output$explore_plot <- renderPlot({
    x <- explored()
    validate(need(is.null(x$error), tx(x$error %||% "Please check the inputs.")))
    validate(need(nrow(x$data) > 0, tx("No valid model combinations.")))
    data <- x$data; axis <- input$explore_axis %||% "Item4"; key <- input$explore_outcome %||% "Prob_one_year"
    req(key %in% c("Prob_one_year", "Prob_two_years", "Copas_rate"))
    data$Sex <- tx(ifelse(data$Item1 == "m", "Male", "Female"))
    data$Offence <- factor(data$Item6, levels = ogrs_offences$code, labels = offence_labels(language()))
    data$Group <- interaction(data$Item1, data$Item6, drop = TRUE)
    line_data <- data[ave(seq_len(nrow(data)), data$Group, FUN = length) > 1L, , drop = FALSE]
    plot <- ggplot(data, aes(x = .data[[axis]], y = .data[[key]], colour = Offence, group = Group)) +
      geom_point(size = 1.4) + labs(x = item_labels(language())[[axis]],
        y = tx(switch(key, Prob_one_year = "General reoffending · one year", Prob_two_years = "General reoffending · two years", Copas_rate = "Copas rate")), colour = NULL) +
      scale_colour_brewer(palette = "Dark2", drop = TRUE) + theme_minimal(base_size = 13) +
      theme(legend.position = "bottom", legend.text = element_text(size = 10)) + guides(colour = guide_legend(ncol = 1))
    if (nrow(line_data)) {
      if (axis == "Item3") plot <- plot + geom_step(data = line_data, linewidth = 0.7, direction = "hv")
      else plot <- plot + geom_line(data = line_data, linewidth = 0.7)
    }
    if (key != "Copas_rate") plot <- plot + scale_y_continuous(labels = probabilities, limits = c(0, 1))
    if (length(unique(data$Sex)) > 1L) plot <- plot + facet_wrap(vars(Sex), ncol = 1)
    plot
  }, alt = reactive(tx("OGRS 3 model estimates across the selected axis range. Invalid combinations are excluded.")))

  lookup <- read.csv(file.path(.app_root, "data/offence-mapping.csv"), stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  output$offence_lookup <- renderTable({
    query <- trimws(tolower(input$offence_search %||% ""))
    english_categories <- english_manual_category(lookup$Manual_category)
    matches <- if (!nzchar(query)) seq_len(min(5L, nrow(lookup))) else
      which(grepl(query, tolower(paste(lookup$Provision, lookup$Manual_category, english_categories)), fixed = TRUE))
    if (!length(matches)) return(setNames(data.frame(Issue = tx("No match. Please consult the full coding reference in the manual.")), tx("Issue")))
    matches <- head(matches, 20L)
    if (language() == "de") return(setNames(lookup[matches, , drop = FALSE], c("Vorschrift", "Manualkategorie")))
    data.frame(`Statutory provision` = lookup$Provision[matches], Category = english_categories[matches],
      `Manual category (German)` = lookup$Manual_category[matches], check.names = FALSE)
  }, rownames = FALSE, colnames = TRUE)
}

# Local null fallback, independent of internal Shiny operators.
`%||%` <- function(x, y) if (is.null(x)) y else x
shinyApp(ui = localize_ui(ui), server = server)
