# English is the source language. German terminology follows the German manual.
# Translation is session-local and never changes model codes or uploaded values.
# Shiny may automatically source R/ files before app.R has set up its globals.
.translation_catalog <- unlist(jsonlite::fromJSON(
  file.path("R", "translations.json")), use.names = TRUE)

translate <- function(text, language = "en") {
  if (!identical(language, "de")) return(text)
  translated <- unname(.translation_catalog[match(text, names(.translation_catalog))])
  translated[is.na(translated)] <- text[is.na(translated)]
  translated
}

item_labels <- function(language = "en") {
  if (identical(language, "de")) ogrs_items_de else ogrs_items
}
offence_labels <- function(language = "en") {
  if (identical(language, "de")) ogrs_offences$german else ogrs_offences$label
}

# Add translation markers only to application-authored text. Raw preview tables,
# filenames, observed categories, IDs and column names are never traversed.
localize_ui <- function(node, language = "en") {
  if (inherits(node, "html")) return(node)
  if (is.character(node)) {
    if (length(node) == 1L && !is.na(node) && node %in% names(.translation_catalog)) {
      return(htmltools::span(`data-i18n` = node, translate(node, language)))
    }
    return(node)
  }
  if (inherits(node, "shiny.tag")) {
    if (!is.null(node$attribs[["data-i18n-raw"]]) || !is.null(node$attribs[["data-i18n"]])) return(node)
    if (node$name %in% c("script", "style", "option", "textarea", "code", "pre")) return(node)
    for (attribute in c("placeholder", "title", "aria-label")) {
      text <- node$attribs[[attribute]]
      if (is.character(text) && length(text) == 1L && text %in% names(.translation_catalog)) {
        node$attribs[[paste0("data-i18n-", attribute)]] <- text
        node$attribs[[attribute]] <- translate(text, language)
      }
    }
    node$children <- lapply(node$children, localize_ui, language = language)
    return(node)
  }
  if (is.list(node)) node[] <- lapply(node, localize_ui, language = language)
  node
}

# Historical source categories retain their German wording alongside the
# English display label. This reference never assigns an offence automatically.
english_manual_category <- function(x) {
  aliases <- c("Betrug" = "Fraud", "Diebstahl" = "Theft (non-motor)",
    "Drogenhandel/-produktion (Drogenbesitz)" = "Drug trafficking/production (possession)",
    "Flucht/Weisungsverstoß" = "Absconding or bail offences",
    "Prostitution/Zuhälterei" = "Soliciting or prostitution",
    "Sexualdelikt (Kind)" = "Sexual (against child)",
    "Sexualdelikt (nicht Kind)" = "Sexual (not against child)",
    "Verkehrsdelikt" = "Other motoring", "sonstige Delikte" = "Other offence",
    "Öffentliche Ordnung" = "Public order")
  labels <- setNames(ogrs_offences$label, ogrs_offences$german)
  labels[names(aliases)] <- aliases
  result <- unname(labels[match(x, names(labels))])
  result[is.na(result)] <- x[is.na(result)]
  result
}

# Select option markup is generated as HTML by Shiny. Attach the source labels
# explicitly instead of parsing that HTML or touching arbitrary user values.
.attach_choice_catalog <- function(control, input_id, choices) {
  labels <- names(choices)
  if (is.null(labels)) labels <- as.character(choices)
  labels[is.na(labels) | labels == ""] <- as.character(choices)[is.na(labels) | labels == ""]
  authored <- !grepl("^column_[1-6]$", input_id) && !input_id %in% c("batch_id", "language")
  indexes <- which(labels %in% names(.translation_catalog) & (authored | as.character(choices) == ""))
  if (!length(indexes)) return(control)
  options <- lapply(indexes, function(i) list(value = as.character(choices[[i]]), text = labels[[i]]))
  htmltools::tagQuery(control)$find("select")$addAttrs(
    `data-i18n-options` = jsonlite::toJSON(options, auto_unbox = TRUE))$allTags()
}
i18n_select_input <- function(inputId, label, choices, ...) {
  .attach_choice_catalog(shiny::selectInput(inputId, label, choices, ...), inputId, choices)
}
i18n_selectize_input <- function(inputId, label, choices, ...) {
  .attach_choice_catalog(shiny::selectizeInput(inputId, label, choices, ...), inputId, choices)
}
