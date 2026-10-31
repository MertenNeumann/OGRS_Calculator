# Change history

## 2.1.2 · Unreleased · 31 October 2026

- Replace the short introductions in both languages with instructions for each
  tab's inputs, calculation, export and reference functions.
- Use “Berechnung für Datensätze” instead of “Stapelverarbeitung”.
- Explain graphical exploration and direct users to Single case or Dataset for
  calculations on specific cases.
- Explain that six static factors estimate statistical reoffending risk and
  should be considered alongside other information in an individual assessment.
- Fix translated selection prompts appearing as clipped text beside already
  selected columns and categories. Empty controls retain translated prompts.
- Wrap long selected labels without overlapping the dropdown arrow.

## 2.1.1 · Unreleased · 31 October 2026

- Restore the original grey/white palette and blue accents, with grey histogram
  bars and a red density line. Add space between the brand subtitle and tabs.
- Keep the explicit violent/sexual reoffending limitation in Methods; other
  views refer to general reoffending without repeating that warning.
- Show CSV encoding, separator, decimal mark and probability guidance beside
  single-case and dataset downloads.
- Include UTC calculation timestamps in result filenames and download timestamps
  in example filenames. Language changes preserve result filenames and contents.
- Translate Explore as “Ausprobieren”. Separate Methods from About the app,
  with factual headings, calculation details, contact and data-processing notes.
- Restore the VRAG-R calculator link and invite feature requests and suggestions
  for additions through GitHub Issues.

## 2.1.0 · Unreleased · 31 October 2026

### Language

- English is the default interface language, with a Language / Sprache dropdown
  for German. German terminology follows the German manual.
- Language changes preserve inputs, custom mappings, calculation timestamps,
  results and download validity. Each session has an independent language.
- Localised headings, controls, validation messages, summaries, plot labels,
  alternative text and numeric display. Uploaded values and IDs are not translated.
- English source comments, R API messages, documentation, examples and CI labels.
- Stable English CSV headers/status codes and comma-separated, decimal-point
  exports in both interface languages. The interim German export schema is retired.
- No changes to the model, coefficients, validation rules or MIT license.

### Modernisation retained from the preceding working revision

- Separate responsive views for single cases, datasets, Explore and guidance.
- A shared dependency-free vectorised scoring core using a stable logistic
  function, with original coefficients unchanged.
- Shared checks for completeness, categories, whole numbers, age bounds and
  chronology; a defined result shape for empty data.
- File preview, explicit column/category mappings and row-level validation.
  Invalid rows remain identifiable in exports.
- Stale results and downloads blocked after scoring inputs change.
- Export row numbers, optional pseudonymous IDs, status, errors and version/time
  metadata.
- Explore supports both horizons and the Copas rate, excludes invalid combinations
  and explains empty selections. Age-band effects are drawn as steps.
- Concise item/measure explanations and limitations, linked coding guidance,
  a historical searchable statutory table and factual data-processing information.
- Documented age-20 discrepancy in the official German Excel tool.
- R 4.6.2, current CRAN packages, renv lockfile, reference/regression/browser
  tests and GitHub Actions.
- MIT license added with the author's approval. No automatic publication.

### R API migration from the original calculator

Datasets require named columns `Item1` through `Item6`. Single-case items must
be supplied explicitly. `Data = NULL` replaces `Data = NA`; the former
app-internal `Shiny_Progress` argument is removed. Invalid inputs stop by default;
`strict = FALSE` retains them as `NA` with validation explanations.
