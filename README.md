# OGRS 3 · Calculator

A Shiny application for the **Offender Group Reconviction Scale, version 3**.
It estimates general reoffending within one or two years using the German
coding by Breiling et al. (2022) and the original coefficients from Howard
et al. (2009).

**English is the default language.** The **Language / Sprache** dropdown switches
the interface to German and back without clearing inputs, mappings or current
results. German item names and terminology follow the
[German manual](https://krimpub.krimz.de/files/223/bm-online30.pdf).

The language selector changes presentation only. Both languages use the same
convictions-only German coding, not different British and German models.
OGRS 3 complements an individual risk assessment; it is not a specific prediction
of violent or sexual reoffending. The manual remains the full coding reference.

## Installation and startup

Tested toolchain, 31 October 2026: **R 4.6.2**, Shiny 1.14.0, bslib 0.12.0,
ggplot2 4.0.3, haven 2.5.5, readxl 1.6.0 and readr 2.2.0. Exact versions of all
75 dependencies, including the development tools, are recorded in `renv.lock`.
`.R-version` pins the R version.

1. Install R 4.6.2, for example from [CRAN](https://cran.r-project.org/).
2. Restore the locked packages from the repository root:

   ```sh
   Rscript --vanilla scripts/setup.R
   ```

3. Start the application:

   ```sh
   Rscript scripts/run.R
   ```

Alternatively, from an R session in the repository root:

```r
shiny::runApp(".", host = "127.0.0.1", port = 3838, launch.browser = TRUE)
```

`.Rprofile` activates the project library. Installation requires access to CRAN
and, when compiling from source on Linux, compilers and development libraries.
On Debian/Ubuntu these include `build-essential`, `gfortran`, `cmake`,
`pkg-config`, `zlib1g-dev`, `libuv1-dev` and `libx11-dev`. The application itself
does not require an external database or API credentials.

`scripts/run.R` accepts `OGRS_HOST` and `OGRS_PORT`; it defaults to a local
binding at 127.0.0.1:3838. A server deployment needs its own network, HTTPS
and access-control configuration.

## Interface and language selection

- **Single case:** Complete the six items or explicitly load an example. Results
  only appear after a valid calculation. Editing inputs hides previous results
  and disables their export until recalculation.
- **Dataset:** Upload a file, inspect the preview, map columns and categories,
  review validation issues, then calculate. Changes to the file, worksheet,
  import settings, mappings or case ID invalidate previous results and downloads.
- **Explore:** Compare whole-number ranges across up to five offence groups.
  Invalid combinations are excluded and counted. Choose the one-year estimate,
  two-year estimate or Copas rate.
- **Methods:** Instrument, calculation, interpretation, references, limitations
  and historical statutory mappings.
- **About the app:** Source code, contact, feedback and feature requests, the
  related VRAG-R calculator and data-processing information.

In German, these tabs are labelled **Einzelfall**, **Datensatz**, **Ausprobieren**,
**Methodische Hinweise** and **Über die App**.

The language setting is local to each Shiny session. It translates headings,
controls, choices, validation messages, result summaries, plot labels and
accessibility text. It does not translate uploaded values, IDs, column names or
filenames, or alter their mappings. A new session starts in English.

Displayed probabilities use one decimal place, with a decimal point in English
and a decimal comma in German. The two-year probability covers both years
cumulatively, not just the second year. No arbitrary risk bands are added.

**Exports always use a stable English schema, UTF-8, comma-separated fields and
a decimal point**, regardless of the interface language. Values are unrounded;
probabilities are between 0 and 1. This avoids changing the meaning or structure
of an export when the display language changes.

Single-case and dataset result filenames include the calculation timestamp in
UTC, matching `Calculated_at_UTC` inside the file, for example
`OGRS3_Single_case_2026-10-31_14-30-00_UTC.csv`. Repeated downloads of the same
calculation keep the same name in either interface language. Example dataset
downloads use the download time. CSV-format guidance appears next to both
result download controls.

## Input data and coding

A synthetic example is available at
[`examples/example-data.csv`](examples/example-data.csv) and through the app.
Supported formats are XLSX, comma- or semicolon-separated CSV, tab-separated TXT
and SPSS SAV. The first row must contain unique column names. Limits are 10 MiB
and 100,000 data rows per file; invalid data rows are not silently discarded.

| R column | Item | Format |
|---|---|---|
| `Item1` | Sex | Model code `m` or `f` |
| `Item2` | Age at the current conviction | Completed years |
| `Item3` | Age at the start of the at-risk period | Completed years |
| `Item4` | Number of previous convictions | Nonnegative whole number, excluding the current conviction |
| `Item5` | Age at the first conviction | Completed years |
| `Item6` | Principal current offence | Stable code from `ogrs_offences$code` |

The interface supports arbitrary column names and explicit category mappings.
German manual labels, original English labels and model codes provide visible
suggestions in either interface language. Unknown categories remain unmapped.
SPSS value labels may suggest a mapping; the stored values remain the keys.

Missing values, nonnumeric inputs, fractional counts and inconsistent chronology
produce row-level validation issues. The rules include `Item5 <= Item2 <= Item3`
and, when `Item4 == 0`, `Item5 == Item2`. Equal ages with multiple convictions
are allowed because ages are recorded in completed years.

The software accepts ages from 10 to 120: 10 is the lower bound of the original
age bands, while 120 is a software plausibility cap. **These bounds are not an
empirically validated application range** and do not replace the manual's
substantive requirements.

Use the German manual to code the principal offence. English category labels
follow the British source; they are not a standalone translation of German
criminal law. For example, the German category labels and statutory mappings
must not be replaced with a literal interpretation of an English label.
The historical statutory table is not used for automatic coding, is incomplete
and does not reflect legislative changes after 2022. The English reference view
also retains the original German category wording for inspection.

## Export schema

Each dataset export retains the original row order and includes:

- `Row` (data row, excluding the header) and optional `Case_ID`;
- `Status`: `calculated` or `not_calculable`, and `Error`;
- `Prob_one_year`, `Prob_two_years`, `Copas_rate`;
- `App_version`, `Model_version`, `Source_file` and `Calculated_at_UTC`.

Invalid rows have empty result fields and English error explanations. A selected
case ID must be present and unique in every row. CSV import reads IDs as text,
preserving leading zeroes. Spreadsheet software may still infer numeric IDs when
opening a CSV; import the ID column explicitly as text when necessary.

The interface preview shows only the first rows and issues; the export includes
all row-level error explanations. If no rows can be scored, calculation remains
blocked. Only a suitable pseudonymous ID is needed; names and case reference
numbers are not required.

## Standalone R API

The scoring core has no package dependencies. Its comments, labels and error
messages are English by default; German terminology is available as data in
`ogrs_items_de` and `ogrs_offences$german`.

```r
source("OGRS_Calculator.R")
ogrs(Item1 = "m", Item2 = 20, Item3 = 20, Item4 = 0,
     Item5 = 20, Item6 = "Violence")
# Prob_one_year = 0.1216155350; Prob_two_years = 0.2213052114
# Copas_rate = -2.302585093

issues <- validate_ogrs(my_data)
scores <- ogrs(Data = my_data, strict = FALSE)
attr(scores, "validation")
```

`Data` requires named columns `Item1` through `Item6`, in any order. By default,
invalid inputs raise a structured `ogrs_validation_error`. With `strict = FALSE`,
invalid rows retain `NA` scores and the `validation` attribute explains why.
An empty, correctly structured data frame returns an empty result table.

**Migration from the original calculator:** Supply one explicit value per
categorical item. Dataset columns are identified by name rather than position.
`Data = NA` has been replaced with `Data = NULL`. The former app-internal
`Shiny_Progress` argument has been removed; the vectorised core is independent
of Shiny. The interim German export headers have been replaced by the stable
English schema above.

## Development and testing

```sh
Rscript tests/run.R
```

The suite checks 1,840 independently calculated reference cases, every age
boundary and offence category, validation, empty data, supported imports,
SPSS value labels, IDs, export status, stale-download guards and Explore.
Language tests verify German manual terminology, English defaults, session
isolation and unchanged scoring state when switching languages.

Optional browser checks:

```sh
python3 -m pip install -r tests/requirements-browser.txt
python3 -m playwright install chromium
python3 tests/browser_smoke.py
```

The browser test starts its own local Shiny server and uses synthetic data only.
`OGRS_RSCRIPT` selects an Rscript executable; `CHROMIUM_PATH` can select an
installed Chromium browser. Checks cover English and German, switching with
existing results and custom mappings, upload/export, invalidation, plots and
mobile layout. GitHub Actions runs the R and browser suites.

Translation strings live in `R/translations.json` with English source keys.
`R/i18n.R` marks application-authored text; `www/i18n.js` updates marked text
and select labels without replacing input widgets. Dynamic summaries and plots
use the session's language on the server. Mapping widgets are deliberately
independent of the language reactive, and language is excluded from calculation
specifications. Raw uploaded values must never be marked for translation.

Declare dependencies in `DESCRIPTION`, test changes, then run
`renv::snapshot(type = "explicit", dev = TRUE)`. Do not regenerate reference
expectations from the scoring implementation under test.

## Method, operation and licensing

- [Model, sources and documented Excel discrepancy](docs/MODEL_AND_SOURCES.md)
- [Data processing and deployment-specific requirements](docs/DATA_AND_DEPLOYMENT.md)
- [Change history](CHANGELOG.md)

The official German Excel tool from 2022 differs from the original report at
age 20. This app retains the original bands **18–20** and **21–24**. Switching
the display language does not change this source decision or recalibrate the
model. Absolute probabilities are not newly calibrated German norms.

The existing `rsconnect` target is retained. Setup and tests do not publish the
app. Before a live deployment, complete the privacy information for the actual
operator and hosting arrangements; the app does not invent contact details,
contracts or deletion guarantees.

Application code is under the [MIT license](LICENSE), Merten Neumann. This does
not grant rights to the cited publications or external Excel tool. Technical
feedback, feature requests and suggestions for additions are welcome via
[GitHub Issues](https://github.com/MertenNeumann/OGRS_Calculator/issues), without
personal information or original datasets. The separate
[VRAG-R calculator](https://mertenneumann.shinyapps.io/VRAGR_Calculator/) and its
[source code](https://github.com/MertenNeumann/VRAGR_Calculator) are linked from
the app's About tab.
