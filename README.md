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
OGRS 3 is not a specific prediction of violent or sexual reoffending.
The manual remains the full coding reference.

## Installation and startup

Use **R 4.6.2** and the package versions recorded in `renv.lock`.
`.R-version` records the supported R version. From the repository root, run:

```sh
Rscript --vanilla scripts/setup.R
Rscript scripts/run.R
```

The setup script restores the project library, including test dependencies.
`.Rprofile` activates that library when R starts in the repository root.
Installation requires access to CRAN. Linux source installations also require
compilers and development libraries; see [Contributing](CONTRIBUTING.md).
No external database or API credentials are required for local calculations.

The default local address is `127.0.0.1:3838`. Set `OGRS_PORT` to use another
port and `OGRS_HOST` to change the bind address. When running both calculators
at once, use different ports. See [Data processing and deployment](docs/DATA_AND_DEPLOYMENT.md)
for hosting requirements.

## Interface and language selection

- **Single case:** Complete the six items or explicitly load an example. Results
  only appear after a valid calculation.
- **Dataset:** Upload a file, inspect the preview, map columns and categories,
  review validation issues, then calculate.
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
controls, validation messages, results and plots. It does not translate uploaded
values, IDs, column names or filenames, or alter their mappings. A new session
starts in English.

Results and downloads require the current inputs and settings to match a saved
calculation. Edits hide results while they differ; reverting those edits can
restore access to the existing calculation. Replacing an upload requires
recalculation. Language changes do not affect this check.

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
blocked. Use only data that cannot be linked to an individual; replacing names with
case IDs does not by itself make data anonymous.

## Standalone R API

The scoring core uses base R. German terminology is available as data in
`ogrs_items_de` and `ogrs_offences$german`.

```r
source("OGRS_Calculator.R")
ogrs(Item1 = "m", Item2 = 20, Item3 = 20, Item4 = 0,
     Item5 = 20, Item6 = "Violence")
# Prob_one_year = 0.1216155350; Prob_two_years = 0.2213052114
# Copas_rate = -2.302585093

data <- data.frame(Item1 = "m", Item2 = 20, Item3 = 20, Item4 = 0,
                   Item5 = 20, Item6 = "Violence")
issues <- validate_ogrs(data)
scores <- ogrs(Data = data, strict = FALSE)
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
of Shiny. Exports use the stable English schema above.

## Simulated data

```r
source("OGRS_Calculator.R")
set.seed(123)
data <- sim_ogrs_data(100, prob_missing = 0)
scores <- ogrs(Data = data)
case <- sim_ogrs_case()
```

`sim_ogrs_data(N, prob_missing = 0.1)` returns
`N` rows in the scoring input schema. Complete cases use canonical sex/offence
codes and consistent ages; cases with no previous convictions have the same
age at first and current conviction. `sim_ogrs_case(prob_missing = 0, envir = NULL)`
returns one row without assigning global variables. Pass an environment explicitly
to assign the six item bindings there.

The dataset helper accepts nonnegative whole-number row counts, including zero.
Both helpers accept missingness probabilities between 0 and 1, applied independently
to each input field. Use `prob_missing = 0` for fully scoreable cases; otherwise
use the non-strict scoring mode to retain rows with insufficient information.
The functions use R's current random-number generator; call `set.seed()` externally
for reproducibility. Their illustrative distributions are not calibrated to a
real population and must not be used as independent scoring references.

## Development and testing

Run the R regression suite from the repository root:

```sh
Rscript tests/run.R
```

The suite covers independent scoring references, validation, imports, exports,
session state and translations. Browser tests exercise calculations, uploads,
downloads and responsive layouts in both languages. See
[Validation](docs/VALIDATION.md) for coverage and limitations, and
[Contributing](CONTRIBUTING.md) for test setup and development guidance.

## Method, operation and licensing

Calculations and uploads are processed **on the server**. Read
[Data processing and deployment](docs/DATA_AND_DEPLOYMENT.md) for temporary-file
handling, hosting requirements and deployment-specific privacy information.
Use only data that cannot be linked to an individual; removing names or using
row IDs does not by itself establish anonymity.

- [Model, sources and documented Excel discrepancy](docs/MODEL_AND_SOURCES.md)
- [Data processing and deployment-specific requirements](docs/DATA_AND_DEPLOYMENT.md)
- [Change history](CHANGELOG.md)

The official German Excel tool from 2022 differs from the original report at
age 20. This app retains the original bands **18–20** and **21–24**. Switching
the display language does not change this source decision or recalibrate the
model. Absolute probabilities are not newly calibrated German norms.

Application code is under the [MIT license](LICENSE), Merten Neumann.
The license does not grant rights to the cited manuals, instruments or other
published source material.

Report bugs, feature requests and suggestions through
[GitHub Issues](https://github.com/MertenNeumann/OGRS_Calculator/issues), using
synthetic examples rather than personal information or original case records.
The separate [VRAG-R calculator](https://mertenneumann.shinyapps.io/VRAGR_Calculator/)
and its [source code](https://github.com/MertenNeumann/VRAGR_Calculator) are also available.

[OpenAI Codex](https://github.com/codex) assisted with code, interface,
documentation and tests. Case calculations use the documented R code; no AI
model is used to score cases.
