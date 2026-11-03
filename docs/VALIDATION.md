# Software validation

The test suites check scoring consistency and application behavior. They are
not a clinical validation study, certification or evidence of calibration in a
new population. Coding and interpretation require the published instrument
manuals; see [Model and sources](MODEL_AND_SOURCES.md).

## Independent references

`tests/fixtures/howard-reference.csv` contains 1,840 synthetic cases combining
sex, age boundaries, offence categories and criminal-history conditions.
Expected probabilities and Copas rates were calculated separately from the R
implementation using the original Howard et al. (2009) coefficients.
The age-20 discrepancy in the German Excel tool is documented in the model guide.

Fixture expectations are independent of the scoring implementation. See the
[fixture notes](../tests/fixtures/README.md) for provenance. Tests and examples
contain no user case records.

## Coverage

The R suite checks age bands, all offence categories, chronology, invalid and
empty data, structured validation errors, imports, labelled SPSS values, IDs,
export status, calculation freshness, translations and Explore. Large invalid
batches exercise error aggregation as well as scoring.

The R runner reports the tests and assertions executed and fails on failures,
warnings, skipped checks or an empty run. Browser tests exercise real Shiny
sessions, calculations, mappings, uploads, downloads, language switching and
responsive layouts. Inspect the current test output and CI results for the
revision being evaluated; historical results do not establish that a later
revision passes.

See [Contributing](../CONTRIBUTING.md) for reproducible commands, browser
requirements and artifact locations. Test dependencies are recorded in
`renv.lock` and `tests/requirements-browser.txt`.

## Limits

Passing tests establish the behavior exercised by those checks. They do not
verify every possible input, assess the quality of professional item coding,
validate local risk calibration or establish that a hosting environment is
suitable for sensitive data. A hosted release needs its own functional checks
and deployment-specific operating/privacy information.
