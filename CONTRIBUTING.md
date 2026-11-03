# Contributing

## Development environment

Use R 4.6.2 and run these commands from the repository root:

```sh
Rscript --vanilla scripts/setup.R
Rscript tests/run.R
```

The setup script restores the versions in `renv.lock` into the project library.
It preserves configured proxy and certificate settings. Keep TLS and package
verification enabled. `.Rprofile` activates the restored library for subsequent
R sessions in this directory.

Linux source installations require a C/C++ toolchain and system libraries.
On Debian/Ubuntu, typical prerequisites are `build-essential`, `gfortran`,
`cmake`, `pkg-config`, `zlib1g-dev`, `libuv1-dev` and `libx11-dev`. System
requirements depend on the R installation and distribution.

## Browser tests

Playwright exercises the actual Shiny interface, including uploads, downloads,
translations and responsive layouts. It is test tooling; the application does
not require Python, Playwright or Chromium for ordinary use.

In a separate Python environment (the CI workflow uses Python 3.12), install the
requirements from the repository root. The following commands use a POSIX shell:

```sh
python3 -m venv .venv
. .venv/bin/activate
python -m pip install -r tests/requirements-browser.txt
python -m playwright install chromium
```

On Linux, install Chromium's system dependencies if needed with
`python -m playwright install-deps chromium`; this may require administrator
privileges. `CHROMIUM_PATH` can select an already installed compatible browser.

The OGRS browser suite starts and stops its own local Shiny server:

```sh
python tests/browser_smoke.py
```

`OGRS_RSCRIPT` selects the Rscript executable. Set `OGRS_SCREENSHOTS` to an
output directory to retain screenshots; other downloads and server logs use a
temporary test directory. The directory `.venv/` is ignored by Git.

Use synthetic data for tests, screenshots, issue reports and debugging. Tests
and examples contain no user case records. GitHub Actions runs the R and
browser suites on pushes and pull requests. Passing tests demonstrate software
behavior, not clinical validity; see [Validation](docs/VALIDATION.md).

## Code and translations

`OGRS_Calculator.R` contains the base-R scoring and validation core.
`R/workflow.R` handles imports, mappings and exports; `app.R` manages the
Shiny interface and session state.

English is the source language in `R/translations.json`. `R/i18n.R` marks
application-authored text, and `www/i18n.js` updates labels without replacing
input widgets. Language and plot presentation must not change scoring inputs,
results, calculation timestamps or CSV contents. Never translate raw uploaded
values, column names, IDs or filenames.

Prefer a focused regression test for a bug fix. Independent fixture expectations
must come from the published coding rules or a separate calculation, not the
implementation under test. Document source ambiguities and numerical conventions
in the model/source documents rather than silently changing scoring rules.

## Dependencies and releases

Declare application dependencies in `DESCRIPTION`. For a deliberate dependency
update, restore and test the new versions, then run in an activated project:

```r
renv::snapshot(type = "explicit", dev = TRUE)
```

Keep `snapshot.dev` enabled in `renv/settings.json` so test dependencies remain
in the lockfile. Keep the supported R version consistent across `.R-version`,
`DESCRIPTION`, `renv.lock`, scripts and CI. Keep `OGRS_APP_VERSION` and the `Version` field in `DESCRIPTION` in sync,
and record user-visible changes in `CHANGELOG.md`.

Before a release, run both suites and review model/source decisions, exports
and relevant browser screenshots. A hosted release also requires the operating
and privacy information described in [Data processing and deployment](docs/DATA_AND_DEPLOYMENT.md).
