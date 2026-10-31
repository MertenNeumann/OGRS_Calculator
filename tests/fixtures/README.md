# Independent scoring reference

`howard-reference.csv` contains 1,840 synthetic cases with precomputed expected
values: two sex categories, 23 ages covering both sides of every boundary,
20 offence categories, and zero or two previous convictions.

The reference was calculated independently of the R implementation using Python's
`math.log` and logistic function, with coefficients extracted from the appendix
of Howard et al. (2009), pp. 5–6. There are no real case records. Do not regenerate
expectations from the `ogrs()` function under test.

In particular, age 20 belongs to the original 18–20 band, not the different
band in the German Excel tool dated 2022. See `docs/MODEL_AND_SOURCES.md`.

`cases.xlsx` is a synthetic three-row workbook used to exercise Excel import
and category mapping. Mixed English and German labels in other tests deliberately
exercise multilingual input values and SPSS value-label suggestions.
