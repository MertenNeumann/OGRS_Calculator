# Model, coding and reference checks

Technical revision: 31 October 2026. Model: OGRS 3. This revision does not
re-estimate or recalibrate coefficients, or upgrade the instrument to OGRS 4.
English and German interfaces share the same German coding and calculations.

## Authoritative sources

- Breiling, L., Dahle, K.-P., Oberlader, V. & Rettenberger, M. (2022).
  *Die deutsche Version der Offender Group Reconviction Scale, Version 3
  (OGRS 3).* BM-Online, volume 30. Kriminologische Zentralstelle.
  [Publication and Excel tool](https://krimpub.krimz.de/frontdoor/index/index/docId/223),
  [German manual](https://krimpub.krimz.de/files/223/bm-online30.pdf),
  [Persistent identifier](https://nbn-resolving.org/urn:nbn:de:hebis:2378-opus-2238).
- Howard, P., Francis, B., Soothill, K. & Humphreys, L. (2009).
  *OGRS 3: The revised Offender Group Reconviction Scale.* Ministry of Justice,
  Research Summary 7/09.
  [Full text](https://www.researchgate.net/profile/Brian-Francis-4/publication/258365273_OGRS3_the_revised_Offender_Group_Reconviction_Score/links/00b4952a8f828c47ca000000/OGRS3-the-revised-Offender-Group-Reconviction-Score.pdf).

German item names and offence labels follow chapter 5.1 of the German manual
(pp. 14–16). The app includes key explanations and links to the complete coding
rules. English offence labels follow the original British source. They do not
supersede the German statutory mappings or define a separate British scoring
mode. The order of the two sexual-offence categories differs between source
lists; mappings therefore use stable codes and meanings, never just positions.

`data/offence-mapping.csv` contains factual statutory mappings from the official
Excel tool's offence-category sheet, dated 13 January 2022. Full statutory titles
and manual text are not bundled. Original German category wording is retained
as source data and displayed alongside English reference labels. Ambiguity is
not resolved by automatically choosing a category. The table is incomplete,
contains historical provisions and does not independently incorporate later
legislative changes. Consult the manual or seek substantive clarification when
necessary.

## Scoring model

For the German coding:

```
Copas = ln((1 + number of previous convictions) /
           (10 + age at current conviction - age at first conviction))
z = 1.251124464 * Copas + criminal history + age/sex + principal offence
p1 = plogis(1.402562384 + z)
p2 = plogis(2.121705678 + z)
```

Item 4 excludes the current conviction; the `1 +` in the formula accounts for it.
The history coefficient is 0.126142106 with no previous convictions and
0.463062792 otherwise. The 22 age/sex and 20 offence coefficients match the
original appendix (pp. 5–6). `plogis()` is mathematically equivalent to
`exp(z)/(1+exp(z))` while being numerically stable.

## Documented discrepancies in the official Excel tool

The published 2022 Excel tool is not a completely consistent numerical reference:

1. Cell F12 of sheet “Kategorien” assigns age 20 to “20-24”. The coefficient
   sheet likewise uses “18 oder 19” and “20-24”. Howard et al. instead specify
   **18–20 and 21–24**. The app follows the original report. A male case with
   ages 20/20/20, no previous convictions and Violence yields 12.1615535% and
   22.1305211%. The Excel formulas yield approximately 8.2896306% and 15.6502262%.
2. Cell E22 of “SCORING” multiplies the Copas rate by 1.251124 rather than the
   1.251124464 listed in the coefficient table. The app uses the full original
   coefficient; this rounding discrepancy is very small.

These differences are documented rather than resolved by changing the model to
match the spreadsheet. No message has been sent to the source authors.

## Interpretation

The German version concerns general reoffending resulting in a further final
conviction. The British original also includes cautions, reprimands and warnings
in both criminal history and outcome. German coding is not a regression
re-estimated using German data.

Both probabilities are cumulative from the start of the at-risk period. They
are not predictions of specifically violent or sexual reoffending or of the
severity of a future offence. Unrecorded offences, dynamic changes and treatment
progress are not captured. OGRS 3 complements an individual assessment. Good AUC
values alone do not demonstrate calibration of absolute probabilities. Chapter 6
of the German manual (pp. 19–20) expressly notes the absence of recalibration
using German samples.

The interface rounds probabilities to one decimal place using the selected
language's decimal mark. Exports retain unrounded probabilities in [0, 1] and a
stable English schema. The Copas rate is a logarithmic measure, not a percentage.
