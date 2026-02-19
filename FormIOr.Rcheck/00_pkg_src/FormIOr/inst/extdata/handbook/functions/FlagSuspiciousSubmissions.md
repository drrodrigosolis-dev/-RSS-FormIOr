# FlagSuspiciousSubmissions

**Title:** Flag suspicious submissions and check pre/post comparability

## What this function is for

Use this when your survey changed during collection (for example, anonymous at
first, then identity-verified later) and you need to know whether pre and post
responses can be analyzed together.

## Overview

`FlagSuspiciousSubmissions()` performs two jobs:

1. Runs a formal pre/post comparability analysis across analyzable fields.
2. Produces row-level suspiciousness flags (duplicate IDs/fingerprints and rapid repeats).

By default, it returns split datasets (`pre`, `post`, `full`) and does not force
omission. You can choose to omit one phase using `group_action`.

## Usage

```r
FlagSuspiciousSubmissions(
  x,
  id_col = 1,
  time_col = NULL,
  cutoff_time,
  include_cols = NULL,
  group_action = c("split_only", "omit_pre", "omit_post", "none"),
  comparability_rule = c("fdr_effect"),
  alpha = 0.05,
  fdr_method = "BH",
  min_effect_numeric = 0.2,
  min_effect_categorical = 0.1,
  min_flagged_field_share = 0.2,
  min_complete_rate = 0.6,
  risk_weights = NULL,
  risk_threshold = 0.7,
  action = c("flag_only", "exclude_high_risk", "exclude_confirmed_duplicates"),
  return_flat = FALSE,
  quiet = FALSE
)
```

## Statistical comparability rule

Per field:
- Numeric: Wilcoxon + absolute SMD
- Categorical: Chi-square (or Fisher fallback) + Cramer's V

A field is marked different when:
- adjusted p-value <= `alpha`, and
- effect size >= threshold for that field type.

Global non-comparability is flagged when:
- at least 3 fields were tested, and
- the share of flagged fields >= `min_flagged_field_share`.

## When groups differ

If groups are non-comparable, pooled inference should not be trusted.
Recommended default is to omit pre-change anonymous data (`group_action = "omit_pre"`) if post-change data is identity-verified.

## Outputs

- `data`: primary dataset after group and row actions
- `datasets`: `pre`, `post`, `full`
- `comparability`: global metrics + `field_tests`
- `decision_sentence`: one concise sentence with the key decision outcome
- `flags`: row-level suspiciousness indicators and risk score
- `summary`, `diagnostics`, optional `flat`

## Try with bundled demo data

FormIOr includes `SuspiciousSurveyDemo`, a synthetic dataset built to exercise
this function's comparability and duplicate-detection logic.

```r
data("SuspiciousSurveyDemo", package = "FormIOr")

out <- FlagSuspiciousSubmissions(
  SuspiciousSurveyDemo,
  id_col = "submissionId",
  time_col = "created",
  cutoff_time = "2026-01-06 00:00:00",
  group_action = "split_only"
)

out$comparability$non_comparable
head(out$comparability$field_tests)
out$decision_sentence
```

---

Navigation
- Home: [Handbook Index](../index.md)
- Function Index: [All Functions](index.md)
