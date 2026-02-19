
# FixDups

**Title:** Clean duplicated rows caused by multi-value fields in flattened FormIO data

## What this function is for

This function focuses on cleaning or standardizing the flattened dataset.

## Overview

Interactive function designed for users with little or no R experience.
It helps resolve columns that contain multiple values per submission
(e.g. repeating sections, "add another" items, multi-select questions).

## How it fits into a workflow

This function typically appears after you have created (or loaded) a
flattened dataset. If you are using the wizard, this step is handled for
you, but the function is also safe to call directly in scripts.

## Usage

```r
FixDups(
  x,
  multi_counts = NULL,
  id_col = 1,
  ask_threshold = 1.05,
  quiet = FALSE,
  dry_run = FALSE
)
```

## Key inputs

- `x`: List returned by `FlattenSubmissions()`, must contain verb{$FlatResponses}
- `multi_counts`: Optional. Output from `findMultilines()`. If `NULL`,
it is computed automatically.
- `id_col`: Character or integer. Name or position of the submission ID
column (usually `"submissionId"` or column 1). Default: 1.
- `ask_threshold`: Numeric. Only prompt about columns where the maximum
number of distinct values per submission exceeds this value.
Default = 1.05 (catches almost all real multiples while ignoring noise).
- `quiet`: Logical. Suppress most messages and summaries? Default `FALSE`.
- `dry_run`: Logical. If `TRUE`, shows what would happen without actually
modifying the data. Default `FALSE`.

## Outputs

- `cleaned`: A tibble containing the cleaned, deduplicated responses
- `decisions`: A tibble recording which strategy was applied (or if
the column was removed) for each processed column

## Details and behavior

Shows a summary of problematic columns first, then asks the user one column
at a time how to handle each one (concatenate, keep first, sum, count,
remove the column entirely, etc.).
If audit logging is active (see `StartAuditLog()`), this action is recorded.

Most strategies reduce the data to one row per submission.
Choosing "remove" drops the column completely from the output.
All decisions are logged so you can review or reproduce the cleaning steps.

## Examples

```r
# Typical workflow
responses <- GetResponses(form_id = "your-form-id", api_key = "your-api-key")
flat <- FlattenSubmissions(responses)

# Run interactive cleaning
result <- FixDups(flat)

# View the cleaned data
View(result$cleaned)

# See what was done to each column
result$decisions

# Dry run to preview changes
FixDups(flat, dry_run = TRUE)
```

## Notes and tips

- Turn on audit logging if you need a paper trail
- Keep raw flattened data in your export for traceability
- Prefer consistent column names before downstream diagnostics

---

Navigation
- Home: [Handbook Index](../index.md)
- Function Index: [All Functions](index.md)
- Prev: [findMultilines](findMultilines.md)
- Next: [Next Function](FlattenSubmissions.md)
