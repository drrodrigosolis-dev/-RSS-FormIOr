
# MakeCodebook

**Title:** Create a codebook for a dataset

## What this function is for

This function produces human-readable outputs for reporting.

## Overview

This produces a plain-language table that documents each column in your data.
It can optionally use a FormIO schema (via `FieldDictionary()`) to add labels,
sections, and descriptions.
If audit logging is active (see `StartAuditLog()`), this action is recorded.

## How it fits into a workflow

This function typically appears after you have created (or loaded) a
flattened dataset. If you are using the wizard, this step is handled for
you, but the function is also safe to call directly in scripts.

## Usage

```r
MakeCodebook(
  data,
  form = NULL,
  include = c("input", "all"),
  include_summary = TRUE,
  max_levels = 20,
  quiet = FALSE
)
```

## Key inputs

- `data`: A data.frame of responses, or a list containing `FlatResponses`
or `submission_data`.
- `form`: Optional form schema, JSON string, or metadata object (see
`FieldDictionary()`). If provided, labels and sections are included.
- `include`: Which components to include when `form` is provided:
`"input"` (default) or `"all"`.
- `include_summary`: Logical. If `TRUE` (default), include basic numeric
summaries (min, max, mean).
- `max_levels`: Maximum number of category levels to list for character or
factor columns.
- `quiet`: Logical. If `FALSE`, prints a short message.

## Outputs

- (See `?MakeCodebook` for return value details.)

## Details and behavior

When a schema is provided, the codebook also includes a `schema_key` column
showing which form field key was matched (best-effort). If a column could not
be matched to the schema, schema-related columns (type/required/section/etc.)
will be `NA`.

## Examples

```r
df <- data.frame(age = c(10, 12, NA), color = c("red", "blue", "red"))
MakeCodebook(df)
```

## Notes and tips

- Turn on audit logging if you need a paper trail
- Keep raw flattened data in your export for traceability
- Prefer consistent column names before downstream diagnostics

---

Navigation
- Home: [Handbook Index](../index.md)
- Function Index: [All Functions](index.md)
- Prev: [IsAuditLogActive](IsAuditLogActive.md)
- Next: [Next Function](NormalizeColumnNames.md)
