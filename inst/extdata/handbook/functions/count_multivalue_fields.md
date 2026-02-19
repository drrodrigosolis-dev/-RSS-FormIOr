
# count_multivalue_fields

**Title:** Identify columns with multiple distinct values per submission

## What this function is for

This function focuses on cleaning or standardizing the flattened dataset.

## Overview

Creates a diagnostic data frame with the same shape as the flattened responses,
where each original value (except in the ID column) is replaced by the number
of **distinct non-NA values** that appear in that column for the corresponding
submission. This makes it easy to detect which questions/fields caused row
duplication due to repeated sections, multi-select choices, or repeating groups.

## How it fits into a workflow

This function typically appears after you have created (or loaded) a
flattened dataset. If you are using the wizard, this step is handled for
you, but the function is also safe to call directly in scripts.

## Usage

```r
count_multivalue_fields(x, id_col = 1)
```

## Key inputs

- `x`: A list returned by `flatten_submission_records()`, containing at minimum
verb{$FlatResponses} (the flattened data frame) and optionally verb{$ColumnNames}.
- `id_col`: Integer. The column number that contains the unique submission
identifier (usually `submissionId`). Default = 1 (first column).

## Outputs

- (See `?count_multivalue_fields` for return value details.)

## Details and behavior

This function is typically used before `resolve_duplicate_values()` to help non-technical users
identify which columns need special handling (e.g. concatenate, pick first,
sum, pivot wider, etc.).
If audit logging is active (see `start_audit_log()`), this action is recorded.

Columns that are constant within each submission will show `1` everywhere
(or `NA` if the column is entirely missing for that submission).

## Examples

```r
responses <- fetch_form_responses(form_id = "123", api_key = "abc...")
flat <- flatten_submission_records(responses)

# See which columns have multiple values per submission
multi_counts <- count_multivalue_fields(flat)

# Quick check: which columns ever have more than one distinct value?
multi_counts |>
summarise(across(-1, ~ max(.x, na.rm = TRUE))) |>
pivot_longer(everything(), names_to = "column", values_to = "max_distinct") |>
filter(max_distinct > 1)
```

## Notes and tips

- Turn on audit logging if you need a paper trail
- Keep raw flattened data in your export for traceability
- Prefer consistent column names before downstream diagnostics

---

Navigation
- Home: [Handbook Index](../index.md)
- Function Index: [All Functions](index.md)
- Prev: [build_field_dictionary](build_field_dictionary.md)
- Next: [Next Function](resolve_duplicate_values.md)
