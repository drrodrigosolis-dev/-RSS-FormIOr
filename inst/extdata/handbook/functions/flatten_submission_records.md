
# flatten_submission_records

**Title:** Flatten nested FormIO submissions into a single table

## What this function is for

This function focuses on cleaning or standardizing the flattened dataset.

## Overview

FormIO submissions often contain nested lists (for repeating sections,
address blocks, uploads, etc.). `flatten_submission_records()` expands those nested
structures into a regular 2D data frame so you can export and analyze the
data more easily.

## How it fits into a workflow

This function typically appears after you have created (or loaded) a
flattened dataset. If you are using the wizard, this step is handled for
you, but the function is also safe to call directly in scripts.

## Usage

```r
flatten_submission_records(x)
```

## Key inputs

- `x`: A data frame or list of submissions, typically returned by
`fetch_form_responses()`.

## Outputs

- `FlatResponses`: A flattened data frame (one row per submission row in the source).
- `ColumnNames`: A data frame listing the new column names and their order.

## Details and behavior

Nested columns are flattened using their parent name as a prefix, then a
`"-"` separator, and then the nested field name.
If audit logging is active (see `start_audit_log()`), this action is recorded.

## Examples

```r
x <- FoodTypes

# Flattened structure
xFlat <- flatten_submission_records(x)
head(xFlat$FlatResponses)
head(xFlat$ColumnNames)
```

## Notes and tips

- Turn on audit logging if you need a paper trail
- Keep raw flattened data in your export for traceability
- Prefer consistent column names before downstream diagnostics

---

Navigation
- Home: [Handbook Index](../index.md)
- Function Index: [All Functions](index.md)
- Prev: [resolve_duplicate_values](resolve_duplicate_values.md)
- Next: [Next Function](run_form_processing_workflow.md)
