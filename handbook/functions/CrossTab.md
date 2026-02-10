
# CrossTab

**Title:** Cross-tabulate two fields

## What this function is for

This function focuses on diagnostics and reporting.

## Overview

Builds a simple cross-tabulation between two columns, returning both a
wide table and a long table that includes counts and (optional) percents.
If audit logging is active (see `StartAuditLog()`), this action is recorded.

## How it fits into a workflow

This function typically appears after you have created (or loaded) a
flattened dataset. If you are using the wizard, this step is handled for
you, but the function is also safe to call directly in scripts.

## Usage

```r
CrossTab(
  x,
  row,
  col,
  include_na = FALSE,
  percent = c("overall", "row", "col", "none"),
  digits = 1,
  quiet = FALSE
)
```

## Key inputs

- `x`: A data frame of responses, or a list from `FlattenSubmissions()`.
- `row`: Column name or number for the rows.
- `col`: Column name or number for the columns.
- `include_na`: Logical. If `TRUE`, treat missing values as "(Missing)".
- `percent`: How to calculate percentages. One of `"overall"` (default),
`"row"`, `"col"`, or `"none"`.
- `digits`: Number of decimal places for percentages.
- `quiet`: Logical. If `FALSE`, prints a short summary.

## Outputs

- `row`: Resolved row field name
- `col`: Resolved column field name
- `percent`: Percent calculation mode
- `table`: Wide counts table as a data frame
- `long`: Long data frame with counts and percents

## Details and behavior

This function follows the package defaults and logs actions when audit logging is active.

## Examples

```r
CrossTab(flat, "region", "program", percent = "row")
```

## Notes and tips

- Turn on audit logging if you need a paper trail
- Keep raw flattened data in your export for traceability
- Prefer consistent column names before downstream diagnostics

---

Navigation
- Home: [Handbook Index](../index.md)
- Function Index: [All Functions](index.md)
- Prev: [CompareFormVersions](CompareFormVersions.md)
- Next: [Next Function](DeduplicateSubmissions.md)
