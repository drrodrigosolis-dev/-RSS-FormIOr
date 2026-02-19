
# summarize_field_distribution

**Title:** Summarize a single field in a FormIO response dataset

## What this function is for

This function focuses on diagnostics and reporting.

## Overview

Designed for non-technical users: it accepts either a data frame or the list
produced by `flatten_submission_records()` and returns a simple summary. Numeric
fields get descriptive statistics; categorical fields get counts and percents.
If audit logging is active (see `start_audit_log()`), this action is recorded.

## How it fits into a workflow

This function typically appears after you have created (or loaded) a
flattened dataset. If you are using the wizard, this step is handled for
you, but the function is also safe to call directly in scripts.

## Usage

```r
summarize_field_distribution(
  x,
  field,
  top_n = 10,
  include_na = FALSE,
  digits = 2,
  quiet = FALSE
)
```

## Key inputs

- `x`: A data frame of responses, or a list from `flatten_submission_records()`.
- `field`: Column name or number to summarize.
- `top_n`: For categorical fields, the number of top values to return
(default 10). Use `NULL` to return all values.
- `include_na`: Logical. If `TRUE`, include missing values as "(Missing)"
in categorical summaries.
- `digits`: Number of decimal places for percentages/statistics.
- `quiet`: Logical. If `FALSE`, prints a short summary.

## Outputs

- `field`: Resolved column name
- `type`: Detected type: "numeric", "categorical", or "date"
- `total`: Total rows
- `missing`: Missing value count
- `distinct`: Distinct non-missing values
- `summary`: Data frame of summary stats or counts

## Details and behavior

This function follows the package defaults and logs actions when audit logging is active.

## Examples

```r
responses <- fetch_form_responses(form_id = "123", api_key = "abc")
flat <- flatten_submission_records(responses)
summarize_field_distribution(flat, "age")
summarize_field_distribution(flat, "favorite_food", top_n = 5)
```

## Notes and tips

- Turn on audit logging if you need a paper trail
- Keep raw flattened data in your export for traceability
- Prefer consistent column names before downstream diagnostics

---

Navigation
- Home: [Handbook Index](../index.md)
- Function Index: [All Functions](index.md)
- Prev: [stop_audit_log](stop_audit_log.md)
- Next: [Next Function](append_audit_log_entry.md)
