
# FieldDictionary

**Title:** Build a field dictionary for a form

## What this function is for

This function produces human-readable outputs for reporting.

## Overview

This creates a clean table that lists each field in your form. It is meant
to be easy to read and share with non-technical staff.
If audit logging is active (see `StartAuditLog()`), this action is recorded.

## How it fits into a workflow

This function typically appears after you have created (or loaded) a
flattened dataset. If you are using the wizard, this step is handled for
you, but the function is also safe to call directly in scripts.

## Usage

```r
FieldDictionary(
  form,
  include = c("input", "all"),
  version = "latest",
  expand_surveys = FALSE,
  quiet = FALSE
)
```

## Key inputs

- `form`: Form schema list, JSON string, or path to a JSON file.
- `include`: Which components to include: `"input"` (default) or `"all"`.
- `version`: Which form version to use when `form` is metadata (from
`GetFormMetadata()`). Use `"latest"` (default), a version number (e.g., `3`),
or a version ID.
- `expand_surveys`: Logical. If `TRUE`, expands survey components into one row
per question.
- `quiet`: Logical. If `FALSE`, prints a short summary.

## Outputs

- (See `?FieldDictionary` for return value details.)

## Details and behavior

If you pass the output of `GetFormMetadata()`, the schema is **not** included
in that object. In that case the function will automatically fetch the schema
using stored credentials (from `AskCredentials()` or `GetFormMetadata()`).
If credentials are not available, it will stop with a clear message.

Note (CHEF): Some schema endpoints live at `/api/v1` while other endpoints
(like exports) are under `/app/api/v1`. If a schema fetch fails at the
provided `base_url`, FormIOr automatically retries the alternate base.

Tip: Use `include = "all"` to include layout components (panels, tabs,
fieldsets). These usually have `input = FALSE` and no field key.

## Examples

```r
form <- list(
title = "Sample Form",
components = list(
list(type = "textfield", key = "first_name", label = "First name", input = TRUE),
list(type = "select", key = "color", label = "Favorite color", input = TRUE,
data = list(values = list(list(label = "Red", value = "red"))))
)
)
FieldDictionary(form)


meta <- GetFormMetadata(form_id = "123", api_key = "abc")
FieldDictionary(meta, include = "all")
```

## Notes and tips

- Turn on audit logging if you need a paper trail
- Keep raw flattened data in your export for traceability
- Prefer consistent column names before downstream diagnostics

---

Navigation
- Home: [Handbook Index](../index.md)
- Function Index: [All Functions](index.md)
- Prev: [ExportToExcel](ExportToExcel.md)
- Next: [Next Function](findMultilines.md)
