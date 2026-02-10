
# DescribeForm

**Title:** Describe a form in plain language

## What this function is for

This function produces human-readable outputs for reporting.

## Overview

This is a simple overview of a form: the name, version, and how many fields
it contains. It is meant for non-technical users who want a quick summary.
If audit logging is active (see `StartAuditLog()`), this action is recorded.

## How it fits into a workflow

This function typically appears after you have created (or loaded) a
flattened dataset. If you are using the wizard, this step is handled for
you, but the function is also safe to call directly in scripts.

## Usage

```r
DescribeForm(
  form,
  include_fields = FALSE,
  version = "latest",
  include = c("input", "all"),
  quiet = FALSE
)
```

## Key inputs

- `form`: Form schema list, JSON string, or path to a JSON file.
- `include_fields`: Logical. If `TRUE`, include a field dictionary in the
output (from `FieldDictionary()`).
- `version`: Which form version to use when `form` is metadata (from
`GetFormMetadata()`). Use `"latest"` (default), a version number (e.g., `3`),
or a version ID.
- `include`: Which components to count for fields. Defaults to input fields
only (`"input"`). Use `"all"` to include layout components too.
- `quiet`: Logical. If `FALSE`, prints a short summary.

## Outputs

- `meta`: Form details like title, name, ID, and version
- `counts`: How many components, fields, and sections
- `fields`: Field dictionary data frame (only if `include_fields = TRUE`)

## Details and behavior

What you can pass to `form`:
- A **schema list** (from a schema API response)
- A **JSON string**
- A **path to a JSON file**
- The output of `GetFormMetadata()`

Important: `GetFormMetadata()` returns **metadata only**, not the full form.
If you pass metadata, this function will automatically fetch the form schema
using stored credentials (from `AskCredentials()` or `GetFormMetadata()`).
If credentials are not available, it will stop with a clear message.

Tip: Use `include = "input"` (default) to count only the fields people fill in,
or `include = "all"` to include layout items like panels and tabs.

## Examples

```r
form <- list(
title = "Sample Form",
name = "sample_form",
version = 2,
components = list(
list(type = "textfield", key = "first_name", label = "First name", input = TRUE),
list(type = "panel", title = "Details", components = list(
list(type = "number", key = "age", label = "Age", input = TRUE)
))
)
)
DescribeForm(form)


meta <- GetFormMetadata(form_id = "123", api_key = "abc")
DescribeForm(meta)
```

## Notes and tips

- Turn on audit logging if you need a paper trail
- Keep raw flattened data in your export for traceability
- Prefer consistent column names before downstream diagnostics

---

Navigation
- Home: [Handbook Index](../index.md)
- Function Index: [All Functions](index.md)
- Prev: [DeduplicateSubmissions](DeduplicateSubmissions.md)
- Next: [Next Function](ExportToExcel.md)
