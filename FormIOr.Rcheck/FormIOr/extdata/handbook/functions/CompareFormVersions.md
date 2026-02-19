
# CompareFormVersions

**Title:** Compare two versions of a form

## What this function is for

This function supports the FormIOr workflow by handling a specific task.

## Overview

Shows what changed between two form versions:
fields added, removed, or updated.
If audit logging is active (see `StartAuditLog()`), this action is recorded.

## How it fits into a workflow

This function typically appears after you have created (or loaded) a
flattened dataset. If you are using the wizard, this step is handled for
you, but the function is also safe to call directly in scripts.

## Usage

```r
CompareFormVersions(
  old,
  new,
  by = c("key", "path"),
  include = c("input", "all"),
  old_version = "latest",
  new_version = "latest",
  compare_cols = c("label", "type", "required", "description", "default", "options",
    "section"),
  include_unchanged = FALSE,
  quiet = FALSE
)
```

## Key inputs

- `old`: Form schema list, JSON string, or path to a JSON file (older version).
- `new`: Form schema list, JSON string, or path to a JSON file (newer version).
- `by`: How to match fields across versions: `"key"` (default) or `"path"`.
- `include`: Which components to include: `"input"` (default) or `"all"`.
- `old_version`: Which form version to use when `old` is metadata (from
`GetFormMetadata()`). Use `"latest"` (default), a version number, or a
version ID.
- `new_version`: Which form version to use when `new` is metadata (from
`GetFormMetadata()`). Use `"latest"` (default), a version number, or a
version ID.
- `compare_cols`: Character vector of columns to compare for changes.
- `include_unchanged`: Logical. If `TRUE`, include unchanged fields in the output.
- `quiet`: Logical. If `FALSE`, prints a short summary.

## Outputs

- `summary`: Counts of added, removed, changed, and unchanged fields
- `added`: Fields only in the new version
- `removed`: Fields only in the old version
- `changed`: Fields that changed (with before/after values)
- `unchanged`: Unchanged fields (if `include_unchanged = TRUE`)

## Details and behavior

If you pass the output of `GetFormMetadata()`, the schema is **not** included
in that object. In that case the function will automatically fetch the schema
using stored credentials (from `AskCredentials()` or `GetFormMetadata()`). If
credentials are not available, it will stop with a clear message.

Tip: Use `by = "key"` for stable field keys, or `by = "path"` when keys are
reused in repeating sections.

## Examples

```r
old <- list(components = list(
list(type = "textfield", key = "name", label = "Name", input = TRUE)
))
new <- list(components = list(
list(type = "textfield", key = "name", label = "Full name", input = TRUE),
list(type = "number", key = "age", label = "Age", input = TRUE)
))
CompareFormVersions(old, new)


old_meta <- GetFormMetadata(form_id = "123", api_key = "abc")
new_meta <- GetFormMetadata(form_id = "123", api_key = "abc")
CompareFormVersions(old_meta, new_meta)
```

## Notes and tips

- Turn on audit logging if you need a paper trail
- Keep raw flattened data in your export for traceability
- Prefer consistent column names before downstream diagnostics

---

Navigation
- Home: [Handbook Index](../index.md)
- Function Index: [All Functions](index.md)
- Prev: [CompactSelections](CompactSelections.md)
- Next: [Next Function](CrossTab.md)
