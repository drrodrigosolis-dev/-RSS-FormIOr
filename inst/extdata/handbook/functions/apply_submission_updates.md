
# apply_submission_updates

**Title:** Adjust submissions by ID (delete or edit specific values)

## What this function is for

This function supports the FormIOr workflow by handling a specific task.

## Overview

Sometimes you need to make small, targeted fixes before you export:
remove test submissions, correct a value for one submission, or blank out
a field for privacy reasons.

## How it fits into a workflow

This function typically appears after you have created (or loaded) a
flattened dataset. If you are using the wizard, this step is handled for
you, but the function is also safe to call directly in scripts.

## Usage

```r
apply_submission_updates(
  x,
  id_col = 1,
  delete_ids = NULL,
  updates = NULL,
  return_flat = FALSE,
  quiet = FALSE
)
```

## Key inputs

- `x`: A data frame of responses, or a list from `flatten_submission_records()`.
- `id_col`: Integer or character. Submission ID column (default 1).
- `delete_ids`: Optional character vector of submission IDs to delete.
- `updates`: Optional updates to apply. Supported formats:
- A data.frame with columns `id`, `column`, `value`
- A data.frame with columns `submissionId`/`submission_id`, `column`, `value`
- A list of lists, each with elements `id`, `column`, `value`

Values are coerced to the target column type when possible. Use `"NA"` (or
an actual `NA`) to set a value to missing.
- `return_flat`: Logical. If `TRUE` and `x` came from `flatten_submission_records()`,
include the updated list as `flat` in the output.
- `quiet`: Logical. If `FALSE`, prints a short summary.

## Outputs

- `data`: Updated data frame
- `summary`: Data frame summarizing deletions/updates
- `changes`: Data frame listing each requested update and its status
- `flat`: If `return_flat = TRUE` and `x` was a flatten_submission_records list, the updated list

## Details and behavior

This helper lets you:
- Delete submission(s) by ID (remove rows)
- Update one or more column values for specific submission ID(s)

It works on either a plain data frame or the list produced by
`flatten_submission_records()`. If audit logging is active (see `start_audit_log()`),
this action is recorded.

## Examples

```r
df <- data.frame(
submissionId = c("a", "b", "c"),
status = c("ok", "test", "ok"),
stringsAsFactors = FALSE
)

# Delete one submission and update a value
updates <- data.frame(
  id = "c",
  column = "status",
  value = "review",
  stringsAsFactors = FALSE
)

out <- apply_submission_updates(
  df,
  id_col = "submissionId",
  delete_ids = "b",
  updates = updates,
  quiet = TRUE
)
out$data
```

## Notes and tips

- Turn on audit logging if you need a paper trail
- Keep raw flattened data in your export for traceability
- Prefer consistent column names before downstream diagnostics

---

Navigation
- Home: [Handbook Index](../index.md)
- Function Index: [All Functions](index.md)
- Prev: [assign_section_hierarchy](assign_section_hierarchy.md)
- Next: [Next Function](AskCredentials.md)
