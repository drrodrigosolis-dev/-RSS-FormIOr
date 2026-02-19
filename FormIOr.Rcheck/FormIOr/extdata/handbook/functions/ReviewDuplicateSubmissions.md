
# ReviewDuplicateSubmissions

**Title:** Interactively review and resolve duplicate submissions

## What this function is for

This function helps you manually resolve duplicated submission IDs when you want
full control over which rows to keep. It shows each duplicate group and lets you
pick one or more rows to keep.

## Overview

`ReviewDuplicateSubmissions()` walks you through duplicate groups defined by key
columns (for example `email`, `username`, `project_id`). For each duplicate group,
it prints the columns you choose (so you can compare submissions), then asks which
submissions to keep. You can keep multiple submissions or drop all submissions
for a given group if needed.

If audit logging is active (see `StartAuditLog()`), each decision is logged.

## How it fits into a workflow

Use this after you have a flattened dataset and want to resolve duplicates
manually. If you want a fully automatic approach, use `DeduplicateSubmissions()`.

## Usage

```r
ReviewDuplicateSubmissions(
  x,
  id_col = 1,
  key_cols = NULL,
  compare_cols = NULL,
  return_flat = FALSE,
  quiet = FALSE,
  keep_map = NULL,
  prompt = TRUE,
  default_keep = "all"
)
```

## Key inputs

- `x`: A data frame of responses, or a list from `FlattenSubmissions()`.
- `id_col`: Integer or character. Submission ID column (default 1).
- `key_cols`: Columns used to define duplicates (for example `username`, `email`, `project_id`).
  If `NULL`, you will be prompted to choose columns interactively (unless `prompt = FALSE`).
  The default suggestions are the first three columns that do not start with `form-` or `form_`.
- `compare_cols`: Optional. Columns to show when comparing duplicates.
  If `NULL`, you will be prompted to choose columns interactively (unless `prompt = FALSE`).
- `return_flat`: Logical. If `TRUE` and `x` came from `FlattenSubmissions()`,
  include the updated list as `flat` in the output.
- `quiet`: Logical. If `FALSE`, prints guidance and summaries.
- `keep_map`: Optional. Non-interactive decisions for which submissions to keep,
  keyed by duplicate `group_id` or `group_key`. Accepts a named list/vector or
  a data frame with columns `group_id`/`group_key` and `keep`.
  Values can be row numbers, ranges like `"1:3"`, or keywords `"all"` / `"none"`.
- `prompt`: Logical. If `TRUE` (default), ask interactively. If `FALSE`, uses `keep_map`
  and `default_keep` without prompting.
- `default_keep`: Default action when `prompt = FALSE` and no `keep_map` entry exists
  for a given ID. Default `"all"`.

## Outputs

- `data`: Data frame after duplicate review
- `summary`: List with counts and whether the review stopped early
- `decisions`: Data frame describing what was kept/dropped for each duplicate group
- `flat`: If `return_flat = TRUE` and `x` was a FlattenSubmissions list, the updated list

## Details and behavior

- The function is interactive by default. For scripts, provide `key_cols`, `keep_map`,
  and `prompt = FALSE`.
- At either prompt, type `suggest` to reprint the recommended defaults.
- You can type `all` to keep all rows for an ID, `none` to drop all rows,
  or `q` to quit early.
- If you quit early, all remaining duplicate groups are kept as-is.

## Examples

```r
responses <- GetResponses(form_id = "123", api_key = "abc")
flat <- FlattenSubmissions(responses)

out <- ReviewDuplicateSubmissions(flat, id_col = "form_submissionid")
View(out$decisions)

# Non-interactive example (scriptable)
df <- data.frame(
  submissionId = c("a", "a", "b", "b", "b"),
  email = c("x@example.com", "x@example.com", "x@example.com", "x@example.com", "x@example.com"),
  status = c("draft", "final", "test", "final", "final"),
  stringsAsFactors = FALSE
)
# Group 1 will be the email=x@example.com group (the only group in this example)
keep_map <- list(`1` = 2)
out2 <- ReviewDuplicateSubmissions(
  df,
  id_col = "submissionId",
  key_cols = "email",
  compare_cols = c("submissionId", "email", "status"),
  keep_map = keep_map,
  prompt = FALSE,
  quiet = TRUE
)
```

## Notes and tips

- Use this when you need human judgment (test submissions, edits, or data entry errors).
- If you want an automatic rule-based approach, use `DeduplicateSubmissions()` instead.
- Turn on audit logging if you need a clear record of the decisions.

---

Navigation
- Home: [Handbook Index](../index.md)
- Function Index: [All Functions](index.md)
- Prev: [RenameCols](RenameCols.md)
- Next: [Next Function](ResolveRepeats.md)
