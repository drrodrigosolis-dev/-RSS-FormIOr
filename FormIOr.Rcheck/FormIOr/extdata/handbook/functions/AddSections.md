
# AddSections

**Title:** Add Hierarchical Sections to FormIO Response Columns

## What this function is for

This function supports the FormIOr workflow by handling a specific task.

## Overview

This interactive function guides the user through categorizing columns of a flattened FormIO response dataset into hierarchical sections (up to 3 levels deep). It is designed to facilitate easier analysis by adding grouping variables to columns. The function handles input that may be a data frame, a list from `FlattenSubmissions()`, or raw output from `GetResponses()`.
If audit logging is active (see `StartAuditLog()`), this action is recorded.

## How it fits into a workflow

This function typically appears after you have created (or loaded) a
flattened dataset. If you are using the wizard, this step is handled for
you, but the function is also safe to call directly in scripts.

## Usage

```r
AddSections(x)
```

## Key inputs

- `x`: A data frame (possibly with nested list-columns), or a list containing `FlatResponses` (a flattened data frame) or `submission_data` (from `GetResponses()` with `content.only = FALSE`).

## Outputs

- `FlatResponses`: The original flattened data frame of responses.
- `Sections`: A `tibble` with columns `No` (column number), `Names` (original column names), and `Level-1`, `Level-2`, `Level-3` (section assignments, filled with "General" if empty).

## Details and behavior

The function first extracts or flattens the input to obtain a flat data frame of responses. It then prompts the user to:
- Select the depth of sections (1, 2, or 3).
- Provide comma-separated names for sections at each level (no spaces or special characters).
- Assign row numbers (from the displayed column list) to each section at each level. Row numbers can be single values, comma-separated lists, or ranges (e.g., "1:5,8,10:12").
Empty assignments are filled with "General". The process uses console clearing (`014`) and colored prompts (requires the `crayon` package).

## Examples

```r
if (interactive()) {
# Assuming FoodTypes is a sample dataset with possible nests
data("FoodTypes")
sectioned <- AddSections(FoodTypes)
print(sectioned$Sections)
```

## Notes and tips

- Turn on audit logging if you need a paper trail
- Keep raw flattened data in your export for traceability
- Prefer consistent column names before downstream diagnostics

---

Navigation
- Home: [Handbook Index](../index.md)
- Function Index: [All Functions](index.md)
- Prev: [Function Index](index.md)
- Next: [Next Function](AdjustSubmissions.md)
