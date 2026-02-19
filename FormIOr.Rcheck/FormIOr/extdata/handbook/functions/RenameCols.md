
# RenameCols

**Title:** Interactively rename columns

## What this function is for

This function focuses on cleaning or standardizing the flattened dataset.

## Overview

Guides you through renaming each column in a flattened FormIO dataset.
This is helpful for non-technical users who want friendly column names
before analysis or export.

## How it fits into a workflow

This function typically appears after you have created (or loaded) a
flattened dataset. If you are using the wizard, this step is handled for
you, but the function is also safe to call directly in scripts.

## Usage

```r
RenameCols(
  x,
  NamesDF = TRUE,
  renameDF = TRUE
)
```

## Key inputs

- `x`: A list returned by `FlattenSubmissions()`.
- `NamesDF`: Logical. If `TRUE` (default), return a data frame of the old
and new names.
- `renameDF`: Logical. If `TRUE` (default), return the dataset with the
updated column names.

## Outputs

- List elements (when enabled):
- Renaming table (`Number`, `OldNames`, `NewNames`)
- Updated `FlattenSubmissions()` list with renamed columns

## Details and behavior

This helper is interactive and asks for each new name in sequence.

## Examples

```r
responses <- GetResponses(form_id = "123", api_key = "abc")
flat <- FlattenSubmissions(responses)
RenameCols(flat)
```

## Notes and tips

- Turn on audit logging if you need a paper trail
- Keep raw flattened data in your export for traceability
- Prefer consistent column names before downstream diagnostics

---

Navigation
- Home: [Handbook Index](../index.md)
- Function Index: [All Functions](index.md)
- Prev: [PlotWordcloud](PlotWordcloud.md)
- Next: [Next Function](ReviewDuplicateSubmissions.md)
