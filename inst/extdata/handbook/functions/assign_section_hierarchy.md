# assign_section_hierarchy

**Title:** Assign section labels to flattened columns

## What this function is for

`assign_section_hierarchy()` helps you group columns into high-level sections (up to 3
levels) so downstream analysis and reporting are easier to read.

## Overview

You can use `assign_section_hierarchy()` in two ways:

- Interactive: answer prompts for depth, section names, and row assignments.
- Non-interactive: pass `depth`, `section_names`, and `section_rows` to skip
  prompts entirely.

Accepted inputs:

- A flat data frame.
- A `flatten_submission_records()` result (`$FlatResponses`).
- A list with `$submission_data` from `fetch_form_responses(..., content.only = FALSE)`.

If nested columns are detected, they are flattened automatically first.

## Usage

```r
assign_section_hierarchy(
  x,
  depth = NULL,
  section_names = NULL,
  section_rows = NULL
)
```

## Key inputs

- `x`: Data source to section.
- `depth`: Optional section depth (`1`, `2`, or `3`).
- `section_names`: Optional list of names by level.
- `section_rows`: Optional list of row assignments by level and section.

`section_rows` can contain:

- Numeric vectors: `c(1, 2, 5)`
- Range strings: `"1:5"`
- Comma strings: `"1,3,7"`

## Outputs

- `FlatResponses`: Flattened response table.
- `Sections`: Mapping table with `No`, `Names`, and `Level-1..Level-3`.
  Unassigned cells are filled with `"General"`.

## Examples

```r
# Non-interactive example (no prompts)
sectioned <- assign_section_hierarchy(
  x = FoodTypes,
  depth = 2,
  section_names = list(
    c("ParticipantInfo", "ProgramInfo"),
    c("Core", "Optional")
  ),
  section_rows = list(
    list("1:4", "5:12"),
    list("1,2,5,6", "7:12")
  )
)

# Interactive example
# sectioned <- assign_section_hierarchy(FoodTypes)
```

## Notes and tips

- For scripts/pipelines, always provide all three non-interactive arguments.
- Keep section names short and consistent across projects.
- If audit logging is active, this step is recorded (not the raw prompt text).

---

Navigation
- Home: [Handbook Index](../index.md)
- Function Index: [All Functions](index.md)
- Prev: [Function Index](index.md)
- Next: [apply_submission_updates](apply_submission_updates.md)
