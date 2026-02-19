
# FormIOrWorkflow

**Title:** Guided end-to-end workflow (download -> clean -> report -> export)

## What this function is for

This is the end-to-end guided workflow for most users.

## Overview

This interactive helper walks you through a complete FormIOr workflow:
downloading responses, flattening and cleaning the data, generating simple
diagnostics (like a codebook and basic plots), and creating output files.

## How it fits into a workflow

This function typically appears after you have created (or loaded) a
flattened dataset. If you are using the wizard, this step is handled for
you, but the function is also safe to call directly in scripts.

## Usage

```r
FormIOrWorkflow(
  data = NULL,
  base_url = "https://submit.digital.gov.bc.ca/app/api/v1",
  form_id = NULL,
  api_key = NULL,
  output_dir = NULL,
  overwrite = FALSE,
  plan = NULL,
  quiet = FALSE
)
```

## Key inputs

- `data`: Optional. A data.frame of responses to start from. If `NULL`
(default), the workflow can download responses using `GetResponses()`.
- `base_url`: Character. API base URL. Default:
`"https://submit.digital.gov.bc.ca/app/api/v1"`.
  Note (CHEF): Response export endpoints use `/app/api/v1`, while some
  metadata/schema endpoints live at `/api/v1`. FormIOr automatically retries
  the alternate base for metadata/schema requests when needed.
- `form_id`: Character. Form identifier. If `NULL`, uses stored credentials
or prompts via `AskCredentials()`.
- `api_key`: Character. API key / secret. If `NULL`, uses stored credentials
or prompts via `AskCredentials()`.
- `output_dir`: Folder to write output files into. If `NULL`, a new folder
is created in the current working directory. When downloading from FormIO,
the default folder name uses the form name (when available) plus a timestamp.
- `overwrite`: Logical. If `TRUE`, allow overwriting output files.
- `plan`: Optional list. For non-interactive/scripted runs you can pass a
simple plan list (created manually). If provided, the workflow runs without
prompting. (This is separate from the `workflow_plan.rds` file saved by the
interactive wizard.)
- `quiet`: Logical. If `FALSE`, prints progress messages.

## Outputs

- `data_raw`: The downloaded/raw responses (if downloaded).
- `flat_raw`: The fully flattened data *before* any cleaning/wrangling steps.
- `flat`: A `FlattenSubmissions()`-style list with `FlatResponses`.
- `reports`: Named list of diagnostic tables (codebook, summaries).
- `files`: Named list of output file paths.
- `plan`: The plan that was executed (useful for reproducibility).

## Details and behavior

It is designed for non-technical users: you can accept the defaults by
pressing Enter at each prompt.

Audit logging:
- If you start an audit log (see `StartAuditLog()`), each step is recorded.
- If you do not start a log, FormIOr will ask once per session whether you
want to begin logging.

Output folder naming:
- When downloading from FormIO and you don't supply `output_dir`, the workflow
will try to fetch basic form metadata first so it can suggest a folder name
based on the form name. This may prompt for credentials a bit earlier than
the download step.

Output files:
- The export workbook includes a `FlattenedRaw` sheet (the flattened data
before any cleaning) and, when available, an `AuditLog` sheet.
- Plots are saved as `.png` files under `output_dir/plots` and are listed in
a `Plots` sheet.
- The workflow also saves your wizard choices to `workflow_plan.rds` and
`workflow_plan.json` inside the output folder, so you can repeat the same
steps later.

Repeatable sessions:
- When you run `FormIOrWorkflow()` again, it looks for a previous
`workflow_plan.rds/json` and asks whether you want to reuse it.
- You can reuse a previous session in two ways:
1. Apply automatically (no prompts; uses saved answers where available)
1. Use as defaults (prompts still appear, but you can press Enter to accept)

## Examples

```r
# Fully guided interactive run
out <- FormIOrWorkflow()

# Start from an existing data.frame (skip download)
out <- FormIOrWorkflow(data = FoodTypes)
```

## Notes and tips

- Turn on audit logging if you need a paper trail
- Keep raw flattened data in your export for traceability
- Prefer consistent column names before downstream diagnostics

---

Navigation
- Home: [Handbook Index](../index.md)
- Function Index: [All Functions](index.md)
- Prev: [FlattenSubmissions](FlattenSubmissions.md)
- Next: [Next Function](GetFormMetadata.md)
