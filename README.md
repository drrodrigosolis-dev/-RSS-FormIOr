# FormIOr

Helper functions for downloading, flattening, cleaning, and simplifying FormIO submissions.
This package is designed for non-technical users who want a clean, analysis-ready data table.

Maintained by **Rodrigo Solis-Sosa, PhD** (Human Dimensions of Wildlife Specialist,
BC Ministry of Water, Land and Resource Stewardship).

FormIOr is designed for the Common Hosted Forms Service (CHEF) FormIO used by
the BC Public Service. It may work with other FormIO services, but that is not
guaranteed.

**Default CHEF base URL:** `https://submit.digital.gov.bc.ca/app/api/v1`  
If you use a different FormIO service, you must override `base_url` in
`fetch_form_responses()` / `fetch_form_submissions()`.

Note (CHEF): Response export endpoints use `/app/api/v1`, while some
metadata/schema endpoints live at `/api/v1`. FormIOr automatically retries
the alternate base for metadata/schema requests when needed.

## Install

```r
# From local folder
install.packages("/path/to/FormIOr", repos = NULL, type = "source")
```

## Typical Workflow

```r
library(FormIOr)

# Optional: cache credentials once per R session (skips prompts)
AskCredentials(form_id = "your-form-id", api = "your-api-key")

# 1) Download responses
responses <- fetch_form_responses(form_id = "your-form-id", api_key = "your-api-key")
# or: responses <- fetch_form_responses()  # if already cached with AskCredentials()

# 2) Flatten nested submissions
flat <- flatten_submission_records(responses)

# 3) Optional: rename columns interactively
rename_columns_from_dictionary(flat)

# 4) Simplify and clean
norm <- standardize_column_names(flat)                # clean column names
resolved <- collapse_repeated_values(flat)                  # collapse repeated answers
compact <- collapse_checkbox_selections(flat)                # combine checkbox columns

dedup <- deduplicate_submission_rows(flat)             # remove duplicate submissions
```

## Quick Start: Guided Wizard

For most users, the easiest entry point is the guided workflow:

```r
out <- run_form_processing_workflow()
```

The wizard can:

- Download responses (or start from an existing data frame)
- Flatten nested columns
- Apply cleaning steps (normalize names, deduplicate, resolve repeats, etc.)
- Generate diagnostics (codebook + plots)
- Export a multi-sheet Excel workbook (or CSV fallback)

It also saves your wizard choices to `workflow_plan.rds/json` so you can repeat
the same steps later.

## RStudio Addin (Point-and-Click)

Launch the addin from **RStudio > Addins > FormIOr Addin** or run:

```r
launch_formior_addin()
```

The addin provides tabs for downloading and flattening data, cleaning helpers,
exporting to Excel/CSV, summaries by field, and a build_field_dictionary explorer.
If the optional \`reactable\` package is installed, preview tables have
resizable columns.

## Vignette

See `inst/extdata/handbook/index.md` in this repo for a detailed end-to-end guide.

If you have the package installed, you can locate the same guide with:

```r
system.file("extdata", "handbook/index.md", package = "FormIOr")
```

## Getting your API key and Form ID (CHEF)

You can generate your API key in the **Manage** page of your form.  
Your **Form ID** is the final alphanumeric code after the `=` sign in the
Manage page URL.

![CHEF API Key location](inst/extdata/handbook/assets/API.png)

![CHEF Form ID location](inst/extdata/handbook/assets/FormID.png)

## Cleaning & Simplification Helpers

All functions below accept either a plain data frame or the output of
`flatten_submission_records()`.

### standardize_column_names()
Standardizes column names for readability and consistency.

```r
norm <- standardize_column_names(flat)
head(norm$data)
```

### collapse_repeated_values()
Collapses repeated values within each submission ID using either a fixed strategy
or simple auto-detection.

```r
resolved <- collapse_repeated_values(flat, id_col = "submissionId")
resolved$summary
```

### deduplicate_submission_rows()
Keeps one row per submission ID (uses a timestamp column when available).

```r
dedup <- deduplicate_submission_rows(flat, id_col = "submissionId")
```

### detect_suspicious_submissions()
Checks whether pre/post phases can be pooled and flags suspicious rows
(duplicate IDs/fingerprints, rapid repeats).

```r
data("SuspiciousSurveyDemo", package = "FormIOr")

risk <- detect_suspicious_submissions(
  SuspiciousSurveyDemo,
  id_col = "submissionId",
  time_col = "created",
  cutoff_time = "2026-01-06 00:00:00",
  group_action = "split_only"
)

risk$comparability$non_comparable
```

## Built-in Demo Dataset

FormIOr includes `SuspiciousSurveyDemo`, a synthetic dataset built to test
time-group comparability and suspicious-response detection.

### collapse_checkbox_selections()
Combines checkbox/multi-select columns into a single comma-separated column.

```r
compact <- collapse_checkbox_selections(flat)
compact$summary
```

## Notes

- If you want the updated `flatten_submission_records()` list in the output, pass
  `return_flat = TRUE` to any of the new cleaning functions.
- Use `resolve_duplicate_values()` for a guided, interactive cleanup of multi-value fields.
- To keep an audit trail of downloads/cleaning/exports, enable logging in the
  wizard or call `start_audit_log()` before running FormIOr functions.
