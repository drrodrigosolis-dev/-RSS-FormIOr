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
`GetResponses()` / `GetSubmissions()`.

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

# 1) Download responses
responses <- GetResponses(form_id = "your-form-id", api_key = "your-api-key")

# 2) Flatten nested submissions
flat <- FlattenSubmissions(responses)

# 3) Optional: rename columns interactively
RenameCols(flat)

# 4) Simplify and clean
norm <- NormalizeColumnNames(flat)                # clean column names
resolved <- ResolveRepeats(flat)                  # collapse repeated answers
compact <- CompactSelections(flat)                # combine checkbox columns

dedup <- DeduplicateSubmissions(flat)             # remove duplicate submissions
```

## Quick Start: Guided Wizard

For most users, the easiest entry point is the guided workflow:

```r
out <- FormIOrWorkflow()
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
FormIOrAddin()
```

The addin provides tabs for downloading and flattening data, cleaning helpers,
exporting to Excel/CSV, summaries by field, and a FieldDictionary explorer.
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
`FlattenSubmissions()`.

### NormalizeColumnNames()
Standardizes column names for readability and consistency.

```r
norm <- NormalizeColumnNames(flat)
head(norm$data)
```

### ResolveRepeats()
Collapses repeated values within each submission ID using either a fixed strategy
or simple auto-detection.

```r
resolved <- ResolveRepeats(flat, id_col = "submissionId")
resolved$summary
```

### DeduplicateSubmissions()
Keeps one row per submission ID (uses a timestamp column when available).

```r
dedup <- DeduplicateSubmissions(flat, id_col = "submissionId")
```

### FlagSuspiciousSubmissions()
Checks whether pre/post phases can be pooled and flags suspicious rows
(duplicate IDs/fingerprints, rapid repeats).

```r
data("SuspiciousSurveyDemo", package = "FormIOr")

risk <- FlagSuspiciousSubmissions(
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

### CompactSelections()
Combines checkbox/multi-select columns into a single comma-separated column.

```r
compact <- CompactSelections(flat)
compact$summary
```

## Notes

- If you want the updated `FlattenSubmissions()` list in the output, pass
  `return_flat = TRUE` to any of the new cleaning functions.
- Use `FixDups()` for a guided, interactive cleanup of multi-value fields.
- To keep an audit trail of downloads/cleaning/exports, enable logging in the
  wizard or call `StartAuditLog()` before running FormIOr functions.
