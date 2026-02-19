# Core Workflow (Most Common Path)

**Goal:** Download -&gt; Flatten -&gt; Clean -&gt; Diagnose -&gt; Export

<img src="assets/core-workflow.svg" alt="" width="100%" />

**CHEF credentials (quick visual):** API key and Form ID are in the
form’s **Manage** page.  
Form ID is the alphanumeric code after `=` in the Manage page URL.

<img src="assets/API.png" alt="" width="100%" /><img src="assets/FormID.png" alt="" width="100%" />

Default CHEF base URL used by FormIOr:

    https://submit.digital.gov.bc.ca/app/api/v1

Note (CHEF): Metadata/schema endpoints may live at `/api/v1`. FormIOr
will automatically retry the alternate base for metadata/schema requests
when needed.

1.  **Download responses**

<!-- -->

    raw <- GetResponses(form_id = "YOUR_FORM_ID")

Tip: Many cleaning helpers return a list with `out$data` (the cleaned
`data.frame`) and, when `return_flat = TRUE`, `out$flat` (an updated
FlattenSubmissions-style list). In this cheat sheet we keep both:

- `df` = the cleaned `data.frame` you typically analyze/export
- `flat` = the updated FlattenSubmissions list (when you need
  `FlatResponses` + metadata)

1.  **Flatten nested responses**

<!-- -->

    flat_raw <- FlattenSubmissions(raw)
    df <- flat_raw$FlatResponses

1.  **Normalize column names (recommended)**

<!-- -->

    norm <- NormalizeColumnNames(flat_raw, return_flat = TRUE)
    flat <- norm$flat
    df <- norm$data

1.  **Deduplicate (optional)**

<!-- -->

    dedup <- DeduplicateSubmissions(flat, id_col = "form_submissionid", return_flat = TRUE)
    flat <- dedup$flat
    df <- dedup$data

1.  **Resolve repeated answers (optional)**

<!-- -->

    resolved <- ResolveRepeats(flat, id_col = "form_submissionid", return_flat = TRUE)
    flat <- resolved$flat
    df <- resolved$data

1.  **Create a codebook**

<!-- -->

    codebook <- MakeCodebook(df)
    # Optional: include labels from the form schema
    # form_meta <- GetFormMetadata(form_id = "your-form-id", api_key = "your-api-key")
    # codebook <- MakeCodebook(df, form = form_meta, include = "all")

1.  **Run quick diagnostics**

<!-- -->

    summary_age <- SummaryByField(df, field = "age")
    PlotHistogram(df, "age")
    PlotBarSummary(df, "program")

1.  **Export to Excel**

<!-- -->

    sheets <- list(
      Raw_Flattened = flat_raw$FlatResponses,
      Cleaned_Data = df,
      Codebook = codebook,
      Summary_Age = summary_age$summary
    )
    ExportToExcel(sheets, path = file.path(tempdir(), "FormIOr_output.xlsx"), overwrite = TRUE)

# Helpful Extras (Common Add-Ons)

<img src="assets/helpers-grid.svg" alt="" width="100%" />

**Audit logging (highly recommended)**

    StartAuditLog(file.path(tempdir(), "audit_log.csv"))
    WriteAuditLog("flatten", details = "Flattened submissions")
    StopAuditLog()

**Targeted adjustments**

    updates <- data.frame(
      id = "sub_020",
      column = "region",
      value = "North",
      stringsAsFactors = FALSE
    )

    adj <- AdjustSubmissions(
      flat,
      id_col = "form_submissionid",
      delete_ids = c("sub_001", "sub_017"),
      updates = updates,
      return_flat = TRUE
    )
    flat <- adj$flat
    df <- adj$data

**Compact multi-select columns**

    comp <- CompactSelections(flat, return_flat = TRUE)
    flat <- comp$flat
    df <- comp$data
    # Optional: change the separator used to detect checkbox groups
    # flat <- CompactSelections(flat, sep = "-")

**Rename columns**

    RenameCols(flat)

**Cross-tab summaries**

    CrossTab(df, row = "region", col = "program")

**Response timelines**

    ResponseTimeline(df, date_col = "created")
    PlotResponseTimeline(df, date_col = "created")

**Pre/post comparability + suspicious responses**

    data("SuspiciousSurveyDemo", package = "FormIOr")

    risk <- FlagSuspiciousSubmissions(
      SuspiciousSurveyDemo,
      id_col = "submissionId",
      time_col = "created",
      cutoff_time = "2026-01-06 00:00:00",
      group_action = "split_only",
      action = "flag_only"
    )

    risk$comparability$non_comparable
    head(risk$flags)

# Wizard (Automated Workflow)

    out <- FormIOrWorkflow()

This will: - create an output folder - start or reuse an audit log -
download, flatten, clean, diagnose, and export - save a plan so you can
replay the workflow later

# Quick Reference: Key Functions

**Download** - `GetResponses()` - `GetSubmissions()` -
`GetFormMetadata()`

**Flatten & Clean** - `FlattenSubmissions()` -
`NormalizeColumnNames()` - `DeduplicateSubmissions()` -
`ResolveRepeats()` - `CompactSelections()` - `AdjustSubmissions()`

**Diagnostics** - `MakeCodebook()` - `SummaryByField()` - `CrossTab()` -
`PlotHistogram()` - `PlotBarSummary()` - `PlotWordcloud()` -
`PlotResponseTimeline()` - `FlagSuspiciousSubmissions()`

**Bundled Demo Data** - `SuspiciousSurveyDemo`

**Export & Logging** - `ExportToExcel()` - `StartAuditLog()` -
`WriteAuditLog()` - `StopAuditLog()`
