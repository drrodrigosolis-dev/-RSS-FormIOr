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

Credential shortcut (skip prompts):

    # Store credentials once for this R session (non-interactive)
    AskCredentials(form_id = "YOUR_FORM_ID", api = "YOUR_API_KEY")

    # You can then call API functions without repeating credentials each time
    # raw <- fetch_form_responses()

1.  **Download responses**

<!-- -->

    raw <- fetch_form_responses(form_id = "YOUR_FORM_ID", api_key = "YOUR_API_KEY")
    # or, if already cached with AskCredentials():
    # raw <- fetch_form_responses()

Tip: Many cleaning helpers return a list with `out$data` (the cleaned
`data.frame`) and, when `return_flat = TRUE`, `out$flat` (an updated
flatten_submission_records-style list). In this cheat sheet we keep both:

- `df` = the cleaned `data.frame` you typically analyze/export
- `flat` = the updated flatten_submission_records list (when you need
  `FlatResponses` + metadata)

1.  **Flatten nested responses**

<!-- -->

    flat_raw <- flatten_submission_records(raw)
    df <- flat_raw$FlatResponses

1.  **Normalize column names (recommended)**

<!-- -->

    norm <- standardize_column_names(flat_raw, return_flat = TRUE)
    flat <- norm$flat
    df <- norm$data

1.  **Deduplicate (optional)**

<!-- -->

    dedup <- deduplicate_submission_rows(flat, id_col = "form_submissionid", return_flat = TRUE)
    flat <- dedup$flat
    df <- dedup$data

1.  **Resolve repeated answers (optional)**

<!-- -->

    resolved <- collapse_repeated_values(flat, id_col = "form_submissionid", return_flat = TRUE)
    flat <- resolved$flat
    df <- resolved$data

1.  **Create a codebook**

<!-- -->

    codebook <- build_data_codebook(df)
    # Optional: include labels from the form schema
    # form_meta <- fetch_form_metadata(form_id = "your-form-id", api_key = "your-api-key")
    # codebook <- build_data_codebook(df, form = form_meta, include = "all")

1.  **Run quick diagnostics**

<!-- -->

    summary_age <- summarize_field_distribution(df, field = "age")
    plot_numeric_distribution(df, "age")
    plot_categorical_summary(df, "program")

1.  **Export to Excel**

<!-- -->

    sheets <- list(
      Raw_Flattened = flat_raw$FlatResponses,
      Cleaned_Data = df,
      Codebook = codebook,
      Summary_Age = summary_age$summary
    )
    export_results_to_excel(sheets, path = file.path(tempdir(), "FormIOr_output.xlsx"), overwrite = TRUE)

# Helpful Extras (Common Add-Ons)

<img src="assets/helpers-grid.svg" alt="" width="100%" />

**Audit logging (highly recommended)**

    start_audit_log(file.path(tempdir(), "audit_log.csv"))
    append_audit_log_entry("flatten", details = "Flattened submissions")
    stop_audit_log()

**Targeted adjustments**

    updates <- data.frame(
      id = "sub_020",
      column = "region",
      value = "North",
      stringsAsFactors = FALSE
    )

    adj <- apply_submission_updates(
      flat,
      id_col = "form_submissionid",
      delete_ids = c("sub_001", "sub_017"),
      updates = updates,
      return_flat = TRUE
    )
    flat <- adj$flat
    df <- adj$data

**Compact multi-select columns**

    comp <- collapse_checkbox_selections(flat, return_flat = TRUE)
    flat <- comp$flat
    df <- comp$data
    # Optional: change the separator used to detect checkbox groups
    # flat <- collapse_checkbox_selections(flat, sep = "-")

**Rename columns**

    # Interactive
    rename_columns_from_dictionary(flat)

    # Non-interactive
    # rename_map <- data.frame(
    #   OldNames = c("form_submissionid", "form_created_at"),
    #   NewNames = c("submission_id", "created_at"),
    #   stringsAsFactors = FALSE
    # )
    # rename_columns_from_dictionary(flat, rename_map = rename_map, quiet = TRUE)

**Add column sections**

    # Fully non-interactive example
    # assign_section_hierarchy(
    #   flat,
    #   depth = 1,
    #   section_names = list(c("Core")),
    #   section_rows = list(list("1:10"))  # same order as section_names
    # )

**Cross-tab summaries**

    tabulate_field_by_group(df, row = "region", col = "program")

**Response timelines**

    summarize_response_timeline(df, date_col = "created")
    plot_response_timeline(df, date_col = "created")

**Pre/post comparability + suspicious responses**

    data("SuspiciousSurveyDemo", package = "FormIOr")

    risk <- detect_suspicious_submissions(
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

    out <- run_form_processing_workflow()

This will: - create an output folder - start or reuse an audit log -
download, flatten, clean, diagnose, and export - save a plan so you can
replay the workflow later

# Quick Reference: Key Functions

**Download** - `fetch_form_responses()` - `fetch_form_submissions()` -
`fetch_form_metadata()`

**Flatten & Clean** - `flatten_submission_records()` -
`standardize_column_names()` - `deduplicate_submission_rows()` -
`collapse_repeated_values()` - `collapse_checkbox_selections()` - `apply_submission_updates()`

**Diagnostics** - `build_data_codebook()` - `summarize_field_distribution()` - `tabulate_field_by_group()` -
`plot_numeric_distribution()` - `plot_categorical_summary()` - `plot_word_cloud()` -
`plot_response_timeline()` - `detect_suspicious_submissions()`

**Bundled Demo Data** - `SuspiciousSurveyDemo`

**Export & Logging** - `export_results_to_excel()` - `start_audit_log()` -
`append_audit_log_entry()` - `stop_audit_log()`
