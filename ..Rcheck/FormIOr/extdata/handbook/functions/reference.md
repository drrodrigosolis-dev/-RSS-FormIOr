# Function Reference (Quick Lookup)

This page lists every exported FormIOr function with a short description. 
Click a function name to open its full handbook page.

| Function | Summary |
| --- | --- |
| [append_audit_log_entry](append_audit_log_entry.md) | This writes a small CSV/TSV log of key actions (e.g., downloads, cleaning, exports). It is designed to be easy for non-technical users to open in Excel. If you are using automatic logging, you gene... |
| [apply_submission_updates](apply_submission_updates.md) | Sometimes you need to make small, targeted fixes before you export: remove test submissions, correct a value for one submission, or blank out a field for privacy reasons. |
| [AskCredentials](AskCredentials.md) | Collects the Form ID and API key and caches them for this R session. |
| [assign_section_hierarchy](assign_section_hierarchy.md) | Assigns hierarchical section labels to columns in flattened FormIO response data (up to 3 levels deep). You can run it interactively with prompts, or non-interactively by supplying depth, section_n... |
| [build_data_codebook](build_data_codebook.md) | This produces a plain-language table that documents each column in your data. It can optionally use a FormIO schema (via build_field_dictionary()) to add labels, sections, and descriptions. If audi... |
| [build_field_dictionary](build_field_dictionary.md) | This creates a clean table that lists each field in your form. It is meant to be easy to read and share with non-technical staff. If audit logging is active (see start_audit_log()), this action is ... |
| [collapse_checkbox_selections](collapse_checkbox_selections.md) | When a question generates multiple TRUE/FALSE columns (e.g., select boxes), this function combines them into a single comma-separated column. If audit logging is active (see start_audit_log()), thi... |
| [collapse_repeated_values](collapse_repeated_values.md) | Automatically collapses repeated values within each submission ID using a consistent strategy (or simple heuristics when strategy = "auto"). This is a non-interactive alternative to resolve_duplica... |
| [compare_form_versions](compare_form_versions.md) | Shows what changed between two form versions: fields added, removed, or updated. If audit logging is active (see start_audit_log()), this action is recorded. |
| [count_multivalue_fields](count_multivalue_fields.md) | Creates a diagnostic data frame with the same shape as the flattened responses, where each original value (except in the ID column) is replaced by the number of distinct non-NA values that appear i... |
| [deduplicate_submission_rows](deduplicate_submission_rows.md) | Keeps one row per submission ID, using a timestamp column when available (for example created or modified), otherwise keeps first/last row. If audit logging is active (see start_audit_log()), this ... |
| [describe_form_schema](describe_form_schema.md) | This is a simple overview of a form: the name, version, and how many fields it contains. It is meant for non-technical users who want a quick summary. If audit logging is active (see start_audit_lo... |
| [detect_suspicious_submissions](detect_suspicious_submissions.md) | Evaluates whether pre/post time groups are statistically comparable and produces row-level suspiciousness flags. This is designed for workflows where a survey changed mid-collection (for example, f... |
| [export_results_to_excel](export_results_to_excel.md) | This is a simple, user-friendly export helper for FormIOr outputs. It accepts a data.frame, a list of data.frames (multiple sheets), or a list returned by fetch_form_responses() / flatten_submissio... |
| [fetch_form_metadata](fetch_form_metadata.md) | Fetches form metadata from the API and retries the alternate base URL when parsing fails (CHEF compatibility behavior). |
| [fetch_form_responses](fetch_form_responses.md) | Downloads response export JSON from a form endpoint and returns parsed submissions by default. |
| [fetch_form_submissions](fetch_form_submissions.md) | Fetches submission-level metadata (not full response payloads) from a 'FormIO' endpoint. |
| [flatten_submission_records](flatten_submission_records.md) | This function will flatten submissions coming from a FormIO list that has nested elements |
| [is_audit_log_active](is_audit_log_active.md) | This is most useful in scripts when you want to conditionally add a manual note using append_audit_log_entry() only when logging is enabled. |
| [launch_formior_addin](launch_formior_addin.md) | Provides a minimal point-and-click interface for key FormIOr functions. |
| [plot_categorical_summary](plot_categorical_summary.md) | If audit logging is active (see start_audit_log()), this action is recorded. |
| [plot_numeric_distribution](plot_numeric_distribution.md) | If audit logging is active (see start_audit_log()), this action is recorded. |
| [plot_response_timeline](plot_response_timeline.md) | Convenience wrapper around summarize_response_timeline() that draws a simple line chart using base graphics. If audit logging is active (see start_audit_log()), this action is recorded. |
| [plot_word_cloud](plot_word_cloud.md) | Requires the optional wordcloud package. If it is not installed, the function will stop with a helpful message. If audit logging is active (see start_audit_log()), this action is recorded. |
| [rename_columns_from_dictionary](rename_columns_from_dictionary.md) | Renames columns in a list returned by flatten_submission_records(). You can run it interactively (prompt for each column) or non-interactively with rename_map. |
| [resolve_duplicate_values](resolve_duplicate_values.md) | Interactive function designed for users with little or no R experience. It helps resolve columns that contain multiple values per submission (e.g. repeating sections, "add another" items, multi-sel... |
| [review_duplicate_rows](review_duplicate_rows.md) | This function walks you through duplicate groups defined by key columns (for example email, username, project_id). For each duplicate group, it shows selected columns so you can compare submissions... |
| [run_form_processing_workflow](run_form_processing_workflow.md) | This interactive helper walks you through a complete FormIOr workflow: downloading responses, flattening and cleaning the data, generating simple diagnostics (like a codebook and basic plots), and ... |
| [standardize_column_names](standardize_column_names.md) | Designed for non-technical users who want consistent column names after downloading FormIO submissions. Works with either a raw data frame or the list returned by flatten_submission_records(). If a... |
| [start_audit_log](start_audit_log.md) | Creates a new audit log file and turns on automatic logging for FormIOr functions. Each subsequent action (downloads, cleaning, exports, etc.) will append a new row to the log. |
| [stop_audit_log](stop_audit_log.md) | Turns off automatic logging. The log file is not deleted. You can start a new log later using start_audit_log(). |
| [summarize_field_distribution](summarize_field_distribution.md) | Designed for non-technical users: it accepts either a data frame or the list produced by flatten_submission_records() and returns a simple summary. Numeric fields get descriptive statistics; catego... |
| [summarize_response_timeline](summarize_response_timeline.md) | Counts responses per day/week/month (or hour), using a timestamp column. If date_col is NULL, the function tries common timestamp names automatically (e.g., created, modified, _createdAt). If audit... |
| [tabulate_field_by_group](tabulate_field_by_group.md) | Builds a simple cross-tabulation between two columns, returning both a wide table and a long table that includes counts and (optional) percents. If audit logging is active (see start_audit_log()), ... |

---

Navigation
- Home: [Handbook Index](../index.md)
- Function Index: [All Functions](index.md)
