# Page 24: Common Cases and How to Solve Them

This section collects practical situations people often face when working with
FormIO data and shows which FormIOr functions solve each case.

---

## Case 1: EditGrid numeric data creates multiple rows per submission

**Question:** I have budget data entered through an EditGrid component. Each
submission now has multiple rows, so my totals are split across rows. What do I do?

**Answer:** Use `collapse_repeated_values()` to collapse repeated rows into one row per
submission. For numeric values, the default auto strategy is **sum**. You can
override it with `strategy = "mean"` or `"last"` if needed.

```r
resolved <- collapse_repeated_values(flat, id_col = "form_submissionid", strategy = "sum", return_flat = TRUE)
flat <- resolved$flat
```

---

## Case 2: Checkbox columns are scattered across many fields

**Question:** My multi‑select question shows up as many `field-option` columns.
How do I combine them?

**Answer:** Use `collapse_checkbox_selections()` to collapse checkbox groups into a single
“selected” column.

```r
compacted <- collapse_checkbox_selections(flat, return_flat = TRUE)
flat <- compacted$flat
```

---

## Case 3: Duplicate submissions after edits or resubmits

**Question:** Some respondents edited their submissions, and I now see duplicates.
How do I keep only the latest?

**Answer:** Use `deduplicate_submission_rows()` to keep the most recent row per
submission ID. If you have a timestamp column, the function will use it.

```r
dedup <- deduplicate_submission_rows(flat, id_col = "form_submissionid", keep = "last", return_flat = TRUE)
flat <- dedup$flat
```

---

## Case 4: Remove test submissions or delete specific IDs

**Question:** I ran tests during form setup. How can I delete specific IDs?

**Answer:** Use `apply_submission_updates()` with `delete_ids`.

```r
clean <- apply_submission_updates(
  flat,
  id_col = "form_submissionid",
  delete_ids = c("sub_001", "sub_017"),
  return_flat = TRUE
)
flat <- clean$flat
```

---

## Case 5: Correct a value for a specific submission

**Question:** One submission has an incorrect value. Can I fix just that row?

**Answer:** Yes. Use `apply_submission_updates()` with an `updates` data.frame.

```r
updates <- data.frame(
  id = "sub_020",
  column = "region",
  value = "North",
  stringsAsFactors = FALSE
)

clean <- apply_submission_updates(flat, id_col = "form_submissionid", updates = updates, return_flat = TRUE)
flat <- clean$flat
```

---

## Case 6: Identify which columns have repeated values

**Question:** I’m not sure which questions repeat within each submission.

**Answer:** Run `count_multivalue_fields()` to see which columns have more than one
value per submission.

```r
multi_counts <- count_multivalue_fields(flat)
```

---

## Case 7: Build a codebook with labels and sections

**Question:** I need a codebook that includes question labels and sections.

**Answer:** Provide the form metadata or schema and use `build_data_codebook()`.

```r
form_meta <- fetch_form_metadata(form_id = "your-form-id", api_key = "your-api-key")
codebook <- build_data_codebook(flat, form = form_meta, include = "all")
```

---

## Case 8: Compare two versions of the form

**Question:** The form changed over time. How do I see what changed?

**Answer:** Use `compare_form_versions()` with form metadata or schema.

```r
meta <- fetch_form_metadata(form_id = "your-form-id", api_key = "your-api-key")
changes <- compare_form_versions(meta, meta, old_version = 1, new_version = "latest")
```

---

## Case 9: Quick counts by two fields (cross‑tabs)

**Question:** I want a quick table of `region` by `program`.

**Answer:** Use `tabulate_field_by_group()`.

```r
xt <- tabulate_field_by_group(flat, row = "region", col = "program", percent = "row")
```

---

## Case 10: Trends over time

**Question:** I want to see submissions over time.

**Answer:** Use `summarize_response_timeline()` for the data and `plot_response_timeline()`
for a quick plot.

```r
timeline <- summarize_response_timeline(flat, date_col = "created")
plot_response_timeline(flat, date_col = "created")
```

---

## Case 11: Summarize one question quickly

**Question:** I need a quick summary of one field.

**Answer:** Use `summarize_field_distribution()`.

```r
summary_age <- summarize_field_distribution(flat, field = "age")
```

---

## Case 12: Export even if Excel packages are not installed

**Question:** I tried to export but don’t have Excel libraries.

**Answer:** `export_results_to_excel()` will fall back to CSV if no Excel writer is
available. You can also force CSV by using a `.csv` path.

```r
export_results_to_excel(flat$FlatResponses, path = file.path(tempdir(), "FormIOr_output.csv"), overwrite = TRUE)
```

---

## Case 13: Work without downloading

**Question:** I already have a data frame. Can I use FormIOr anyway?

**Answer:** Yes. Pass your data directly to `flatten_submission_records()` or the wizard.

```r
flat <- flatten_submission_records(my_data_frame)
# or
out <- run_form_processing_workflow(data = my_data_frame)
```

---

## Case 14: Keep an audit trail of changes

**Question:** I need an audit log showing each transformation.

**Answer:** Start a log before you begin. Most functions will append entries
automatically.

```r
start_audit_log(file.path(tempdir(), "audit_log.csv"), append = TRUE)
# ...run cleaning steps...
stop_audit_log()
```

---

## Case 15: Anonymous phase vs identity-verified phase may not be comparable

**Question:** We collected anonymous responses for a few days, then switched to
identity verification. Can we analyze all responses together?

**Answer:** Use `detect_suspicious_submissions()` with a cutoff timestamp. If the
pre/post groups differ materially, do not pool them. Analyze separately or omit
one phase.

Tip: try this workflow first with the bundled `SuspiciousSurveyDemo` dataset.

```r
# data("SuspiciousSurveyDemo", package = "FormIOr")
# df <- SuspiciousSurveyDemo

res <- detect_suspicious_submissions(
  df,
  id_col = "submissionId",
  time_col = "created",
  cutoff_time = "2026-01-06 00:00:00",
  group_action = "split_only"
)

if (isTRUE(res$comparability$non_comparable)) {
  # Usually preferred when post phase is identity-verified:
  post_only <- detect_suspicious_submissions(
    df,
    id_col = "submissionId",
    time_col = "created",
    cutoff_time = "2026-01-06 00:00:00",
    group_action = "omit_pre"
  )$data
}
```

---

Navigation
- Home: [Handbook Index](../index.md)
- Prev: [Wizard Plans](../technical/wizard-plans.md)
- Next: [Tips and Quality Checks](../tips/tips-and-checks.md)
