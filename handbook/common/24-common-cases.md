# Page 24: Common Cases and How to Solve Them

This section collects practical situations people often face when working with
FormIO data and shows which FormIOr functions solve each case.

---

## Case 1: EditGrid numeric data creates multiple rows per submission

**Question:** I have budget data entered through an EditGrid component. Each
submission now has multiple rows, so my totals are split across rows. What do I do?

**Answer:** Use `ResolveRepeats()` to collapse repeated rows into one row per
submission. For numeric values, the default auto strategy is **sum**. You can
override it with `strategy = "mean"` or `"last"` if needed.

```r
resolved <- ResolveRepeats(flat, id_col = "form_submissionid", strategy = "sum", return_flat = TRUE)
flat <- resolved$flat
```

---

## Case 2: Checkbox columns are scattered across many fields

**Question:** My multi‑select question shows up as many `field-option` columns.
How do I combine them?

**Answer:** Use `CompactSelections()` to collapse checkbox groups into a single
“selected” column.

```r
compacted <- CompactSelections(flat, return_flat = TRUE)
flat <- compacted$flat
```

---

## Case 3: Duplicate submissions after edits or resubmits

**Question:** Some respondents edited their submissions, and I now see duplicates.
How do I keep only the latest?

**Answer:** Use `DeduplicateSubmissions()` to keep the most recent row per
submission ID. If you have a timestamp column, the function will use it.

```r
dedup <- DeduplicateSubmissions(flat, id_col = "form_submissionid", keep = "last", return_flat = TRUE)
flat <- dedup$flat
```

---

## Case 4: Remove test submissions or delete specific IDs

**Question:** I ran tests during form setup. How can I delete specific IDs?

**Answer:** Use `AdjustSubmissions()` with `delete_ids`.

```r
clean <- AdjustSubmissions(
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

**Answer:** Yes. Use `AdjustSubmissions()` with an `updates` data.frame.

```r
updates <- data.frame(
  id = "sub_020",
  column = "region",
  value = "North",
  stringsAsFactors = FALSE
)

clean <- AdjustSubmissions(flat, id_col = "form_submissionid", updates = updates, return_flat = TRUE)
flat <- clean$flat
```

---

## Case 6: Identify which columns have repeated values

**Question:** I’m not sure which questions repeat within each submission.

**Answer:** Run `findMultilines()` to see which columns have more than one
value per submission.

```r
multi_counts <- findMultilines(flat)
```

---

## Case 7: Build a codebook with labels and sections

**Question:** I need a codebook that includes question labels and sections.

**Answer:** Provide the form metadata or schema and use `MakeCodebook()`.

```r
form_meta <- GetFormMetadata(form_id = "your-form-id", api_key = "your-api-key")
codebook <- MakeCodebook(flat, form = form_meta, include = "all")
```

---

## Case 8: Compare two versions of the form

**Question:** The form changed over time. How do I see what changed?

**Answer:** Use `CompareFormVersions()` with form metadata or schema.

```r
meta <- GetFormMetadata(form_id = "your-form-id", api_key = "your-api-key")
changes <- CompareFormVersions(meta, meta, old_version = 1, new_version = "latest")
```

---

## Case 9: Quick counts by two fields (cross‑tabs)

**Question:** I want a quick table of `region` by `program`.

**Answer:** Use `CrossTab()`.

```r
xt <- CrossTab(flat, row = "region", col = "program", percent = "row")
```

---

## Case 10: Trends over time

**Question:** I want to see submissions over time.

**Answer:** Use `ResponseTimeline()` for the data and `PlotResponseTimeline()`
for a quick plot.

```r
timeline <- ResponseTimeline(flat, date_col = "created")
PlotResponseTimeline(flat, date_col = "created")
```

---

## Case 11: Summarize one question quickly

**Question:** I need a quick summary of one field.

**Answer:** Use `SummaryByField()`.

```r
summary_age <- SummaryByField(flat, field = "age")
```

---

## Case 12: Export even if Excel packages are not installed

**Question:** I tried to export but don’t have Excel libraries.

**Answer:** `ExportToExcel()` will fall back to CSV if no Excel writer is
available. You can also force CSV by using a `.csv` path.

```r
ExportToExcel(flat$FlatResponses, path = "FormIOr_output.csv", overwrite = TRUE)
```

---

## Case 13: Work without downloading

**Question:** I already have a data frame. Can I use FormIOr anyway?

**Answer:** Yes. Pass your data directly to `FlattenSubmissions()` or the wizard.

```r
flat <- FlattenSubmissions(my_data_frame)
# or
out <- FormIOrWorkflow(data = my_data_frame)
```

---

## Case 14: Keep an audit trail of changes

**Question:** I need an audit log showing each transformation.

**Answer:** Start a log before you begin. Most functions will append entries
automatically.

```r
StartAuditLog("audit_log.csv", append = TRUE)
# ...run cleaning steps...
StopAuditLog()
```

---

Navigation
- Home: [Handbook Index](../index.md)
- Prev: [Wizard Plans](../technical/wizard-plans.md)
- Next: [Tips and Quality Checks](../tips/tips-and-checks.md)
