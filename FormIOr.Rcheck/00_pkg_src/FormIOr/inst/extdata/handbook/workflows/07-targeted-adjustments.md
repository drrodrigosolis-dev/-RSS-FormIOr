
# Page 7: Targeted Adjustments

Sometimes a dataset needs a small, precise correction rather than a full
reprocessing. Examples include deleting a specific submission ID, or
modifying a single field for a few submissions based on audit notes.

FormIOr includes a targeted adjustment helper so you can document exactly
what changed and why. These changes are safer than manual edits in Excel,
and they are recorded in the audit log if logging is enabled.

## Examples of targeted fixes

- Remove a handful of test submissions from early pilot runs
- Replace a value for a known incorrect submission ID
- Add a corrected value for a field that was missing in the original

## Typical flow

```r
flat <- FlattenSubmissions(raw)
norm <- NormalizeColumnNames(flat, return_flat = TRUE)
flat <- norm$flat

updates <- data.frame(
  id = "sub_020",
  column = "region",
  value = "North",
  stringsAsFactors = FALSE
)

flat_adj <- AdjustSubmissions(
  flat,
  id_col = "form_submissionid",
  delete_ids = c("sub_001", "sub_017"),
  updates = updates,
  return_flat = TRUE
)
```

## Tips

- Always keep a copy of the original flattened data in your output file
- Use the audit log to document who authorized the change and why
- Prefer targeted adjustments to manual edits in external tools

---

Navigation
- Home: [Handbook Index](../index.md)
- Prev: [Manual Diagnostics](06-manual-diagnostics.md)
- Next: [Repeatable Manual Workflows](08-repeatable-manual.md)
