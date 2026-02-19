
# Page 8: Repeatable Manual Workflows

Repeatability is a core goal of FormIOr. Even if you do not use the wizard,
you can still make your workflow repeatable by saving your steps in a script
or by recording the same sequence of function calls each time.

## A simple repeatable template

```r
raw <- fetch_form_responses(form_id = "...")
flat <- flatten_submission_records(raw)
norm <- standardize_column_names(flat, return_flat = TRUE)
flat <- norm$flat
dedup <- deduplicate_submission_rows(flat, return_flat = TRUE)
flat <- dedup$flat
resolved <- collapse_repeated_values(flat, return_flat = TRUE)
flat <- resolved$flat
codebook <- build_data_codebook(flat)
# Optional: include labels from the form schema
# form_meta <- fetch_form_metadata(form_id = "your-form-id", api_key = "your-api-key")
# codebook <- build_data_codebook(flat, form = form_meta, include = "all")
sheets <- list(Data = flat$FlatResponses, Codebook = codebook)
export_results_to_excel(sheets, "output.xlsx", overwrite = TRUE)
```

## Good repeatability habits

- Keep all outputs in a dedicated folder for each run
- Use consistent naming (include the form name and timestamp)
- Turn on audit logging for every run
- Store your script alongside the output folder

## When to prefer the wizard

If you want built-in prompts, logging, and the ability to replay prior
choices, use the wizard. If you want full control or to integrate into a
larger script, stay manual.

---

Navigation
- Home: [Handbook Index](../index.md)
- Prev: [Targeted Adjustments](07-targeted-adjustments.md)
- Next: [Wizard Overview](../wizard/09-wizard-overview.md)
