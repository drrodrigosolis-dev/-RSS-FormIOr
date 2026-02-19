
# Page 6: Manual Diagnostics (Codebook + Summaries + Plots)

If you are not using the wizard, you can still generate diagnostics in a
clear, repeatable way. The manual diagnostics workflow focuses on three
outputs that help you understand what the form collected and whether any
cleaning is needed before analysis or reporting:

1. A codebook (field names, labels, types, and options)
2. Summary tables (counts, missingness, key distributions)
3. Plots (simple visuals that highlight patterns and issues)

## Typical sequence

1. Flatten your data (if you started from submissions or nested JSON).
2. Normalize names so your columns are consistent.
3. Create the codebook and save it with the rest of your outputs.
4. Generate summary tables for the fields that matter most.
5. Create plots for key questions or important outcomes.

## Recommended manual flow

First, make sure you have a `raw` object. This is the unflattened data returned
by `GetResponses()` (or any data.frame of submissions you already have):

```r
# Option A: download from FormIO
raw <- GetResponses(form_id = "your-form-id", api_key = "your-api-key")

# Option B: use an existing data.frame of submissions
# raw <- your_data_frame
```

```r
flat <- FlattenSubmissions(raw)
norm <- NormalizeColumnNames(flat, return_flat = TRUE)
flat <- norm$flat
codebook <- MakeCodebook(flat)
# Optional: include labels from the form schema
# form_meta <- GetFormMetadata(form_id = "your-form-id", api_key = "your-api-key")
# codebook <- MakeCodebook(norm, form = form_meta, include = "all")

# Summary examples (one field at a time)
summary_age <- SummaryByField(flat, field = "age")
summary_program <- SummaryByField(flat, field = "program")
summary_region <- SummaryByField(flat, field = "region")

PlotHistogram(flat, "age")
PlotBarSummary(flat, "program")
```

## What to look for

- Fields with unexpected missingness or blank values
- Options that appear in the data but not in the form schema
- Repeated answers that should have been one value per submission
- Numeric fields that look like text due to formatting problems

## If you need a paper trail

Turn on audit logging before diagnostics to capture the fact that you
created a codebook or produced plots. This helps with reporting and
repeatability for future reviews.

---

Navigation
- Home: [Handbook Index](../index.md)
- Prev: [Standard Workflow](05-standard-workflow.md)
- Next: [Targeted Adjustments](07-targeted-adjustments.md)
