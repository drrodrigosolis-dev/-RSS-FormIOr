# Page 5: Standard Workflow (Download -> Clean -> Export)

[Home](../index.md) | [Prev: Security and Credentials](../rationale/04-security-credentials.md) | [Next: Manual Diagnostics](06-manual-diagnostics.md)

This is the recommended manual workflow when you do not want to use the wizard.
It is structured to be repeatable and easy to audit.

### Step-by-step explanation

1. Download responses using the FormIO export API
2. Flatten nested columns into a clean table
3. Normalize column names for readability
4. Identify the submission ID column
5. Resolve repeated answers (one row per submission)
6. Export to Excel and optionally create a codebook

### Example

```r
responses <- fetch_form_responses(form_id = "your-form-id", api_key = "your-api-key")
flat <- flatten_submission_records(responses)

norm <- standardize_column_names(flat, return_flat = TRUE, quiet = TRUE)
flat <- norm$flat

id_col <- "form_submissionid"

resolved <- collapse_repeated_values(flat, id_col = id_col, return_flat = TRUE, quiet = TRUE)
flat <- resolved$flat

codebook <- build_data_codebook(flat, quiet = TRUE)
sheets <- list(Responses = flat$FlatResponses, Codebook = codebook)
export_results_to_excel(sheets, path = file.path(tempdir(), "FormIOr_output.xlsx"), overwrite = TRUE)
```

### When to use this

- You want full control over each step
- You are building a script for repeatable reporting
- You do not want interactive prompts

---

Navigation
- Prev: [API Key and Form ID](../rationale/05-api-formid.md)
- Next: [Manual Diagnostics](06-manual-diagnostics.md)
- Home: [Handbook Index](../index.md)
