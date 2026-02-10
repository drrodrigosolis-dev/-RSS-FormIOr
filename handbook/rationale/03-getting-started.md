# Page 3: Getting Started in R

[Home](../index.md) | [Prev: Core Concepts](02-core-concepts.md) | [Next: Security and Credentials](04-security-credentials.md)

The simplest way to start is to use the wizard:

```r
library(FormIOr)
out <- FormIOrWorkflow()
```

If you want a manual workflow, use this minimal sequence:

```r
library(FormIOr)

responses <- GetResponses(form_id = "your-form-id", api_key = "your-api-key")
flat <- FlattenSubmissions(responses)

norm <- NormalizeColumnNames(flat, return_flat = TRUE, quiet = TRUE)
flat <- norm$flat

id_col <- "form_submissionid"  # confirm this name in your data

resolved <- ResolveRepeats(flat, id_col = id_col, return_flat = TRUE, quiet = TRUE)
flat <- resolved$flat

ExportToExcel(flat$FlatResponses, path = "FormIOr_output.xlsx", overwrite = TRUE)
```

If you prefer to skip downloads and work with a data frame you already have,
pass it directly into `FlattenSubmissions()` or `FormIOrWorkflow(data = ...)`.

```
┌────────────────────────────────────────────────────────────────────┐
│ CHEF credentials tip                                                │
│ - API key and Form ID are in the form’s Manage page                 │
│ - Form ID is the alphanumeric code after “=” in the URL             │
│ - See: How to Obtain the API Key and Form ID (CHEF)                 │
│   ../rationale/05-api-formid.md                                     │
└────────────────────────────────────────────────────────────────────┘
```

If you are using CHEF (BC Public Service FormIO), see the next page for
screenshots showing where to obtain your API key and Form ID.

---

Navigation
- Prev: [Core Concepts and Data Shapes](02-core-concepts.md)
- Next: [Security and Credentials](04-security-credentials.md)
- Home: [Handbook Index](../index.md)
