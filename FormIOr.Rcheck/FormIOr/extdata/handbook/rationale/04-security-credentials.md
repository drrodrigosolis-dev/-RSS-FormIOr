# Page 4: Security and Credentials

[Home](../index.md) | [Prev: Getting Started](03-getting-started.md) | [Next: Standard Workflow](../workflows/05-standard-workflow.md)

FormIOr does not store API keys on disk. Credentials are kept only in memory for
the current R session. This is important for safety and compliance.

You can provide credentials directly in function calls:

```r
responses <- GetResponses(form_id = "your-form-id", api_key = "your-api-key")
```

Or enter them interactively if prompted. The credentials are stored in memory
only and are not written to the audit log.

Tips:

- Do not paste credentials into shared scripts.
- Use environment variables if you need automated runs.
- Restarting R clears stored credentials.

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
screenshots showing where to obtain your API key and Form ID. The default
CHEF base URL used by FormIOr is:

```
https://submit.digital.gov.bc.ca/app/api/v1
```

Note (CHEF): Metadata/schema endpoints may live at `/api/v1`. FormIOr will
automatically retry the alternate base for metadata/schema requests when needed.

---

Navigation
- Prev: [Getting Started in R](03-getting-started.md)
- Next: [API Key and Form ID](05-api-formid.md)
- Home: [Handbook Index](../index.md)
