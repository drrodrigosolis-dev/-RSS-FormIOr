
# GetSubmissions

**Title:** Retrieve metadata for submissions of a specific form

## What this function is for

This function focuses on downloading or retrieving data from FormIO.

## Overview

Fetches a list of submission metadata (not the full submission data) from a
Digital.gov.bc.ca form, including drafts, completed, and deleted submissions.
If audit logging is active (see `StartAuditLog()`), this action is recorded.

## How it fits into a workflow

This function typically appears after you have created (or loaded) a
flattened dataset. If you are using the wizard, this step is handled for
you, but the function is also safe to call directly in scripts.

## Usage

```r
GetSubmissions(
  base_url = "https://submit.digital.gov.bc.ca/app/api/v1",
  form_id = NULL,
  api_key = NULL,
  content.only = TRUE,
  reenter.credentials = FALSE,
  AdditionalCols = c("")
)
```

## Key inputs

- `base_url`: Character. API base URL. Default: "https://submit.digital.gov.bc.ca/app/api/v1"
- `form_id`: Character. Form identifier. If NULL, read from stored credentials.
- `api_key`: Character. API key / secret. If NULL, read from stored credentials.
- `content.only`: Logical or character.
- `TRUE` (default): return cleaned data.frame of submission metadata
- `FALSE`: return full response object (status, headers, content)
- `"raw"`: return raw JSON response as text
- `reenter.credentials`: Logical. Force re-entry of credentials (default: FALSE)
- `AdditionalCols`: Character vector. Extra field names to request (passed to `fields` query param)

## Outputs

- (See `?GetSubmissions` for return value details.)

## Details and behavior

This function follows the package defaults and logs actions when audit logging is active.

## Examples

```r
# See ?GetSubmissions for examples
```

## Notes and tips

- Turn on audit logging if you need a paper trail
- Keep raw flattened data in your export for traceability
- Prefer consistent column names before downstream diagnostics

---

Navigation
- Home: [Handbook Index](../index.md)
- Function Index: [All Functions](index.md)
- Prev: [GetResponses](GetResponses.md)
- Next: [Next Function](IsAuditLogActive.md)
