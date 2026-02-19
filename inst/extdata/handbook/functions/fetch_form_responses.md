
# fetch_form_responses

**Title:** Download responses from a FormIO form

## What this function is for

This function focuses on downloading or retrieving data from FormIO.

## Overview

Retrieves submissions ("responses") for a FormIO form using the export API.
This is one of the main entry points for getting data into FormIOr.

## How it fits into a workflow

This function typically appears after you have created (or loaded) a
flattened dataset. If you are using the wizard, this step is handled for
you, but the function is also safe to call directly in scripts.

## Usage

```r
fetch_form_responses(
  base_url = "https://submit.digital.gov.bc.ca/app/api/v1",
  form_id = NULL,
  api_key = NULL,
  drafts = FALSE,
  deleted = FALSE,
  content.only = TRUE,
  reenter.credentials = FALSE
)
```

## Key inputs

- `base_url`: Character. API base URL. Default:
`"https://submit.digital.gov.bc.ca/app/api/v1"`.
  Note (CHEF): Response export endpoints use `/app/api/v1`. Metadata/schema
  functions will automatically retry the alternate base (`/api/v1` <-> `/app/api/v1`)
  if needed.
- `form_id`: Character. Form ID. If `NULL`, uses stored credentials or
prompts via `AskCredentials()`.
- `api_key`: Character. API key/secret. If `NULL`, uses stored credentials
or prompts via `AskCredentials()`.
- `drafts`: Logical. Include draft submissions? Default `FALSE`.
- `deleted`: Logical. Include deleted submissions? Default `FALSE`.
- `content.only`: Logical or character.
- `TRUE` (default): return the submissions as a data frame/list
- `FALSE`: return a list with status, headers, and the parsed content
- `"raw"`: return the raw JSON response as text
- `reenter.credentials`: Logical. Force re-entry of credentials (default:
`FALSE`).

## Outputs

- (See `?fetch_form_responses` for return value details.)

## Details and behavior

Credentials:
- If `form_id` and `api_key` are `NULL`, FormIOr will use previously entered
credentials from the current R session (stored internally), or prompt via
`AskCredentials()`.

Audit logging:
- If audit logging is active (see `start_audit_log()`), the download action is
recorded (but your API key is never written to the log).

## Examples

```r
responses <- fetch_form_responses(form_id = "your-form-id", api_key = "your-api-key")
flat <- flatten_submission_records(responses)
```

## Notes and tips

- Turn on audit logging if you need a paper trail
- Keep raw flattened data in your export for traceability
- Prefer consistent column names before downstream diagnostics

---

Navigation
- Home: [Handbook Index](../index.md)
- Function Index: [All Functions](index.md)
- Prev: [fetch_form_metadata](fetch_form_metadata.md)
- Next: [Next Function](fetch_form_submissions.md)
