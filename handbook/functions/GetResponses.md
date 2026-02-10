
# GetResponses

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
GetResponses(
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

- (See `?GetResponses` for return value details.)

## Details and behavior

Credentials:
- If `form_id` and `api_key` are `NULL`, FormIOr will use previously entered
credentials from the current R session (stored internally), or prompt via
`AskCredentials()`.

Audit logging:
- If audit logging is active (see `StartAuditLog()`), the download action is
recorded (but your API key is never written to the log).

## Examples

```r
responses <- GetResponses(form_id = "your-form-id", api_key = "your-api-key")
flat <- FlattenSubmissions(responses)
```

## Notes and tips

- Turn on audit logging if you need a paper trail
- Keep raw flattened data in your export for traceability
- Prefer consistent column names before downstream diagnostics

---

Navigation
- Home: [Handbook Index](../index.md)
- Function Index: [All Functions](index.md)
- Prev: [GetFormMetadata](GetFormMetadata.md)
- Next: [Next Function](GetSubmissions.md)
