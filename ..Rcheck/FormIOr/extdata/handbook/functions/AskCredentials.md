# AskCredentials

**Title:** Store FormIO credentials for the current R session

## What this function is for

Use this function to set (or refresh) the Form ID and API key that FormIOr
uses for API calls.

## Overview

`AskCredentials()` now supports both interactive and non-interactive use:

- If you provide both `form_id` and `api`/`api_key`, no prompt is shown.
- If one value is missing, it prompts only for the missing one.
- If both are missing, it prompts for both in an interactive session.

Credentials are cached in memory for the current R session and reused by
functions like `fetch_form_responses()`, `fetch_form_submissions()`, and `fetch_form_metadata()`.

## Usage

```r
AskCredentials(form_id = NULL, api = NULL, api_key = NULL)
```

## Key inputs

- `form_id`: Optional Form ID.
- `api`: Optional API key alias.
- `api_key`: Optional API key. If both `api` and `api_key` are provided, they
  must match.

## Outputs

- Named character vector: `c(ID = "...", Key = "...")`.

## Examples

```r
# Fully non-interactive (no prompts)
AskCredentials(form_id = "myformID", api = "myapiToken")

# Also valid: use api_key
AskCredentials(form_id = "myformID", api_key = "myapiToken")

# Interactive fallback (prompts only for missing value)
AskCredentials(form_id = "myformID")
```

## Notes and tips

- Credentials are kept in memory only (not saved to disk).
- Credential values are never written to the audit log.
- Restarting R clears stored credentials.

---

Navigation
- Home: [Handbook Index](../index.md)
- Function Index: [All Functions](index.md)
- Prev: [apply_submission_updates](apply_submission_updates.md)
- Next: [collapse_checkbox_selections](collapse_checkbox_selections.md)
