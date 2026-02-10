
# AskCredentials

**Title:** Enter FormIO credentials

## What this function is for

This function supports the FormIOr workflow by handling a specific task.

## Overview

Prompts for the Form ID and API key used to access the FormIO API.
This is usually called automatically by `GetResponses()` and related
functions when credentials are not already stored.

## How it fits into a workflow

This function typically appears after you have created (or loaded) a
flattened dataset. If you are using the wizard, this step is handled for
you, but the function is also safe to call directly in scripts.

## Usage

```r
AskCredentials()
```

## Key inputs

- (See `?AskCredentials` for full parameter list.)

## Outputs

- (See `?AskCredentials` for return value details.)

## Details and behavior

If audit logging is active (see `StartAuditLog()`), the action is recorded,
but the credentials themselves are **not** written to the log.

## Examples

```r
dontshow{if (interactive()) withAutoprint({ # examplesIf}
AskCredentials()
dontshow{}) # examplesIf}
```

## Notes and tips

- Turn on audit logging if you need a paper trail
- Keep raw flattened data in your export for traceability
- Prefer consistent column names before downstream diagnostics

---

Navigation
- Home: [Handbook Index](../index.md)
- Function Index: [All Functions](index.md)
- Prev: [AdjustSubmissions](AdjustSubmissions.md)
- Next: [Next Function](CompactSelections.md)
