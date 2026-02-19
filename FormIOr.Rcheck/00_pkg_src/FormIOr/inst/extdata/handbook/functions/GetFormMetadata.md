
# GetFormMetadata

**Title:** Retrieve form metadata (name, versions, settings)

## What this function is for

This function focuses on downloading or retrieving data from FormIO.

## Overview

Downloads basic metadata for a FormIO form. This can be useful for:

## How it fits into a workflow

This function typically appears after you have created (or loaded) a
flattened dataset. If you are using the wizard, this step is handled for
you, but the function is also safe to call directly in scripts.

## Usage

```r
GetFormMetadata(
  base_url = "https://submit.digital.gov.bc.ca/app/api/v1",
  form_id = NULL,
  api_key = NULL,
  reenter.credentials = FALSE
)
```

## Key inputs

- `base_url`: API base URL. Default `"https://submit.digital.gov.bc.ca/app/api/v1"`.
  For CHEF, metadata/schema endpoints may live at `/api/v1`; FormIOr will
  automatically retry the alternate base if needed.
- `form_id`: Form ID. If `NULL`, credentials are read from `AskCredentials()`.
- `api_key`: API key/secret. If `NULL`, credentials are read from `AskCredentials()`.
- `reenter.credentials`: Logical. If `TRUE`, forces re-entry of credentials.

## Outputs

- (See `?GetFormMetadata` for return value details.)

## Details and behavior

- confirming you have the right form (title/name/version)
- naming output folders (the wizard uses the form title when it can)
- understanding version history (useful before comparing versions)

Note: this returns *metadata only*, not the full form schema/components. If
you want a field list and labels, use `DescribeForm()` or `FieldDictionary()`
(those functions can use the metadata to fetch the schema when credentials
are available).

If audit logging is active (see `StartAuditLog()`), this action is recorded.

For CHEF, FormIOr automatically retries the alternate base URL (`/api/v1` <-> `/app/api/v1`)
if the first request does not return valid JSON.

This function returns its result using `invisible()`. Assign it to a variable
to inspect it.

## Examples

```r
meta <- GetFormMetadata(form_id = "your-form-id", api_key = "your-api-key")
meta$title
str(meta$versions)
```

## Notes and tips

- Turn on audit logging if you need a paper trail
- Keep raw flattened data in your export for traceability
- Prefer consistent column names before downstream diagnostics

---

Navigation
- Home: [Handbook Index](../index.md)
- Function Index: [All Functions](index.md)
- Prev: [FormIOrWorkflow](FormIOrWorkflow.md)
- Next: [Next Function](GetResponses.md)
