
# is_audit_log_active

**Title:** Check whether audit logging is active

## What this function is for

This function manages audit logging to track dataset changes.

## Overview

This is most useful in scripts when you want to conditionally add a manual
note using `append_audit_log_entry()` only when logging is enabled.

## How it fits into a workflow

This function typically appears after you have created (or loaded) a
flattened dataset. If you are using the wizard, this step is handled for
you, but the function is also safe to call directly in scripts.

## Usage

```r
is_audit_log_active()
```

## Key inputs

- (See `?is_audit_log_active` for full parameter list.)

## Outputs

- (See `?is_audit_log_active` for return value details.)

## Details and behavior

This function follows the package defaults and logs actions when audit logging is active.

## Examples

```r
is_audit_log_active()
```

## Notes and tips

- Turn on audit logging if you need a paper trail
- Keep raw flattened data in your export for traceability
- Prefer consistent column names before downstream diagnostics

---

Navigation
- Home: [Handbook Index](../index.md)
- Function Index: [All Functions](index.md)
- Prev: [fetch_form_submissions](fetch_form_submissions.md)
- Next: [Next Function](build_data_codebook.md)
