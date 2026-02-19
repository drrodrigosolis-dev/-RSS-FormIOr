
# IsAuditLogActive

**Title:** Check whether audit logging is active

## What this function is for

This function manages audit logging to track dataset changes.

## Overview

This is most useful in scripts when you want to conditionally add a manual
note using `WriteAuditLog()` only when logging is enabled.

## How it fits into a workflow

This function typically appears after you have created (or loaded) a
flattened dataset. If you are using the wizard, this step is handled for
you, but the function is also safe to call directly in scripts.

## Usage

```r
IsAuditLogActive()
```

## Key inputs

- (See `?IsAuditLogActive` for full parameter list.)

## Outputs

- (See `?IsAuditLogActive` for return value details.)

## Details and behavior

This function follows the package defaults and logs actions when audit logging is active.

## Examples

```r
IsAuditLogActive()
```

## Notes and tips

- Turn on audit logging if you need a paper trail
- Keep raw flattened data in your export for traceability
- Prefer consistent column names before downstream diagnostics

---

Navigation
- Home: [Handbook Index](../index.md)
- Function Index: [All Functions](index.md)
- Prev: [GetSubmissions](GetSubmissions.md)
- Next: [Next Function](MakeCodebook.md)
