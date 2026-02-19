
# stop_audit_log

**Title:** Stop audit logging for FormIOr

## What this function is for

This function manages audit logging to track dataset changes.

## Overview

Turns off automatic logging. The log file is not deleted.
You can start a new log later using `start_audit_log()`.

## How it fits into a workflow

This function typically appears after you have created (or loaded) a
flattened dataset. If you are using the wizard, this step is handled for
you, but the function is also safe to call directly in scripts.

## Usage

```r
stop_audit_log(quiet = FALSE)
```

## Key inputs

- `quiet`: Logical. If `FALSE`, prints a short message.

## Outputs

- (See `?stop_audit_log` for return value details.)

## Details and behavior

This function follows the package defaults and logs actions when audit logging is active.

## Examples

```r
stop_audit_log()
```

## Notes and tips

- Turn on audit logging if you need a paper trail
- Keep raw flattened data in your export for traceability
- Prefer consistent column names before downstream diagnostics

---

Navigation
- Home: [Handbook Index](../index.md)
- Function Index: [All Functions](index.md)
- Prev: [start_audit_log](start_audit_log.md)
- Next: [Next Function](summarize_field_distribution.md)
