
# Audit Log Schema

The audit log is a CSV file that records key actions taken in FormIOr.
It provides transparency and a repeatable record of what happened to the
dataset over time.

## Core columns

- `timestamp`: When the action occurred
- `action`: A short action name (e.g., `flatten`, `deduplicate`, `export`)
- `details`: A readable summary of what happened
- `rows_before` / `rows_after`: Optional counts to show impact
- `file`: Optional path to output file if relevant

## How it grows

Audit logs are append-only. Each function that changes or exports data can
write a new row. This is especially useful for long-term projects where the
same form is downloaded multiple times.

## Best practices

- Store the audit log in the same output folder as other files
- Keep a copy in the final Excel export for sharing
- Do not manually edit the log unless corrections are needed

---

Navigation
- Home: [Handbook Index](../index.md)
- Prev: [Data Shapes and Types](data-shapes.md)
- Next: [Naming Conventions](naming-conventions.md)
