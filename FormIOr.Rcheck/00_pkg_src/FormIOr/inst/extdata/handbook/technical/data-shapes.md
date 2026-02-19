
# Data Shapes and Types

FormIOr works with a few consistent data shapes. Understanding these helps
avoid surprises when moving between functions.

## Raw responses (nested)

Raw FormIO responses are nested JSON. They often contain lists of components
and repeated fields. These structures are powerful but difficult to analyze
in R without flattening.

## Flattened data (wide)

Flattened data turns each submission into a single row. Each column is a
field from the form. This is the primary format used by most FormIOr
functions, and it is the easiest format to export to Excel.

## Long-format summaries

Some diagnostic functions return long-format tables for easier reporting or
visualization. These typically include field names, counts, and value labels.

## Common pitfalls

- Mixed data types in the same column (text + numbers)
- Repeated answers stored as lists, not atomic values
- Missing ID fields after renaming columns

---

Navigation
- Home: [Handbook Index](../index.md)
- Prev: [Function Index](../functions/index.md)
- Next: [Audit Log Schema](audit-log-schema.md)
