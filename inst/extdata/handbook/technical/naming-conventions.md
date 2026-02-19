
# Naming Conventions

Consistent naming makes results easier to interpret and easier to automate.
FormIOr follows a few standard conventions.

## Folder naming

Output folders should include the form name and a timestamp. This makes it
obvious what the run contains and prevents accidental overwrites.

## Column naming

`standardize_column_names()` converts spaces and special characters into safe
names. It also standardizes the submission ID field name.

## File naming

- `FormIOr_output.xlsx` (main export)
- `audit_log.csv` (audit log)
- `wizard_plan.json` or `wizard_plan.rds` (saved choices)

---

Navigation
- Home: [Handbook Index](../index.md)
- Prev: [Audit Log Schema](audit-log-schema.md)
- Next: [Export Behavior and Dependencies](export-behavior.md)
