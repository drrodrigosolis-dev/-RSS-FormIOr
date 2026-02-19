
# Export Behavior and Dependencies

FormIOr can export to `.xlsx` when an Excel writer is available. If no
writer package is installed, it falls back to CSV/TSV and tells you where
files were saved.

## Excel writers

- `writexl` (lightweight, no Java required)
- `openxlsx` (feature-rich)

## What gets exported

By default, exports can include:

- Cleaned data
- Codebook
- Summary tables
- Plots (as separate files)
- Audit log (as its own sheet)
- Raw flattened data (for traceability)

## Tips

- Use explicit file names to avoid confusion
- Set `overwrite = TRUE` only when you are sure
- Review output paths printed by the wizard

---

Navigation
- Home: [Handbook Index](../index.md)
- Prev: [Naming Conventions](naming-conventions.md)
- Next: [Plotting Error Handling](plotting-errors.md)
