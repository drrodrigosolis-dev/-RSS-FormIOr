
# Troubleshooting and FAQ

## The wizard stops with an error

Check the console for the last message. Most steps capture errors and
continue, but if a core step fails (like downloading or flattening), you
may need to fix the root cause and rerun.

## My output file is missing

Verify the output folder printed by the wizard. If an Excel writer was not
available, outputs may be in CSV files instead.

## Plotting errors

Plotting errors are usually caused by data type mismatches. Choose a
different column or use the defaults suggested by the wizard.

## My codebook is missing labels

Ensure you provided the form schema or allowed the wizard to fetch it.
Labels come from the schema, not from the submissions themselves.

---

Navigation
- Home: [Handbook Index](../index.md)
- Prev: [Tips and Quality Checks](../tips/tips-and-checks.md)
- Next: [Future Work](../roadmap/future-work.md)
