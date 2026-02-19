# Page 2: Core Concepts and Data Shapes

[Home](../index.md) | [Prev: Why FormIOr Exists](01-why-formior.md) | [Next: Getting Started](03-getting-started.md)

FormIOr works with two main data shapes:

1. A plain data frame (a regular R table)
2. A "flat" object returned by `FlattenSubmissions()`

The flat object is a list that contains:

- `FlatResponses`: the actual data frame
- `ColumnNames`: a simple index of column names

Most functions accept either a data frame or a flat object. If you pass a flat
object, FormIOr automatically uses `flat$FlatResponses` as the working table.

### Why flattening matters

FormIO exports often include nested lists (for example, repeating sections or
edit grids). Flattening expands these nested structures into columns and rows so
that the data can be analyzed or exported without special tooling.

Flattening can create multiple rows per submission. This is not an error. It is
how repeated answers are represented. FormIOr provides multiple ways to reduce
those repeated answers down to one row per submission:

- `ResolveRepeats()` for automatic collapsing
- `FixDups()` for interactive, column-by-column decisions
- `DeduplicateSubmissions()` when duplicates are true duplicates

### The submission ID column

Many cleaning steps require a submission ID column (`id_col`). This is used to
group repeated rows that belong to the same submission. If you do not know which
column is the submission ID, the wizard will suggest one, or you can inspect
column names manually.

---

Navigation
- Prev: [Why FormIOr Exists](01-why-formior.md)
- Next: [Getting Started in R](03-getting-started.md)
- Home: [Handbook Index](../index.md)
