# CRAN Compliance Checklist (FormIOr)

This file captures the CRAN submission rules and recommendations used for the
current resubmission, along with the concrete checks applied to this package.

## Official sources (primary)
- CRAN Repository Policy: <https://cran.r-project.org/web/packages/policies.html>
- Writing R Extensions (R-exts): <https://cran.r-project.org/doc/manuals/r-patched/R-exts.html>
- CRAN Cookbook:
  - Description formatting: <https://contributor.r-project.org/cran-cookbook/description_issues.html#formatting-software-names>
  - References/URLs in DESCRIPTION: <https://contributor.r-project.org/cran-cookbook/description_issues.html#references>
  - Console output: <https://contributor.r-project.org/cran-cookbook/code_issues.html#using-printcat>
  - Interactive examples: <https://contributor.r-project.org/cran-cookbook/general_issues.html#structuring-of-examples>
- CRAN incoming checks (review manually before submission):
  <https://cran.r-project.org/web/checks/check_incoming.html>

## General checklist (from official guidance)
### DESCRIPTION
- Quote software/package/API names with single quotes.
- Include API references in the Description with angle‑bracketed URLs and no
  whitespace after `http:` / `https:`.
- Keep Description factual and actionable; avoid marketing language.

### Examples and vignettes
- Do not use `if (interactive())` in examples for non‑interactive code.
  Use `\dontrun{}` or `\donttest{}` for interactive or long‑running examples.
- Avoid any writes to the user’s home, package directory, or working directory.
  Use `tempdir()` / `tempfile()` in examples/tests.

### Console output
- Avoid unsuppressable output for non‑interactive functions.
- Prefer `message()`/`warning()` with `quiet`/`verbose` flags.
- Reserve `cat()`/`print()` for interactive functions or print/summary methods.

### File writing
- Do not write files by default in non‑interactive code paths.
- Require explicit paths for non‑interactive workflows.
- If interactive, default to `tempdir()` or prompt for a location.

### Checks
- Run `R CMD build` and `R CMD check --as-cran` on the generated tarball.
- Review NOTEs/WARNINGS/ERRORS and address before submission.

## FormIOr‑specific checks
- DESCRIPTION: ensure `'FormIO'` / `'FormIO API'` and `'Excel'` are quoted; API
  reference included as `<https://apidocs.form.io>`.
- Examples: replace `@examplesIf interactive()` with `\dontrun{}`.
- Console output: ensure `quiet`/interactive gating for messages/prints.
- Workflow plan runner: require explicit `output_dir` for non‑interactive runs.
