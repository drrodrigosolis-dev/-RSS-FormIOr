# Rendering the Handbook (All Pages)

If you preview `index.md` in RStudio, only that single file renders. The
links still point to `.md` files, so clicking them opens the raw Markdown.

To render the entire handbook into HTML (so links work), use the provided
script. It converts every `.md` file to `.html` and updates links so they
stay navigable.

## Quick render (recommended)

```r
source("inst/extdata/handbook/render_handbook.R")
```

This creates a local folder:

```
handbook_site/
```

Open this file in your browser or RStudio Viewer:

```
handbook_site/index.html
```

## Why this works

The script uses the `markdown` R package (not pandoc), which means it works
even if pandoc is not installed. It also rewrites `.md` links to `.html`
links so navigation works across pages.

## Maintenance helpers (built-in)

Two maintenance scripts live under `inst/extdata/handbook/maintenance/`:

- `check_docs.R` validates that function calls in code blocks use valid
  argument names.
- `build_function_reference.R` regenerates the function quick‑lookup page
  from the roxygen/Rd documentation.

The render script calls these automatically before building the HTML, so the
handbook stays consistent.

## Optional: Render with pandoc

If you prefer higher‑fidelity HTML, install pandoc and render with
`rmarkdown::render()` or Quarto. The same folder layout will work, but
pandoc is optional for local viewing.

---

Navigation
- Home: [Handbook Index](../index.md)
- Prev: [Wizard Plans](wizard-plans.md)
- Next: [Tips and Quality Checks](../tips/tips-and-checks.md)
