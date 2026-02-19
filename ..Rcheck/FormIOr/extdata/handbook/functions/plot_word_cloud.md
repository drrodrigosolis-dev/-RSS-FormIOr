
# plot_word_cloud

**Title:** Plot a wordcloud for a text field

## What this function is for

This function focuses on diagnostics and reporting.

## Overview

Requires the optional `wordcloud` package. If it is not installed, the
function will stop with a helpful message.
If audit logging is active (see `start_audit_log()`), this action is recorded.

## How it fits into a workflow

This function typically appears after you have created (or loaded) a
flattened dataset. If you are using the wizard, this step is handled for
you, but the function is also safe to call directly in scripts.

## Usage

```r
plot_word_cloud(
  x,
  field,
  max_words = 100,
  min_freq = 1,
  min_chars = 2,
  remove_stopwords = TRUE,
  stopwords = default_stopwords(),
  seed = NULL,
  colors = NULL,
  ...
)
```

## Key inputs

- `x`: A data frame of responses, or a list from `flatten_submission_records()`.
- `field`: Column name or number to plot.
- `max_words`: Maximum number of words to display.
- `min_freq`: Minimum frequency to keep a word.
- `min_chars`: Minimum character length for a word to be kept.
- `remove_stopwords`: Logical. If `TRUE`, remove common stopwords.
- `stopwords`: Character vector of stopwords to remove.
- `seed`: Optional random seed for reproducible layout.
- `colors`: Vector of colors passed to `wordcloud::wordcloud()`.
- `...`: Additional arguments passed to `wordcloud::wordcloud()`.

## Outputs

- (See `?plot_word_cloud` for return value details.)

## Details and behavior

This function follows the package defaults and logs actions when audit logging is active.

## Examples

```r
plot_word_cloud(flat, "feedback")
```

## Notes and tips

- Turn on audit logging if you need a paper trail
- Keep raw flattened data in your export for traceability
- Prefer consistent column names before downstream diagnostics

---

Navigation
- Home: [Handbook Index](../index.md)
- Function Index: [All Functions](index.md)
- Prev: [plot_response_timeline](plot_response_timeline.md)
- Next: [Next Function](rename_columns_from_dictionary.md)
