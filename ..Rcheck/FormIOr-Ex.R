pkgname <- "FormIOr"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('FormIOr')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("AskCredentials")
### * AskCredentials

flush(stderr()); flush(stdout())

### Name: AskCredentials
### Title: Ask for Form credentials
### Aliases: AskCredentials

### ** Examples

## Not run: 
##D AskCredentials()
##D AskCredentials(form_id = "myformID", api = "myapiToken")
## End(Not run)



cleanEx()
nameEx("SuspiciousSurveyDemo")
### * SuspiciousSurveyDemo

flush(stderr()); flush(stdout())

### Name: SuspiciousSurveyDemo
### Title: Suspicious Survey Demo Dataset
### Aliases: SuspiciousSurveyDemo
### Keywords: datasets

### ** Examples

data("SuspiciousSurveyDemo")
str(SuspiciousSurveyDemo)

out <- detect_suspicious_submissions(
  SuspiciousSurveyDemo,
  id_col = "submissionId",
  time_col = "created",
  cutoff_time = "2026-01-06 00:00:00",
  group_action = "split_only",
  quiet = TRUE
)
out$comparability$non_comparable



cleanEx()
nameEx("append_audit_log_entry")
### * append_audit_log_entry

flush(stderr()); flush(stdout())

### Name: append_audit_log_entry
### Title: Write a simple audit log entry
### Aliases: append_audit_log_entry

### ** Examples

df <- data.frame(a = 1:3)
log_path <- tempfile(fileext = ".csv")
append_audit_log_entry(
  "export",
  details = "Exported survey data",
  data = df,
  file = log_path,
  quiet = TRUE
)



cleanEx()
nameEx("apply_submission_updates")
### * apply_submission_updates

flush(stderr()); flush(stdout())

### Name: apply_submission_updates
### Title: Adjust submissions by ID (delete or edit specific values)
### Aliases: apply_submission_updates

### ** Examples

df <- data.frame(
  submissionId = c("a", "b", "c"),
  status = c("ok", "test", "ok"),
  stringsAsFactors = FALSE
)

# Delete one submission and update a value
out <- apply_submission_updates(
  df,
  id_col = "submissionId",
  delete_ids = "b",
  updates = data.frame(id = "c", column = "status", value = "review", stringsAsFactors = FALSE),
  quiet = TRUE
)
out$data



cleanEx()
nameEx("assign_section_hierarchy")
### * assign_section_hierarchy

flush(stderr()); flush(stdout())

### Name: assign_section_hierarchy
### Title: Add Hierarchical Sections to FormIO Response Columns
### Aliases: assign_section_hierarchy

### ** Examples

## Not run: 
##D # Assuming FoodTypes is a sample dataset with possible nests
##D data("FoodTypes")
##D sectioned <- assign_section_hierarchy(FoodTypes)
##D print(sectioned$Sections)
## End(Not run)




cleanEx()
nameEx("build_data_codebook")
### * build_data_codebook

flush(stderr()); flush(stdout())

### Name: build_data_codebook
### Title: Create a codebook for a dataset
### Aliases: build_data_codebook

### ** Examples

df <- data.frame(age = c(10, 12, NA), color = c("red", "blue", "red"))
build_data_codebook(df)



cleanEx()
nameEx("build_field_dictionary")
### * build_field_dictionary

flush(stderr()); flush(stdout())

### Name: build_field_dictionary
### Title: Build a field dictionary for a form
### Aliases: build_field_dictionary

### ** Examples

form <- list(
  title = "Sample Form",
  components = list(
    list(type = "textfield", key = "first_name", label = "First name", input = TRUE),
    list(type = "select", key = "color", label = "Favorite color", input = TRUE,
         data = list(values = list(list(label = "Red", value = "red"))))
  )
)
build_field_dictionary(form)

## Not run: 
##D meta <- fetch_form_metadata(form_id = "123", api_key = "abc")
##D build_field_dictionary(meta, include = "all")
## End(Not run)



cleanEx()
nameEx("collapse_checkbox_selections")
### * collapse_checkbox_selections

flush(stderr()); flush(stdout())

### Name: collapse_checkbox_selections
### Title: Compact checkbox/multi-select columns into a single readable
###   column
### Aliases: collapse_checkbox_selections

### ** Examples

## Not run: 
##D compacted <- collapse_checkbox_selections(flat)
##D head(compacted$data)
## End(Not run)



cleanEx()
nameEx("collapse_repeated_values")
### * collapse_repeated_values

flush(stderr()); flush(stdout())

### Name: collapse_repeated_values
### Title: Resolve repeated answers into one row per submission
### Aliases: collapse_repeated_values

### ** Examples

## Not run: 
##D flat <- flatten_submission_records(fetch_form_responses(form_id = "123", api_key = "abc"))
##D resolved <- collapse_repeated_values(flat, id_col = "submissionId")
##D head(resolved$data)
## End(Not run)



cleanEx()
nameEx("compare_form_versions")
### * compare_form_versions

flush(stderr()); flush(stdout())

### Name: compare_form_versions
### Title: Compare two versions of a form
### Aliases: compare_form_versions

### ** Examples

old <- list(components = list(
  list(type = "textfield", key = "name", label = "Name", input = TRUE)
))
new <- list(components = list(
  list(type = "textfield", key = "name", label = "Full name", input = TRUE),
  list(type = "number", key = "age", label = "Age", input = TRUE)
))
compare_form_versions(old, new)

## Not run: 
##D old_meta <- fetch_form_metadata(form_id = "123", api_key = "abc")
##D new_meta <- fetch_form_metadata(form_id = "123", api_key = "abc")
##D compare_form_versions(old_meta, new_meta)
## End(Not run)



cleanEx()
nameEx("count_multivalue_fields")
### * count_multivalue_fields

flush(stderr()); flush(stdout())

### Name: count_multivalue_fields
### Title: Identify columns with multiple distinct values per submission
### Aliases: count_multivalue_fields

### ** Examples

## Not run: 
##D flat <- flatten_submission_records(fetch_form_responses(form_id = "123", api_key = "abc..."))
##D 
##D # See which columns have multiple values per submission
##D multi_counts <- count_multivalue_fields(flat)
##D 
##D # Quick check: which columns ever have more than one distinct value?
##D multi_counts |>
##D   summarise(across(-1, ~ max(.x, na.rm = TRUE))) |>
##D   pivot_longer(everything(), names_to = "column", values_to = "max_distinct") |>
##D   filter(max_distinct > 1)
## End(Not run)




cleanEx()
nameEx("deduplicate_submission_rows")
### * deduplicate_submission_rows

flush(stderr()); flush(stdout())

### Name: deduplicate_submission_rows
### Title: Deduplicate submissions by submission ID
### Aliases: deduplicate_submission_rows

### ** Examples

## Not run: 
##D flat <- flatten_submission_records(fetch_form_responses(form_id = "123", api_key = "abc"))
##D dedup <- deduplicate_submission_rows(flat, id_col = "submissionId")
##D nrow(dedup$data)
## End(Not run)



cleanEx()
nameEx("describe_form_schema")
### * describe_form_schema

flush(stderr()); flush(stdout())

### Name: describe_form_schema
### Title: Describe a form in plain language
### Aliases: describe_form_schema

### ** Examples

form <- list(
  title = "Sample Form",
  name = "sample_form",
  version = 2,
  components = list(
    list(type = "textfield", key = "first_name", label = "First name", input = TRUE),
    list(type = "panel", title = "Details", components = list(
      list(type = "number", key = "age", label = "Age", input = TRUE)
    ))
  )
)
describe_form_schema(form)

## Not run: 
##D meta <- fetch_form_metadata(form_id = "123", api_key = "abc")
##D describe_form_schema(meta)
## End(Not run)



cleanEx()
nameEx("detect_suspicious_submissions")
### * detect_suspicious_submissions

flush(stderr()); flush(stdout())

### Name: detect_suspicious_submissions
### Title: Flag suspicious submissions and enforce pre/post comparability
###   checks
### Aliases: detect_suspicious_submissions

### ** Examples

## Not run: 
##D data("SuspiciousSurveyDemo", package = "FormIOr")
##D 
##D out <- detect_suspicious_submissions(
##D   SuspiciousSurveyDemo,
##D   id_col = "submissionId",
##D   time_col = "created",
##D   cutoff_time = "2026-01-06 00:00:00",
##D   group_action = "split_only"
##D )
##D 
##D out$comparability$non_comparable
##D head(out$comparability$field_tests)
##D 
##D # If groups differ and post phase is identity-verified, prefer post-only:
##D post_only <- detect_suspicious_submissions(
##D   SuspiciousSurveyDemo,
##D   id_col = "submissionId",
##D   time_col = "created",
##D   cutoff_time = "2026-01-06 00:00:00",
##D   group_action = "omit_pre"
##D )
## End(Not run)




cleanEx()
nameEx("export_results_to_excel")
### * export_results_to_excel

flush(stderr()); flush(stdout())

### Name: export_results_to_excel
### Title: Export data to Excel (or CSV if needed)
### Aliases: export_results_to_excel

### ** Examples

df <- data.frame(name = c("A", "B"), score = c(1, 2))
## Not run: 
##D   export_results_to_excel(df, tempfile(fileext = ".xlsx"), overwrite = TRUE)
## End(Not run)



cleanEx()
nameEx("fetch_form_metadata")
### * fetch_form_metadata

flush(stderr()); flush(stdout())

### Name: fetch_form_metadata
### Title: Get metadata for a 'FormIO' form
### Aliases: fetch_form_metadata

### ** Examples

## Not run: 
##D fetch_form_metadata(form_id = "your_form_id", api_key = "your_api_key")
## End(Not run)



cleanEx()
nameEx("fetch_form_responses")
### * fetch_form_responses

flush(stderr()); flush(stdout())

### Name: fetch_form_responses
### Title: Get responses submitted to a 'FormIO' form
### Aliases: fetch_form_responses

### ** Examples

## Not run: 
##D fetch_form_responses(form_id = "your_form_id", api_key = "your_api_key")
## End(Not run)



cleanEx()
nameEx("flatten_submission_records")
### * flatten_submission_records

flush(stderr()); flush(stdout())

### Name: flatten_submission_records
### Title: Flatten Submissions
### Aliases: flatten_submission_records

### ** Examples


x<-FoodTypes

# Nested Structure
head(x)

#Flattened Structure
xFlat<-flatten_submission_records(x)

xFlat$FlatResponses ## Survey Output
xFlat$ColumnNames   ## New Columns Formed



cleanEx()
nameEx("is_audit_log_active")
### * is_audit_log_active

flush(stderr()); flush(stdout())

### Name: is_audit_log_active
### Title: Check whether audit logging is active
### Aliases: is_audit_log_active

### ** Examples

is_audit_log_active()



cleanEx()
nameEx("plot_categorical_summary")
### * plot_categorical_summary

flush(stderr()); flush(stdout())

### Name: plot_categorical_summary
### Title: Plot a bar chart for a categorical field
### Aliases: plot_categorical_summary

### ** Examples

## Not run: 
##D plot_categorical_summary(flat, "region")
## End(Not run)



cleanEx()
nameEx("plot_numeric_distribution")
### * plot_numeric_distribution

flush(stderr()); flush(stdout())

### Name: plot_numeric_distribution
### Title: Plot a histogram for a numeric field
### Aliases: plot_numeric_distribution

### ** Examples

## Not run: 
##D plot_numeric_distribution(flat, "age")
## End(Not run)



cleanEx()
nameEx("plot_response_timeline")
### * plot_response_timeline

flush(stderr()); flush(stdout())

### Name: plot_response_timeline
### Title: Plot a response timeline
### Aliases: plot_response_timeline

### ** Examples

## Not run: 
##D plot_response_timeline(flat, date_col = "created", interval = "month")
## End(Not run)



cleanEx()
nameEx("plot_word_cloud")
### * plot_word_cloud

flush(stderr()); flush(stdout())

### Name: plot_word_cloud
### Title: Plot a wordcloud for a text field
### Aliases: plot_word_cloud

### ** Examples

## Not run: 
##D plot_word_cloud(flat, "feedback")
## End(Not run)



cleanEx()
nameEx("rename_columns_from_dictionary")
### * rename_columns_from_dictionary

flush(stderr()); flush(stdout())

### Name: rename_columns_from_dictionary
### Title: Rename columns from a flattened FormIO dataset
### Aliases: rename_columns_from_dictionary

### ** Examples

## Not run: 
##D # Interactive
##D rename_columns_from_dictionary(flat)
##D 
##D # Non-interactive map
##D map_df <- data.frame(
##D   OldNames = c("submissionId", "age"),
##D   NewNames = c("submission_id", "age_years"),
##D   stringsAsFactors = FALSE
##D )
##D rename_columns_from_dictionary(flat, rename_map = map_df, quiet = TRUE)
## End(Not run)



cleanEx()
nameEx("resolve_duplicate_values")
### * resolve_duplicate_values

flush(stderr()); flush(stdout())

### Name: resolve_duplicate_values
### Title: Clean duplicated rows caused by multi-value fields in flattened
###   FormIO data
### Aliases: resolve_duplicate_values

### ** Examples

## Not run: 
##D # Typical workflow
##D responses <- fetch_form_responses(form_id = "your-form-id", api_key = "your-api-key")
##D flat <- flatten_submission_records(responses)
##D 
##D # Run interactive cleaning
##D result <- resolve_duplicate_values(flat)
##D 
##D # View the cleaned data
##D View(result$cleaned)
##D 
##D # See what was done to each column
##D result$decisions
##D 
##D # Dry run to preview changes
##D resolve_duplicate_values(flat, dry_run = TRUE)
## End(Not run)



cleanEx()
nameEx("review_duplicate_rows")
### * review_duplicate_rows

flush(stderr()); flush(stdout())

### Name: review_duplicate_rows
### Title: Interactively review and resolve duplicate submissions
### Aliases: review_duplicate_rows

### ** Examples

## Not run: 
##D flat <- flatten_submission_records(fetch_form_responses(form_id = "123", api_key = "abc"))
##D out <- review_duplicate_rows(flat, id_col = "form_submissionid")
##D View(out$decisions)
##D 
##D # Non-interactive example (scriptable)
##D df <- data.frame(
##D   submissionId = c("a", "a", "b", "b", "b"),
##D   email = c("x@example.com", "x@example.com", "x@example.com", "x@example.com", "x@example.com"),
##D   status = c("draft", "final", "test", "final", "final"),
##D   stringsAsFactors = FALSE
##D )
##D # Group 1 will be the email=x@example.com group (the only group in this example)
##D keep_map <- list(`1` = 2)
##D out2 <- review_duplicate_rows(
##D   df,
##D   id_col = "submissionId",
##D   key_cols = "email",
##D   compare_cols = c("submissionId", "email", "status"),
##D   keep_map = keep_map,
##D   prompt = FALSE,
##D   quiet = TRUE
##D )
## End(Not run)



cleanEx()
nameEx("run_form_processing_workflow")
### * run_form_processing_workflow

flush(stderr()); flush(stdout())

### Name: run_form_processing_workflow
### Title: Guided end-to-end workflow (download -> clean -> report ->
###   export)
### Aliases: run_form_processing_workflow

### ** Examples

## Not run: 
##D # Fully guided interactive run
##D out <- run_form_processing_workflow()
##D 
##D # Start from an existing data.frame (skip download)
##D out <- run_form_processing_workflow(data = FoodTypes)
## End(Not run)



cleanEx()
nameEx("standardize_column_names")
### * standardize_column_names

flush(stderr()); flush(stdout())

### Name: standardize_column_names
### Title: Normalize column names into a clean, readable format
### Aliases: standardize_column_names

### ** Examples

## Not run: 
##D flat <- flatten_submission_records(fetch_form_responses(form_id = "123", api_key = "abc"))
##D norm <- standardize_column_names(flat)
##D names(norm$data)
##D norm$name_map
## End(Not run)



cleanEx()
nameEx("start_audit_log")
### * start_audit_log

flush(stderr()); flush(stdout())

### Name: start_audit_log
### Title: Start an audit log for FormIOr actions
### Aliases: start_audit_log

### ** Examples

## Not run: 
##D log_file <- tempfile(fileext = ".csv")
##D start_audit_log(log_file, overwrite = TRUE)
##D 
##D # Run some FormIOr steps (each will append a row when possible)
##D flat <- flatten_submission_records(FoodTypes)
##D norm <- standardize_column_names(flat, quiet = TRUE)
##D 
##D stop_audit_log()
##D read.csv(log_file, stringsAsFactors = FALSE)
## End(Not run)



cleanEx()
nameEx("stop_audit_log")
### * stop_audit_log

flush(stderr()); flush(stdout())

### Name: stop_audit_log
### Title: Stop audit logging for FormIOr
### Aliases: stop_audit_log

### ** Examples

## Not run: 
##D stop_audit_log()
## End(Not run)



cleanEx()
nameEx("summarize_field_distribution")
### * summarize_field_distribution

flush(stderr()); flush(stdout())

### Name: summarize_field_distribution
### Title: Summarize a single field in a FormIO response dataset
### Aliases: summarize_field_distribution

### ** Examples

## Not run: 
##D flat <- flatten_submission_records(fetch_form_responses(form_id = "123", api_key = "abc"))
##D summarize_field_distribution(flat, "age")
##D summarize_field_distribution(flat, "favorite_food", top_n = 5)
## End(Not run)



cleanEx()
nameEx("summarize_response_timeline")
### * summarize_response_timeline

flush(stderr()); flush(stdout())

### Name: summarize_response_timeline
### Title: Summarize responses over time
### Aliases: summarize_response_timeline

### ** Examples

## Not run: 
##D summarize_response_timeline(flat, date_col = "created", interval = "week")
## End(Not run)



cleanEx()
nameEx("tabulate_field_by_group")
### * tabulate_field_by_group

flush(stderr()); flush(stdout())

### Name: tabulate_field_by_group
### Title: Cross-tabulate two fields
### Aliases: tabulate_field_by_group

### ** Examples

## Not run: 
##D tabulate_field_by_group(flat, "region", "program", percent = "row")
## End(Not run)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
