pkgname <- "FormIOr"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "FormIOr-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('FormIOr')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("AddSections")
### * AddSections

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: AddSections
### Title: Add Hierarchical Sections to FormIO Response Columns
### Aliases: AddSections

### ** Examples

## Not run: 
##D # Assuming FoodTypes is a sample dataset with possible nests
##D data("FoodTypes")
##D sectioned <- AddSections(FoodTypes)
##D print(sectioned$Sections)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("AddSections", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("AdjustSubmissions")
### * AdjustSubmissions

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: AdjustSubmissions
### Title: Adjust submissions by ID (delete or edit specific values)
### Aliases: AdjustSubmissions

### ** Examples

df <- data.frame(
  submissionId = c("a", "b", "c"),
  status = c("ok", "test", "ok"),
  stringsAsFactors = FALSE
)

# Delete one submission and update a value
out <- AdjustSubmissions(
  df,
  id_col = "submissionId",
  delete_ids = "b",
  updates = data.frame(id = "c", column = "status", value = "review", stringsAsFactors = FALSE),
  quiet = TRUE
)
out$data



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("AdjustSubmissions", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("AskCredentials")
### * AskCredentials

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: AskCredentials
### Title: Ask for Form credentials
### Aliases: AskCredentials

### ** Examples

## Not run: 
##D AskCredentials()
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("AskCredentials", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("CompactSelections")
### * CompactSelections

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: CompactSelections
### Title: Compact checkbox/multi-select columns into a single readable
###   column
### Aliases: CompactSelections

### ** Examples

## Not run: 
##D compacted <- CompactSelections(flat)
##D head(compacted$data)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("CompactSelections", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("CompareFormVersions")
### * CompareFormVersions

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: CompareFormVersions
### Title: Compare two versions of a form
### Aliases: CompareFormVersions

### ** Examples

old <- list(components = list(
  list(type = "textfield", key = "name", label = "Name", input = TRUE)
))
new <- list(components = list(
  list(type = "textfield", key = "name", label = "Full name", input = TRUE),
  list(type = "number", key = "age", label = "Age", input = TRUE)
))
CompareFormVersions(old, new)

## Not run: 
##D old_meta <- GetFormMetadata(form_id = "123", api_key = "abc")
##D new_meta <- GetFormMetadata(form_id = "123", api_key = "abc")
##D CompareFormVersions(old_meta, new_meta)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("CompareFormVersions", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("CrossTab")
### * CrossTab

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: CrossTab
### Title: Cross-tabulate two fields
### Aliases: CrossTab

### ** Examples

## Not run: 
##D CrossTab(flat, "region", "program", percent = "row")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("CrossTab", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("DeduplicateSubmissions")
### * DeduplicateSubmissions

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: DeduplicateSubmissions
### Title: Deduplicate submissions by submission ID
### Aliases: DeduplicateSubmissions

### ** Examples

## Not run: 
##D flat <- FlattenSubmissions(GetResponses(form_id = "123", api_key = "abc"))
##D dedup <- DeduplicateSubmissions(flat, id_col = "submissionId")
##D nrow(dedup$data)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("DeduplicateSubmissions", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("DescribeForm")
### * DescribeForm

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: DescribeForm
### Title: Describe a form in plain language
### Aliases: DescribeForm

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
DescribeForm(form)

## Not run: 
##D meta <- GetFormMetadata(form_id = "123", api_key = "abc")
##D DescribeForm(meta)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("DescribeForm", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ExportToExcel")
### * ExportToExcel

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ExportToExcel
### Title: Export data to Excel (or CSV if needed)
### Aliases: ExportToExcel

### ** Examples

df <- data.frame(name = c("A", "B"), score = c(1, 2))
## Not run: 
##D   ExportToExcel(df, tempfile(fileext = ".xlsx"), overwrite = TRUE)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ExportToExcel", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("FieldDictionary")
### * FieldDictionary

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: FieldDictionary
### Title: Build a field dictionary for a form
### Aliases: FieldDictionary

### ** Examples

form <- list(
  title = "Sample Form",
  components = list(
    list(type = "textfield", key = "first_name", label = "First name", input = TRUE),
    list(type = "select", key = "color", label = "Favorite color", input = TRUE,
         data = list(values = list(list(label = "Red", value = "red"))))
  )
)
FieldDictionary(form)

## Not run: 
##D meta <- GetFormMetadata(form_id = "123", api_key = "abc")
##D FieldDictionary(meta, include = "all")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("FieldDictionary", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("FixDups")
### * FixDups

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: FixDups
### Title: Clean duplicated rows caused by multi-value fields in flattened
###   FormIO data
### Aliases: FixDups

### ** Examples

## Not run: 
##D # Typical workflow
##D responses <- GetResponses(form_id = "your-form-id", api_key = "your-api-key")
##D flat <- FlattenSubmissions(responses)
##D 
##D # Run interactive cleaning
##D result <- FixDups(flat)
##D 
##D # View the cleaned data
##D View(result$cleaned)
##D 
##D # See what was done to each column
##D result$decisions
##D 
##D # Dry run to preview changes
##D FixDups(flat, dry_run = TRUE)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("FixDups", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("FlagSuspiciousSubmissions")
### * FlagSuspiciousSubmissions

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: FlagSuspiciousSubmissions
### Title: Flag suspicious submissions and enforce pre/post comparability
###   checks
### Aliases: FlagSuspiciousSubmissions

### ** Examples

## Not run: 
##D data("SuspiciousSurveyDemo", package = "FormIOr")
##D 
##D out <- FlagSuspiciousSubmissions(
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
##D post_only <- FlagSuspiciousSubmissions(
##D   SuspiciousSurveyDemo,
##D   id_col = "submissionId",
##D   time_col = "created",
##D   cutoff_time = "2026-01-06 00:00:00",
##D   group_action = "omit_pre"
##D )
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("FlagSuspiciousSubmissions", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("FlattenSubmissions")
### * FlattenSubmissions

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: FlattenSubmissions
### Title: Flatten Submissions
### Aliases: FlattenSubmissions

### ** Examples


x<-FoodTypes

# Nested Structure
head(x)

#Flattened Structure
xFlat<-FlattenSubmissions(x)

xFlat$FlatResponses ## Survey Output
xFlat$ColumnNames   ## New Columns Formed



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("FlattenSubmissions", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("FormIOrWorkflow")
### * FormIOrWorkflow

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: FormIOrWorkflow
### Title: Guided end-to-end workflow (download -> clean -> report ->
###   export)
### Aliases: FormIOrWorkflow

### ** Examples

## Not run: 
##D # Fully guided interactive run
##D out <- FormIOrWorkflow()
##D 
##D # Start from an existing data.frame (skip download)
##D out <- FormIOrWorkflow(data = FoodTypes)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("FormIOrWorkflow", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("GetFormMetadata")
### * GetFormMetadata

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: GetFormMetadata
### Title: Get metadata for a 'FormIO' form
### Aliases: GetFormMetadata

### ** Examples

## Not run: 
##D GetFormMetadata(form_id = "your_form_id", api_key = "your_api_key")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("GetFormMetadata", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("GetResponses")
### * GetResponses

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: GetResponses
### Title: Get responses submitted to a 'FormIO' form
### Aliases: GetResponses

### ** Examples

## Not run: 
##D GetResponses(form_id = "your_form_id", api_key = "your_api_key")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("GetResponses", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("IsAuditLogActive")
### * IsAuditLogActive

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: IsAuditLogActive
### Title: Check whether audit logging is active
### Aliases: IsAuditLogActive

### ** Examples

IsAuditLogActive()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("IsAuditLogActive", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("MakeCodebook")
### * MakeCodebook

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: MakeCodebook
### Title: Create a codebook for a dataset
### Aliases: MakeCodebook

### ** Examples

df <- data.frame(age = c(10, 12, NA), color = c("red", "blue", "red"))
MakeCodebook(df)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("MakeCodebook", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("NormalizeColumnNames")
### * NormalizeColumnNames

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: NormalizeColumnNames
### Title: Normalize column names into a clean, readable format
### Aliases: NormalizeColumnNames

### ** Examples

## Not run: 
##D flat <- FlattenSubmissions(GetResponses(form_id = "123", api_key = "abc"))
##D norm <- NormalizeColumnNames(flat)
##D names(norm$data)
##D norm$name_map
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("NormalizeColumnNames", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("PlotBarSummary")
### * PlotBarSummary

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: PlotBarSummary
### Title: Plot a bar chart for a categorical field
### Aliases: PlotBarSummary

### ** Examples

## Not run: 
##D PlotBarSummary(flat, "region")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("PlotBarSummary", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("PlotHistogram")
### * PlotHistogram

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: PlotHistogram
### Title: Plot a histogram for a numeric field
### Aliases: PlotHistogram

### ** Examples

## Not run: 
##D PlotHistogram(flat, "age")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("PlotHistogram", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("PlotResponseTimeline")
### * PlotResponseTimeline

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: PlotResponseTimeline
### Title: Plot a response timeline
### Aliases: PlotResponseTimeline

### ** Examples

## Not run: 
##D PlotResponseTimeline(flat, date_col = "created", interval = "month")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("PlotResponseTimeline", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("PlotWordcloud")
### * PlotWordcloud

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: PlotWordcloud
### Title: Plot a wordcloud for a text field
### Aliases: PlotWordcloud

### ** Examples

## Not run: 
##D PlotWordcloud(flat, "feedback")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("PlotWordcloud", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("RenameCols")
### * RenameCols

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: RenameCols
### Title: Rename columns from a flattened FormIO dataset
### Aliases: RenameCols

### ** Examples

## Not run: 
##D # Interactive
##D RenameCols(flat)
##D 
##D # Non-interactive map
##D map_df <- data.frame(
##D   OldNames = c("submissionId", "age"),
##D   NewNames = c("submission_id", "age_years"),
##D   stringsAsFactors = FALSE
##D )
##D RenameCols(flat, rename_map = map_df, quiet = TRUE)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("RenameCols", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ResolveRepeats")
### * ResolveRepeats

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ResolveRepeats
### Title: Resolve repeated answers into one row per submission
### Aliases: ResolveRepeats

### ** Examples

## Not run: 
##D flat <- FlattenSubmissions(GetResponses(form_id = "123", api_key = "abc"))
##D resolved <- ResolveRepeats(flat, id_col = "submissionId")
##D head(resolved$data)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ResolveRepeats", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ResponseTimeline")
### * ResponseTimeline

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ResponseTimeline
### Title: Summarize responses over time
### Aliases: ResponseTimeline

### ** Examples

## Not run: 
##D ResponseTimeline(flat, date_col = "created", interval = "week")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ResponseTimeline", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ReviewDuplicateSubmissions")
### * ReviewDuplicateSubmissions

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ReviewDuplicateSubmissions
### Title: Interactively review and resolve duplicate submissions
### Aliases: ReviewDuplicateSubmissions

### ** Examples

## Not run: 
##D flat <- FlattenSubmissions(GetResponses(form_id = "123", api_key = "abc"))
##D out <- ReviewDuplicateSubmissions(flat, id_col = "form_submissionid")
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
##D out2 <- ReviewDuplicateSubmissions(
##D   df,
##D   id_col = "submissionId",
##D   key_cols = "email",
##D   compare_cols = c("submissionId", "email", "status"),
##D   keep_map = keep_map,
##D   prompt = FALSE,
##D   quiet = TRUE
##D )
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ReviewDuplicateSubmissions", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("StartAuditLog")
### * StartAuditLog

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: StartAuditLog
### Title: Start an audit log for FormIOr actions
### Aliases: StartAuditLog

### ** Examples

## Not run: 
##D log_file <- tempfile(fileext = ".csv")
##D StartAuditLog(log_file, overwrite = TRUE)
##D 
##D # Run some FormIOr steps (each will append a row when possible)
##D flat <- FlattenSubmissions(FoodTypes)
##D norm <- NormalizeColumnNames(flat, quiet = TRUE)
##D 
##D StopAuditLog()
##D read.csv(log_file, stringsAsFactors = FALSE)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("StartAuditLog", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("StopAuditLog")
### * StopAuditLog

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: StopAuditLog
### Title: Stop audit logging for FormIOr
### Aliases: StopAuditLog

### ** Examples

## Not run: 
##D StopAuditLog()
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("StopAuditLog", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("SummaryByField")
### * SummaryByField

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: SummaryByField
### Title: Summarize a single field in a FormIO response dataset
### Aliases: SummaryByField

### ** Examples

## Not run: 
##D flat <- FlattenSubmissions(GetResponses(form_id = "123", api_key = "abc"))
##D SummaryByField(flat, "age")
##D SummaryByField(flat, "favorite_food", top_n = 5)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("SummaryByField", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("SuspiciousSurveyDemo")
### * SuspiciousSurveyDemo

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: SuspiciousSurveyDemo
### Title: Suspicious Survey Demo Dataset
### Aliases: SuspiciousSurveyDemo
### Keywords: datasets

### ** Examples

data("SuspiciousSurveyDemo")
str(SuspiciousSurveyDemo)

out <- FlagSuspiciousSubmissions(
  SuspiciousSurveyDemo,
  id_col = "submissionId",
  time_col = "created",
  cutoff_time = "2026-01-06 00:00:00",
  group_action = "split_only",
  quiet = TRUE
)
out$comparability$non_comparable



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("SuspiciousSurveyDemo", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("WriteAuditLog")
### * WriteAuditLog

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: WriteAuditLog
### Title: Write a simple audit log entry
### Aliases: WriteAuditLog

### ** Examples

df <- data.frame(a = 1:3)
log_path <- tempfile(fileext = ".csv")
WriteAuditLog(
  "export",
  details = "Exported survey data",
  data = df,
  file = log_path,
  quiet = TRUE
)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("WriteAuditLog", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("findMultilines")
### * findMultilines

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: findMultilines
### Title: Identify columns with multiple distinct values per submission
### Aliases: findMultilines

### ** Examples

## Not run: 
##D flat <- FlattenSubmissions(GetResponses(form_id = "123", api_key = "abc..."))
##D 
##D # See which columns have multiple values per submission
##D multi_counts <- findMultilines(flat)
##D 
##D # Quick check: which columns ever have more than one distinct value?
##D multi_counts |>
##D   summarise(across(-1, ~ max(.x, na.rm = TRUE))) |>
##D   pivot_longer(everything(), names_to = "column", values_to = "max_distinct") |>
##D   filter(max_distinct > 1)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("findMultilines", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
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
