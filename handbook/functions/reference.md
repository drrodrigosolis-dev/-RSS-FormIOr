# Function Reference (Quick Lookup)

This page lists every exported FormIOr function with a short description. 
Click a function name to open its full handbook page.

| Function | Summary |
| --- | --- |
| [AddSections](AddSections.md) | This interactive function guides the user through categorizing columns of a flattened FormIO response dataset into hierarchical sections (up to 3 levels deep). It is designed to facilitate easier a... |
| [AdjustSubmissions](AdjustSubmissions.md) | Sometimes you need to make small, targeted fixes before you export: remove test submissions, correct a value for one submission, or blank out a field for privacy reasons. |
| [AskCredentials](AskCredentials.md) | Prompts for the Form ID and API key used to access the FormIO API. This is usually called automatically by GetResponses() and related functions when credentials are not already stored. |
| [CompactSelections](CompactSelections.md) | When a question generates multiple TRUE/FALSE columns (e.g., select boxes), this function combines them into a single comma-separated column. If audit logging is active (see StartAuditLog()), this ... |
| [CompareFormVersions](CompareFormVersions.md) | Shows what changed between two form versions: fields added, removed, or updated. If audit logging is active (see StartAuditLog()), this action is recorded. |
| [CrossTab](CrossTab.md) | Builds a simple cross-tabulation between two columns, returning both a wide table and a long table that includes counts and (optional) percents. If audit logging is active (see StartAuditLog()), th... |
| [DeduplicateSubmissions](DeduplicateSubmissions.md) | Keeps one row per submission ID, using a timestamp column when available (for example created or modified), otherwise keeps first/last row. If audit logging is active (see StartAuditLog()), this ac... |
| [DescribeForm](DescribeForm.md) | This is a simple overview of a form: the name, version, and how many fields it contains. It is meant for non-technical users who want a quick summary. If audit logging is active (see StartAuditLog(... |
| [ExportToExcel](ExportToExcel.md) | This is a simple, user-friendly export helper for FormIOr outputs. It accepts a data.frame, a list of data.frames (multiple sheets), or a list returned by GetResponses() / FlattenSubmissions(). If ... |
| [FieldDictionary](FieldDictionary.md) | This creates a clean table that lists each field in your form. It is meant to be easy to read and share with non-technical staff. If audit logging is active (see StartAuditLog()), this action is re... |
| [findMultilines](findMultilines.md) | Creates a diagnostic data frame with the same shape as the flattened responses, where each original value (except in the ID column) is replaced by the number of distinct non-NA values that appear i... |
| [FixDups](FixDups.md) | Interactive function designed for users with little or no R experience. It helps resolve columns that contain multiple values per submission (e.g. repeating sections, "add another" items, multi-sel... |
| [FlattenSubmissions](FlattenSubmissions.md) | FormIO submissions often contain nested lists (for repeating sections, address blocks, uploads, etc.). FlattenSubmissions() expands those nested structures into a regular 2D data frame so you can e... |
| [FormIOrWorkflow](FormIOrWorkflow.md) | This interactive helper walks you through a complete FormIOr workflow: downloading responses, flattening and cleaning the data, generating simple diagnostics (like a codebook and basic plots), and ... |
| [GetFormMetadata](GetFormMetadata.md) | Downloads basic metadata for a FormIO form. This can be useful for: |
| [GetResponses](GetResponses.md) | Retrieves submissions ("responses") for a FormIO form using the export API. This is one of the main entry points for getting data into FormIOr. |
| [GetSubmissions](GetSubmissions.md) | Fetches a list of submission metadata (not the full submission data) from a Digital.gov.bc.ca form, including drafts, completed, and deleted submissions. If audit logging is active (see StartAuditL... |
| [IsAuditLogActive](IsAuditLogActive.md) | This is most useful in scripts when you want to conditionally add a manual note using WriteAuditLog() only when logging is enabled. |
| [MakeCodebook](MakeCodebook.md) | This produces a plain-language table that documents each column in your data. It can optionally use a FormIO schema (via FieldDictionary()) to add labels, sections, and descriptions. If audit loggi... |
| [NormalizeColumnNames](NormalizeColumnNames.md) | Designed for non-technical users who want consistent column names after downloading FormIO submissions. Works with either a raw data frame or the list returned by FlattenSubmissions(). If audit log... |
| [PlotBarSummary](PlotBarSummary.md) | If audit logging is active (see StartAuditLog()), this action is recorded. |
| [PlotHistogram](PlotHistogram.md) | If audit logging is active (see StartAuditLog()), this action is recorded. |
| [PlotResponseTimeline](PlotResponseTimeline.md) | Convenience wrapper around ResponseTimeline() that draws a simple line chart using base graphics. If audit logging is active (see StartAuditLog()), this action is recorded. |
| [PlotWordcloud](PlotWordcloud.md) | Requires the optional wordcloud package. If it is not installed, the function will stop with a helpful message. If audit logging is active (see StartAuditLog()), this action is recorded. |
| [RenameCols](RenameCols.md) | Guides you through renaming each column in a flattened FormIO dataset. This is helpful for non-technical users who want friendly column names before analysis or export. |
| [ResolveRepeats](ResolveRepeats.md) | Automatically collapses repeated values within each submission ID using a consistent strategy (or simple heuristics when strategy = "auto"). This is a non-interactive alternative to FixDups(). |
| [ResponseTimeline](ResponseTimeline.md) | Counts responses per day/week/month (or hour), using a timestamp column. If date_col is NULL, the function tries common timestamp names automatically (e.g., created, modified, _createdAt). If audit... |
| [ReviewDuplicateSubmissions](ReviewDuplicateSubmissions.md) | This function walks you through duplicate groups defined by key columns (for example email, username, project_id). For each duplicate group, it shows selected columns so you can compare submissions... |
| [StartAuditLog](StartAuditLog.md) | Creates a new audit log file and turns on automatic logging for FormIOr functions. Each subsequent action (downloads, cleaning, exports, etc.) will append a new row to the log. |
| [StopAuditLog](StopAuditLog.md) | Turns off automatic logging. The log file is not deleted. You can start a new log later using StartAuditLog(). |
| [SummaryByField](SummaryByField.md) | Designed for non-technical users: it accepts either a data frame or the list produced by FlattenSubmissions() and returns a simple summary. Numeric fields get descriptive statistics; categorical fi... |
| [WriteAuditLog](WriteAuditLog.md) | This writes a small CSV/TSV log of key actions (e.g., downloads, cleaning, exports). It is designed to be easy for non-technical users to open in Excel. If you are using automatic logging, you gene... |

---

Navigation
- Home: [Handbook Index](../index.md)
- Function Index: [All Functions](index.md)
