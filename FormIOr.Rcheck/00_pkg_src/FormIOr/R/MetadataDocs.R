#' Describe a form in plain language
#'
#' This is a simple overview of a form: the name, version, and how many fields
#' it contains. It is meant for non-technical users who want a quick summary.
#' If audit logging is active (see [StartAuditLog()]), this action is recorded.
#'
#' What you can pass to `form`:
#' - A **schema list** (from a schema API response)
#' - A **JSON string**
#' - A **path to a JSON file**
#' - The output of [GetFormMetadata()]
#'
#' Important: [GetFormMetadata()] returns **metadata only**, not the full form.
#' If you pass metadata, this function will automatically fetch the form schema
#' using stored credentials (from [AskCredentials()] or `GetFormMetadata()`).
#' If credentials are not available, it will stop with a clear message.
#'
#' Note (CHEF): Some schema endpoints live at `/api/v1` while other endpoints
#' (like exports) are under `/app/api/v1`. If a schema fetch fails at the
#' provided `base_url`, FormIOr automatically retries the alternate base.
#'
#' Tip: Use `include = "input"` (default) to count only the fields people fill in,
#' or `include = "all"` to include layout items like panels and tabs.
#'
#' @param form Form schema list, JSON string, or path to a JSON file.
#' @param include_fields Logical. If `TRUE`, include a field dictionary in the
#'   output (from [FieldDictionary()]).
#' @param version Which form version to use when `form` is metadata (from
#'   [GetFormMetadata()]). Use `"latest"` (default), a version number (e.g., `3`),
#'   or a version ID.
#' @param include Which components to count for fields. Defaults to input fields
#'   only (`"input"`). Use `"all"` to include layout components too.
#' @param quiet Logical. If `FALSE`, prints a short summary.
#'
#' @return A list with:
#' \describe{
#'   \item{meta}{Form details like title, name, ID, and version}
#'   \item{counts}{How many components, fields, and sections}
#'   \item{fields}{Field dictionary data frame (only if `include_fields = TRUE`)}
#' }
#'
#' @export
#'
#' @examples
#' form <- list(
#'   title = "Sample Form",
#'   name = "sample_form",
#'   version = 2,
#'   components = list(
#'     list(type = "textfield", key = "first_name", label = "First name", input = TRUE),
#'     list(type = "panel", title = "Details", components = list(
#'       list(type = "number", key = "age", label = "Age", input = TRUE)
#'     ))
#'   )
#' )
#' DescribeForm(form)
#'
#' \dontrun{
#' meta <- GetFormMetadata(form_id = "123", api_key = "abc")
#' DescribeForm(meta)
#' }
DescribeForm <- function(
		form,
		include_fields = FALSE,
		version = "latest",
		include = c("input", "all"),
		quiet = FALSE
) {
	audit_depth <- audit_enter() # Track nested audit calls so we only prompt once.
	on.exit(audit_exit(), add = TRUE) # Ensure audit state is restored on exit.
	if (audit_depth == 1) maybe_prompt_audit_log() # Ask to start audit logging at the top-level call.

	include <- match.arg(include) # Validate component inclusion option.
	form <- as_form_schema(form, version = version) # Resolve to a full schema list.

	meta <- list( # Collect top-level metadata fields.
		title = form$title %||% form$name %||% form$path %||% NA_character_,
		name = form$name %||% NA_character_,
		path = form$path %||% NA_character_,
		id = form[["_id"]] %||% form$id %||% NA_character_,
		version = form$version %||% NA_character_,
		created = form$created %||% form$createdAt %||% NA_character_,
		modified = form$modified %||% form$modifiedAt %||% NA_character_,
		status = form$status %||% NA_character_
	)

	components <- form$components # Extract raw components list.
	all_components <- flatten_components(components, include = "all") # Count all components.
	field_components <- flatten_components(components, include = include) # Count input or all components.

	fields_df <- FieldDictionary(form, include = include, quiet = TRUE, version = version) # Build field dictionary.
	sections <- unique(stats::na.omit(fields_df$section)) # Count unique sections.

	counts <- list( # Summarize counts.
		components = nrow(all_components),
		fields = nrow(field_components),
		sections = length(sections)
	)

	out <- list( # Package outputs.
		meta = meta,
		counts = counts
	)

	if (include_fields) {
		out$fields <- fields_df # Optionally attach field dictionary.
	}

	if (!quiet) {
		form_name <- meta$title %||% meta$name %||% "(unnamed form)"
		message("Form ", form_name, " has ", counts$fields, " field(s) across ", counts$sections, " section(s).")
	}

	if (audit_depth == 1) {
		maybe_write_audit("DescribeForm", details = paste0("version=", meta$version)) # Record the action.
	}

	out # Return results.
}


#' Build a field dictionary for a form
#'
#' This creates a clean table that lists each field in your form. It is meant
#' to be easy to read and share with non-technical staff.
#' If audit logging is active (see [StartAuditLog()]), this action is recorded.
#'
#' If you pass the output of [GetFormMetadata()], the schema is **not** included
#' in that object. In that case the function will automatically fetch the schema
#' using stored credentials (from [AskCredentials()] or `GetFormMetadata()`).
#' If credentials are not available, it will stop with a clear message.
#'
#' Note (CHEF): Some schema endpoints live at `/api/v1` while other endpoints
#' (like exports) are under `/app/api/v1`. If a schema fetch fails at the
#' provided `base_url`, FormIOr automatically retries the alternate base.
#'
#' Tip: Use `include = "all"` to include layout components (panels, tabs,
#' fieldsets). These usually have `input = FALSE` and no field key.
#'
#' @param form Form schema list, JSON string, or path to a JSON file.
#' @param include Which components to include: `"input"` (default) or `"all"`.
#' @param version Which form version to use when `form` is metadata (from
#'   [GetFormMetadata()]). Use `"latest"` (default), a version number (e.g., `3`),
#'   or a version ID.
#' @param expand_surveys Logical. If `TRUE`, expands FormIO survey components
#'   (e.g., `simplesurveyadvanced`) into one row per question.
#' @param quiet Logical. If `FALSE`, prints a short summary.
#'
#' @return A data.frame with columns such as:
#' - `key`: field key (may be `NA` for layout components)
#' - `label`: field label or title
#' - `type`: component type (text, number, panel, etc.)
#' - `required`: whether the field is required
#' - `description`: help text/tooltip
#' - `default`: default value
#' - `options`: choice labels (for select/radio)
#' - `section`: nearest section/panel title
#' - `path`: location within the form
#' - `input`: `TRUE` for input fields, `FALSE` for layout
#'
#' @export
#'
#' @examples
#' form <- list(
#'   title = "Sample Form",
#'   components = list(
#'     list(type = "textfield", key = "first_name", label = "First name", input = TRUE),
#'     list(type = "select", key = "color", label = "Favorite color", input = TRUE,
#'          data = list(values = list(list(label = "Red", value = "red"))))
#'   )
#' )
#' FieldDictionary(form)
#'
#' \dontrun{
#' meta <- GetFormMetadata(form_id = "123", api_key = "abc")
#' FieldDictionary(meta, include = "all")
#' }
FieldDictionary <- function(
		form,
		include = c("input", "all"),
		version = "latest",
		expand_surveys = FALSE,
		quiet = FALSE
) {
	audit_depth <- audit_enter() # Track nested audit calls so we only prompt once.
	on.exit(audit_exit(), add = TRUE) # Ensure audit state is restored on exit.
	if (audit_depth == 1) maybe_prompt_audit_log() # Ask to start audit logging at the top-level call.

	include <- match.arg(include) # Validate component inclusion option.
	form <- as_form_schema(form, version = version) # Resolve to a full schema list.

	components <- form$components # Extract raw components list.
	rows <- flatten_components(components, include = include, expand_surveys = expand_surveys) # Flatten to a table.

	if (nrow(rows) == 0) {
		if (!quiet) message("No components found in form schema.")
		return(rows)
	}

	rows <- rows[, c( # Keep only the core columns.
		"key",
		"label",
		"type",
		"required",
		"description",
		"default",
		"options",
		"section",
		"path",
		"input"
	), drop = FALSE]

	if (!quiet) {
		message("Found ", nrow(rows), " field(s).")
	}

	if (audit_depth == 1) {
		maybe_write_audit("FieldDictionary", data = rows, details = paste0("include=", include)) # Record the action.
	}

	rows # Return the field dictionary.
}


#' Compare two versions of a form
#'
#' Shows what changed between two form versions:
#' fields added, removed, or updated.
#' If audit logging is active (see [StartAuditLog()]), this action is recorded.
#'
#' If you pass the output of [GetFormMetadata()], the schema is **not** included
#' in that object. In that case the function will automatically fetch the schema
#' using stored credentials (from [AskCredentials()] or `GetFormMetadata()`). If
#' credentials are not available, it will stop with a clear message.
#'
#' Tip: Use `by = "key"` for stable field keys, or `by = "path"` when keys are
#' reused in repeating sections.
#'
#' @param old Form schema list, JSON string, or path to a JSON file (older version).
#' @param new Form schema list, JSON string, or path to a JSON file (newer version).
#' @param by How to match fields across versions: `"key"` (default) or `"path"`.
#' @param include Which components to include: `"input"` (default) or `"all"`.
#' @param old_version Which form version to use when `old` is metadata (from
#'   [GetFormMetadata()]). Use `"latest"` (default), a version number, or a
#'   version ID.
#' @param new_version Which form version to use when `new` is metadata (from
#'   [GetFormMetadata()]). Use `"latest"` (default), a version number, or a
#'   version ID.
#' @param compare_cols Character vector of columns to compare for changes.
#' @param include_unchanged Logical. If `TRUE`, include unchanged fields in the output.
#' @param quiet Logical. If `FALSE`, prints a short summary.
#'
#' @return A list with:
#' \describe{
#'   \item{summary}{Counts of added, removed, changed, and unchanged fields}
#'   \item{added}{Fields only in the new version}
#'   \item{removed}{Fields only in the old version}
#'   \item{changed}{Fields that changed (with before/after values)}
#'   \item{unchanged}{Unchanged fields (if `include_unchanged = TRUE`)}
#' }
#'
#' @export
#'
#' @examples
#' old <- list(components = list(
#'   list(type = "textfield", key = "name", label = "Name", input = TRUE)
#' ))
#' new <- list(components = list(
#'   list(type = "textfield", key = "name", label = "Full name", input = TRUE),
#'   list(type = "number", key = "age", label = "Age", input = TRUE)
#' ))
#' CompareFormVersions(old, new)
#'
#' \dontrun{
#' old_meta <- GetFormMetadata(form_id = "123", api_key = "abc")
#' new_meta <- GetFormMetadata(form_id = "123", api_key = "abc")
#' CompareFormVersions(old_meta, new_meta)
#' }
CompareFormVersions <- function(
		old,
		new,
		by = c("key", "path"),
		include = c("input", "all"),
		old_version = "latest",
		new_version = "latest",
		compare_cols = c("label", "type", "required", "description", "default", "options", "section"),
		include_unchanged = FALSE,
		quiet = FALSE
) {
	audit_depth <- audit_enter() # Track nested audit calls so we only prompt once.
	on.exit(audit_exit(), add = TRUE) # Ensure audit state is restored on exit.
	if (audit_depth == 1) maybe_prompt_audit_log() # Ask to start audit logging at the top-level call.

	by <- match.arg(by) # Validate comparison key choice.
	include <- match.arg(include) # Validate component inclusion option.

	old_dict <- FieldDictionary(old, include = include, quiet = TRUE, version = old_version) # Build old dictionary.
	new_dict <- FieldDictionary(new, include = include, quiet = TRUE, version = new_version) # Build new dictionary.

	if (!by %in% names(old_dict) || !by %in% names(new_dict)) {
		stop("Comparison column '", by, "' not found in field dictionaries.")
	}

	old_dict <- old_dict[!is.na(old_dict[[by]]) & old_dict[[by]] != "", , drop = FALSE] # Drop missing keys.
	new_dict <- new_dict[!is.na(new_dict[[by]]) & new_dict[[by]] != "", , drop = FALSE]

	if (any(duplicated(old_dict[[by]]))) {
		warning("Duplicate keys found in old form; using first occurrence.")
		old_dict <- old_dict[!duplicated(old_dict[[by]]), , drop = FALSE]
	}
	if (any(duplicated(new_dict[[by]]))) {
		warning("Duplicate keys found in new form; using first occurrence.")
		new_dict <- new_dict[!duplicated(new_dict[[by]]), , drop = FALSE]
	}

	old_ids <- old_dict[[by]] # IDs from old dictionary.
	new_ids <- new_dict[[by]] # IDs from new dictionary.

	added <- new_dict[!new_ids %in% old_ids, , drop = FALSE] # New fields.
	removed <- old_dict[!old_ids %in% new_ids, , drop = FALSE] # Removed fields.

	common_ids <- intersect(old_ids, new_ids) # Fields present in both versions.
	idx_old <- match(common_ids, old_ids)
	idx_new <- match(common_ids, new_ids)

	compare_cols <- intersect(compare_cols, intersect(names(old_dict), names(new_dict))) # Only compare shared columns.

	changed_rows <- list() # Accumulate changed fields.
	unchanged_rows <- list() # Accumulate unchanged rows when requested.

	for (i in seq_along(common_ids)) {
		old_row <- old_dict[idx_old[i], , drop = FALSE]
		new_row <- new_dict[idx_new[i], , drop = FALSE]

		diff_cols <- compare_cols[vapply(compare_cols, function(col) { # Identify changed columns.
			!values_equal(old_row[[col]], new_row[[col]])
		}, logical(1))]

		if (length(diff_cols) > 0) {
			row <- list()
			row[[by]] <- common_ids[i]
			row$changes <- paste(diff_cols, collapse = ", ")
			for (col in compare_cols) {
				row[[paste0(col, "_old")]] <- old_row[[col]]
				row[[paste0(col, "_new")]] <- new_row[[col]]
			}
			changed_rows[[length(changed_rows) + 1]] <- row
		} else if (include_unchanged) {
			unchanged_rows[[length(unchanged_rows) + 1]] <- new_row
		}
	}

	changed <- bind_rows_safe(changed_rows) # Bind change rows safely.
	unchanged <- bind_rows_safe(unchanged_rows) # Bind unchanged rows safely.

	summary <- data.frame( # Build summary counts.
		metric = c("added", "removed", "changed", "unchanged"),
		count = c(nrow(added), nrow(removed), nrow(changed), length(common_ids) - nrow(changed)),
		stringsAsFactors = FALSE
	)

	out <- list( # Package outputs.
		by = by,
		summary = summary,
		added = added,
		removed = removed,
		changed = changed
	)

	if (include_unchanged) {
		out$unchanged <- unchanged
	}

	if (!quiet) {
		message("Added: ", nrow(added), ", removed: ", nrow(removed), ", changed: ", nrow(changed), ".")
	}

	if (audit_depth == 1) {
		maybe_write_audit("CompareFormVersions", details = paste0("by=", by, "; include=", include)) # Record the action.
	}

	out # Return results.
}


# ---- Internal helpers -----------------------------------------------------------

#' Resolve a form schema from various inputs
#'
#' Accepts a schema list, JSON string, file path, or metadata list and returns
#' a schema list with `components`.
#'
#' @param form Form schema list, JSON string, file path, or metadata list.
#' @param version Version identifier used when fetching schema from metadata.
#' @return A form schema list.
#' @keywords internal
as_form_schema <- function(form, version = "latest") {
	if (is.null(form)) stop("form cannot be NULL.")

	if (is.character(form) && length(form) == 1) {
		if (file.exists(form)) {
			json <- paste(readLines(form, warn = FALSE), collapse = "\n") # Read JSON file.
			form <- jsonlite::fromJSON(json, simplifyVector = FALSE) # Parse into list.
		} else if (grepl("^\\s*\\{", form)) {
			form <- jsonlite::fromJSON(form, simplifyVector = FALSE) # Parse JSON string.
		} else {
			stop("form must be a list, JSON string, or file path.")
		}
	}

	if (!is.list(form)) stop("form must be a list, JSON string, or file path.")

	# If this looks like metadata, prefer fetching the full schema before
	# falling back to any embedded/nested components.
	if (looks_like_metadata(form)) {
		fetched <- try_fetch_schema(form, version = version) # Attempt to fetch full schema.
		if (!is.null(fetched)) return(fetched)
	}

	# Prefer the best schema-like candidate anywhere in the object, even if
	# the top-level contains a small 'components' list.
	found <- find_schema_root(form, require_schema_like = TRUE) # Search for schema-like candidates.
	if (!is.null(found)) return(found)

	if (has_components_field(form)) return(form)
	if (is.list(form$form) && has_components_field(form$form)) return(form$form)
	if (is.list(form$schema) && has_components_field(form$schema)) return(form$schema)

	found <- find_schema_root(form, require_schema_like = FALSE) # Fallback to any component root.
	if (!is.null(found)) return(found)

	fetched <- try_fetch_schema(form, version = version) # Last-ditch fetch attempt.
	if (!is.null(fetched)) return(fetched)

	stop("Could not find a form schema with components. If you passed GetFormMetadata() output, make sure credentials are available so the schema can be fetched automatically.")
}


#' Check whether an object looks like metadata
#'
#' @param x Object to inspect.
#' @return `TRUE` if it looks like a metadata response.
#' @keywords internal
looks_like_metadata <- function(x) {
	if (!is.list(x)) return(FALSE)
	if (!is.null(x$versions)) return(TRUE)
	if (!is.null(x$metadata)) return(TRUE)
	FALSE
}


#' Flatten components into a row table
#'
#' @param components Component list from a schema.
#' @param include Which components to include ("input" or "all").
#' @param expand_surveys Logical. Expand survey components into question rows.
#' @return A data.frame of component rows.
#' @keywords internal
flatten_components <- function(components, include = c("input", "all"), expand_surveys = FALSE) {
	include <- match.arg(include)
	components <- normalize_list(components) # Normalize list shape.
	rows <- collect_components(
		components,
		parent_path = NULL,
		section = NULL,
		include = include,
		expand_surveys = expand_surveys
	)

	if (length(rows) == 0) {
		return(data.frame(
			key = character(0),
			label = character(0),
			type = character(0),
			required = logical(0),
			description = character(0),
			default = character(0),
			options = character(0),
			section = character(0),
			path = character(0),
			input = logical(0),
			stringsAsFactors = FALSE
		))
	}

	df <- do.call(rbind, lapply(rows, function(x) {
		as.data.frame(x, stringsAsFactors = FALSE)
	})) # Combine rows into a data frame.

	rownames(df) <- NULL # Drop row names.
	df # Return the flattened component table.
}


#' Collect components recursively
#'
#' @param components Component list to traverse.
#' @param parent_path Current path prefix.
#' @param section Current section label.
#' @param include Which components to include.
#' @param expand_surveys Logical. Expand survey components into question rows.
#' @return A list of row lists describing components.
#' @keywords internal
collect_components <- function(
		components,
		parent_path = NULL,
		section = NULL,
		include = "input",
		expand_surveys = FALSE
) {
	rows <- list() # Accumulate component rows.
	components <- normalize_list(components) # Normalize list shape.
	if (is.null(components) || length(components) == 0) return(rows)

	for (i in seq_along(components)) {
		comp <- components[[i]] # Current component.
		if (!is.list(comp)) next

		comp_type <- comp$type %||% NA_character_ # Component type.
		key <- comp$key %||% NA_character_ # Component key.
		label <- first_non_empty(comp$label, comp$title, comp$legend, comp$caption, key, comp_type, paste0("component_", i)) # Best label.

		input_flag <- detect_input_flag(comp) # Input vs layout.
		path_key <- build_path_key(key, label, comp_type, i) # Stable path key.
		path <- build_path(parent_path, path_key) # Hierarchical path.

		required_raw <- comp$validate$required %||% comp$required
		required <- if (is.null(required_raw)) NA else isTRUE(required_raw)

		description <- comp$description %||% comp$tooltip %||% comp$help # Help text.
		default <- comp$defaultValue %||% comp$default # Default value.
		options <- extract_options(comp) # Options for select-like components.

		row <- list( # Build component row.
			key = if (is.null(key) || is.na(key)) NA_character_ else as.character(key),
			label = as.character(label),
			type = if (is.null(comp_type) || is.na(comp_type)) NA_character_ else as.character(comp_type),
			required = required,
			description = to_character(description),
			default = to_character(default),
			options = to_character(options),
			section = if (is.null(section)) NA_character_ else as.character(section),
			path = if (is.null(path)) NA_character_ else as.character(path),
			input = input_flag
		)

		is_survey <- isTRUE(expand_surveys) &&
			is_survey_type(comp_type) &&
			is.list(comp$questions) &&
			length(comp$questions) > 0

		if (is_survey) {
			question_rows <- build_survey_question_rows(
				comp = comp,
				parent_path = parent_path,
				section = section,
				parent_key = key,
				parent_path_full = path
			)
			if (length(question_rows) > 0) {
				rows <- c(rows, question_rows) # Append survey question rows.
			}
		} else if (include == "all" || (include == "input" && isTRUE(input_flag))) {
			rows[[length(rows) + 1]] <- row # Append component row.
		}

		section_next <- section # Default to current section.
		if (is_section_type(comp_type)) {
			section_next <- label # Update section label when a section is encountered.
		}

		child_components <- extract_child_components(comp) # Recurse into children.
		if (length(child_components) > 0) {
			rows <- c(rows, collect_components(
				child_components,
				parent_path = path,
				section = section_next,
				include = include,
				expand_surveys = expand_surveys
			))
		}
	}

	rows # Return collected rows.
}


#' Check if component type is a survey
#'
#' @param type Component type string.
#' @return `TRUE` if the type is a survey component.
#' @keywords internal
is_survey_type <- function(type) {
	if (is.null(type) || is.na(type)) return(FALSE)
	grepl("survey", tolower(as.character(type)))
}


#' Build rows for survey question components
#'
#' @param comp Survey component definition.
#' @param parent_path Parent path key.
#' @param section Current section label.
#' @param parent_key Parent key name.
#' @param parent_path_full Parent path for display.
#' @return A list of row lists for each survey question.
#' @keywords internal
build_survey_question_rows <- function(comp, parent_path, section, parent_key, parent_path_full) {
	questions <- normalize_list(comp$questions)
	if (length(questions) == 0) return(list())

	comp_type <- comp$type %||% NA_character_
	required_raw <- comp$validate$required %||% comp$required
	required <- if (is.null(required_raw)) NA else isTRUE(required_raw)
	description <- comp$description %||% comp$tooltip %||% comp$help
	default <- comp$defaultValue %||% comp$default
	options <- extract_options(comp)

	rows <- list() # Accumulate question rows.
	for (i in seq_along(questions)) {
		q <- questions[[i]]
		if (!is.list(q)) next

		q_label <- first_non_empty(q$label, q$value, paste0("question_", i)) # Best label.
		q_value <- q$value %||% q$label %||% paste0("question_", i) # Underlying value.
		q_value_chr <- if (is.null(q_value) || is.na(q_value)) paste0("question_", i) else as.character(q_value)

		q_key <- q_value_chr # Default key from value.
		if (!is.null(parent_key) && !is.na(parent_key) && nzchar(as.character(parent_key))) {
			q_key <- paste0(as.character(parent_key), ".", q_value_chr) # Prefix with parent key.
		}

		q_path_key <- build_path_key(q_value_chr, q_label, comp_type, i) # Path key per question.
		q_path <- build_path(parent_path_full, q_path_key) # Full path for display.

		rows[[length(rows) + 1]] <- list( # Append question row.
			key = q_key,
			label = as.character(q_label),
			type = if (is.null(comp_type) || is.na(comp_type)) NA_character_ else as.character(comp_type),
			required = required,
			description = to_character(description),
			default = to_character(default),
			options = to_character(options),
			section = if (is.null(section)) NA_character_ else as.character(section),
			path = if (is.null(q_path)) NA_character_ else as.character(q_path),
			input = TRUE
		)
	}

	rows # Return question rows.
}


#' Extract child components from a component
#'
#' @param comp Component definition.
#' @return A list of child components (possibly empty).
#' @keywords internal
extract_child_components <- function(comp) {
	children <- list() # Accumulate child components.

	if (!is.null(comp$components)) {
		children <- c(children, normalize_list(comp$components)) # Direct child components.
	}
	if (!is.null(comp$columns)) {
		cols <- normalize_list(comp$columns)
		for (col in cols) {
			if (is.list(col) && !is.null(col$components)) {
				children <- c(children, normalize_list(col$components)) # Column components.
			}
		}
	}
	if (!is.null(comp$rows)) {
		rows <- normalize_list(comp$rows)
		for (row in rows) {
			if (!is.list(row)) next
			for (col in row) {
				if (is.list(col) && !is.null(col$components)) {
					children <- c(children, normalize_list(col$components)) # Row components.
				}
			}
		}
	}
	if (!is.null(comp$tabs)) {
		tabs <- normalize_list(comp$tabs)
		for (tab in tabs) {
			if (is.list(tab) && !is.null(tab$components)) {
				children <- c(children, normalize_list(tab$components)) # Tab components.
			}
		}
	}

	children # Return child components.
}


#' Extract selectable options from a component
#'
#' @param comp Component definition.
#' @return A character string of options, or `NULL` if unavailable.
#' @keywords internal
extract_options <- function(comp) {
	values <- NULL
	if (is.list(comp$data) && !is.null(comp$data$values)) {
		values <- comp$data$values
	} else if (!is.null(comp$values)) {
		values <- comp$values
	}

	if (is.null(values)) return(NULL)

	if (is.data.frame(values)) {
		values <- split(values, seq_len(nrow(values))) # Convert rows to list.
	}

	if (is.list(values) && length(values) > 0 && is.list(values[[1]])) {
		labels <- vapply(values, function(v) {
			lab <- v$label %||% v$value %||% ""
			val <- v$value %||% v$label %||% ""
			if (identical(lab, val) || val == "") {
				as.character(lab)
			} else {
				paste0(lab, " (", val, ")")
			}
		}, character(1))
		return(paste(labels, collapse = "; ")) # Concatenate labels for display.
	}

	if (is.atomic(values)) {
		return(paste(as.character(values), collapse = "; "))
	}

	jsonlite::toJSON(values, auto_unbox = TRUE)
}


#' Coerce to a single character string
#'
#' @param x Value to coerce.
#' @return A single character value or `NA_character_`.
#' @keywords internal
to_character <- function(x) {
	if (is.null(x)) return(NA_character_)
	if (is.atomic(x)) return(paste(as.character(x), collapse = "; "))
	jsonlite::toJSON(x, auto_unbox = TRUE)
}


#' Normalize list inputs
#'
#' @param x List-like input.
#' @return A list (possibly empty).
#' @keywords internal
normalize_list <- function(x) {
	if (is.null(x)) return(list())
	if (is.data.frame(x)) {
		if (nrow(x) == 0) return(list())
		rows <- split(x, seq_len(nrow(x)))
		return(lapply(rows, normalize_row)) # Normalize each row.
	}
	if (is.list(x)) return(x)
	list()
}


#' Normalize a row to a list
#'
#' @param row_df Data frame row or list.
#' @return A list with scalar elements.
#' @keywords internal
normalize_row <- function(row_df) {
	row <- as.list(row_df)
	lapply(row, function(cell) {
		if (is.list(cell) && length(cell) == 1 && !is.data.frame(cell)) {
			cell[[1]]
		} else {
			cell
		}
	})
}


#' Check for a components field
#'
#' @param x Object to inspect.
#' @return `TRUE` if `x$components` exists and is list-like.
#' @keywords internal
has_components_field <- function(x) {
	is.list(x) && !is.null(x$components) && (is.list(x$components) || is.data.frame(x$components))
}


#' Check if an object looks like a schema root
#'
#' @param x Object to inspect.
#' @return `TRUE` when the object is a plausible schema root.
#' @keywords internal
is_schema_root_candidate <- function(x) {
	if (!has_components_field(x)) return(FALSE)

	type <- tolower(x$type %||% "")
	allowed_types <- c("", "form", "wizard", "survey", "resource", "container")
	if (!(type %in% allowed_types)) return(FALSE)
	if (type != "") return(TRUE)

	key <- x$key %||% ""
	if (nzchar(key)) return(FALSE)

	TRUE
}


#' Count components in a list or data frame
#'
#' @param components Component list or data frame.
#' @return Integer count of components.
#' @keywords internal
components_count <- function(components) {
	if (is.null(components)) return(0L)
	if (is.data.frame(components)) return(nrow(components))
	if (is.list(components)) return(length(components))
	0L
}


#' Collect schema-like candidates from nested lists
#'
#' @param x Object to traverse.
#' @param require_schema_like Logical. Require strict schema root criteria.
#' @param out Accumulator for recursion.
#' @return A list of candidate schema objects.
#' @keywords internal
collect_schema_candidates <- function(x, require_schema_like = TRUE, out = NULL) {
	if (!is.list(x) || is.data.frame(x)) return(out)

	if (require_schema_like) {
		if (is_schema_root_candidate(x)) out <- c(out, list(x))
	} else if (has_components_field(x)) {
		out <- c(out, list(x))
	}

	for (child in x) {
		if (is.list(child)) {
			out <- collect_schema_candidates(child, require_schema_like = require_schema_like, out = out)
		}
	}

	out
}


#' Find the best schema root in a nested object
#'
#' @param x Object to traverse.
#' @param require_schema_like Logical. Require strict schema root criteria.
#' @return The best matching schema list, or `NULL`.
#' @keywords internal
find_schema_root <- function(x, require_schema_like = TRUE) {
	candidates <- collect_schema_candidates(x, require_schema_like = require_schema_like, out = NULL)
	if (length(candidates) == 0) return(NULL)

	if (require_schema_like) {
		types <- vapply(candidates, function(obj) {
			tolower(obj$type %||% "")
		}, character(1))
		if (any(nzchar(types))) {
			candidates <- candidates[nzchar(types)]
		}
	}

	scores <- vapply(candidates, function(obj) {
		components_count(obj$components) # Prefer objects with more components.
	}, numeric(1))

	candidates[[which.max(scores)]]
}


#' Get the configured base URL
#'
#' @return Base URL for FormIO API.
#' @keywords internal
get_base_url <- function() {
	getOption("FormIOr.base_url", "https://submit.digital.gov.bc.ca/app/api/v1")
}


#' Compute the alternate CHEF base URL
#'
#' @param base_url Base API URL.
#' @return Alternate base URL or `NULL`.
#' @keywords internal
alternate_base_url <- function(base_url) {
	if (is.null(base_url) || !nzchar(base_url)) return(NULL)
	if (grepl("/app/api/v1/?$", base_url)) {
		return(sub("/app/api/v1/?$", "/api/v1", base_url))
	}
	if (grepl("/api/v1/?$", base_url) && !grepl("/app/api/v1/?$", base_url)) {
		return(sub("/api/v1/?$", "/app/api/v1", base_url))
	}
	NULL
}


#' Resolve a form ID from a metadata object
#'
#' @param form Metadata list.
#' @return Form ID string or `NULL`.
#' @keywords internal
resolve_form_id <- function(form) {
	if (!is.list(form)) return(NULL)
	candidates <- c(
		form$formId %||% NULL,
		form$id %||% NULL,
		form[["_id"]] %||% NULL
	)

	if (is.data.frame(form$versions) && "formId" %in% names(form$versions)) {
		row <- select_version_row(form$versions) # Prefer latest/published version.
		candidates <- c(candidates, row$formId %||% NULL)
	}

	candidates <- candidates[!is.na(candidates) & candidates != ""]
	if (length(candidates) == 0) return(NULL)
	candidates[1]
}


#' Resolve a version ID from metadata
#'
#' @param form Metadata list with versions.
#' @param version Version identifier or "latest".
#' @return Version ID string or `NULL`.
#' @keywords internal
resolve_version_id <- function(form, version = "latest") {
	if (!is.data.frame(form$versions) || !"id" %in% names(form$versions)) return(NULL)

	versions <- form$versions
	if (!is.null(version) && !identical(version, "latest")) {
		version_chr <- as.character(version)[1]

		if ("id" %in% names(versions) && version_chr %in% versions$id) {
			return(versions$id[match(version_chr, versions$id)])
		}

		if ("version" %in% names(versions)) {
			ver_vals <- versions$version
			if (is.numeric(version)) {
				idx <- which(suppressWarnings(as.numeric(ver_vals)) == as.numeric(version))
			} else {
				idx <- which(as.character(ver_vals) == version_chr)
			}
			if (length(idx) > 0) return(versions$id[idx[1]])
		}

		available <- if ("version" %in% names(versions)) unique(versions$version) else versions$id # Show available versions.
		stop("Version '", version, "' not found. Available versions: ", paste(available, collapse = ", "))
	}

	row <- select_version_row(versions)
	row$id %||% NULL
}


#' Resolve FormIO credentials
#'
#' @param form_id Optional form ID.
#' @param api_key Optional API key.
#' @param reenter.credentials Logical. Force re-entry of credentials.
#' @return A list with `form_id` and `api_key`.
#' @keywords internal
resolve_credentials <- function(form_id = NULL, api_key = NULL, reenter.credentials = FALSE) {
	Form_Info <- NULL

	stored <- .formior_state$Form_Info
	has_stored <- !is.null(stored) &&
		length(stored) >= 2 &&
		nzchar(as.character(stored[[1]])) &&
		nzchar(as.character(stored[[2]]))

	if (reenter.credentials) {
		Form_Info <- AskCredentials() # Force prompt.
	} else if (has_stored) {
		Form_Info <- stored # Use cached credentials.
	} else if (interactive() && (is.null(form_id) || is.null(api_key) || !nzchar(form_id) || !nzchar(api_key))) {
		Form_Info <- AskCredentials() # Prompt if missing.
	}

	if ((is.null(form_id) || !nzchar(form_id)) && !is.null(Form_Info)) {
		form_id <- Form_Info[["ID"]] %||% Form_Info[1]
	}
	if ((is.null(api_key) || !nzchar(api_key)) && !is.null(Form_Info)) {
		api_key <- Form_Info[["Key"]] %||% Form_Info[2]
	}

	if (is.null(form_id) || is.null(api_key) || !nzchar(form_id) || !nzchar(api_key)) {
		stop("Form ID or API key not available. Run AskCredentials() or pass form_id and api_key.")
	}

	# Store the resolved credentials for the current R session so subsequent calls
	# do not re-prompt.
	.formior_state$Form_Info <- c(ID = form_id, Key = api_key) # Cache for the session.
	list(form_id = form_id, api_key = api_key)
}


#' Attempt to fetch the schema from metadata
#'
#' @param form Metadata list.
#' @param version Version identifier.
#' @return A schema list or `NULL`.
#' @keywords internal
try_fetch_schema <- function(form, version = "latest") {
	form_id <- resolve_form_id(form)
	if (is.null(form_id)) return(NULL)

	creds <- tryCatch(resolve_credentials(form_id = form_id), error = function(e) NULL) # Resolve credentials.
	if (is.null(creds)) return(NULL)

	version_id <- resolve_version_id(form, version = version) # Pick a version ID.
	base_url <- get_base_url() # Use configured base URL.
	fetch_form_schema(base_url, creds$form_id, creds$api_key, version_id = version_id)
}


#' Fetch a form schema from the API
#'
#' @param base_url Base API URL.
#' @param form_id Form ID.
#' @param api_key API key.
#' @param version_id Optional version ID.
#' @param tried_alt Logical. Whether an alternate base URL has been tried.
#' @return A schema list or `NULL`.
#' @keywords internal
fetch_form_schema <- function(base_url, form_id, api_key, version_id = NULL, tried_alt = FALSE) {
	paths <- c("/schema", "/form", "/definition", "", "/draft", "/live")

	for (path in paths) {
		url <- paste0(base_url, "/forms/", form_id, path)
		schema <- try_fetch_schema_url(url, form_id, api_key) # Attempt to fetch schema.
		if (!is.null(schema)) return(schema)
	}

	if (!is.null(version_id) && nzchar(version_id)) {
		version_paths <- c(
			paste0(base_url, "/forms/", form_id, "/versions/", version_id, "/schema"),
			paste0(base_url, "/forms/", form_id, "/versions/", version_id, "/form"),
			paste0(base_url, "/forms/", form_id, "/versions/", version_id)
		)
		for (url in version_paths) {
			schema <- try_fetch_schema_url(url, form_id, api_key)
			if (!is.null(schema)) return(schema)
		}
	}

	if (!isTRUE(tried_alt)) {
		alt_base <- alternate_base_url(base_url)
		if (!is.null(alt_base) && !identical(alt_base, base_url)) {
			schema <- fetch_form_schema(alt_base, form_id, api_key, version_id = version_id, tried_alt = TRUE) # Retry with alternate base.
			if (!is.null(schema)) return(schema)
		}
	}

	NULL
}


#' Try fetching a schema URL
#'
#' @param url Schema endpoint URL.
#' @param form_id Form ID.
#' @param api_key API key.
#' @return A schema list or `NULL`.
#' @keywords internal
try_fetch_schema_url <- function(url, form_id, api_key) {
	resp <- tryCatch(
		GET(
			url = url,
			authenticate(user = form_id, password = api_key, type = "basic"),
			add_headers(Accept = "application/json"),
			timeout(30)
		),
		error = function(e) NULL
	)

	if (is.null(resp)) return(NULL)
	if (status_code(resp) < 200 || status_code(resp) >= 300) return(NULL) # Only accept 2xx responses.

	text <- content(resp, as = "text", encoding = "UTF-8") # Read response body.
	if (!nzchar(text)) return(NULL)

	parsed <- tryCatch(jsonlite::fromJSON(text, simplifyVector = FALSE), error = function(e) NULL) # Parse JSON safely.
	if (is.null(parsed)) return(NULL)

	schema <- find_schema_root(parsed, require_schema_like = TRUE) # Prefer schema-like objects.
	if (is.null(schema)) schema <- find_schema_root(parsed, require_schema_like = FALSE)
	schema
}


#' Select the best version row
#'
#' @param versions Versions data.frame.
#' @return A single-row data.frame (or list) for the best version.
#' @keywords internal
select_version_row <- function(versions) {
	if (!is.data.frame(versions) || nrow(versions) == 0) return(list())

	df <- versions
	if ("published" %in% names(df)) {
		pub <- df$published
		if (is.character(pub)) pub <- tolower(pub) %in% c("true", "t", "1", "yes", "y")
		if (is.logical(pub) && any(pub, na.rm = TRUE)) {
			df <- df[pub %in% TRUE, , drop = FALSE] # Prefer published versions.
		}
	}

	if ("version" %in% names(df)) {
		ver_num <- suppressWarnings(as.numeric(df$version))
		if (any(!is.na(ver_num))) {
			return(df[which.max(ver_num), , drop = FALSE])
		}
	}

	for (col in c("updatedAt", "createdAt")) {
		if (col %in% names(df)) {
			times <- suppressWarnings(as.POSIXct(df[[col]]))
			if (any(!is.na(times))) {
				return(df[which.max(times), , drop = FALSE])
			}
		}
	}

	df[1, , drop = FALSE]
}


#' Build a stable path key for a component
#'
#' @param key Component key.
#' @param label Component label.
#' @param comp_type Component type.
#' @param index Position index used as fallback.
#' @return A path-safe key string.
#' @keywords internal
build_path_key <- function(key, label, comp_type, index) {
	if (!is.null(key) && !is.na(key) && nzchar(as.character(key))) {
		return(as.character(key))
	}

	base <- first_non_empty(label, comp_type, paste0("component_", index))
	base <- gsub("[^a-zA-Z0-9]+", "_", base)
	base <- gsub("^_+|_+$", "", base)
	if (is.na(base) || base == "") base <- paste0("component_", index)

	paste0(base, "_", index) # Ensure uniqueness with index suffix.
}


#' Build a dotted path from parent and key
#'
#' @param parent_path Parent path string.
#' @param key Current key segment.
#' @return Combined path string.
#' @keywords internal
build_path <- function(parent_path, key) {
	if (is.null(parent_path) || parent_path == "") return(key)
	paste(parent_path, key, sep = ".")
}


#' Check if a type represents a section
#'
#' @param type Component type string.
#' @return `TRUE` if the type is a section-like layout.
#' @keywords internal
is_section_type <- function(type) {
	if (is.null(type) || is.na(type)) return(FALSE)
	type %in% c("panel", "fieldset", "well", "tabs", "tab")
}


#' Determine whether a component is an input
#'
#' @param comp Component definition.
#' @return `TRUE` if component is an input field.
#' @keywords internal
detect_input_flag <- function(comp) {
	if (!is.null(comp$input)) return(isTRUE(comp$input))
	type <- comp$type %||% ""
	if (!is.null(comp$key) && nzchar(comp$key) && !is_layout_type(type)) {
		return(TRUE)
	}
	FALSE
}


#' Check if a component type is layout-only
#'
#' @param type Component type string.
#' @return `TRUE` if the type is layout-only.
#' @keywords internal
is_layout_type <- function(type) {
	if (is.null(type) || is.na(type)) return(TRUE)
	type %in% c("panel", "fieldset", "well", "tabs", "tab", "columns", "column", "table", "content", "html", "button")
}


#' Compare two values for equality
#'
#' @param x First value.
#' @param y Second value.
#' @return `TRUE` if the normalized values are equal.
#' @keywords internal
values_equal <- function(x, y) {
	x_val <- if (length(x) == 0) NA_character_ else x
	y_val <- if (length(y) == 0) NA_character_ else y

	x_char <- to_character(x_val) # Normalize to character.
	y_char <- to_character(y_val)

	if (is.na(x_char) && is.na(y_char)) return(TRUE)
	# Normalize whitespace and empty strings for comparison.
	normalize <- function(z) {
		z <- trimws(z)
		if (z == "") NA_character_ else z
	}

	x_char <- normalize(x_char)
	y_char <- normalize(y_char)

	if (is.na(x_char) && is.na(y_char)) return(TRUE)
	identical(x_char, y_char)
}


#' Safely bind a list of row lists
#'
#' @param rows List of row lists.
#' @return A data.frame (possibly empty).
#' @keywords internal
bind_rows_safe <- function(rows) {
	if (length(rows) == 0) {
		return(data.frame(stringsAsFactors = FALSE))
	}
	df <- do.call(rbind, lapply(rows, function(x) as.data.frame(x, stringsAsFactors = FALSE))) # Bind rows.
	rownames(df) <- NULL
	df
}


#' Return the first non-empty value
#'
#' @param ... Candidate values.
#' @return First non-empty character value or `NA_character_`.
#' @keywords internal
first_non_empty <- function(...) {
	values <- list(...)
	for (val in values) {
		if (is.null(val)) next
		if (length(val) == 0) next
		if (is.list(val) && length(val) == 1 && !is.data.frame(val)) {
			val <- val[[1]]
		}
		val_chr <- as.character(val)[1]
		if (is.na(val_chr)) next
		if (!nzchar(trimws(val_chr))) next
		return(val_chr)
	}
	NA_character_
}
