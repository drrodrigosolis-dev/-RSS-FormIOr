#' Launch the FormIOr RStudio addin
#'
#' Provides a minimal point-and-click interface for key FormIOr functions.
#'
#' @export
FormIOrAddin <- function() {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required for the FormIOr addin. Install with install.packages('shiny').")
  }
  if (!requireNamespace("miniUI", quietly = TRUE)) {
    stop("Package 'miniUI' is required for the FormIOr addin. Install with install.packages('miniUI').")
  }

  default_log_file <- file.path(tempdir(), "formior_audit_log.csv")
  default_export_path <- file.path(tempdir(), "formior_export.xlsx")

  ui <- miniUI::miniPage(
    shiny::tags$style(shiny::HTML("
      .formior-preview { width: 100%; }
      .formior-preview-wrap {
        width: 100%;
        height: calc(100vh - 200px);
      }
      .formior-preview-wrap .dataTables_wrapper {
        width: 100% !important;
        height: 100% !important;
      }
      .formior-preview-wrap .dataTables_scroll {
        height: 100% !important;
      }
      .formior-preview-wrap .dataTables_scrollBody {
        overflow: auto !important;
        height: calc(100vh - 260px) !important;
        max-height: calc(100vh - 260px) !important;
      }
      .formior-preview table.dataTable { width: 100% !important; }
      .formior-preview-wrap .reactable {
        height: 100% !important;
      }
      .formior-preview-wrap .reactable .rt-table {
        width: 100%;
      }
    ")),
    miniUI::gadgetTitleBar("FormIOr Addin"),
    miniUI::miniTabstripPanel(
      miniUI::miniTabPanel(
        "Get Responses",
        miniUI::miniContentPanel(
          shiny::textInput("base_url", "Base URL", value = "https://submit.digital.gov.bc.ca/app/api/v1"),
          shiny::textInput("form_id", "Form ID", value = ""),
          shiny::passwordInput("api_key", "API Key", value = ""),
          shiny::checkboxInput("drafts", "Include drafts", value = FALSE),
          shiny::checkboxInput("deleted", "Include deleted", value = FALSE),
          shiny::selectInput(
            "content_only",
            "Return content",
            choices = c("Parsed submissions" = "TRUE", "Full response" = "FALSE", "Raw JSON" = "raw"),
            selected = "TRUE"
          ),
          shiny::checkboxInput("reenter_creds", "Force re-enter credentials", value = FALSE),
          shiny::checkboxInput("log_get", "Log this action", value = FALSE),
          shiny::textInput("log_file_get", "Audit log file", value = default_log_file),
          shiny::textInput("responses_name", "Save as (object name)", value = "responses"),
          shiny::actionButton("run_get", "Run GetResponses"),
          shiny::actionButton("undo_get", "Undo GetResponses")
        )
      ),
      miniUI::miniTabPanel(
        "Flatten",
        miniUI::miniContentPanel(
          shiny::textInput("flatten_source", "Source object name", value = "responses"),
          shiny::textInput("flatten_name", "Save as (object name)", value = "flat"),
          shiny::checkboxInput("log_flatten", "Log this action", value = FALSE),
          shiny::textInput("log_file_flatten", "Audit log file", value = default_log_file),
          shiny::actionButton("run_flatten", "Run FlattenSubmissions"),
          shiny::actionButton("undo_flatten", "Undo FlattenSubmissions"),
          shiny::p("Note: objects are read from and saved to the Global Environment.")
        )
      ),
      miniUI::miniTabPanel(
        "Normalize Names",
        miniUI::miniContentPanel(
          shiny::textInput("norm_source", "Source object name", value = "flat"),
          shiny::selectInput(
            "norm_style",
            "Style",
            choices = c("snake", "lower", "upper", "title"),
            selected = "snake"
          ),
          shiny::checkboxInput("norm_unique", "Make unique", value = TRUE),
          shiny::checkboxInput("norm_translit", "Transliterate", value = TRUE),
          shiny::checkboxInput("norm_return_flat", "Return flat list", value = FALSE),
          shiny::checkboxInput("norm_quiet", "Quiet", value = FALSE),
          shiny::checkboxInput("log_norm", "Log this action", value = FALSE),
          shiny::textInput("log_file_norm", "Audit log file", value = default_log_file),
          shiny::textInput("norm_name", "Save as (object name)", value = "norm"),
          shiny::actionButton("run_norm", "Run NormalizeColumnNames"),
          shiny::actionButton("undo_norm", "Undo NormalizeColumnNames")
        )
      ),
      miniUI::miniTabPanel(
        "Resolve Repeats",
        miniUI::miniContentPanel(
          shiny::textInput("resolve_source", "Source object name", value = "flat"),
          shiny::textInput("resolve_id_col", "ID column (name or index)", value = "1"),
          shiny::selectInput(
            "resolve_strategy",
            "Strategy",
            choices = c("auto", "concat", "first", "last", "sum", "mean", "count", "count_yes"),
            selected = "auto"
          ),
          shiny::textInput("resolve_sep", "Separator", value = ", "),
          shiny::checkboxInput("resolve_unique", "Unique values", value = TRUE),
          shiny::checkboxInput("resolve_return_flat", "Return flat list", value = FALSE),
          shiny::checkboxInput("resolve_quiet", "Quiet", value = FALSE),
          shiny::checkboxInput("log_resolve", "Log this action", value = FALSE),
          shiny::textInput("log_file_resolve", "Audit log file", value = default_log_file),
          shiny::textInput("resolve_name", "Save as (object name)", value = "resolved"),
          shiny::actionButton("run_resolve", "Run ResolveRepeats"),
          shiny::actionButton("undo_resolve", "Undo ResolveRepeats")
        )
      ),
      miniUI::miniTabPanel(
        "Codebook",
        miniUI::miniContentPanel(
          shiny::textInput("codebook_source", "Data object name", value = "flat"),
          shiny::textInput("codebook_form", "Form/schema object name (optional)", value = ""),
          shiny::selectInput(
            "codebook_include",
            "Include",
            choices = c("input", "all"),
            selected = "input"
          ),
          shiny::checkboxInput("codebook_summary", "Include summary", value = TRUE),
          shiny::numericInput("codebook_max_levels", "Max levels", value = 20, min = 1),
          shiny::checkboxInput("codebook_quiet", "Quiet", value = FALSE),
          shiny::checkboxInput("log_codebook", "Log this action", value = FALSE),
          shiny::textInput("log_file_codebook", "Audit log file", value = default_log_file),
          shiny::textInput("codebook_name", "Save as (object name)", value = "codebook"),
          shiny::actionButton("run_codebook", "Run MakeCodebook"),
          shiny::actionButton("undo_codebook", "Undo MakeCodebook")
        )
      ),
      miniUI::miniTabPanel(
        "Export",
        miniUI::miniContentPanel(
          shiny::textInput("export_source", "Source object name", value = "flat"),
          shiny::textInput("export_path", "Output path", value = default_export_path),
          shiny::actionButton("export_browse", "Browse"),
          shiny::textInput("export_sheet", "Sheet name (single data.frame)", value = "Data"),
          shiny::checkboxInput("export_overwrite", "Overwrite", value = FALSE),
          shiny::checkboxInput("export_row_names", "Include row names", value = FALSE),
          shiny::checkboxInput("export_quiet", "Quiet", value = FALSE),
          shiny::checkboxInput("log_export", "Log this action", value = FALSE),
          shiny::textInput("log_file_export", "Audit log file", value = default_log_file),
          shiny::textInput("export_name", "Save as (object name)", value = "export"),
          shiny::actionButton("run_export", "Run ExportToExcel"),
          shiny::actionButton("undo_export", "Undo ExportToExcel")
        )
      ),
      miniUI::miniTabPanel(
        "Summary",
        miniUI::miniContentPanel(
          shiny::textInput("summary_source", "Source object name", value = "flat"),
          shiny::selectInput("summary_field", "Field", choices = character(0)),
          shiny::textInput("summary_field_manual", "Manual field (name or index)", value = ""),
          shiny::numericInput("summary_top_n", "Top N (categorical)", value = 10, min = 1),
          shiny::checkboxInput("summary_all_values", "All values (ignore Top N)", value = FALSE),
          shiny::checkboxInput("summary_include_na", "Include missing", value = FALSE),
          shiny::numericInput("summary_digits", "Digits", value = 2, min = 0),
          shiny::checkboxInput("summary_quiet", "Quiet", value = FALSE),
          shiny::checkboxInput("log_summary", "Log this action", value = FALSE),
          shiny::textInput("log_file_summary", "Audit log file", value = default_log_file),
          shiny::textInput("summary_name", "Save as (object name)", value = "summary"),
          shiny::actionButton("run_summary", "Run SummaryByField"),
          shiny::actionButton("undo_summary", "Undo SummaryByField")
        )
      ),
      miniUI::miniTabPanel(
        "Field Dictionary",
        miniUI::miniContentPanel(
          shiny::textInput("fd_source", "Schema source (object name or JSON path/string)", value = ""),
          shiny::selectInput(
            "fd_include",
            "Include",
            choices = c("input", "all"),
            selected = "input"
          ),
          shiny::textInput("fd_version", "Version (for metadata)", value = "latest"),
          shiny::checkboxInput("fd_expand_surveys", "Expand surveys", value = FALSE),
          shiny::checkboxInput("fd_quiet", "Quiet", value = FALSE),
          shiny::checkboxInput("log_fd", "Log this action", value = FALSE),
          shiny::textInput("log_file_fd", "Audit log file", value = default_log_file),
          shiny::textInput("fd_name", "Save as (object name)", value = "field_dict"),
          shiny::actionButton("run_fd", "Run FieldDictionary"),
          shiny::actionButton("undo_fd", "Undo FieldDictionary"),
          shiny::hr(),
          shiny::selectInput("fd_filter_section", "Section", choices = c("All")),
          shiny::selectInput("fd_filter_type", "Type", choices = c("All")),
          shiny::selectInput("fd_filter_required", "Required", choices = c("All", "TRUE", "FALSE"))
        )
      ),
      miniUI::miniTabPanel(
        "Preview",
        miniUI::miniContentPanel(
          shiny::uiOutput("preview_ui")
        )
      ),
      miniUI::miniTabPanel(
        "Status",
        miniUI::miniContentPanel(
          shiny::verbatimTextOutput("status")
        )
      )
    )
  )

  server <- function(input, output, session) {
    status <- shiny::reactiveVal("Ready.")
    output$status <- shiny::renderText(status())
    preview_text <- shiny::reactiveVal("")
    output$preview_text <- shiny::renderText(preview_text())
    last_obj <- shiny::reactiveVal(NULL)
    last_name <- shiny::reactiveVal(NULL)
    field_dict_full <- shiny::reactiveVal(NULL)

    has_dt <- requireNamespace("DT", quietly = TRUE)
    has_reactable <- requireNamespace("reactable", quietly = TRUE)
    has_rstudioapi <- requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()
    user_env <- globalenv()

    make_preview_df <- function(df, max_rows = 1000) {
      df <- utils::head(df, max_rows)
      n <- nrow(df)
      for (nm in names(df)) {
        col <- df[[nm]]
        if (is.data.frame(col) || is.matrix(col)) {
          if (!is.null(nrow(col)) && nrow(col) == n) {
            df[[nm]] <- apply(col, 1, function(row) {
              jsonlite::toJSON(as.list(row), auto_unbox = TRUE)
            })
          } else {
            df[[nm]] <- rep(jsonlite::toJSON(col, auto_unbox = TRUE), n)
          }
          next
        }
        if (is.list(col)) {
          df[[nm]] <- vapply(seq_len(n), function(i) {
            if (i > length(col)) return("")
            x <- col[[i]]
            if (length(x) == 0) return("")
            if (is.atomic(x) && length(x) == 1) return(as.character(x))
            jsonlite::toJSON(x, auto_unbox = TRUE)
          }, character(1))
        }
      }
      df
    }

    set_preview <- function(obj, name = NULL) {
      last_obj(obj)
      last_name(name)
      preview_text("")
      if (is.null(obj)) {
        preview_text("No preview available yet.")
        return(invisible(NULL))
      }
      if (is.data.frame(obj)) {
        preview_text("")
      } else if (is.list(obj)) {
        df_idx <- which(vapply(obj, is.data.frame, logical(1)))
        if (length(df_idx) == 0) {
          obj_str <- utils::capture.output(utils::str(obj, max.level = 2))
          preview_text(paste(c("str():", obj_str), collapse = "\n"))
        } else {
          preview_text("")
        }
      } else {
        obj_str <- utils::capture.output(utils::str(obj, max.level = 2))
        preview_text(paste(c("str():", obj_str), collapse = "\n"))
      }
      invisible(NULL)
    }

    get_preview_df <- function() {
      obj <- last_obj()
      if (is.null(obj)) return(NULL)
      if (is.data.frame(obj)) return(make_preview_df(obj))
      if (is.list(obj)) {
        df_idx <- which(vapply(obj, is.data.frame, logical(1)))
        if (length(df_idx) == 0) return(NULL)
        choice <- input$preview_list_choice
        if (is.null(choice)) return(make_preview_df(obj[[df_idx[1]]]))
        display_names <- names(obj)
        if (is.null(display_names)) display_names <- rep("", length(obj))
        display_names <- ifelse(nzchar(display_names), display_names, paste0("[[", seq_along(obj), "]]"))
        picked <- match(choice, display_names)
        if (is.na(picked)) picked <- df_idx[1]
        return(make_preview_df(obj[[picked]]))
      }
      NULL
    }

    parse_id_col <- function(x) {
      x <- trimws(x)
      if (!nzchar(x)) return(1L)
      if (grepl("^[0-9]+$", x)) return(as.integer(x))
      x
    }

    resolve_source_obj <- function(name, label) {
      name <- trimws(name)
      if (!nzchar(name)) {
        status(paste0(label, " error: source object name is empty."))
        return(NULL)
      }
      if (!exists(name, envir = user_env)) {
        status(paste0(label, " error: source object not found in Global Environment."))
        return(NULL)
      }
      get(name, envir = user_env)
    }

    resolve_source_df <- function(name, label) {
      obj <- resolve_source_obj(name, label)
      if (is.null(obj)) return(NULL)
      out <- tryCatch(
        extract_flat_df(obj),
        error = function(e) {
          status(paste0(label, " error: ", e$message))
          NULL
        }
      )
      out
    }

    resolve_fd_source <- function(text) {
      text <- trimws(text)
      if (!nzchar(text)) {
        status("FieldDictionary error: source is empty.")
        return(NULL)
      }
      if (exists(text, envir = user_env)) {
        return(get(text, envir = user_env))
      }
      if (file.exists(text)) {
        return(text)
      }
      if (grepl("^\\s*\\{", text)) {
        return(text)
      }
      status("FieldDictionary error: source must be an object name, JSON file path, or JSON string.")
      NULL
    }

    build_export_targets <- function(data, path, sheet) {
      sheets <- coerce_export_sheets(data, sheet = sheet)
      ext <- tolower(tools::file_ext(path))
      if (!nzchar(ext)) {
        path <- paste0(path, ".xlsx")
        ext <- "xlsx"
      }
      if (ext %in% c("csv", "tsv")) {
        if (length(sheets) == 1) {
          paths <- path
        } else {
          base <- tools::file_path_sans_ext(path)
          paths <- paste0(base, "_", names(sheets), ".", ext)
        }
      } else {
        paths <- path
      }
      list(paths = paths, ext = ext, sheets = sheets, path = path)
    }

    update_summary_field_choices <- function() {
      name <- trimws(input$summary_source)
      if (!nzchar(name) || !exists(name, envir = user_env)) {
        shiny::updateSelectInput(session, "summary_field", choices = character(0))
        return()
      }
      df <- tryCatch(
        extract_flat_df(get(name, envir = user_env)),
        error = function(e) NULL
      )
      if (is.null(df)) {
        shiny::updateSelectInput(session, "summary_field", choices = character(0))
        return()
      }
      shiny::updateSelectInput(session, "summary_field", choices = names(df), selected = names(df)[1])
    }

    preview_output_widget <- function() {
      if (has_reactable) {
        reactable::reactableOutput("preview_reactable")
      } else if (has_dt) {
        DT::dataTableOutput("preview_table")
      } else {
        shiny::div(style = "height:100%; overflow:auto;", shiny::tableOutput("preview_table"))
      }
    }

    output$preview_ui <- shiny::renderUI({
      obj <- last_obj()
      if (is.null(obj)) return(NULL)
      if (is.data.frame(obj)) {
        shiny::tagList(
          shiny::p(sprintf("Rows: %s  Columns: %s", nrow(obj), ncol(obj))),
          shiny::div(
            class = "formior-preview-wrap",
            shiny::div(
              class = "formior-preview",
              preview_output_widget()
            )
          )
        )
      } else if (is.list(obj)) {
        df_names <- names(obj)
        df_idx <- which(vapply(obj, is.data.frame, logical(1)))
        if (length(df_idx) == 0) return(shiny::verbatimTextOutput("preview_text"))
        if (is.null(df_names)) df_names <- rep("", length(obj))
        display_names <- ifelse(nzchar(df_names), df_names, paste0("[[", seq_along(obj), "]]"))
        choices <- display_names[df_idx]
        names(choices) <- display_names[df_idx]
        shiny::tagList(
          shiny::selectInput("preview_list_choice", "Data frame in list", choices = choices),
          shiny::div(
            class = "formior-preview-wrap",
            shiny::div(
              class = "formior-preview",
              preview_output_widget()
            )
          )
        )
      } else {
        shiny::verbatimTextOutput("preview_text")
      }
    })

    if (has_reactable) {
      output$preview_reactable <- reactable::renderReactable({
        df <- get_preview_df()
        if (is.null(df)) return(NULL)
        reactable::reactable(
          df,
          resizable = TRUE,
          searchable = TRUE,
          sortable = TRUE,
          highlight = TRUE,
          striped = TRUE,
          compact = TRUE,
          wrap = FALSE,
          pagination = TRUE,
          defaultPageSize = 25,
          height = "100%"
        )
      })
    }

    output$preview_table <- if (has_dt) {
      DT::renderDataTable({
        get_preview_df()
      }, options = list(pageLength = 25, scrollX = TRUE, scrollY = "calc(100vh - 260px)", scroller = TRUE, deferRender = TRUE))
    } else {
      shiny::renderTable({
        df <- get_preview_df()
        if (is.null(df)) return(NULL)
        utils::head(df, 20)
      })
    }

    field_dict_filtered <- shiny::reactive({
      df <- field_dict_full()
      if (is.null(df)) return(NULL)
      out <- df
      if (!is.null(input$fd_filter_section) && input$fd_filter_section != "All") {
        out <- out[out$section == input$fd_filter_section, , drop = FALSE]
      }
      if (!is.null(input$fd_filter_type) && input$fd_filter_type != "All") {
        out <- out[out$type == input$fd_filter_type, , drop = FALSE]
      }
      if (!is.null(input$fd_filter_required) && input$fd_filter_required != "All") {
        req_val <- if (identical(input$fd_filter_required, "TRUE")) TRUE else FALSE
        out <- out[!is.na(out$required) & out$required == req_val, , drop = FALSE]
      }
      out
    })

    shiny::observeEvent(field_dict_full(), {
      df <- field_dict_full()
      if (is.null(df)) return()
      sections <- sort(unique(stats::na.omit(df$section)))
      types <- sort(unique(stats::na.omit(df$type)))
      shiny::updateSelectInput(session, "fd_filter_section", choices = c("All", sections), selected = "All")
      shiny::updateSelectInput(session, "fd_filter_type", choices = c("All", types), selected = "All")
      shiny::updateSelectInput(session, "fd_filter_required", choices = c("All", "TRUE", "FALSE"), selected = "All")
    }, ignoreInit = TRUE)

    shiny::observeEvent(
      {
        list(input$fd_filter_section, input$fd_filter_type, input$fd_filter_required)
      },
      {
        df <- field_dict_filtered()
        if (!is.null(df)) {
          set_preview(df, "field_dict (filtered)")
        }
      },
      ignoreInit = TRUE
    )

    undo_cache <- new.env(parent = emptyenv())
    export_undo_cache <- new.env(parent = emptyenv())
    save_undo <- function(name) {
      if (exists(name, envir = user_env)) {
        undo_cache[[name]] <- get(name, envir = user_env)
      } else {
        undo_cache[[name]] <- NULL
      }
    }

    save_export_undo <- function(name, paths, backups, created) {
      export_undo_cache[[name]] <- list(
        paths = paths,
        backups = backups,
        created = created
      )
    }

    undo_export_files <- function(name) {
      if (!exists(name, envir = export_undo_cache, inherits = FALSE)) {
        return("No export file undo info available.")
      }
      info <- export_undo_cache[[name]]
      restored <- 0L
      deleted <- 0L
      if (length(info$backups) > 0) {
        for (i in seq_along(info$backups)) {
          from <- unname(info$backups[[i]])
          to <- names(info$backups)[i]
          if (file.exists(from)) {
            dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
            file.copy(from, to, overwrite = TRUE)
            restored <- restored + 1L
            file.remove(from)
          }
        }
      }
      if (length(info$created) > 0) {
        existing_created <- info$created[file.exists(info$created)]
        if (length(existing_created) > 0) {
          file.remove(existing_created)
          deleted <- deleted + length(existing_created)
        }
      }
      paste0("Export files restored: ", restored, "; deleted new files: ", deleted, ".")
    }
    do_undo <- function(name, extra_msg = NULL) {
      if (!exists(name, envir = undo_cache, inherits = FALSE)) {
        msg <- "Undo not available: no previous value saved."
        if (!is.null(extra_msg) && nzchar(extra_msg)) msg <- paste(msg, extra_msg)
        status(msg)
        return(invisible(NULL))
      }
      prior <- undo_cache[[name]]
      if (is.null(prior)) {
        if (exists(name, envir = user_env)) rm(list = name, envir = user_env)
      } else {
        assign(name, prior, envir = user_env)
      }
      msg <- paste0("Undo complete. Restored '", name, "' in Global Environment.")
      if (!is.null(extra_msg) && nzchar(extra_msg)) msg <- paste(msg, extra_msg)
      status(msg)
      set_preview(prior)
      invisible(NULL)
    }

    run_with_audit <- function(log_this, log_file, fn) {
      state <- get_audit_state()
      on.exit(set_audit_state(state), add = TRUE)

      if (isTRUE(log_this)) {
        if (!isTRUE(state$active)) {
          log_path <- log_file
          if (is.null(log_path) || !nzchar(log_path)) {
            log_path <- state$file
          }
          if (is.null(log_path) || !nzchar(log_path)) {
            log_path <- default_log_file
          }
          ok <- tryCatch(
            {
              StartAuditLog(
                file = log_path,
                overwrite = FALSE,
                append = file.exists(log_path),
                quiet = TRUE
              )
              TRUE
            },
            error = function(e) {
              status(paste("Audit log error:", e$message))
              FALSE
            }
          )
          if (!isTRUE(ok)) return(NULL)
        }
      } else {
        state_no_log <- state
        state_no_log$active <- FALSE
        state_no_log$prompted <- TRUE
        if (!is.null(log_file) && nzchar(log_file)) state_no_log$file <- log_file
        set_audit_state(state_no_log)
      }

      fn()
    }

    shiny::observeEvent(input$summary_source, {
      update_summary_field_choices()
    }, ignoreInit = FALSE)

    shiny::observeEvent(input$export_browse, {
      if (!isTRUE(has_rstudioapi)) {
        status("Browse requires the rstudioapi package and RStudio.")
        return()
      }
      picked <- rstudioapi::selectFile(caption = "Select export path", existing = FALSE)
      if (!is.null(picked) && nzchar(picked)) {
        shiny::updateTextInput(session, "export_path", value = picked)
      }
    })

    shiny::observeEvent(input$run_get, {
      content_only <- if (identical(input$content_only, "TRUE")) {
        TRUE
      } else if (identical(input$content_only, "FALSE")) {
        FALSE
      } else {
        "raw"
      }

      target_name <- trimws(input$responses_name)
      if (!nzchar(target_name)) target_name <- "responses"

      save_undo(target_name)
      out <- run_with_audit(
        log_this = isTRUE(input$log_get),
        log_file = input$log_file_get,
        fn = function() {
          tryCatch(
            GetResponses(
              base_url = input$base_url,
              form_id = input$form_id,
              api_key = input$api_key,
              drafts = isTRUE(input$drafts),
              deleted = isTRUE(input$deleted),
              content.only = content_only,
              reenter.credentials = isTRUE(input$reenter_creds)
            ),
            error = function(e) {
              status(paste("GetResponses error:", e$message))
              NULL
            }
          )
        }
      )

      if (!is.null(out)) {
        assign(target_name, out, envir = user_env)
        status(paste0("GetResponses complete. Saved to '", target_name, "' in Global Environment."))
        set_preview(out, target_name)
      }
    })

    shiny::observeEvent(input$undo_get, {
      target_name <- trimws(input$responses_name)
      if (!nzchar(target_name)) target_name <- "responses"
      do_undo(target_name)
    })

    shiny::observeEvent(input$run_flatten, {
      source_name <- trimws(input$flatten_source)
      if (!nzchar(source_name) || !exists(source_name, envir = user_env)) {
        status("FlattenSubmissions error: source object not found in Global Environment.")
        return()
      }

      target_name <- trimws(input$flatten_name)
      if (!nzchar(target_name)) target_name <- "flat"

      source_obj <- get(source_name, envir = user_env)

      save_undo(target_name)
      out <- run_with_audit(
        log_this = isTRUE(input$log_flatten),
        log_file = input$log_file_flatten,
        fn = function() {
          tryCatch(
            FlattenSubmissions(source_obj),
            error = function(e) {
              status(paste("FlattenSubmissions error:", e$message))
              NULL
            }
          )
        }
      )

      if (!is.null(out)) {
        assign(target_name, out, envir = user_env)
        status(paste0("FlattenSubmissions complete. Saved to '", target_name, "' in Global Environment."))
        set_preview(out, target_name)
      }
    })

    shiny::observeEvent(input$undo_flatten, {
      target_name <- trimws(input$flatten_name)
      if (!nzchar(target_name)) target_name <- "flat"
      do_undo(target_name)
    })

    shiny::observeEvent(input$run_norm, {
      source_name <- trimws(input$norm_source)
      if (!nzchar(source_name) || !exists(source_name, envir = user_env)) {
        status("NormalizeColumnNames error: source object not found in Global Environment.")
        return()
      }

      target_name <- trimws(input$norm_name)
      if (!nzchar(target_name)) target_name <- "norm"

      source_obj <- get(source_name, envir = user_env)

      save_undo(target_name)
      out <- run_with_audit(
        log_this = isTRUE(input$log_norm),
        log_file = input$log_file_norm,
        fn = function() {
          tryCatch(
            NormalizeColumnNames(
              x = source_obj,
              style = input$norm_style,
              make_unique = isTRUE(input$norm_unique),
              transliterate = isTRUE(input$norm_translit),
              return_flat = isTRUE(input$norm_return_flat),
              quiet = isTRUE(input$norm_quiet)
            ),
            error = function(e) {
              status(paste("NormalizeColumnNames error:", e$message))
              NULL
            }
          )
        }
      )

      if (!is.null(out)) {
        assign(target_name, out, envir = user_env)
        status(paste0("NormalizeColumnNames complete. Saved to '", target_name, "' in Global Environment."))
        set_preview(out, target_name)
      }
    })

    shiny::observeEvent(input$undo_norm, {
      target_name <- trimws(input$norm_name)
      if (!nzchar(target_name)) target_name <- "norm"
      do_undo(target_name)
    })

    shiny::observeEvent(input$run_resolve, {
      source_name <- trimws(input$resolve_source)
      if (!nzchar(source_name) || !exists(source_name, envir = user_env)) {
        status("ResolveRepeats error: source object not found in Global Environment.")
        return()
      }

      target_name <- trimws(input$resolve_name)
      if (!nzchar(target_name)) target_name <- "resolved"

      source_obj <- get(source_name, envir = user_env)
      id_col <- parse_id_col(input$resolve_id_col)

      save_undo(target_name)
      out <- run_with_audit(
        log_this = isTRUE(input$log_resolve),
        log_file = input$log_file_resolve,
        fn = function() {
          tryCatch(
            ResolveRepeats(
              x = source_obj,
              id_col = id_col,
              strategy = input$resolve_strategy,
              sep = input$resolve_sep,
              unique = isTRUE(input$resolve_unique),
              return_flat = isTRUE(input$resolve_return_flat),
              quiet = isTRUE(input$resolve_quiet)
            ),
            error = function(e) {
              status(paste("ResolveRepeats error:", e$message))
              NULL
            }
          )
        }
      )

      if (!is.null(out)) {
        assign(target_name, out, envir = user_env)
        status(paste0("ResolveRepeats complete. Saved to '", target_name, "' in Global Environment."))
        set_preview(out, target_name)
      }
    })

    shiny::observeEvent(input$undo_resolve, {
      target_name <- trimws(input$resolve_name)
      if (!nzchar(target_name)) target_name <- "resolved"
      do_undo(target_name)
    })

    shiny::observeEvent(input$run_codebook, {
      source_name <- trimws(input$codebook_source)
      if (!nzchar(source_name) || !exists(source_name, envir = user_env)) {
        status("MakeCodebook error: source object not found in Global Environment.")
        return()
      }

      target_name <- trimws(input$codebook_name)
      if (!nzchar(target_name)) target_name <- "codebook"

      source_obj <- get(source_name, envir = user_env)

      form_val <- NULL
      form_name <- trimws(input$codebook_form)
      if (nzchar(form_name)) {
        if (exists(form_name, envir = user_env)) {
          form_val <- get(form_name, envir = user_env)
        } else {
          form_val <- form_name
        }
      }

      save_undo(target_name)
      out <- run_with_audit(
        log_this = isTRUE(input$log_codebook),
        log_file = input$log_file_codebook,
        fn = function() {
          tryCatch(
            MakeCodebook(
              data = source_obj,
              form = form_val,
              include = input$codebook_include,
              include_summary = isTRUE(input$codebook_summary),
              max_levels = as.integer(input$codebook_max_levels),
              quiet = isTRUE(input$codebook_quiet)
            ),
            error = function(e) {
              status(paste("MakeCodebook error:", e$message))
              NULL
            }
          )
        }
      )

      if (!is.null(out)) {
        assign(target_name, out, envir = user_env)
        status(paste0("MakeCodebook complete. Saved to '", target_name, "' in Global Environment."))
        set_preview(out, target_name)
      }
    })

    shiny::observeEvent(input$undo_codebook, {
      target_name <- trimws(input$codebook_name)
      if (!nzchar(target_name)) target_name <- "codebook"
      do_undo(target_name)
    })

    shiny::observeEvent(input$run_export, {
      source_obj <- resolve_source_obj(input$export_source, "ExportToExcel")
      if (is.null(source_obj)) return()

      target_name <- trimws(input$export_name)
      if (!nzchar(target_name)) target_name <- "export"

      path <- trimws(input$export_path)
      if (!nzchar(path)) {
        status("ExportToExcel error: output path is empty.")
        return()
      }

      sheet <- trimws(input$export_sheet)
      if (!nzchar(sheet)) sheet <- "Data"

      export_info <- tryCatch(
        build_export_targets(source_obj, path, sheet),
        error = function(e) {
          status(paste("ExportToExcel error:", e$message))
          NULL
        }
      )
      if (is.null(export_info)) return()

      path <- export_info$path
      paths <- export_info$paths
      existing <- file.exists(paths)
      backups <- list()
      created <- paths[!existing]

      if (isTRUE(input$export_overwrite) && any(existing)) {
        for (i in seq_along(paths)) {
          if (!existing[i]) next
          backup_path <- tempfile(pattern = "formior_export_backup_", fileext = paste0(".", tools::file_ext(paths[i])))
          file.copy(paths[i], backup_path, overwrite = TRUE)
          backups[[paths[i]]] <- backup_path
        }
      }

      save_undo(target_name)
      out <- run_with_audit(
        log_this = isTRUE(input$log_export),
        log_file = input$log_file_export,
        fn = function() {
          tryCatch(
            ExportToExcel(
              data = source_obj,
              path = path,
              sheet = sheet,
              overwrite = isTRUE(input$export_overwrite),
              include_row_names = isTRUE(input$export_row_names),
              quiet = isTRUE(input$export_quiet)
            ),
            error = function(e) {
              status(paste("ExportToExcel error:", e$message))
              NULL
            }
          )
        }
      )

      if (is.null(out)) {
        if (length(backups) > 0) {
          for (i in seq_along(backups)) {
            from <- unname(backups[[i]])
            to <- names(backups)[i]
            if (file.exists(from)) {
              dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
              file.copy(from, to, overwrite = TRUE)
              file.remove(from)
            }
          }
        }
        if (length(created) > 0) {
          existing_created <- created[file.exists(created)]
          if (length(existing_created) > 0) file.remove(existing_created)
        }
        return()
      }

      save_export_undo(target_name, paths = paths, backups = backups, created = created)
      assign(target_name, out, envir = user_env)
      status(paste0("ExportToExcel complete. Saved to '", target_name, "' in Global Environment."))
      set_preview(out, target_name)
    })

    shiny::observeEvent(input$undo_export, {
      target_name <- trimws(input$export_name)
      if (!nzchar(target_name)) target_name <- "export"
      msg <- undo_export_files(target_name)
      do_undo(target_name, extra_msg = msg)
    })

    shiny::observeEvent(input$run_summary, {
      source_name <- trimws(input$summary_source)
      df <- resolve_source_df(source_name, "SummaryByField")
      if (is.null(df)) return()
      source_obj <- get(source_name, envir = user_env)

      target_name <- trimws(input$summary_name)
      if (!nzchar(target_name)) target_name <- "summary"

      manual_field <- trimws(input$summary_field_manual)
      if (nzchar(manual_field)) {
        field <- parse_id_col(manual_field)
      } else {
        field <- input$summary_field
      }

      if (is.null(field) || !nzchar(as.character(field))) {
        status("SummaryByField error: field is required.")
        return()
      }

      top_n <- if (isTRUE(input$summary_all_values)) NULL else as.integer(input$summary_top_n)

      save_undo(target_name)
      out <- run_with_audit(
        log_this = isTRUE(input$log_summary),
        log_file = input$log_file_summary,
        fn = function() {
          tryCatch(
            SummaryByField(
              x = source_obj,
              field = field,
              top_n = top_n,
              include_na = isTRUE(input$summary_include_na),
              digits = as.integer(input$summary_digits),
              quiet = isTRUE(input$summary_quiet)
            ),
            error = function(e) {
              status(paste("SummaryByField error:", e$message))
              NULL
            }
          )
        }
      )

      if (!is.null(out)) {
        assign(target_name, out, envir = user_env)
        status(paste0("SummaryByField complete. Saved to '", target_name, "' in Global Environment."))
        if (is.list(out) && !is.null(out$summary) && is.data.frame(out$summary)) {
          set_preview(out$summary, target_name)
        } else {
          set_preview(out, target_name)
        }
      }
    })

    shiny::observeEvent(input$undo_summary, {
      target_name <- trimws(input$summary_name)
      if (!nzchar(target_name)) target_name <- "summary"
      do_undo(target_name)
    })

    shiny::observeEvent(input$run_fd, {
      form_val <- resolve_fd_source(input$fd_source)
      if (is.null(form_val)) return()

      target_name <- trimws(input$fd_name)
      if (!nzchar(target_name)) target_name <- "field_dict"

      save_undo(target_name)
      out <- run_with_audit(
        log_this = isTRUE(input$log_fd),
        log_file = input$log_file_fd,
        fn = function() {
          tryCatch(
            FieldDictionary(
              form = form_val,
              include = input$fd_include,
              version = input$fd_version,
              expand_surveys = isTRUE(input$fd_expand_surveys),
              quiet = isTRUE(input$fd_quiet)
            ),
            error = function(e) {
              status(paste("FieldDictionary error:", e$message))
              NULL
            }
          )
        }
      )

      if (!is.null(out)) {
        assign(target_name, out, envir = user_env)
        field_dict_full(out)
        status(paste0("FieldDictionary complete. Saved to '", target_name, "' in Global Environment."))
        set_preview(out, target_name)
      }
    })

    shiny::observeEvent(input$undo_fd, {
      target_name <- trimws(input$fd_name)
      if (!nzchar(target_name)) target_name <- "field_dict"
      field_dict_full(NULL)
      do_undo(target_name)
    })

    shiny::observeEvent(input$done, {
      shiny::stopApp(invisible(NULL))
    })

  }

  get_addin_viewer <- function() {
    pref <- getOption("formior.addin.viewer", "pane")
    pref <- tolower(pref)
    if (identical(pref, "browser")) return(shiny::browserViewer())
    if (identical(pref, "dialog")) return(shiny::dialogViewer("FormIOr Addin", width = 1000, height = 700))
    shiny::paneViewer()
  }

  shiny::runGadget(
    ui,
    server,
    viewer = get_addin_viewer()
  )
}
