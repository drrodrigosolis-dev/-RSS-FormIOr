#' Flag suspicious submissions and enforce pre/post comparability checks
#'
#' Evaluates whether pre/post time groups are statistically comparable and
#' produces row-level suspiciousness flags. This is designed for workflows where
#' a survey changed mid-collection (for example, from anonymous to identity
#' verified) and pooled analysis may no longer be valid.
#'
#' For each analyzable field, the function compares pre (`time < cutoff_time`)
#' and post (`time >= cutoff_time`) groups using:
#' - Numeric fields: Wilcoxon rank-sum + standardized mean difference (SMD)
#' - Categorical fields: Chi-square (or Fisher fallback) + Cramer's V
#'
#' A field is marked different when both adjusted p-value and effect-size
#' thresholds are met. If enough fields differ, pooled analysis is flagged as
#' non-comparable.
#'
#' @param x A data frame of responses, or a list from [flatten_submission_records()].
#' @param id_col Integer or character. Submission ID column (default 1).
#' @param time_col Optional timestamp column name. If `NULL`, common timestamp
#'   names are guessed.
#' @param cutoff_time Required cutoff separating pre and post groups.
#' @param include_cols Optional columns to include in comparability tests. If
#'   `NULL`, all analyzable non-ID, non-time columns are considered.
#' @param group_action One of `"split_only"` (default), `"omit_pre"`,
#'   `"omit_post"`, or `"none"`.
#' @param comparability_rule Currently only `"fdr_effect"`.
#' @param alpha Significance threshold for adjusted p-values.
#' @param fdr_method P-adjust method passed to [stats::p.adjust()].
#' @param min_effect_numeric Minimum absolute numeric effect size (SMD).
#' @param min_effect_categorical Minimum categorical effect size (Cramer's V).
#' @param min_flagged_field_share Minimum share of tested fields marked
#'   different to classify groups as non-comparable.
#' @param min_complete_rate Minimum non-missing rate required for a field to be
#'   tested.
#' @param risk_weights Named list or vector of row-level risk weights.
#' @param risk_threshold Threshold for high-risk labeling.
#' @param action One of `"flag_only"`, `"exclude_high_risk"`,
#'   `"exclude_confirmed_duplicates"`.
#' @param return_flat Logical. If `TRUE` and `x` came from [flatten_submission_records()],
#'   include updated list as `flat`.
#' @param quiet Logical. If `FALSE`, prints a short summary.
#'
#' @return A list with:
#' \describe{
#'   \item{data}{Primary analysis dataset after group action + row action}
#'   \item{datasets}{List with `pre`, `post`, and `full` datasets}
#'   \item{comparability}{List with global comparability metrics and `field_tests`}
#'   \item{decision_sentence}{One-sentence human-readable decision summary}
#'   \item{flags}{Row-level suspiciousness flags and risk scores}
#'   \item{summary}{High-level counts and selected actions}
#'   \item{diagnostics}{Supplementary diagnostic tables}
#'   \item{flat}{If `return_flat = TRUE` and input is a flat list, updated list}
#' }
#'
#' @section When pre/post groups must not be pooled:
#' If `comparability$non_comparable` is `TRUE`, pooled pre+post inference should
#' be treated as invalid by default. Prefer either:
#' - `group_action = "omit_pre"` (usually preferred when post is identity-verified),
#' - `group_action = "omit_post"`, or
#' - `group_action = "split_only"` and analyze groups separately.
#'
#' @examples
#' \dontrun{
#' data("SuspiciousSurveyDemo", package = "FormIOr")
#'
#' out <- detect_suspicious_submissions(
#'   SuspiciousSurveyDemo,
#'   id_col = "submissionId",
#'   time_col = "created",
#'   cutoff_time = "2026-01-06 00:00:00",
#'   group_action = "split_only"
#' )
#'
#' out$comparability$non_comparable
#' head(out$comparability$field_tests)
#'
#' # If groups differ and post phase is identity-verified, prefer post-only:
#' post_only <- detect_suspicious_submissions(
#'   SuspiciousSurveyDemo,
#'   id_col = "submissionId",
#'   time_col = "created",
#'   cutoff_time = "2026-01-06 00:00:00",
#'   group_action = "omit_pre"
#' )
#' }
#'
#' @export
detect_suspicious_submissions <- function(
    x,
    id_col = 1,
    time_col = NULL,
    cutoff_time,
    include_cols = NULL,
    group_action = c("split_only", "omit_pre", "omit_post", "none"),
    comparability_rule = c("fdr_effect"),
    alpha = 0.05,
    fdr_method = "BH",
    min_effect_numeric = 0.2,
    min_effect_categorical = 0.1,
    min_flagged_field_share = 0.2,
    min_complete_rate = 0.6,
    risk_weights = NULL,
    risk_threshold = 0.7,
    action = c("flag_only", "exclude_high_risk", "exclude_confirmed_duplicates"),
    return_flat = FALSE,
    quiet = FALSE
) {
  audit_depth <- audit_enter()
  on.exit(audit_exit(), add = TRUE)
  if (audit_depth == 1) maybe_prompt_audit_log()

  group_action <- match.arg(group_action)
  comparability_rule <- match.arg(comparability_rule)
  action <- match.arg(action)

  if (missing(cutoff_time) || is.null(cutoff_time) || !nzchar(as.character(cutoff_time)[1])) {
    stop("cutoff_time is required for pre/post comparability analysis.")
  }

  df <- extract_flat_df(x)
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  id_col_name <- resolve_id_col(df, id_col)

  if (is.null(time_col)) {
    time_col <- guess_time_col(names(df))
  }
  if (is.null(time_col) || !time_col %in% names(df)) {
    stop("A valid time_col is required (or one that can be auto-detected).")
  }

  times <- coerce_time_to_utc(df[[time_col]])
  if (all(is.na(times))) {
    stop("time_col could not be parsed to timestamps.")
  }
  cutoff_ts <- coerce_cutoff_to_utc(cutoff_time)
  if (is.na(cutoff_ts)) {
    stop("cutoff_time could not be parsed to a valid timestamp.")
  }

  pre_idx <- which(!is.na(times) & times < cutoff_ts)
  post_idx <- which(!is.na(times) & times >= cutoff_ts)
  if (length(pre_idx) == 0 || length(post_idx) == 0) {
    stop("Both pre and post groups must have at least one row after applying cutoff_time.")
  }

  field_pool <- resolve_include_columns(df, include_cols, id_col_name, time_col)

  field_tests <- run_pre_post_field_tests(
    df = df,
    pre_idx = pre_idx,
    post_idx = post_idx,
    field_pool = field_pool,
    alpha = alpha,
    fdr_method = fdr_method,
    min_effect_numeric = min_effect_numeric,
    min_effect_categorical = min_effect_categorical,
    min_complete_rate = min_complete_rate
  )

  n_tested <- sum(!is.na(field_tests$p_value))
  n_different <- sum(field_tests$different %in% TRUE, na.rm = TRUE)
  flagged_field_share <- if (n_tested > 0) n_different / n_tested else 0
  non_comparable <- n_tested >= 3 && flagged_field_share >= min_flagged_field_share

  datasets <- list(
    pre = df[pre_idx, , drop = FALSE],
    post = df[post_idx, , drop = FALSE],
    full = df
  )

  base_data <- select_group_data(
    datasets = datasets,
    non_comparable = non_comparable,
    group_action = group_action,
    quiet = quiet
  )

  flags <- build_suspicious_flags(
    df = base_data,
    id_col_name = id_col_name,
    time_col = time_col,
    include_cols = field_pool,
    risk_weights = risk_weights,
    risk_threshold = risk_threshold
  )

  out_data <- apply_suspicious_action(
    df = base_data,
    flags = flags,
    action = action,
    risk_threshold = risk_threshold
  )

  removed_rows <- nrow(base_data) - nrow(out_data)
  decision_parts <- build_decision_parts(
    non_comparable = non_comparable,
    n_different = n_different,
    n_tested = n_tested,
    flagged_field_share = flagged_field_share,
    min_flagged_field_share = min_flagged_field_share,
    group_action = group_action,
    pre_rows = nrow(datasets$pre),
    post_rows = nrow(datasets$post),
    analysis_rows = nrow(base_data),
    action = action,
    removed_rows = removed_rows,
    output_rows = nrow(out_data)
  )
  decision_sentence <- decision_parts$plain
  decision_sentence_colored <- decision_parts$colored

  if (audit_depth == 1) {
    details <- paste0(
      "group_action=", group_action,
      "; action=", action,
      "; non_comparable=", non_comparable,
      "; n_tested=", n_tested,
      "; n_different=", n_different,
      "; decision=", decision_sentence
    )
    maybe_write_audit("detect_suspicious_submissions", details = details, data = out_data)
  }

  out <- list(
    data = out_data,
    datasets = datasets,
    comparability = list(
      rule = comparability_rule,
      non_comparable = non_comparable,
      n_tested = n_tested,
      n_different = n_different,
      flagged_field_share = flagged_field_share,
      field_tests = field_tests
    ),
    decision_sentence = decision_sentence,
    flags = flags,
    summary = list(
      original_rows = nrow(df),
      pre_rows = nrow(datasets$pre),
      post_rows = nrow(datasets$post),
      analysis_rows = nrow(base_data),
      output_rows = nrow(out_data),
      removed_rows = removed_rows,
      group_action = group_action,
      action = action,
      non_comparable = non_comparable,
      decision_sentence = decision_sentence,
      decision_sentence_colored = decision_sentence_colored
    ),
    diagnostics = list(
      duplicate_ids = flags[flags$duplicate_id, c("row_index", id_col_name, time_col), drop = FALSE],
      duplicate_fingerprints = flags[flags$duplicate_fingerprint, c("row_index", id_col_name, time_col, "fingerprint"), drop = FALSE],
      rapid_repeats = flags[flags$rapid_repeat, c("row_index", id_col_name, time_col, "fingerprint", "time_delta_secs"), drop = FALSE]
    )
  )

  if (is_flat_list(x) && isTRUE(return_flat)) {
    x$FlatResponses <- out_data
    x$ColumnNames <- update_column_names(x$ColumnNames, names(out_data))
    out$flat <- x
  }

  class(out) <- c("FlagSuspiciousSubmissionsResult", "list")

  if (!quiet && !interactive()) {
    # In non-interactive runs there is no auto-print, so emit the decision.
    message(decision_sentence)
  }

  out
}

coerce_time_to_utc <- function(values) {
  if (inherits(values, "POSIXt")) return(as.POSIXct(values, tz = "UTC"))
  if (inherits(values, "Date")) return(as.POSIXct(values, tz = "UTC"))
  suppressWarnings(as.POSIXct(values, tz = "UTC"))
}

coerce_cutoff_to_utc <- function(value) {
  if (inherits(value, "POSIXt")) return(as.POSIXct(value, tz = "UTC"))
  if (inherits(value, "Date")) return(as.POSIXct(value, tz = "UTC"))
  suppressWarnings(as.POSIXct(as.character(value)[1], tz = "UTC"))
}

resolve_include_columns <- function(df, include_cols, id_col_name, time_col) {
  if (is.null(include_cols)) {
    out <- setdiff(names(df), c(id_col_name, time_col))
    return(out)
  }

  if (is.numeric(include_cols)) {
    include_cols <- names(df)[include_cols]
  }
  include_cols <- unique(as.character(include_cols))
  include_cols <- include_cols[include_cols %in% names(df)]
  setdiff(include_cols, c(id_col_name, time_col))
}

run_pre_post_field_tests <- function(
    df,
    pre_idx,
    post_idx,
    field_pool,
    alpha,
    fdr_method,
    min_effect_numeric,
    min_effect_categorical,
    min_complete_rate
) {
  if (length(field_pool) == 0) {
    return(data.frame(
      field = character(),
      type = character(),
      test = character(),
      p_value = numeric(),
      p_adj = numeric(),
      effect_size = numeric(),
      complete_rate = numeric(),
      different = logical(),
      stringsAsFactors = FALSE
    ))
  }

  rows <- lapply(field_pool, function(field) {
    values <- coerce_repeat_values(df[[field]])
    complete_rate <- mean(!is.na(values))

    if (is.na(complete_rate) || complete_rate < min_complete_rate) {
      return(data.frame(
        field = field,
        type = "excluded",
        test = "excluded_low_completeness",
        p_value = NA_real_,
        p_adj = NA_real_,
        effect_size = NA_real_,
        complete_rate = complete_rate,
        different = FALSE,
        stringsAsFactors = FALSE
      ))
    }

    pre_vals <- values[pre_idx]
    post_vals <- values[post_idx]

    if (is_numeric_like(values)) {
      out <- compare_numeric_field(pre_vals, post_vals)
      return(data.frame(
        field = field,
        type = "numeric",
        test = out$test,
        p_value = out$p_value,
        p_adj = NA_real_,
        effect_size = out$effect_size,
        complete_rate = complete_rate,
        different = FALSE,
        stringsAsFactors = FALSE
      ))
    }

    out <- compare_categorical_field(pre_vals, post_vals)
    data.frame(
      field = field,
      type = "categorical",
      test = out$test,
      p_value = out$p_value,
      p_adj = NA_real_,
      effect_size = out$effect_size,
      complete_rate = complete_rate,
      different = FALSE,
      stringsAsFactors = FALSE
    )
  })

  tests <- do.call(rbind, rows)
  valid <- !is.na(tests$p_value)
  tests$p_adj[valid] <- stats::p.adjust(tests$p_value[valid], method = fdr_method)

  numeric_mask <- tests$type == "numeric"
  cat_mask <- tests$type == "categorical"

  tests$different[numeric_mask] <- !is.na(tests$p_adj[numeric_mask]) &
    tests$p_adj[numeric_mask] <= alpha &
    !is.na(tests$effect_size[numeric_mask]) &
    tests$effect_size[numeric_mask] >= min_effect_numeric

  tests$different[cat_mask] <- !is.na(tests$p_adj[cat_mask]) &
    tests$p_adj[cat_mask] <= alpha &
    !is.na(tests$effect_size[cat_mask]) &
    tests$effect_size[cat_mask] >= min_effect_categorical

  tests
}

compare_numeric_field <- function(pre_vals, post_vals) {
  pre_num <- suppressWarnings(as.numeric(pre_vals))
  post_num <- suppressWarnings(as.numeric(post_vals))
  pre_num <- pre_num[!is.na(pre_num)]
  post_num <- post_num[!is.na(post_num)]

  if (length(pre_num) < 2 || length(post_num) < 2) {
    return(list(test = "wilcox", p_value = NA_real_, effect_size = NA_real_))
  }

  p_value <- tryCatch(
    stats::wilcox.test(pre_num, post_num, exact = FALSE)$p.value,
    error = function(e) NA_real_
  )

  effect <- abs(compute_smd(pre_num, post_num))
  list(test = "wilcox", p_value = p_value, effect_size = effect)
}

compare_categorical_field <- function(pre_vals, post_vals) {
  pre_chr <- as.character(pre_vals)
  post_chr <- as.character(post_vals)
  grp <- c(rep("pre", length(pre_chr)), rep("post", length(post_chr)))
  vals <- c(pre_chr, post_chr)
  keep <- !is.na(vals) & nzchar(vals)

  vals <- vals[keep]
  grp <- grp[keep]

  if (length(vals) == 0) {
    return(list(test = "chisq", p_value = NA_real_, effect_size = NA_real_))
  }

  tbl <- table(vals, grp)
  if (nrow(tbl) < 2 || ncol(tbl) < 2) {
    return(list(test = "chisq", p_value = NA_real_, effect_size = NA_real_))
  }

  chisq_fit <- suppressWarnings(stats::chisq.test(tbl, correct = FALSE))
  expected_small <- any(chisq_fit$expected < 5)

  if (expected_small && all(dim(tbl) == c(2, 2))) {
    p_value <- tryCatch(stats::fisher.test(tbl)$p.value, error = function(e) NA_real_)
    return(list(test = "fisher", p_value = p_value, effect_size = cramer_v(tbl)))
  }

  p_value <- tryCatch(chisq_fit$p.value, error = function(e) NA_real_)
  list(test = "chisq", p_value = p_value, effect_size = cramer_v(tbl))
}

compute_smd <- function(pre_num, post_num) {
  m1 <- mean(pre_num)
  m2 <- mean(post_num)
  s1 <- stats::sd(pre_num)
  s2 <- stats::sd(post_num)
  n1 <- length(pre_num)
  n2 <- length(post_num)

  pooled <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / max(1, n1 + n2 - 2))
  if (is.na(pooled) || pooled == 0) return(0)
  (m1 - m2) / pooled
}

cramer_v <- function(tbl) {
  n <- sum(tbl)
  if (n == 0) return(NA_real_)
  chi <- suppressWarnings(stats::chisq.test(tbl, correct = FALSE)$statistic)
  k <- min(nrow(tbl) - 1, ncol(tbl) - 1)
  if (k <= 0) return(NA_real_)
  as.numeric(sqrt(chi / (n * k)))
}

select_group_data <- function(datasets, non_comparable, group_action, quiet = FALSE) {
  if (!non_comparable) return(datasets$full)

  if (group_action == "omit_pre") {
    return(datasets$post)
  }
  if (group_action == "omit_post") {
    return(datasets$pre)
  }
  if (group_action == "split_only") {
    return(datasets$full)
  }

  if (!quiet) {
    warning(
      "Pre/post groups are non-comparable; pooled inference is invalid. ",
      "Use group_action = 'omit_pre' (usually preferred after identity verification), ",
      "'omit_post', or 'split_only'.",
      call. = FALSE
    )
  }
  datasets$full
}

build_suspicious_flags <- function(df, id_col_name, time_col, include_cols, risk_weights, risk_threshold) {
  if (is.null(risk_weights)) {
    risk_weights <- c(duplicate_id = 0.45, duplicate_fingerprint = 0.35, rapid_repeat = 0.20)
  }
  risk_weights <- unlist(risk_weights)

  ids <- as.character(df[[id_col_name]])
  times <- coerce_time_to_utc(df[[time_col]])
  fingerprint <- build_row_fingerprint(df, include_cols)

  duplicate_id <- duplicated(ids) | duplicated(ids, fromLast = TRUE)
  duplicate_fingerprint <- duplicated(fingerprint) | duplicated(fingerprint, fromLast = TRUE)

  time_delta <- rep(NA_real_, nrow(df))
  rapid_repeat <- rep(FALSE, nrow(df))

  ord <- order(fingerprint, times, na.last = TRUE)
  for (i in seq_along(ord)) {
    if (i == 1) next
    curr <- ord[i]
    prev <- ord[i - 1]
    if (is.na(fingerprint[curr]) || fingerprint[curr] != fingerprint[prev]) next
    if (is.na(times[curr]) || is.na(times[prev])) next

    delta <- as.numeric(difftime(times[curr], times[prev], units = "secs"))
    time_delta[curr] <- delta
    time_delta[prev] <- min(time_delta[prev], delta, na.rm = TRUE)
    if (!is.na(delta) && delta >= 0 && delta <= 600) {
      rapid_repeat[curr] <- TRUE
      rapid_repeat[prev] <- TRUE
    }
  }

  score <- numeric(nrow(df))
  w_dup_id <- if ("duplicate_id" %in% names(risk_weights)) as.numeric(risk_weights[["duplicate_id"]]) else 0
  w_dup_fp <- if ("duplicate_fingerprint" %in% names(risk_weights)) as.numeric(risk_weights[["duplicate_fingerprint"]]) else 0
  w_rapid <- if ("rapid_repeat" %in% names(risk_weights)) as.numeric(risk_weights[["rapid_repeat"]]) else 0

  score <- score + ifelse(duplicate_id, w_dup_id, 0)
  score <- score + ifelse(duplicate_fingerprint, w_dup_fp, 0)
  score <- score + ifelse(rapid_repeat, w_rapid, 0)
  score <- pmin(1, score)

  level <- ifelse(score >= risk_threshold, "high", ifelse(score >= risk_threshold / 2, "medium", "low"))

  reasons <- vapply(seq_len(nrow(df)), function(i) {
    tags <- character(0)
    if (duplicate_id[i]) tags <- c(tags, "duplicate_id")
    if (duplicate_fingerprint[i]) tags <- c(tags, "duplicate_fingerprint")
    if (rapid_repeat[i]) tags <- c(tags, "rapid_repeat")
    if (length(tags) == 0) return("")
    paste(tags, collapse = ";")
  }, character(1))

  out <- data.frame(
    row_index = seq_len(nrow(df)),
    stringsAsFactors = FALSE
  )
  out[[id_col_name]] <- ids
  out[[time_col]] <- as.character(times)
  out$fingerprint <- fingerprint
  out$duplicate_id <- duplicate_id
  out$duplicate_fingerprint <- duplicate_fingerprint
  out$rapid_repeat <- rapid_repeat
  out$time_delta_secs <- time_delta
  out$risk_score <- score
  out$risk_level <- level
  out$reasons <- reasons
  out
}

build_row_fingerprint <- function(df, include_cols) {
  if (length(include_cols) == 0) {
    return(rep(NA_character_, nrow(df)))
  }

  parts <- lapply(include_cols, function(col) {
    vals <- coerce_repeat_values(df[[col]])
    vals <- as.character(vals)
    vals[is.na(vals)] <- "<NA>"
    vals
  })

  do.call(paste, c(parts, sep = "||"))
}

apply_suspicious_action <- function(df, flags, action, risk_threshold) {
  if (action == "flag_only") return(df)

  if (action == "exclude_high_risk") {
    keep <- flags$risk_score < risk_threshold
    return(df[keep, , drop = FALSE])
  }

  keep <- !(flags$duplicate_id | flags$duplicate_fingerprint)
  df[keep, , drop = FALSE]
}

build_decision_parts <- function(
    non_comparable,
    n_different,
    n_tested,
    flagged_field_share,
    min_flagged_field_share,
    group_action,
    pre_rows,
    post_rows,
    analysis_rows,
    action,
    removed_rows,
    output_rows
) {
  comp_txt <- if (non_comparable) "NON-COMPARABLE" else "COMPARABLE"
  field_txt <- paste0(
    n_different, "/", n_tested,
    " fields flagged (", sprintf("%.1f", 100 * flagged_field_share),
    "%; threshold ", sprintf("%.1f", 100 * min_flagged_field_share), "%)"
  )

  phase_plain <- if (!non_comparable) {
    paste0("Phase decision: use pooled full data (", analysis_rows, " rows)")
  } else if (group_action == "omit_pre") {
    paste0("Phase decision: omit pre and use post data (", post_rows, " rows)")
  } else if (group_action == "omit_post") {
    paste0("Phase decision: omit post and use pre data (", pre_rows, " rows)")
  } else if (group_action == "split_only") {
    paste0("Phase decision: keep phases split for analyst review (pre=", pre_rows, ", post=", post_rows, ")")
  } else {
    paste0("Phase decision: keep pooled data despite non-comparability (", analysis_rows, " rows)")
  }

  row_plain <- if (action == "flag_only") {
    paste0("Row action: flag_only retained ", output_rows, " rows")
  } else {
    paste0("Row action: ", action, " removed ", removed_rows, " rows; ", output_rows, " rows remain")
  }

  plain <- paste0(
    "Survey comparability is ", comp_txt, ". ",
    "Evidence: ", field_txt, ". ",
    phase_plain, ". ",
    row_plain, "."
  )

  status_col <- if (non_comparable) crayon::red$bold(comp_txt) else crayon::green$bold(comp_txt)
  evidence_col <- if (non_comparable) crayon::yellow(field_txt) else crayon::green(field_txt)

  phase_col <- if (!non_comparable) {
    crayon::green(phase_plain)
  } else if (group_action %in% c("omit_pre", "omit_post")) {
    crayon::yellow(phase_plain)
  } else if (group_action == "split_only") {
    crayon::cyan(phase_plain)
  } else {
    crayon::red(phase_plain)
  }

  row_col <- if (action == "flag_only") {
    crayon::cyan(row_plain)
  } else if (removed_rows > 0) {
    crayon::yellow(row_plain)
  } else {
    crayon::green(row_plain)
  }

  colored <- paste0(
    crayon::bold("Survey comparability: "), status_col, ". ",
    crayon::bold("Evidence: "), evidence_col, ". ",
    phase_col, ". ",
    row_col, "."
  )

  list(plain = plain, colored = colored)
}

#' Print method for detect_suspicious_submissions results
#'
#' Prints the full result list and then prints `decision_sentence` as the final
#' line so users can quickly see the overall survey-level decision.
#'
#' @param x A `FlagSuspiciousSubmissionsResult` object.
#' @param ... Passed to [base::print()].
#' @return Invisibly returns `x`.
#' @export
print.FlagSuspiciousSubmissionsResult <- function(x, ...) {
  decision <- x$decision_sentence
  decision_colored <- x$summary$decision_sentence_colored
  printable <- x
  printable$decision_sentence <- NULL

  print(unclass(printable), ...)

  if (!is.null(decision_colored) && length(decision_colored) == 1 && nzchar(decision_colored)) {
    cat("\nDecision: ", decision_colored, "\n", sep = "")
  } else if (!is.null(decision) && length(decision) == 1 && nzchar(decision)) {
    cat("\nDecision: ", decision, "\n", sep = "")
  }

  invisible(x)
}
