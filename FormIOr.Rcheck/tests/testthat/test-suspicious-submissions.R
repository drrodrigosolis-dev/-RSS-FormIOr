make_shifted_dataset <- function(n_pre = 80, n_post = 80) {
  set.seed(42)
  pre_time <- as.POSIXct("2026-01-01 08:00:00", tz = "UTC") + seq_len(n_pre) * 60
  post_time <- as.POSIXct("2026-01-08 08:00:00", tz = "UTC") + seq_len(n_post) * 60

  pre <- data.frame(
    submissionId = paste0("pre_", seq_len(n_pre)),
    created = pre_time,
    score = rnorm(n_pre, mean = 0, sd = 1),
    region = sample(c("North", "South"), n_pre, replace = TRUE, prob = c(0.9, 0.1)),
    same_field = rnorm(n_pre, mean = 0, sd = 1),
    stringsAsFactors = FALSE
  )

  post <- data.frame(
    submissionId = paste0("post_", seq_len(n_post)),
    created = post_time,
    score = rnorm(n_post, mean = 1.6, sd = 1),
    region = sample(c("North", "South"), n_post, replace = TRUE, prob = c(0.1, 0.9)),
    same_field = rnorm(n_post, mean = 0, sd = 1),
    stringsAsFactors = FALSE
  )

  rbind(pre, post)
}


test_that("FlagSuspiciousSubmissions detects pre/post drift and non-comparability", {
  df <- make_shifted_dataset()

  out <- FlagSuspiciousSubmissions(
    df,
    id_col = "submissionId",
    time_col = "created",
    cutoff_time = "2026-01-05 00:00:00",
    quiet = TRUE
  )

  expect_true(out$comparability$non_comparable)
  expect_gte(out$comparability$n_tested, 3)
  expect_true("score" %in% out$comparability$field_tests$field)
  expect_true("region" %in% out$comparability$field_tests$field)
  expect_true(is.character(out$decision_sentence))
  expect_equal(length(out$decision_sentence), 1)
  expect_true(grepl("NON-COMPARABLE", out$decision_sentence, fixed = TRUE))
  expect_s3_class(out, "FlagSuspiciousSubmissionsResult")
})

test_that("print method ends with decision sentence", {
  df <- make_shifted_dataset()
  out <- FlagSuspiciousSubmissions(
    df,
    id_col = "submissionId",
    time_col = "created",
    cutoff_time = "2026-01-05 00:00:00",
    quiet = TRUE
  )

  printed <- capture.output(print(out))
  printed <- printed[nzchar(trimws(printed))]
  expect_gt(length(printed), 0)
  expect_true(grepl("^Decision: ", tail(printed, 1)))
})


test_that("FlagSuspiciousSubmissions keeps comparable groups when no drift exists", {
  set.seed(11)
  n <- 120
  df <- data.frame(
    submissionId = paste0("id_", seq_len(n)),
    created = as.POSIXct("2026-01-01 00:00:00", tz = "UTC") + seq_len(n) * 120,
    score = rnorm(n, mean = 0, sd = 1),
    region = sample(c("North", "South"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )

  out <- FlagSuspiciousSubmissions(
    df,
    id_col = "submissionId",
    time_col = "created",
    cutoff_time = "2026-01-01 02:00:00",
    quiet = TRUE
  )

  expect_false(out$comparability$non_comparable)
})


test_that("FlagSuspiciousSubmissions applies BH adjustment and does not over-flag borderline fields", {
  df <- make_shifted_dataset()

  out <- FlagSuspiciousSubmissions(
    df,
    id_col = "submissionId",
    time_col = "created",
    cutoff_time = "2026-01-05 00:00:00",
    quiet = TRUE
  )

  ft <- out$comparability$field_tests
  same_row <- ft[ft$field == "same_field", , drop = FALSE]
  expect_equal(nrow(same_row), 1)
  expect_false(is.na(same_row$p_adj))
  expect_false(same_row$different)
})


test_that("group_action omit_pre returns post-only primary data when non-comparable", {
  df <- make_shifted_dataset()

  out <- FlagSuspiciousSubmissions(
    df,
    id_col = "submissionId",
    time_col = "created",
    cutoff_time = "2026-01-05 00:00:00",
    group_action = "omit_pre",
    quiet = TRUE
  )

  expect_equal(nrow(out$data), nrow(out$datasets$post))
  expect_true(all(as.POSIXct(out$data$created, tz = "UTC") >= as.POSIXct("2026-01-05 00:00:00", tz = "UTC")))
})


test_that("group_action omit_post returns pre-only primary data when non-comparable", {
  df <- make_shifted_dataset()

  out <- FlagSuspiciousSubmissions(
    df,
    id_col = "submissionId",
    time_col = "created",
    cutoff_time = "2026-01-05 00:00:00",
    group_action = "omit_post",
    quiet = TRUE
  )

  expect_equal(nrow(out$data), nrow(out$datasets$pre))
  expect_true(all(as.POSIXct(out$data$created, tz = "UTC") < as.POSIXct("2026-01-05 00:00:00", tz = "UTC")))
})


test_that("group_action split_only always returns pre/post/full datasets", {
  df <- make_shifted_dataset()

  out <- FlagSuspiciousSubmissions(
    df,
    id_col = "submissionId",
    time_col = "created",
    cutoff_time = "2026-01-05 00:00:00",
    group_action = "split_only",
    quiet = TRUE
  )

  expect_true(is.data.frame(out$datasets$pre))
  expect_true(is.data.frame(out$datasets$post))
  expect_true(is.data.frame(out$datasets$full))
  expect_equal(nrow(out$datasets$pre) + nrow(out$datasets$post), nrow(out$datasets$full))
})


test_that("low completeness fields are excluded from comparability tests", {
  df <- make_shifted_dataset()
  df$mostly_missing <- NA_character_
  df$mostly_missing[1:10] <- "x"

  out <- FlagSuspiciousSubmissions(
    df,
    id_col = "submissionId",
    time_col = "created",
    cutoff_time = "2026-01-05 00:00:00",
    min_complete_rate = 0.6,
    quiet = TRUE
  )

  ft <- out$comparability$field_tests
  mm <- ft[ft$field == "mostly_missing", , drop = FALSE]
  expect_equal(nrow(mm), 1)
  expect_equal(mm$test, "excluded_low_completeness")
  expect_true(is.na(mm$p_value))
})


test_that("mixed numeric and categorical fields use expected test pipeline", {
  df <- make_shifted_dataset()

  out <- FlagSuspiciousSubmissions(
    df,
    id_col = "submissionId",
    time_col = "created",
    cutoff_time = "2026-01-05 00:00:00",
    include_cols = c("score", "region"),
    quiet = TRUE
  )

  ft <- out$comparability$field_tests
  expect_equal(ft$test[ft$field == "score"], "wilcox")
  expect_true(ft$test[ft$field == "region"] %in% c("chisq", "fisher"))
  expect_equal(ft$type[ft$field == "score"], "numeric")
  expect_equal(ft$type[ft$field == "region"], "categorical")
})
