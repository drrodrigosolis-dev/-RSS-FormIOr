
test_that("review_duplicate_rows supports non-interactive keep_map", {
  df <- data.frame(
    submissionId = c("a", "a", "b", "b", "b", "c", "c"),
    email = c(
      "u1@example.com",
      "u1@example.com",
      "u1@example.com",
      "u1@example.com",
      "u1@example.com",
      "u2@example.com",
      "u2@example.com"
    ),
    value = c(1, 2, 3, 4, 5, 6, 7),
    stringsAsFactors = FALSE
  )

  # Group 1 is the u1@example.com group (duplicate across submissionId a and b)
  keep_map <- list(`1` = 2)

  out <- review_duplicate_rows(
    df,
    id_col = "submissionId",
    key_cols = "email",
    compare_cols = c("submissionId", "email", "value"),
    keep_map = keep_map,
    prompt = FALSE,
    quiet = TRUE
  )

  expect_equal(out$data$submissionId, c("b", "b", "b", "c", "c"))
  expect_equal(out$data$value, c(3, 4, 5, 6, 7))
  expect_equal(out$summary$removed_rows, 2)
  expect_equal(out$summary$processed_groups, 1)
})
