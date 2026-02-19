set.seed(20260218)

n_pre <- 120
n_post <- 120

pre_time <- as.POSIXct("2026-01-01 08:00:00", tz = "UTC") + seq_len(n_pre) * 180
post_time <- as.POSIXct("2026-01-07 08:00:00", tz = "UTC") + seq_len(n_post) * 180

pre <- data.frame(
  submissionId = sprintf("pre_%03d", seq_len(n_pre)),
  created = pre_time,
  age = pmax(18, round(rnorm(n_pre, mean = 37, sd = 11))),
  satisfaction_score = pmin(100, pmax(0, round(rnorm(n_pre, mean = 48, sd = 12), 1))),
  region = sample(c("North", "South", "East", "West"), n_pre, replace = TRUE, prob = c(0.50, 0.15, 0.20, 0.15)),
  program = sample(c("A", "B", "C"), n_pre, replace = TRUE, prob = c(0.55, 0.30, 0.15)),
  consent = sample(c("Yes", "No"), n_pre, replace = TRUE, prob = c(0.82, 0.18)),
  comment = sample(c("ok", "fine", "good", "needs support", ""), n_pre, replace = TRUE, prob = c(0.30, 0.22, 0.18, 0.20, 0.10)),
  stringsAsFactors = FALSE
)

post <- data.frame(
  submissionId = sprintf("post_%03d", seq_len(n_post)),
  created = post_time,
  age = pmax(18, round(rnorm(n_post, mean = 41, sd = 10))),
  satisfaction_score = pmin(100, pmax(0, round(rnorm(n_post, mean = 63, sd = 11), 1))),
  region = sample(c("North", "South", "East", "West"), n_post, replace = TRUE, prob = c(0.22, 0.35, 0.23, 0.20)),
  program = sample(c("A", "B", "C"), n_post, replace = TRUE, prob = c(0.30, 0.40, 0.30)),
  consent = sample(c("Yes", "No"), n_post, replace = TRUE, prob = c(0.92, 0.08)),
  comment = sample(c("ok", "fine", "good", "needs support", ""), n_post, replace = TRUE, prob = c(0.28, 0.26, 0.20, 0.18, 0.08)),
  stringsAsFactors = FALSE
)

base <- rbind(pre, post)
base$phase_expected <- ifelse(grepl("^pre_", base$submissionId), "pre", "post")

# Duplicate-ID rows (same submissionId repeated with close timestamps)
dup_id_rows <- base[sample(which(base$phase_expected == "pre"), 12), , drop = FALSE]
dup_id_rows$created <- dup_id_rows$created + sample(30:480, nrow(dup_id_rows), replace = TRUE)

# Fingerprint repeats with different IDs (same answers, different submission IDs)
fp_src <- base[sample(which(base$phase_expected == "pre"), 10), , drop = FALSE]
fp_rows <- fp_src
fp_rows$submissionId <- sprintf("anon_repeat_%03d", seq_len(nrow(fp_rows)))
fp_rows$created <- fp_src$created + sample(45:540, nrow(fp_rows), replace = TRUE)

# Low-completeness field to test exclusion logic
base$mostly_missing <- NA_character_
base$mostly_missing[sample(seq_len(nrow(base)), 20)] <- sample(c("x", "y"), 20, replace = TRUE)

dup_id_rows$mostly_missing <- NA_character_
fp_rows$mostly_missing <- NA_character_

SuspiciousSurveyDemo <- rbind(base, dup_id_rows, fp_rows)
SuspiciousSurveyDemo <- SuspiciousSurveyDemo[order(SuspiciousSurveyDemo$created), , drop = FALSE]
row.names(SuspiciousSurveyDemo) <- NULL

save(SuspiciousSurveyDemo, file = "data/SuspiciousSurveyDemo.rda")
