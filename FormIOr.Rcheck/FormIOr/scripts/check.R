source("inst/scripts/setup-lib.R")

devtools::check(args = c("--no-manual", "--no-build-vignettes"))
