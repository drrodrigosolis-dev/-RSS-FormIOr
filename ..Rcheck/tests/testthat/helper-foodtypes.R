foodtypes_raw <- function() {
	# Keep the dataset out of the global environment during tests.
	env <- new.env(parent = emptyenv())
	data("FoodTypes", package = "FormIOr", envir = env)
	env$FoodTypes
}

foodtypes_flat <- function() {
	flatten_submission_records(foodtypes_raw())
}

foodtypes_resolved <- function() {
	collapse_repeated_values(
		foodtypes_flat(),
		id_col = "form-submission_id",
		return_flat = TRUE,
		quiet = TRUE
	)$flat
}

