#' Internal package state
#'
#' Stores ephemeral session state used by interactive helpers.
#'
#' @keywords internal
.formior_state <- new.env(parent = emptyenv())
.formior_state$Form_Info <- NULL
