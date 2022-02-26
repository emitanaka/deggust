

#' Miniature version of the input
#'
#' This trims the table to a smaller scale but preserving the basic structure of
#' the the design.
#'
#' @param edibble The table design
#'
#'
#' @export
miniature <- function(edibble, ...) {
  UseMethod("miniature")
}


#' @describeIn miniature
#' @export
miniature.edbl_table <- function(edibble, ...) {

}
