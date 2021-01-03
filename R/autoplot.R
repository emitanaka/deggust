


#' Plots to visualise edibble objects
#'
#'
#' @param .data An edibble design.
#'
#' @importFrom ggplot2 autoplot
#' @export
autoplot.EdibbleDesign <- function(.data, ...) {
  if(is_null(.data$table)) {
    d_tmp <- names(.data$graph)

  } else {

    .data$serve_table()
    autoplot(.data$table)
  }
}

#' @export
autoplot.edbl_df <- function(.data, ...) {
  abort("not implemented yet")
}

