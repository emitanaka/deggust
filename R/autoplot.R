


#' @export
ggplot2::autoplot

#' Auto plot or `ggplot2` of an edibble design
#'
#'
#' @param .edibble An edibble desgin, an edibble table or an edibble graph.
#' @param ... Unused at the moment.
#' @param title The title of the plot. By default it is the
#'  name of the edibble design if available.
#' @param aspect_ratio The aspect ratio of the graph.
#' @param shape The shape of the unit.
#' @param text A logical value of whether to show the text or not. Alternatively,
#'   it can be a `ggplot2::element_text()` object to customise other elements of text,
#'   e.g., size, font, font face, color, etc.
#' @param image An image to use instead of `shape`. The file path to the image should be
#'   supplied. If an `image` is supplied, `shape` is ingored.
#' @param fill A character vector of variable names to display. Only a maximum of three
#'   variables are allowed. Currently, it's assumed that the the variables are discrete.
#'   In general, it's assumed that the variables are treatment variables.
#' @param horizontal A logical value indicating whether the display should be
#'  optimized for horizontal display (default) or vertical display. Not yet implemented.
#' @return A `ggplot` object.
#' @export
autoplot.edbl_table <- function(.edibble, title = NULL, aspect_ratio = 1,
                                shape = "circle", text = FALSE, image = NULL,
                                fill = NULL, horizontal = TRUE) {

  des <- edbl_design(.edibble)
  unames <- unit_names(des)
  nunits <- length(unames)
  tnames <- trt_names(des)
  ntrts <- length(tnames)
  rnames <- rcrd_names(des)
  flist <- list(units = unames,
                trts = tnames,
                rcrds = rnames,
                fill = fill %||% tnames)

  N <- nrow(.edibble)
  obsid <- fct_obs_unit(des)
  parentids <- intersect(fct_parent(des, obsid), unit_ids(des))
  title <- title %||% des$name

  if(nunits==1) {
    plot <- plot_single_unit(.edibble, flist, shape, image, text, aspect_ratio)
  } else if(nunits==2 & ntrts==1) {
    plot <- plot_two_units(.edibble, nlevels_unit, ntrts, unit_names, trt_names, shape, image, text, aspect_ratio)
  } else if(nunits==3 & length(parentids)==2 & ntrts==1) {
    plot <- plot_three_units(.edibble, nlevels_unit, ntrts, unit_names, trt_names, shape, image, text, aspect_ratio,
                             obsid, parentids)
  } else {
    abort("`autoplot` is not yet supported for this design.")
  }
  plot +
    theme_void() +
    theme(plot.margin = margin(7, 7, 7, 7)) +
    ggtitle(title)
}




is_edibble_unit <- function(x) {
  inherits(x, "edbl_unit")
}

is_edibble_trt <- function(x) {
  inherits(x, "edbl_trt")
}


