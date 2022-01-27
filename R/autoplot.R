


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
#' @param node A character vector of variable names. It's assumed that the variables
#'   are units.
#' @param horizontal A logical value indicating whether the display should be
#'  optimized for horizontal display (default) or vertical display. Not yet implemented.
#' @return A `ggplot` object.
#' @export
autoplot.edbl_table <- function(.edibble, title = NULL, aspect_ratio = 1,
                                shape = "circle", text = FALSE, image = NULL,
                                fill = NULL, node = NULL, horizontal = TRUE) {

  des <- edbl_design(.edibble)
  unames <- unit_names(des)
  tnames <- trt_names(des)
  rnames <- rcrd_names(des)
  flist <- list(units = unames,
                trts = tnames,
                rcrds = rnames,
                fill = fill %||% tnames,
                node = node %||% unames)
  shapes <- rep(shape, length.out = length(flist$fill))
  images <- rep(image %||% NA, length.out = length(flist$fill))
  nnodes <- length(flist$node)
  nfill <- length(flist$fill)
  obsid <- fct_obs_unit(des)
  parentids <- intersect(fct_parent(des, obsid), unit_ids(des))
  title <- title %||% des$name

  if(nnodes==1) {
    plot <- plot_single_unit(.edibble, flist, shapes, images, text, aspect_ratio)
  } else if(nnodes==2 & nfill==1) {
    plot <- plot_two_units(.edibble, flist, shapes, images, text, aspect_ratio, obsid, parentids)
  } else if(nnodes==3 & length(parentids)==2 & nfill==1) {
    plot <- plot_three_units(.edibble, flist, shapes, images, text, aspect_ratio, obsid, parentids)
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


