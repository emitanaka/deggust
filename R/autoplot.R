


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
  shapes <- rep(shape, length.out = min(length(flist$fill), 3))
  images <- rep(image %||% NA, length.out = min(length(flist$fill), 3))
  nnodes <- length(flist$node)
  nfill <- length(flist$fill)
  obsid <- fct_obs_unit(des)
  parentids <- intersect(fct_parent(des, obsid), unit_ids(des))
  title <- title %||% des$name
  show_border <- show_axis_labels <- FALSE
  if(nnodes==1) {
    # snake-like plot
    plot <- plot_single_unit(.edibble, flist, shapes, images, text, aspect_ratio)
  } else if(nnodes==2) {
    # facets of snake-like plots
    plot <- plot_two_units(.edibble, flist, shapes, images, text, aspect_ratio, obsid, parentids)
    show_border <- TRUE
  } else if(nnodes==3 & length(parentids)==2) {
    # tile plots
    plot <- plot_three_units(.edibble, flist, shapes, images, text, aspect_ratio, obsid, parentids)
    show_axis_labels <- TRUE
  } else {
    abort("`autoplot` is not yet supported for this design.")
  }
  plot <- plot +
    theme_void() +
    theme(plot.margin = margin(7, 7, 7, 7),
          strip.background = element_rect(color = "black", size = 2),
          strip.text = element_text(margin = margin(5, 5, 5, 5), face = "bold"),
          plot.title = element_text(margin = margin(b = 5)),
          plot.title.position = "plot") +
    ggtitle(title)

  if(show_border) {
    plot <- plot +
      theme(panel.border = element_rect(color = "black", fill = NA))
  }

  if(show_axis_labels) {
    plot <- plot + theme(axis.text.x = element_text(color = "black", size = 8,
                                                    margin = margin(t = 5)),
                         axis.text.y = element_text(color = "black", size = 8,
                                                    margin = margin(r = 5)),
                         axis.title.x = element_text(color = "black", margin = margin(t = 5)),
                         axis.title.y = element_text(color = "black", margin = margin(r = 5)))
  }

  plot
}




is_edibble_unit <- function(x) {
  inherits(x, "edbl_unit")
}

is_edibble_trt <- function(x) {
  inherits(x, "edbl_trt")
}


