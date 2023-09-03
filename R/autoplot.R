


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
#' @param control A named list that contain values to control the choice of data to be displayed.
#' @return A `ggplot` object.
#' @export
autoplot.edbl_table <- function(.edibble,
                                title = NULL,
                                subtitle = NULL,
                                aspect_ratio = 4/3,
                                shape = "circle",
                                text = FALSE,
                                image = NULL,
                                fill = NULL,
                                node = NULL,
                                horizontal = TRUE,
                                control = deggust_control()) {

  prov <- edibble::activate_provenance(.edibble)
  unames <- prov$unit_names()
  tnames <- prov$trt_names()
  rnames <- prov$rcrd_names()
  flist <- list(units = unames,
                trts = tnames,
                rcrds = rnames,
                fill = fill %||% tnames,
                node = node %||% unames)
  flvls <- prov$fct_levels(return = "value")
  shapes <- rep(shape, length.out = min(length(flist$fill), 3))
  images <- rep(image %||% NA, length.out = min(length(flist$fill), 3))
  nnodes <- length(flist$node)
  nfill <- length(flist$fill)
  obsid <- prov$fct_id_leaves(role = "edbl_unit")
  parentids <- prov$fct_id_parent(id = obsid, role = "edbl_unit")


  plot_set <- list(show_border = FALSE,
                   show_axis_labels = FALSE,
                   title = title %||% prov$get_title(),
                   subtitle = subtitle %||% paste("Unit:", prov$fct_names(id = obsid)))

  # FIXME: control should be probably applied at the plot not the data

  # nnode_max
  # FIXME: not using control$random_units at the moment
  # This however makes the page control hard though so perhaps don't go ahead with it?
  min_index <- ifelse(is.infinite(control$nnode_max), 1L, (control$page - 1) * control$nnode_max + 1)
  max_index <- min(control$page * control$nnode_max, nrow(.edibble))
  data <- .edibble[min_index:max_index, ]


  # # nfill_max
  # control$nfill_max <- rep(control$nfill_max, length.out = nfill)
  # control$random_fills <- rep(control$random_fills, length.out = nfill)
  # for(ifill in seq_along(flist$fill)) {
  #   data[[flist$fill[ifill]]] <- lvl_lump(data[[flist$fill[ifill]]],
  #                                       control$nfill_max[ifill],
  #                                       control$random_fills[ifill])
  # }
  if(control$page==1) warn_drop(.edibble, data)

  if(nnodes==1) {
    # snake-like plot, CRD
    plot <- plot1(data, flist, flvls, shapes, images, text, aspect_ratio, control)
  } else if(nnodes==2) {
    # facets of snake-like plots, e.g. RCBD
    plot <- plot2(data, flist, flvls, shapes, images, text, aspect_ratio, control, obsid, parentids)
    plot_set$show_border <- TRUE
  } else if(nnodes==3 & length(parentids)==2) {
    # tile plots e.g. LSD, graeco, youden
    plot <- plot3(data, flist, flvls, shapes, images, text, aspect_ratio, control, obsid, parentids)
    plot_set$show_axis_labels <- TRUE
  } else if(nnodes==3 & length(parentids)==1) {
    # block/pot/plant
    plot <- plot3(data, flist, flvls, shapes, images, text, aspect_ratio, control, obsid, parentids)
    plot_set$show_axis_labels <- FALSE
  } else if(nnodes==4 & length(parentids) %in% c(3, 2)) {
    # tile plots + facet_wrap
    plot <- plot4(data, flist, flvls, shapes, images, text, aspect_ratio, control, obsid, parentids)
    plot_set$show_axis_labels <- TRUE
  } else if(nnodes > 4 & length(parentids)==4) {
    # tile plots + facet_grid e.g. hyper-graeco latin square design
    plot <- plot5(data, flist, flvls, shapes, images, text, aspect_ratio, control, obsid, parentids)
    plot_set$show_axis_labels <- TRUE
  } else {
    abort("`autoplot` is not yet supported for this design.")
  }

  plot_set_last(plot, plot_set)
}

plot_set_last <- function(plot, set) {
  plot <- plot +
    plot_default() +
    labs(title = set$title, subtitle = set$subtitle)

  if(set$show_border) {
    plot <- plot + theme_border()
  }

  if(set$show_axis_labels) {
    plot <- plot + theme_axis()
  }

  plot
}

theme_axis <- function() {
  theme(axis.text.x = element_text(color = "black", size = 8,
                                   angle = 270,
                                   hjust = 0,
                                   margin = margin(t = 5)),
        axis.text.y = element_text(color = "black", size = 8,
                                   margin = margin(r = 5)),
        axis.title.x = element_text(color = "black", margin = margin(t = 5)),
        axis.title.y = element_text(color = "black", margin = margin(r = 5)),
        strip.text.y = element_text(angle = 270))
}

theme_border <- function() {
  theme(panel.border = element_rect(color = "black", fill = NA))
}


plot_default <- function() {
  list(theme_void() +
         theme(plot.margin = margin(7, 7, 7, 7),
               strip.background = element_rect(color = "black", size = 2),
               strip.text = element_text(margin = margin(5, 5, 5, 5), face = "bold"),
               plot.title = element_text(margin = margin(b = 5), face = "bold"),
               plot.subtitle = element_text(family = "mono"),
               plot.title.position = "plot"))
  #coord_equal()

}
