


#' Auto plot or `ggplot2` of an edibble design
#'
#' @description
#'
#' This plot produces the same type of plot using the `plot` functions in
#' `edibble` package except the plots are `ggplot`
#'
#' @param .edibble An edibble desgin, an edibble table or an edibble graph.
#' @param view A string specifying the level of the view ("high" or "low") to plot.
#'  By default it is "high".
#' @param ... Additional parameters passed to `plot.igraph`.
#' @param main The title of the plot. By default it is the
#'  name of the edibble design.
#' @name autoplot.edibble
#' @return A `ggraph` and `ggplot` object.
#' @seealso See \code{\link[edibble]{plot.edibble}} for the base plot version of this plot.
#'
NULL

#' @importFrom ggplot2 autoplot
#' @rdname autoplot.edibble
#' @export
autoplot.EdibbleDesign <- function(.edibble, view = c("high", "low"),
                                   ..., main = NULL) {
  main <- main %||% .edibble$name
  .graph <- .edibble$graph
  autoplot(.graph, view = view, ..., main = main)
}

#' @rdname autoplot.edibble
#' @importFrom edibble subset_vars subset_levels
#' @importFrom ggplot2 aes scale_color_manual ggtitle
#' @importFrom ggraph ggraph label_rect geom_edge_link geom_node_point circle geom_node_text
#' @importFrom grid arrow unit
#' @export
autoplot.edbl_graph <- function(.edibble, view = c("high", "low"),
                                ..., main = NULL) {
  main <- main %||% "An edibble design"
  view <- match.arg(view)
  out <- switch(view,
                high = subset_vars(.edibble),
                low = subset_levels(.edibble))
  g <- ggraph(out) +
    geom_edge_link(arrow = arrow(length = unit(4, 'mm')),
                   aes(start_cap = label_rect(node1.name),
                       end_cap = label_rect(node2.name),
                       ## this is fine when loading
                       ## but get object "etype" not found error
                       # linetype = edge_ltype_dict[etype]
                       )) +
    geom_node_text(aes(label = label, color = class)) +
    scale_color_manual(name = "Variable Type",
                       breaks = c("edbl_trt", "edbl_unit", "edbl_rcrd"),
                       labels = c("Treatment", "Unit", "Record"),
                       values = c(deggust_opt("vertex.label.color.unit"),
                                  deggust_opt("vertex.label.color.trt"),
                                  deggust_opt("vertex.label.color.rcrd"))) +
    ggtitle(main)
  g
}

#' @rdname autoplot.edibble
#' @importFrom edibble is_edibble get_edibble_design
#' @export
autoplot.edbl_table <- function(.edibble, aspect_ratio = 1,
                                shape = "circle", text = FALSE, image = NULL,
                                trts = NULL, scales = NULL) {
  if(!is_edibble(.edibble)) {
    abort("Don't know how to plot an edibble table with no design.")
  }

  ind_units <- unlist(lapply(.edibble, is_edibble_unit))
  unit_names <- names(ind_units)[ind_units]
  nunits <- sum(ind_units)

  ind_trts <- unlist(lapply(.edibble, is_edibble_trt))
  trt_names <- trts %||% names(ind_trts)[ind_trts]
  ntrts <- length(trt_names)

  scales <- scales %||% lapply(1:ntrts, function(x) ggplot2::scale_fill_discrete())

  nlevels_unit <- nrow(.edibble)

  if(nunits==1) {
    # make it snake-like
    unit_vec <- .edibble[[unit_names]]
    nodes <- .edibble[c(unit_names, setdiff(names(.edibble), unit_names))]
    nodes <- as_data_frame(nodes)
    nodes <- cbind(nodes, coord_snake(nlevels_unit, aspect_ratio))
    plot <- ggplot(nodes, aes(x, y)) +
      geom_path()
    if(is.null(image)) {
        plot <- plot + geom_unit_node(aes(fill = !!parse_expr(trt_names[1])), shape = shape)
    } else {
      if(!require("ggimage")) {
        stop("Please install `ggimage` package to use the image argument.")
      }
      plot <- plot + ggimage::geom_image(aes(color = !!parse_expr(trt_names[1])), image = image, size = 0.1)
    }
    if(ntrts > 1) {
      plot <- another_fill_scale(plot, 2, scales, trt_names, shape = shape, image = image,
                                 height = 0.6, width = 0.6, size = 0.08)
      if(ntrts > 2) {
        plot <- another_fill_scale(plot, 3, scales, trt_names, shape = shape, image = image,
                                   height = 0.4, width = 0.4, size = 0.05)
      }
    }

    plot <- plot +
      theme(axis.ticks.length = grid::unit(0, "npc"),
            panel.grid = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank())
    if(isTRUE(text) || inherits(text, "element_text")) {
      text_aes <- list()
      if(inherits(text, "element_text")) {
        text_aes <- remove_nulls(as.list(text))
        names(text_aes) <- gsub("face", "fontface", names(text_aes))
      }
      plot <- plot +
        do.call("geom_text", c(list(mapping = aes(label = unit_vec)),
                               text_aes))
    }
  } else if(nunits==2 & ntrts==1) {
    # make it two-dimensional
    unames <- unit_names(.edibble)
    nlevels_units <- unlist(lapply(.edibble[unames], nlevels))
    unit_name <- unames[which.max(nlevels_units)]
    parent_name <- setdiff(unames, unit_name)
    unit_vec <- .edibble[[unit_name]]

    nodes <- .edibble[c(unit_names, setdiff(names(.edibble), unit_names))]
    nodes <- as_data_frame(nodes)

    tt <- vapply(split(nodes, nodes[unames[1]]), nrow, numeric(1))
    nodes$xnames <- names_to_nesting_names(.edibble, paste0(unames[2], ":", nodes[[unames[2]]]))

    #if(max(tt) <= w) {
      nodes$x <- as.numeric(as.factor(nodes$xnames))
      nodes$y <- as.numeric(factor(nodes[[parent_name]],
                                   levels = unique(nodes[[parent_name]])))
      block_data <- data.frame(x = (max(nodes$x) + 1)/2,
                               y =  (1:max(nodes$y)),
                               height = 1.6)
    #} else {

    #}

    plot <- ggplot(nodes, aes(x = x, y = y)) +
      geom_unit_node(data = block_data,
                     shape = "box", aes(width = max(nodes$x) * 2, height = height)) +
      geom_path(aes(group = !!parse_expr(parent_name))) +
      geom_unit_node(aes(fill = !!parse_expr(trt_names)), shape = shape) +
      theme(axis.ticks.length = grid::unit(0, "npc"),
            panel.grid = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank())
    if(isTRUE(text) || inherits(text, "element_text")) {
      text_aes <- list()
      if(inherits(text, "element_text")) {
        text_aes <- remove_nulls(as.list(text))
        names(text_aes) <- gsub("face", "fontface", names(text_aes))
      }
      plot <- plot +
        do.call("geom_text", c(list(mapping = aes(label = unit_vec)),
                               text_aes))
    }
  } else if(nunits==1 && ntrts==2) {

  }

  plot
}

is_edibble_unit <- function(x) {
  inherits(x, "edbl_unit")
}

is_edibble_trt <- function(x) {
  inherits(x, "edbl_trt")
}


#' @importFrom edibble is_edibble_table
autoplot_test <- function(.table, ...) {
  if(!is_edibble_table(.table)) {
    abort("The supplied object is not an edibble table.")
  }


}
