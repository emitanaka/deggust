


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
autoplot.edbl_table <- function(.edibble, width = NULL, height = NULL) {
  if(!is_edibble(.edibble)) {
    abort("Don't know how to plot an edibble table with no design.")
  }

  width <- width %||% 12
  height <- height %||% 9
  wratio <- width / (width + height)

  ind_units <- unlist(lapply(.edibble, is_edibble_unit))
  unit_names <- names(ind_units)[ind_units]
  nunits <- sum(ind_units)

  ind_trts <- unlist(lapply(.edibble, is_edibble_trt))
  trt_names <- names(ind_trts)[ind_trts]
  ntrts <- sum(ind_trts)

  if(nunits==1) {
    # make it snake-like
    nlevels_unit <- nrow(.edibble)
    unit_dims <- c(round(sqrt(nlevels_unit)), round(sqrt(nlevels_unit))) # y vs. x
    unit_vec <- .edibble[[unit_names]]
    edges <- data.frame(from = unit_vec[-1],
                        to = unit_vec[-length(unit_vec)])
    nodes <- .edibble[c(unit_names, setdiff(names(.edibble), unit_names))]
    nodes <- edibble:::as_data_frame(nodes)
    nodes$x <- rep(1:unit_dims[1], length.out = nlevels_unit)
    nodes$y <- sort(rep(1:unit_dims[2], length.out = nlevels_unit))
    plot <- ggplot(nodes, aes(x, y)) +
      geom_node_shape(aes(fill = !!parse_expr(trt_names)), shape = "circle") +
      theme(axis.ticks.length = grid::unit(0, "npc"),
            panel.grid = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank())
    # plot <- ggraph::ggraph(graph,
    #                layout = "manual",
    #                x = rep(1:unit_dims[1], length.out = nlevels_unit),
    #                y = sort(rep(1:unit_dims[2], length.out = nlevels_unit))) +
    #   ggraph::geom_edge_diagonal() +
    #   # assumes one trt only
    #   geom_node_shape(aes(shape = "circle", fill = !!parse_expr(trt_names))) +
    #   #ggraph::geom_node_circle(ggplot2::aes_string(r = 0.3, fill = trt_names)) +
    #   #ggraph::geom_node_tile(width = 0.8, height = 0.8, aes_string(fill = trt_names)) +
    #   ggraph::geom_node_text(aes(label =  unit_vec)) +
    #   ggplot2::coord_equal()

    addGeomClass(plot, identify_layer(plot, "geom", "GeomNodeShape"), "GeomUnit")
    #addGeomClass(plot, identify_layer(plot, "geom", "GeomText"), "GeomUnitText")

  } else if(nunits==2) {
    # make it two-dimensional
    unames <- unit_names(.edibble)
    nlevels_units <- unlist(lapply(.edibble[unames], nlevels))
    unit_dims <- c(min(nlevels_units), max(nlevels_units))
    unit_name <- unames[which.max(nlevels_units)]
    unit_vec <- .edibble[[unit_name]]
    edges <- data.frame(from = unit_vec[-1],
                        to = unit_vec[-length(unit_vec)]) %>%
      unique()
    nodes <- .edibble[c(unit_name, setdiff(names(.edibble), unit_name))]
    graph <- igraph::graph_from_data_frame(edges,
                                           vertices = edibble:::as_data_frame(nodes))
    ggraph::ggraph(graph,
                   layout = "manual",
                   x = as.integer(nodes[[unit_name]]),
                   y = as.integer(nodes[[setdiff(unames, unit_name)]])) +
      ggraph::geom_edge_diagonal() +
      # assumes one trt only
      ggraph::geom_node_circle(ggplot2::aes_string(r = 0.3, fill = trt_names)) +
      ggraph::geom_node_text(aes(label =  unit_vec))

  }

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
