


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
autoplot.edbl_table <- function(.edibble, view = c("high", "low"), ..., main = NULL) {
  if(!is_edibble(.edibble)) {
    abort("Don't know how to plot an edibble table with no design.")
  }
  autoplot(get_edibble_design(.edibble))
}

