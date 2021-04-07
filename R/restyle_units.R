
#' @export
restyle_units <- function(shape = NULL,
                      image = NULL, ...) {
  structure(list(shape = shape,
                 image = image,
                 ...),
            class = c("degg_unit", "list"))
}

#' @importFrom ggplot2 ggplot_add %+%
#' @keywords internal
#' @export
ggplot_add.degg_unit <- function(object, plot, object_name) {
  ilayer <- identify_layer(plot, "geom", "GeomCircle")
  if(!is.null(object$shape)) {
    unit_layer <- plot$layers[[ilayer]]
    plot$layers[[ilayer]] <- switch(object$shape,
                                    "square" = ggraph::geom_node_tile(aes(width = 0.3, height = 0.3,
                                                                          fill =  !!unit_layer$mapping$fill)),
                                    "point" = ggraph::geom_node_point(aes(shape = 1,
                                                                          color =  !!unit_layer$mapping$fill)),
                                    "none" = NULL)
  }
  plot
}
