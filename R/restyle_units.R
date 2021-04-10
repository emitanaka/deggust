
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
  ilayer <- identify_layer(plot, "geom", "GeomUnit")[1]
  # Some how GeomUnit gets into 3rd layer too
  if(!is.null(object$shape)) {
    unit_layer <- plot$layers[[ilayer]]
    width <- object$width %||% 0.5
    height <- object$height %||% 0.5
    r <- object$r %||% 0.3
    plot$layers[[ilayer]] <- switch(object$shape,
                                    "box" = ,
                                    "rect" = ,
                                    "rectangle" = geom_node_shape(aes(x = !!unit_layer$mapping$x0,
                                                                      y = !!unit_layer$mapping$y0,
                                                                      width = width, height = height,
                                                                      fill = !!unit_layer$mapping$fill,
                                                                      shape = "box", phase = object$phase)),
                                    "square" = ,
                                    "pentagon" = ,
                                    "hexagon" = ,
                                    "septagon" = ,
                                    "octagon" = ,
                                    "ellipse" = ,
                                    "triangle" = geom_node_shape(aes(x = !!unit_layer$mapping$x0,
                                                                     y = !!unit_layer$mapping$y0,
                                                                     width = width, height = height,
                                                                     fill = !!unit_layer$mapping$fill,
                                                                     shape = object$shape, phase = object$phase)),

                                    "circle" = geom_node_circle(aes(r = r,
                                                                    fill =  !!unit_layer$mapping$fill)),
                                    "none" = NULL)
    plot <- addGeomClass(plot, ilayer, "GeomUnit")
  }
  plot
}
