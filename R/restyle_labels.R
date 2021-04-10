
#' @export
restyle_labels <- function(nudge_x = 0,
                           nudge_y = 0,
                           color = "black",
                           colour = color,
                           family = NULL,
                           fontface = NULL,
                           size = NULL,
                           lineheight = NULL,
                           ...
                           ) {
  structure(list(nudge_x = nudge_x,
                 nudge_y = nudge_y,
                 color = colour,
                 family = family,
                 fontface = fontface,
                 size = size,
                 lineheight = lineheight,
                 ...),
            class = c("degg_text", "list"))

}

#' @keywords internal
#' @export
ggplot_add.degg_text <- function(object, plot, object_name) {
  ilayer <- identify_layer(plot, "geom", "GeomText")[1]
  unit_layer <- plot$layers[[ilayer]]
  plot$layers[[ilayer]] <- ggraph::geom_node_text(aes(x = !!unit_layer$mapping$x,
                                                      y = !!unit_layer$mapping$y,
                                                      label = !!unit_layer$mapping$label
                                                      #hjust = object$nudge_x,
                                                      #vjust = object$nudge_y,
                                                      ),
                                                  family = object$family %||% "",
                                                  size = object$size %||% 3.88,
                                                  fontface = object$fontface %||% 1,
                                                  color = object$color,
                                                  lineheight = object$lineheight %||% 1.2,
                                                  show.legend = NA
                                                  )
  addGeomClass(plot, ilayer, "GeomUnitText")
}
