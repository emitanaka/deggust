

#' @export
geom_unit_node <- function(mapping = NULL, data = NULL, position = "identity",
                            shape = "circle", stat = "identity",
                            show.legend = NA, inherit.aes = TRUE, ...) {
  layer(data = data, mapping = mapping, stat = stat,
        geom = GeomNodeShape, position = position, show.legend = show.legend,
        inherit.aes = inherit.aes, params = list(na.rm = FALSE, shape = shape, ...))

}


#' @export
GeomUnitNode <- ggproto("GeomUnitNode", Geom,
                         required_aes = c("unit"),
                         #extra_params = c("na.rm"),
                         draw_key = draw_key_unit,
                         default_aes = aes(colour = "black",
                                           lwd = 1,
                                           lty = "solid",
                                           alpha = 1,
                                           angle = 0,
                                           fill = "white"),
                         setup_data = function(data, params) {
                           data$shape <- data$shape %||% params$shape %||% "circle"
                           data$group <- seq_len(NROW(data))
                           data_shape <- split(data, data$shape)
                           data_shape <- lapply(data_shape, function(df) {
                             n <- n_shape(df$shape[1])
                             phase <- phase_shape(df$shape[1])
                             regular_polygon_data(df, n = n, phase = phase)
                           })
                           do.call(rbind, data_shape)
                         },
                         draw_panel = function(data, panel_params, coord, ...) {
                           #browser()
                           n <- nrow(data)
                           if(n==0) {
                             return(zeroGrob())
                           }
                           #browser()
                           munched <- coord_munch(coord, data, panel_params)
                           munched <- munched[order(munched$group), ]
                           if (!is.integer(munched$group)) {
                             munched$group <- match(munched$group, unique(munched$group))
                           }
                           first_idx <- !duplicated(munched$group)
                           first_rows <- munched[first_idx, ]
                           grid::polygonGrob(munched$x, munched$y, default.units = "native",
                                             id = munched$group,
                                             gp = grid::gpar(col = first_rows$colour,
                                                             fill = scales::alpha(first_rows$fill, first_rows$alpha),
                                                             lwd = first_rows$size * .pt, lty = first_rows$linetype))
                         })
