


#' @export
geom_node_shape <- function(mapping = NULL, data = NULL, position = "identity",
                            stat = "identity", shape = "circle",
                            show.legend = NA, inherit.aes = TRUE, ...) {
  layer(data = data, mapping = mapping, stat = stat,
        geom = GeomNodeShape, position = position, show.legend = show.legend,
        inherit.aes = inherit.aes, params = list(na.rm = FALSE, shape = shape, ...))

}

regular_polygon <- function(n = 5, phase = 0, radius = 1){
  stopifnot(n > 2)
  angle <- 2 * pi / n
  locs <- complex(argument = angle * seq_len(n) + phase) * radius
  data.frame(x = Re(locs), y = Im(locs))
}

regular_polygon_data <- function(data, n = 5, phase = 0, suffix = 0) {
  vertices <- regular_polygon(n = n, phase = phase)
  data$group <- paste0(seq_len(NROW(data)), "-", suffix)
  Reduce(rbind, Map(function(i) {
      new <- data
      new$x <- new$x + vertices$x[i] * new$width / 2
      new$y <- new$y + vertices$y[i] * new$height / 2
      new
    }, 1:n))
}

# shapes inspired by https://graphviz.org/doc/info/shapes.html
n_shape <- function(shape) {
  switch(shape,
         "triangle"  = 3,
         "square"    = ,
         "rect"      = ,
         "rectangle" = ,
         "box"       = 4,
         "pentagon"  = 5,
         "hexagon"   = 6,
         "septagon"  = 7,
         "octagon"   = 8,
         "oval"      = ,
         "ellipse"   = ,
         "circle"    = 50)
}

phase_shape <- function(shape) {
  switch(shape,
         "triangle"  = -pi/6,
         "square"    = ,
         "rect"      = ,
         "rectangle" = ,
         "box"       = pi/4,
         "pentagon"  = pi/10,
         "hexagon"   = 0,
         "septagon"  = -pi/14,
         "octagon"   = pi/8,
         "circle"    = 0)
}


#' @export
GeomNodeShape <- ggproto("GeomNodeShape", Geom,
                         required_aes = c("x", "y"),
                         extra_params = c("na.rm", "shape", "phase"),
                         draw_key = draw_key_shape,
                         default_aes = aes(colour = "black",
                                           size = 1,
                                           lwd = 1, lty = "solid",
                                           alpha = 1, fill = "white",
                                           width = NULL, height = NULL, angle = NULL),
                         setup_data = function(data, params) {
                           data$width <- data$width %||% params$width %||% resolution(data$x, FALSE) * 0.8
                           data$height <- data$height %||% params$height %||% resolution(data$y, FALSE) * 0.8
                           data$shape <- data$shape %||% params$shape
                           data$group <- seq_len(NROW(data))
                           data_shape <- split(data, data$shape)
                           data_shape <- lapply(data_shape, function(df) {
                               n <- n_shape(df$shape[1])
                               phase <- data$angle %||% params$angle %||% phase_shape(df$shape[1])
                               regular_polygon_data(df, n = n, phase = phase)
                             })
                           do.call(rbind, data_shape)
                         },
                         draw_panel = function(data, panel_params, coord, ...) {
                            n <- nrow(data)
                            if(n==0) {
                              return(zeroGrob())
                            }
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


