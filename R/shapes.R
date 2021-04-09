


#' @export
geom_node_shape <- function(mapping = NULL, data = NULL, position = "identity",
                            shape = "circle", show.legend = NA, inherit.aes = TRUE, ...) {
  layer(data = data, mapping = mapping, stat = StatNodeShape,
        # or GeomPolygon
        geom = GeomNodeShape, position = position, show.legend = show.legend,
        inherit.aes = inherit.aes, params = list(na.rm = FALSE, shape = shape, ...))

}

custom_polygon_shape <- function(data, scales, ...) {
  UseMethod("custom_polygon_shape")
}


regular_polygon <- function(n = 5, phase = 0, radius = 1){
  stopifnot(n > 2)
  angle <- 2 * pi / n
  locs <- complex(argument = angle * seq_len(n) + phase) * radius
  data.frame(x = Re(locs), y = Im(locs))
}

regular_polygon_data <- function(data, n = 5, phase = 0, radius = 1) {
  corners <- regular_polygon(n = n, phase = phase, radius = radius)
  data$group <- seq_len(NROW(data))
  out <- data[0, ]
  for(i in 1:n) {
    new <- data
    new$x <- new$x + corners$x[i]
    new$y <- new$y + corners$y[i]
    out <- rbind(out, new)
  }
  out[order(out$group), ]
}

custom_polygon_shape.triangle <- function(data, scales, ...) {
  data$group <- seq_len(NROW(data))
  regular_polygon_data(data, n = 3, phase = -pi/6, radius = data$width[1])
}

custom_polygon_shape.square <- function(data, scales, ...) {
  data$group <- seq_len(NROW(data))
  regular_polygon_data(data, n = 4, phase = pi/4, radius = data$width[1])
}

custom_polygon_shape.pentagon <- function(data, scales, ...) {
  data$group <- seq_len(NROW(data))
  regular_polygon_data(data, n = 5, phase = pi/10, radius = data$width[1])
}

custom_polygon_shape.hexagon <- function(data, scales, ...) {
  data$group <- seq_len(NROW(data))
  regular_polygon_data(data, n = 6, phase = 0, radius = data$width[1])
}

custom_polygon_shape.septagon <- function(data, scales, ...) {
  data$group <- seq_len(NROW(data))
  regular_polygon_data(data, n = 7, phase = -pi/14, radius = data$width[1])
}

custom_polygon_shape.octagon <- function(data, scales, ...) {
  data$group <- seq_len(NROW(data))
  regular_polygon_data(data, n = 8, phase = pi/8, radius = data$width[1])
}

custom_polygon_shape.box <- function(data, scales, ...) {
  data$group <- seq_len(NROW(data))
  width <- data$width[1]
  height <- data$height[1]
  corners <- data.frame(x = c(-width/2, -width/2, width/2, width/2),
                        y = c(-height/2, height/2, height/2, -height/2))
  out <- data[0, ]
  for(i in 1:4) {
    new <- data
    new$x <- new$x + corners$x[i]
    new$y <- new$y + corners$y[i]
    out <- rbind(out, new)
  }
  out[order(out$group), ]
}



#' @export
StatNodeShape <- ggproto("StatNodeShape", Stat,
                         compute_panel = function(data, scales, ...) {
                           #browser()
                           do.call(paste0("custom_polygon_shape.", data$shape[1]),
                                   c(list(data = data, scales = scales), list(...)))
                         },
                         extra_params = c("na.rm", "shape"),
                         optional_aes = c("width", "height", "r", "fill", "shape"))


#' @export
GeomNodeShape <- ggproto("GeomNodeShape", Geom,
                         required_aes = c("x", "y"),
                         default_aes = aes(colour = "black",
                                           lwd = 1, lty = "solid",
                                           alpha = 1),
                         draw_panel = function(data, panel_params, coord, ...) {
                           #browser()
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
                            polygonGrob(munched$x, munched$y, default.units = "native",
                                          id = munched$group,
                                          gp = grid::gpar(col = first_rows$colour,
                                                    fill = scales::alpha(first_rows$fill, first_rows$alpha),
                                                    lwd = first_rows$size * .pt, lty = first_rows$linetype))
                         })


