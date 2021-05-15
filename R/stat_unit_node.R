#' @export
StatUnitNode <- ggproto('StatUnitNode', Stat,
                        setup_data = function(data, params) {
                          data$margin_t <- data$margin_t %||% params$margin_t %||% data$margin %||% params$margin %||% 0.01
                          data$margin_b <- data$margin_b %||% params$margin_b %||% data$margin %||% params$margin %||% 0.01
                          data$margin_l <- data$margin_l %||% params$margin_l %||% data$margin %||% params$margin %||% 0.01
                          data$margin_r <- data$margin_r %||% params$margin_r %||% data$margin %||% params$margin %||% 0.01
                          # current setup assumes margin is between 0 and 1. Should set up things according to units.
                          data$width <- data$width %||% params$width %||% (1 - data$margin_l - data$margin_r)
                          data$height <- data$height %||% params$height %||% (1 - data$margin_t - data$margin_b)
                          if(!is.null(data$x)) data$width <- resolution(data$x, FALSE) * data$width
                          if(!is.null(data$y)) data$height <- resolution(data$y, FALSE) * data$height

                          data
                        })
