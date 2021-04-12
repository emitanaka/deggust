#' @export
draw_key_shape <- function(data, params, size) {

  #browser()
  if (is.null(data$size)) {
    data$size <- 1
  }

  lwd <- min(data$size, min(size) / 4)

  shape <- params$shape[1]
  n <- n_shape(shape)
  phase <- phase_shape(shape)
  df <- regular_polygon_data(data.frame(x = 0.5, y = 0.5,
                                        width = data$size/2, height = data$size/2),
                             n = n, phase = phase)
  if (!is.integer(df$group)) {
    df$group <- match(df$group, unique(df$group))
  }


  grid::polygonGrob(
    x = df$x,
    y = df$y,
    id = df$group,
    default.units = "native",
    gp = grid::gpar(
      col = data$colour %||% NA,
      fill = scales::alpha(data$fill %||% "grey20", data$alpha),
      #fontsize = (data$size * .pt + (data$lwd %||% 0.5)) / 5,
      lty = data$linetype %||% 1,
      lwd = data$lwd %||% 0.5
      #linejoin = params$linejoin %||% "mitre"
    ))
}
