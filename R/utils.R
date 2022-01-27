
remove_nulls <- function(.x) {
  .x[!vapply(.x, is.null, logical(1))]
}

coord_snake <- function(n, aspect_ratio, swap = TRUE) {
  w <- ceiling(sqrt(n * aspect_ratio))
  h <- ceiling(sqrt(n / aspect_ratio))
  M <- matrix(ncol = w, nrow = h)
  R <- row(M)
  # swap direction every odd row
  if(swap) {
    R[, c(FALSE, TRUE)] <- R[nrow(R):1, c(FALSE, TRUE)]
  }
  list(x = as.vector(R)[1:n],
       y = as.vector(col(M))[1:n])
}


# scale_reorder <- function(aes, order) {
#   structure(list(aes = ggplot2::standardise_aes_names(aes),
#                  order = order),
#                  class = "scale_reorder")
# }
#
# ggplot_add.scale_reorder <- function(object, plot, object_name) {
#   # To add default scales (I need to build the whole plot because they might be computed aesthetics)
#   plot$scales$get_scales <- function(self, output) {
#     scale <- self$scales[self$find(output)]
#     if (length(scale) == 0)
#       return()
#     scale[[1]]
#   }
#   plot
# }

another_fill_scale <- function(plot, i, scales, trt_names, shape = NULL, image = NULL, height = NA, width = NA, size = NA) {
  plot <- plot +
    scales +
    ggnewscale::new_scale_fill()
  if(is.null(image)) {
    plot <- plot + geom_unit_node(aes(fill = !!parse_expr(trt_names[i])), shape = shape, height = height, width = width)
  } else {
    if(!require("ggimage")) {
      stop("Please install `ggimage` package to use the image argument.")
    }
    plot <- plot + ggimage::geom_image(aes(color = !!parse_expr(trt_names[i])), image = image, size = size)
  }
  plot
}
