
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

another_fill_scale <- function(plot, i, flist, shape = NULL, image = NULL, height = NA, width = NA, size = NA) {
  plot <- plot + ggnewscale::new_scale_fill()
  plot <- add_unit(plot, flist, shape, image, height = height, width = width, size = size)
  plot
}
