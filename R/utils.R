
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

another_fill_scale <- function(plot, i, flist, shape = NULL, image = NULL, height = NA, width = NA, size = NA) {
  plot <- plot + ggnewscale::new_scale_fill()
  plot <- add_unit(plot, flist, shape, image, height = height, width = width, size = size)
  plot
}
