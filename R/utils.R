
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

add_unit_fills <- function(plot, flist, shapes, images) {
  nfill <- length(flist$fill)
  plot <- add_unit(plot, flist$fill[1], shapes[1], images[1], height = 1, width = 1, size = 0.1)
  if(nfill==1) plot <- plot + scale_fill_viridis_d(option = "A")
  if(nfill > 1) {
    plot <- plot + scale_fill_viridis_d(option = "A")
    plot <- another_fill_scale(plot, 2, flist$fill[2], shape = shapes[2], image = images[2],
                               height = ifelse(nfill > 2, 0.66, 0.5),
                               width = ifelse(nfill > 2, 0.66, 0.5),
                               size = ifelse(nfill > 2, 0.066, 0.05)) +
      scale_fill_viridis_d(option = "D")
    if(nfill > 2) {
      plot <- another_fill_scale(plot, 3, flist$fill[3], shape = shapes[3], image = images[3],
                                 height = 0.33, width = 0.33, size = 0.033) +
        scale_fill_viridis_d(option = "E")
    }
  }
  plot
}
