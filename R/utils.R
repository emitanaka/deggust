
remove_nulls <- function(.x) {
  .x[!vapply(.x, is.null, logical(1))]
}

coord_theta <- function(n) {
  -pi/2 + pi^(0.8) * map_dbl(seq(n), function(i) sum(1/seq(i + 0.8)^(0.6)))
  #unlist(lapply(seq(1, n, 1), function(i) seq((i - 1) * 2 * pi, i * 2 * pi, length.out = ceiling(0.5 * i * i) + 1)[c(-ceiling(0.5 * i * i) - 1)]))[1:n]
}

coord_spiral <- function(n) {
  #theta <- seq(0, n / 5 * pi, length.out = n)
  theta <- coord_theta(n)
  r <- 0 + 0.2 * theta
  list(x = r * cos(theta),
       y = r * sin(theta))
}

coord_snake <- function(n, aspect_ratio, swap = TRUE) {
  h <- ceiling(sqrt(n * aspect_ratio))
  w <- ceiling(sqrt(n / aspect_ratio))
  M <- matrix(ncol = w, nrow = h)
  R <- row(M)
  # swap direction every odd row
  if(swap & ncol(R) > 1) {
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



warn_drop <- function(edibble, data) {
  n <- nrow(edibble)
  nd <- nrow(data)
  if(nd < n) {
    rlang::warn(paste0("Too manu units so ", n - nd, " units dropped from the plot. If you want to see all, use `nnode_max = Inf` or use `page = 2` to see the next set."))
  }
}

# maybe don't need this?
lvl_lump <- function(f, n, random = FALSE) {
  lvls <- levels(f)
  nl <- length(lvls)
  if(nl <= n + 1) {
    return(f)
  }
  index <- if(random) sample(1:nl, n) else 1:n
  lvls_keep <- lvls[index]
  other <- paste(nl - n, "others")
  f[!f %in% lvls_keep] <- other
  attr(f, "levels") <- c(lvls_keep, other)
  f
}

lvl_lump_keep <- function(lvls, n, random = FALSE) {
  nl <- length(lvls)
  if(nl <= n + 1) {
    return(lvls)
  }
  index <- if(random) sample(1:nl, n) else 1:n
  other <- paste(nl - n, "others")
  c(lvls[index], other)
}


