add_unit <- function(plot, fill, shape, image, height, width, size) {
  if(is.na(image)) {
    plot <- plot + geom_unit_node(aes(fill = !!parse_expr(fill)), shape = shape, height = height, width = width)
  } else {
    if(!require("ggimage")) {
      stop("Please install `ggimage` package to use the image argument.")
    }
    plot <- plot + ggimage::geom_image(aes(color = !!parse_expr(fill)), image = image, size = size)
  }
  plot
}

plot_single_unit <- function(.edibble, flist, shapes, images, text, aspect_ratio) {
  ufct <- .edibble[[flist$node[1]]]
  nfill <- length(flist$fill)
  N <- nrow(.edibble)
  nodes <- cbind(.edibble, coord_snake(N, aspect_ratio))
  plot <- ggplot(nodes, aes(x, y)) + geom_path()
  plot <- add_unit_fills(plot, flist, shapes, images)
  add_text(plot, text, .edibble[[flist$node[1]]])
}
