
# x ggplot2 object
# ggtype -> geom, stat
# what -> "GeomCircle"
identify_layer <- function(x, ggtype, what) {
  nms <- vapply(x$layers, function(x) class(x[[ggtype]])[1], character(1))
  which(nms == what)
}


