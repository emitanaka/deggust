
# x ggplot2 object
# ggtype -> geom, stat
# what -> "GeomCircle"
identify_layer <- function(x, ggtype, what) {
  idx <- vapply(x$layers, function(zz) any(class(zz[[ggtype]])==what), logical(1))
  which(idx)
}


addGeomClass <- function(plot, ilayer, class) {
  cl <- class(plot$layers[[ilayer]]$geom)
  if(!class %in% cl) {
    class(plot$layers[[ilayer]]$geom) <- c(cl, class)
  }
  plot
}


