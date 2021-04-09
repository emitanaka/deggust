
# x ggplot2 object
# ggtype -> geom, stat
# what -> "GeomCircle"
identify_layer <- function(x, ggtype, what) {
  idx <- vapply(x$layers, function(zz) any(class(zz[[ggtype]])==what), logical(1))
  which(idx)
}


addGeomUnitClass <- function(plot, ilayer) {
  cl <- class(plot$layers[[ilayer]]$geom)
  if(!"GeomUnit" %in% cl) {
    class(plot$layers[[ilayer]]$geom) <- c(cl, "GeomUnit")
  }
  plot
}


