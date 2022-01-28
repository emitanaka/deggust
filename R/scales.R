#' Colour scales for multiple scales of the same aesthetic
#'
#' This function has the same argument as the corresponding `ggplot2` scale
#' functions where the index after the aesthetic name is omitted from the function.
#' E.g. `scale_fill1_binned` has the same argument as `scale_filled_binned`.
#' Where there is multiple `fill` scales, the index after the aesthetic name
#' determines which `fill` scale is modified.
#'
#' @param ... The arguments for the corresponding scale function.
#' @name scale_fills
#' @seealso scale_aes_select
NULL

#' @rdname scale_fills
#' @export
scale_fill1_binned <- function(...) {
  scale_aes_select(1, ggplot2::scale_fill_binned, ...)
}
#' @rdname scale_fills
#' @export
scale_fill2_binned <- function(...) {
  scale_aes_select(2, ggplot2::scale_fill_binned, ...)
}
#' @rdname scale_fills
#' @export
scale_fill3_binned <- function(...) {
  scale_aes_select(3, ggplot2::scale_fill_binned, ...)
}
#' @rdname scale_fills
#' @export
scale_fill1_brewer <- function(...) {
  scale_aes_select(1, ggplot2::scale_fill_brewer, ...)
}
#' @rdname scale_fills
#' @export
scale_fill2_brewer <- function(...) {
  scale_aes_select(2, ggplot2::scale_fill_brewer, ...)
}
#' @rdname scale_fills
#' @export
scale_fill3_brewer <- function(...) {
  scale_aes_select(3, ggplot2::scale_fill_brewer, ...)
}
#' @rdname scale_fills
#' @export
scale_fill1_continuous <- function(...) {
  scale_aes_select(1, ggplot2::scale_fill_continuous, ...)
}
#' @rdname scale_fills
#' @export
scale_fill2_continuous <- function(...) {
  scale_aes_select(2, ggplot2::scale_fill_continuous, ...)
}
#' @rdname scale_fills
#' @export
scale_fill3_continuous <- function(...) {
  scale_aes_select(3, ggplot2::scale_fill_continuous, ...)
}
#' @rdname scale_fills
#' @export
scale_fill1_date <- function(...) {
  scale_aes_select(1, ggplot2::scale_fill_date, ...)
}
#' @rdname scale_fills
#' @export
scale_fill2_date <- function(...) {
  scale_aes_select(2, ggplot2::scale_fill_date, ...)
}
#' @rdname scale_fills
#' @export
scale_fill3_date <- function(...) {
  scale_aes_select(3, ggplot2::scale_fill_date, ...)
}
#' @rdname scale_fills
#' @export
scale_fill1_datetime <- function(...) {
  scale_aes_select(1, ggplot2::scale_fill_datetime, ...)
}
#' @rdname scale_fills
#' @export
scale_fill2_datetime <- function(...) {
  scale_aes_select(2, ggplot2::scale_fill_datetime, ...)
}
#' @rdname scale_fills
#' @export
scale_fill3_datetime <- function(...) {
  scale_aes_select(3, ggplot2::scale_fill_datetime, ...)
}
#' @rdname scale_fills
#' @export
scale_fill1_discrete <- function(...) {
  scale_aes_select(1, ggplot2::scale_fill_discrete, ...)
}
#' @rdname scale_fills
#' @export
scale_fill2_discrete <- function(...) {
  scale_aes_select(2, ggplot2::scale_fill_discrete, ...)
}
#' @rdname scale_fills
#' @export
scale_fill3_discrete <- function(...) {
  scale_aes_select(3, ggplot2::scale_fill_discrete, ...)
}
#' @rdname scale_fills
#' @export
scale_fill1_manual <- function(...) {
  scale_aes_select(1, ggplot2::scale_fill_manual, ...)
}
#' @rdname scale_fills
#' @export
scale_fill2_manual <- function(...) {
  scale_aes_select(2, ggplot2::scale_fill_manual, ...)
}
#' @rdname scale_fills
#' @export
scale_fill3_manual <- function(...) {
  scale_aes_select(3, ggplot2::scale_fill_manual, ...)
}

#' Change selected scale where multiple scales of same aesthetic exist
#'
#' @param i An integer denoting the scale number.
#' @param f The scale function or the scale object.
#' @param ... The arguments to the scale function `f`.
#'
#' @examples
#' scale_aes_select(1, ggplot2::scale_fill_manual, values = c("red", "white", "blue"))
#' scale_aes_select(1, ggplot2::scale_fill_manual(values = c("red", "white", "blue")))
#'
#' @seealso scale_fills
#' @export
scale_aes_select <- function(i, f, ...) {
  if(is.function(f)) {
    s <- f(...)
  } else {
    s <- f
  }
  aes <- s$aesthetics
  class(s) <- c("ScaleNew", class(s))
  # ggnewscale renames old scale so aesthetic is appended by _new.
  # This means that you can't select the scale number by _new
  attr(s, ".select") <- i
  attr(s, ".aes") <- aes
  s
}

#' @export
ggplot_add.ScaleNew <- function(object, plot, object_name) {
  scales <- vapply(plot$scales$scales, function(x) x$aesthetics, character(1))
  select <- attr(object, ".select")
  aes <- attr(object, ".aes")
  scales_pos <- grep(paste0(aes, "_new"), scales)
  nscales <- length(scales_pos) + 1L
  if(select == nscales) {
    object$aesthetics <- aes
    plot$scales$scales[length(scales) + 1L] <- list(object)
  } else {
    object$aesthetics <- paste0(aes, paste0(rep("_new", nscales - select), collapse = ""))
    plot$scales$scales[scales_pos[select]] <- list(object)
  }
  plot
}



#' @importFrom vctrs vec_math
#' @export
vec_math.edbl_trt <- function(.fn, .x, ...) do.call(.fn, list(factor(.x, levels(.x)), ...))

#' @export
vec_math.edbl_unit <- function(.fn, .x, ...) do.call(.fn, list(factor(.x, levels(.x)), ...))



# maybe I should add "character" to edbl_trt/edbl_unit

#' @importFrom ggplot2 scale_type
#' @export
scale_type.edbl_trt <- function(x) "discrete"

#' @export
scale_type.edbl_unit <- function(x) "discrete"
