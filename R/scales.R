


#' @importFrom vctrs vec_math
#' @export
vec_math.edbl_trt <- function(.fn, .x, ...) do.call(.fn, list(as.character(.x), ...))

#' @export
vec_math.edbl_unit <- function(.fn, .x, ...) do.call(.fn, list(as.character(.x), ...))

# maybe I should add "character" to edbl_trt/edbl_unit

#' @importFrom ggplot2 scale_type
#' @export
scale_type.edbl_trt <- function(x) "discrete"

#' @export
scale_type.edbl_unit <- function(x) "discrete"
