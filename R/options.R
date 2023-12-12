op.deggust <- list(
  deggust.nnode_max = 1000,
  deggust.nfill_max = 8,
  deggust.discrete.fill = list(
    # Paul Tol's high contrast
    c("#DDAA33", "#BB5566", "#004488", "#e3e3e3", '#FFFFFF'), # 5
    # okabe ito + white
    c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000", '#FFFFFF'), # 9
    # Paul Tol's muted + black
    c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677",
      "#882255", "#AA4499", "#000000", "#DDDDDD", '#FFFFFF'), # 12,
    # rcaratcolor safe
    c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499",
      "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888", '#FFFFFF') # 15
  )
)

#' Controls for the deggust autoplot
#'
#' @param page A positive integer referencing the page number of the plot. This is only
#'  relevant if you have more than `nnode_max` units to display.
#' @param nnode_max The number of maximum nodes to display.
#' @param nfill_max A single integer or vector of integers to indicate the maximum number of fill levels to
#'   show in the output. If it is a vector then the order determines the maximum number of fill levels to
#'   show for the corresponding fill factor. If the number of fill levels are larger than the maximum,
#'   the fill levels will be collapsed into one.
#' @param random_fills,random_units A logical value to indicate whether the factor levels chosen to display, if
#'   the factor levels are above `nfill_max` or `nnode_max`, should be based on a random or systematic order. The
#'   default is FALSE.
#' @export
deggust_control <- function(page = 1,
                            nnode_max = deggust_opt("nnode_max"),
                            nfill_max = deggust_opt("nfill_max"),
                            random_fills = FALSE,
                            random_units = FALSE,
                            node_connection = c("snake", "spiral")) {

  list(page = page,
       nnode_max = nnode_max,
       nfill_max = nfill_max,
       random_fills = random_fills,
       random_units = random_units,
       node_connection = match.arg(node_connection))
}



deggust_opt <- function(x, prefix = "deggust.") {
  if(missing(x)) {
    op.deggust
  } else {
    # deggust preference takes precedence
    deggust_opt_name <- paste0(prefix, x)
    deggust_res <- getOption(deggust_opt_name)
    if(!is_null(deggust_res)) return(deggust_res)
    # if deggust preference is not set then check edibble
    edibble_opt_name <- paste0("edibble.", x)
    edibble_res <- getOption(edibble_opt_name)
    if(!is_null(edibble_res)) return(edibble_res)
    # otherwise use default in deggust
    op.deggust[[deggust_opt_name]]
  }
}

