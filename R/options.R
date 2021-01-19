op.deggust <- list(
  deggust.vertex.label.color.unit = "#E69F00",
  deggust.vertex.label.color.trt = "#56B4E9",
  deggust.vertex.label.color.rcrd = "#009E73",
  deggust.edge.color.v2l = "gray",
  deggust.edge.color.l2lseq = "gray",
  deggust.edge.color.v2v = "black",
  deggust.edge.color.r2v = "black", # record to var
  deggust.edge.color.l2l = "black",
  deggust.edge.color.t2v = "#56B4E9", # blue
  deggust.edge.color.t2vmay = "#56B4E9",
  deggust.edge.lty.v2l = 3, # vertex to level
  deggust.edge.lty.l2lseq = 3, # level to level sequence
  deggust.edge.lty.r2v = 2, # vertex to vertex nesting
  deggust.edge.lty.v2v = 1, # vertex to vertex nesting
  deggust.edge.lty.l2l = 1, # level to level nesting
  deggust.edge.lty.t2v = 1, # treatment to variable
  deggust.edge.lty.t2vmay = 2 # treatment level to variable level potential
)



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

edge_ltype_dict <- c("v2l" = deggust_opt("edge.lty.v2l"),
                     "l2lseq" = deggust_opt("edge.lty.l2lseq"),
                     "r2v" = deggust_opt("edge.lty.r2v"),
                     "v2v" = deggust_opt("edge.lty.v2v"),
                     "l2l" = deggust_opt("edge.lty.l2l"),
                     "t2v" = deggust_opt("edge.lty.t2v"),
                     "t2vmay" = deggust_opt("edge.lty.t2vmay"))
