


#' Auto plot or `ggplot2` of an edibble design
#'
#'
#' @param .edibble An edibble desgin, an edibble table or an edibble graph.
#' @param ... Unused at the moment.
#' @param title The title of the plot. By default it is the
#'  name of the edibble design.
#' @return A `ggplot` object.
#' @export
autoplot.edbl_table <- function(.edibble, aspect_ratio = 1,
                                shape = "circle", text = FALSE, image = NULL,
                                trts = NULL, scales = NULL) {

  ind_units <- unlist(lapply(.edibble, is_edibble_unit))
  unit_names <- names(ind_units)[ind_units]
  nunits <- sum(ind_units)

  ind_trts <- unlist(lapply(.edibble, is_edibble_trt))
  trt_names <- trts %||% names(ind_trts)[ind_trts]
  ntrts <- length(trt_names)

  scales <- scales %||% lapply(1:ntrts, function(x) ggplot2::scale_fill_discrete())

  nlevels_unit <- nrow(.edibble)

  des <- edbl_design(.edibble)
  obsid <- fct_obs_unit(des)
  parentids <- intersect(fct_parent(des, obsid), unit_ids(des))

  if(nunits==1) {
    plot <- plot_single_unit(.edibble, nlevels_unit, ntrts, unit_names, trt_names, shape, image, text, aspect_ratio)
  } else if(nunits==2 & ntrts==1) {
    plot <- plot_two_units(.edibble, nlevels_unit, ntrts, unit_names, trt_names, shape, image, text, aspect_ratio)
  } else if(nunits==3 & length(parentids)==2 & ntrts==1) {
    plot <- plot_three_units(.edibble, nlevels_unit, ntrts, unit_names, trt_names, shape, image, text, aspect_ratio,
                             obsid, parentids)
  } else {
    abort("`autoplot` is not yet supported for this design.")
  }
  plot
}

plot_three_units <- function(.edibble, nlevels_unit, ntrts, unit_names, trt_names, shape, image, text, aspect_ratio,
                           obsid, parentids) {
  des <- edbl_design(.edibble)
  vlevs <- fct_levels(des)
  parent_labels <- fct_label(des, parentids)
  parent1 <- parent_labels[which.max(lengths(vlevs[parent_labels]))]
  parent2 <- setdiff(parent_labels, parent1)

  ggplot(.edibble, aes(!!parse_expr(parent1), !!parse_expr(parent2), fill = !!parse_expr(trt_names))) +
    geom_tile(color = "black", size = 2) +
    theme(axis.ticks.length = grid::unit(0, "npc"),
          panel.grid = element_blank())


}

plot_two_units <- function(.edibble, nlevels_unit, ntrts, unit_names, trt_names, shape, image, text, aspect_ratio) {
  # make it two-dimensional
  ind_units <- unlist(lapply(.edibble, is_edibble_unit))
  unames <- names(ind_units)[ind_units]
  nlevels_units <- unlist(lapply(.edibble[unames], nlevels))
  unit_name <- unames[which.max(nlevels_units)]
  parent_name <- setdiff(unames, unit_name)
  unit_vec <- .edibble[[unit_name]]

  nodes <- .edibble[c(unit_names, setdiff(names(.edibble), unit_names))]
  nodes <- as_data_frame(nodes)

  tt <- vapply(split(nodes, nodes[unames[1]]), nrow, numeric(1))
  SDF <- split(.edibble[[unames[2]]], .edibble[[unames[1]]])
  LDF <- unlist(lapply(SDF, function(x) paste0("U", 1:length(x))))
  nodes$xnames <- unname(LDF)

  #if(max(tt) <= w) {
  nodes$x <- as.numeric(as.factor(nodes$xnames))
  nodes$y <- as.numeric(factor(nodes[[parent_name]],
                               levels = unique(nodes[[parent_name]])))
  block_data <- data.frame(x = (max(nodes$x) + 1)/2,
                           y =  (1:max(nodes$y)),
                           height = 1.6)
  #} else {

  #}

  plot <- ggplot(nodes, aes(x = x, y = y)) +
    geom_unit_node(data = block_data,
                   shape = "box", aes(width = max(nodes$x) * 2, height = height)) +
    geom_path(aes(group = !!parse_expr(parent_name))) +
    geom_unit_node(aes(fill = !!parse_expr(trt_names)), shape = shape) +
    theme(axis.ticks.length = grid::unit(0, "npc"),
          panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank())
  if(isTRUE(text) || inherits(text, "element_text")) {
    text_aes <- list()
    if(inherits(text, "element_text")) {
      text_aes <- remove_nulls(as.list(text))
      names(text_aes) <- gsub("face", "fontface", names(text_aes))
    }
    plot <- plot +
      do.call("geom_text", c(list(mapping = aes(label = unit_vec)),
                             text_aes))
  }
  plot
}

plot_single_unit <- function(.edibble, nlevels_unit, ntrts, unit_names, trt_names, shape, image, text, aspect_ratio) {
  # make it snake-like
  unit_vec <- .edibble[[unit_names]]
  nodes <- .edibble[c(unit_names, setdiff(names(.edibble), unit_names))]
  nodes <- cbind(nodes, coord_snake(nlevels_unit, aspect_ratio))
  plot <- ggplot(nodes, aes(x, y)) +
    geom_path()
  if(is.null(image)) {
    plot <- plot + geom_unit_node(aes(fill = !!parse_expr(trt_names[1])), shape = shape)
  } else {
    if(!require("ggimage")) {
      stop("Please install `ggimage` package to use the image argument.")
    }
    plot <- plot + ggimage::geom_image(aes(color = !!parse_expr(trt_names[1])), image = image, size = 0.1)
  }
  if(ntrts > 1) {
    plot <- another_fill_scale(plot, 2, scales, trt_names, shape = shape, image = image,
                               height = 0.6, width = 0.6, size = 0.08)
    if(ntrts > 2) {
      plot <- another_fill_scale(plot, 3, scales, trt_names, shape = shape, image = image,
                                 height = 0.4, width = 0.4, size = 0.05)
    }
  }

  plot <- plot +
    theme(axis.ticks.length = grid::unit(0, "npc"),
          panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank())
  if(isTRUE(text) || inherits(text, "element_text")) {
    text_aes <- list()
    if(inherits(text, "element_text")) {
      text_aes <- remove_nulls(as.list(text))
      names(text_aes) <- gsub("face", "fontface", names(text_aes))
    }
    plot <- plot +
      do.call("geom_text", c(list(mapping = aes(label = unit_vec)),
                             text_aes))
  }
  plot
}

is_edibble_unit <- function(x) {
  inherits(x, "edbl_unit")
}

is_edibble_trt <- function(x) {
  inherits(x, "edbl_trt")
}


#' @importFrom edibble is_edibble_table
autoplot_test <- function(.table, ...) {
  if(!is_edibble_table(.table)) {
    abort("The supplied object is not an edibble table.")
  }


}
