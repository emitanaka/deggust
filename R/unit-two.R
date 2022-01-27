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

