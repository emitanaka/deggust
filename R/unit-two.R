plot_two_units <- function(.edibble, flist, shapes, images, text, aspect_ratio, obsid, parentids) {

  des <- edbl_design(.edibble)
  vlevs <- fct_levels(des)
  nlevels_units <- lengths(vlevs[flist$node])
  parent_label <- fct_names(des, parentids)
  obs_label <- fct_names(des, obsid)

  nodes <- .edibble
  SDF <- split(nodes[[obs_label]], nodes[[parent_label]])
  tt <- vapply(SDF, length, numeric(1))
  LDF <- unlist(lapply(SDF, function(x) paste0("U", 1:length(x))))
  nodes$xnames <- unname(LDF)

  nodes$x <- as.numeric(as.factor(nodes$xnames))
  nodes$y <- as.numeric(factor(nodes[[parent_label]],
                               levels = unique(nodes[[parent_label]])))
  block_data <- data.frame(x = (max(nodes$x) + 1)/2,
                           y =  (1:max(nodes$y)),
                           height = 1.6)

  plot <- ggplot(nodes, aes(x = x, y = y)) +
    geom_unit_node(data = block_data,
                   shape = "box", aes(width = max(nodes$x) * 2, height = height)) +
    geom_path(aes(group = !!parse_expr(parent_label))) +
    geom_unit_node(aes(fill = !!parse_expr(flist$fill[1])), shape = shapes[1]) +
    theme(axis.ticks.length = grid::unit(0, "npc"),
          panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank())

  add_text(plot, text, nodes[[obs_label]])
}

add_text <- function(plot, text, label) {
  if(isTRUE(text) || inherits(text, "element_text")) {
    text_aes <- list()
    if(inherits(text, "element_text")) {
      text_aes <- remove_nulls(as.list(text))
      names(text_aes) <- gsub("face", "fontface", names(text_aes))
      text_aes["inherit.blank"] <- NULL
    }
    plot <- plot +
      do.call("geom_text", c(list(mapping = aes(label = label)),
                             text_aes))
  }
  plot
}
