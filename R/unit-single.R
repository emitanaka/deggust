

plot_single_unit <- function(.edibble, title, nlevels_unit, ntrts, unit_names, trt_names, shape, image, text, aspect_ratio, scales) {
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
  plot + ggtitle(title)
}

