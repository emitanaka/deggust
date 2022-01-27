add_unit <- function(plot, fill, shape, image, height, width, size) {
  if(is.na(image)) {
    plot <- plot + geom_unit_node(aes(fill = !!parse_expr(fill)), shape = shape, height = height, width = width)
  } else {
    if(!require("ggimage")) {
      stop("Please install `ggimage` package to use the image argument.")
    }
    plot <- plot + ggimage::geom_image(aes(color = !!parse_expr(fill)), image = image, size = size)
  }
  plot
}

plot_single_unit <- function(.edibble, flist, shapes, images, text, aspect_ratio) {
  ufct <- .edibble[[flist$node[1]]]
  nfill <- length(flist$fill)
  N <- nrow(.edibble)
  nodes <- cbind(.edibble, coord_snake(N, aspect_ratio))
  plot <- ggplot(nodes, aes(x, y)) + geom_path()
  plot <- plot + scale_fill_brewer(palette = "Set1")
  plot <- add_unit(plot, flist$fill[1], shapes[1], images[1], height = 1, width = 1, size = 0.1)
  if(nfill > 1) {
    plot <- another_fill_scale(plot, 2, flist$fill[2], shape = shapes[2], image = images[2],
                               height = ifelse(nfill > 2, 0.66, 0.5),
                               width = ifelse(nfill > 2, 0.66, 0.5),
                               size = ifelse(nfill > 2, 0.066, 0.05)) +
      scale_fill_brewer(palette = "Set2")
    if(nfill > 2) {
      plot <- another_fill_scale(plot, 3, flist$fill[3], shape = shapes[3], image = images[3],
                                 height = 0.33, width = 0.33, size = 0.033) +
        scale_fill_brewer(palette = "Set3")
    }
  }

  add_text(plot, text, .edibble[[flist$node[1]]])
}

plot_single_unit2 <- function(.edibble, nlevels_unit, ntrts, unit_names, trt_names, shape, image, text, aspect_ratio) {
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
    plot <- another_fill_scale(plot, 2, scale_fill_brewer(palette = "Set1"), trt_names, shape = shape, image = image,
                               height = 0.6, width = 0.6, size = 0.08)
    if(ntrts > 2) {
      plot <- another_fill_scale(plot, 3, scale_fill_brewer(palette = "Set2"), trt_names, shape = shape, image = image,
                                 height = 0.4, width = 0.4, size = 0.05) +
        scale_fill_brewer(palette = "Set3")
    }
  }

  plot <- plot
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

