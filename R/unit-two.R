plot_two_units <- function(.edibble, flist, shapes, images, text, aspect_ratio, obsid, parentids) {

  des <- edbl_design(.edibble)
  vlevs <- fct_levels(des)
  nlevels_units <- lengths(vlevs[flist$node])
  parent_label <- fct_names(des, parentids)
  obs_label <- fct_names(des, obsid)

  nodes <- split(.edibble, .edibble[[parent_label]])
  nodes <- lapply(nodes, function(df) cbind(df, coord_snake(nrow(df), aspect_ratio)))
  nodes <- do.call(rbind, nodes)

  nfill <- length(flist$fill)

  plot <- ggplot(nodes, aes(x = x, y = y)) +
    geom_path() +
    facet_wrap(parse_expr(parent_label))

  plot <- add_unit_fills(plot, flist, shapes, images)

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
