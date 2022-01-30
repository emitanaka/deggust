plot_four_units <- function(.edibble, flist, shapes, images, text, aspect_ratio, obsid, parentids) {
  des <- edbl_design(.edibble)
  vlevs <- fct_levels(des)
  parent_labels <- fct_names(des, parentids)
  parents <- names(sort(-lengths(vlevs[parent_labels])))
  unames <- unit_names(des)
  ou <- fct_names(des, obsid)
  block <- ifelse(length(parentids)==3, parents[3], setdiff(unames, c(parents, ou)))
  # FIXME the facet free_x doesn't work with edibble units
  # below is a temporary measure to make it work
  .edibble[[parents[1]]] <- as.character(.edibble[[parents[1]]])
  .edibble[[parents[2]]] <- as.character(.edibble[[parents[2]]])
  ####
  plot <- ggplot(.edibble, aes(!!parse_expr(parents[1]), !!parse_expr(parents[2])))
  plot <- add_unit_fills(plot, flist, shapes, images)
  plot <- plot +
    scale_x_discrete(limits = levels(.edibble[[parents[1]]])) +
    scale_y_discrete(limits = levels(.edibble[[parents[2]]]))
  if(length(parentids)==3) {
    plot + facet_wrap(parse_expr(block))
  } else {
    plot + facet_wrap(parse_expr(block), scales = "free")
  }
}
