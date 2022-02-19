plot_four_units <- function(.edibble, flist, shapes, images, text, aspect_ratio, obsid, parentids) {
  prep <- cook_design(.edibble)
  vlevs <- prep$fct_levels()
  parents <- rev(prep$fct_names(parentids))
  #parents <- names(sort(-lengths(vlevs[parent_labels])))
  unames <- prep$unit_names
  ou <- prep$fct_names(obsid)
  block <- ifelse(length(parentids)==3, parents[3], setdiff(unames, c(parents, ou)))
  # FIXME the facet free_x doesn't work with edibble units
  # below is a temporary measure to make it work
  .edibble[[parents[1]]] <- factor(as.character(.edibble[[parents[1]]]),
                                   levels = levels(.edibble[[parents[1]]]))
  .edibble[[parents[2]]] <- factor(as.character(.edibble[[parents[2]]]),
                                   levels = levels(.edibble[[parents[2]]]))
  ####
  plot <- ggplot(.edibble, aes(!!parse_expr(parents[1]), !!parse_expr(parents[2])))
  plot <- add_unit_fills(plot, flist, shapes, images)
  plot <- plot
  if(length(parentids)==3) {
    plot + facet_wrap(parse_expr(block))
  } else {
    plot + facet_wrap(parse_expr(block), scales = "free")
  }
}
