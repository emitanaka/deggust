plot_five_units <- function(.edibble, flist, shapes, images, text, aspect_ratio, obsid, parentids) {
  prep <- cook_design(.edibble)
  vlevs <- prep$fct_levels()
  parent_labels <- prep$fct_names(parentids)
  parents <- names(sort(-lengths(vlevs[parent_labels])))

  plot <- ggplot(.edibble, aes(!!parse_expr(parents[1]), !!parse_expr(parents[2])))
  plot <- add_unit_fills(plot, flist, shapes, images)
  plot +
    scale_x_discrete(limits = levels(.edibble[[parents[1]]])) +
    scale_y_discrete(limits = levels(.edibble[[parents[2]]])) +
    facet_grid(as.formula(paste(parents[[3]], "~", parents[[4]])))
}
