plot_four_units <- function(.edibble, flist, shapes, images, text, aspect_ratio, obsid, parentids) {
  des <- edbl_design(.edibble)
  vlevs <- fct_levels(des)
  parent_labels <- fct_names(des, parentids)
  parents <- names(sort(-lengths(vlevs[parent_labels])))

  plot <- ggplot(.edibble, aes(!!parse_expr(parents[1]), !!parse_expr(parents[2])))
  plot <- add_unit_fills(plot, flist, shapes, images)
  plot +
    scale_x_discrete(limits = levels(.edibble[[parents[1]]])) +
    scale_y_discrete(limits = levels(.edibble[[parents[2]]])) +
    facet_wrap(parse_expr(parents[3]))
}
