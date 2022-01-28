

plot_three_units <- function(.edibble, flist, shapes, images, text, aspect_ratio, obsid, parentids) {
  des <- edbl_design(.edibble)
  vlevs <- fct_levels(des)
  parent_labels <- fct_names(des, parentids)
  parent1 <- parent_labels[which.max(lengths(vlevs[parent_labels]))]
  parent2 <- setdiff(parent_labels, parent1)

  plot <- ggplot(.edibble, aes(!!parse_expr(parent1), !!parse_expr(parent2)))
  plot <- add_unit_fills(plot, flist, shapes, images)
  plot +
    scale_x_discrete(limits = levels(.edibble[[parent1]])) +
    scale_y_discrete(limits = levels(.edibble[[parent2]]))
}
