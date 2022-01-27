plot_three_units <- function(.edibble, nlevels_unit, ntrts, unit_names, trt_names, shape, image, text, aspect_ratio,
                             obsid, parentids) {
  des <- edbl_design(.edibble)
  vlevs <- fct_levels(des)
  parent_labels <- fct_names(des, parentids)
  parent1 <- parent_labels[which.max(lengths(vlevs[parent_labels]))]
  parent2 <- setdiff(parent_labels, parent1)

  ggplot(.edibble, aes(!!parse_expr(parent1), !!parse_expr(parent2), fill = !!parse_expr(trt_names))) +
    geom_tile(color = "black", size = 2)


}
