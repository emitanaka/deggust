add_unit_fills <- function(plot, flist, shapes, images) {
  nfill <- length(flist$fill)
  plot <- add_unit(plot, flist$fill[1], shapes[1], images[1], height = 1, width = 1, size = 0.1) +
    colorspace::scale_fill_discrete_qualitative(palette = "Dark 3")
  if(nfill > 1) {
    plot <- another_fill_scale(plot, 2, flist$fill[2], shape = shapes[2], image = images[2],
                               height = ifelse(nfill > 2, 0.66, 0.5),
                               width = ifelse(nfill > 2, 0.66, 0.5),
                               size = ifelse(nfill > 2, 0.066, 0.05)) +
      colorspace::scale_fill_discrete_qualitative(palette = "Pastel 1")
    if(nfill > 2) {
      plot <- another_fill_scale(plot, 3, flist$fill[3], shape = shapes[3], image = images[3],
                                 height = 0.33, width = 0.33, size = 0.033) +
        colorspace::scale_fill_discrete_qualitative(palette = "Harmonic")
    }
  }
  plot
}

plot_three_units <- function(.edibble, flist, shapes, images, text, aspect_ratio, obsid, parentids) {
  des <- edbl_design(.edibble)
  vlevs <- fct_levels(des)
  parent_labels <- fct_names(des, parentids)
  parent1 <- parent_labels[which.max(lengths(vlevs[parent_labels]))]
  parent2 <- setdiff(parent_labels, parent1)

  plot <- ggplot(.edibble, aes(!!parse_expr(parent1), !!parse_expr(parent2)))
  plot <- add_unit_fills(plot, flist, shapes, images)
  plot
}
