

plot_three_units <- function(.edibble, flist, shapes, images, text, aspect_ratio, obsid, parentids) {
  des <- edbl_design(.edibble)
  vlevs <- fct_levels(des)
  if(length(parentids)==2) {
    parent_labels <- fct_names(des, parentids)
    parent1 <- parent_labels[which.max(lengths(vlevs[parent_labels]))]
    parent2 <- setdiff(parent_labels, parent1)

    plot <- ggplot(.edibble, aes(!!parse_expr(parent1), !!parse_expr(parent2)))
    plot <- add_unit_fills(plot, flist, shapes, images)
    plot +
      scale_x_discrete(limits = levels(.edibble[[parent1]])) +
      scale_y_discrete(limits = levels(.edibble[[parent2]]))
  } else if(length(parentids)==1) {
    aids <- intersect(fct_ancestor(des, obsid), setdiff(unit_ids(des), obsid))
    ancestors <- fct_names(des, aids)
    nodes <- split(.edibble, paste0(.edibble[[ancestors[1]]], .edibble[[ancestors[2]]]))
    nodes <- lapply(nodes, function(df) cbind(df, coord_snake(nrow(df), aspect_ratio)))
    nodes <- do.call(rbind, nodes)

    nfill <- length(flist$fill)

    plot <- ggplot(nodes, aes(x = x, y = y)) +
      geom_path() +
      facet_wrap(as.formula(paste("~", ancestors[1], "+", ancestors[2])))

    plot <- add_unit_fills(plot, flist, shapes, images)

    add_text(plot, text, nodes[[obs_label]])
  }
}
