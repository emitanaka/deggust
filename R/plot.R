

plot1 <- function(.edibble, flist, flvls, shapes, images, text, aspect_ratio, control) {
  ufct <- .edibble[[flist$node[1]]]
  nfill <- length(flist$fill)
  N <- nrow(.edibble)
  nodes <- cbind(.edibble, coord_snake(N, aspect_ratio))
  plot <- ggplot(nodes, aes(x, y)) + geom_path()
  plot <- add_unit_fills(plot, flist, flvls, shapes, images, control)
  add_text(plot, text, .edibble[[flist$node[1]]])
}

plot2 <- function(.edibble, flist, flvls, shapes, images, text, aspect_ratio, control, obsid, parentids) {

  prep <- cook_design(.edibble)
  vlevs <- prep$fct_levels()
  nlevels_units <- lengths(vlevs[flist$node])
  parent_label <- prep$fct_names(parentids)
  obs_label <- prep$fct_names(obsid)

  nodes <- split(.edibble, .edibble[[parent_label]])
  nodes <- lapply(nodes, function(df) cbind(df, coord_snake(nrow(df), aspect_ratio)))
  nodes <- do.call(rbind, nodes)

  nfill <- length(flist$fill)

  plot <- ggplot(nodes, aes(x = x, y = y)) +
    geom_path() +
    facet_wrap(parse_expr(parent_label))

  plot <- add_unit_fills(plot, flist, flvls, shapes, images, control)

  add_text(plot, text, nodes[[obs_label]])
}


plot3 <- function(.edibble, flist, flvls, shapes, images, text, aspect_ratio, control, obsid, parentids) {
  prep <- cook_design(.edibble)
  vlevs <- prep$fct_levels()
  if(length(parentids)==2) {
    parent_labels <- prep$fct_names(parentids)
    parent1 <- parent_labels[which.max(lengths(vlevs[parent_labels]))]
    parent2 <- setdiff(parent_labels, parent1)

    plot <- ggplot(.edibble, aes(!!parse_expr(parent1), !!parse_expr(parent2)))
    plot <- add_unit_fills(plot, flist, flvls, shapes, images, control)
    plot +
      scale_x_discrete(limits = levels(.edibble[[parent1]])) +
      scale_y_discrete(limits = levels(.edibble[[parent2]]))
  } else if(length(parentids)==1) {
    aids <- intersect(prep$fct_ancestor(obsid), setdiff(prep$unit_ids, obsid))
    ancestors <- prep$fct_names(aids)
    nodes <- split(.edibble, paste0(.edibble[[ancestors[1]]], .edibble[[ancestors[2]]]))
    nodes <- lapply(nodes, function(df) cbind(df, coord_snake(nrow(df), aspect_ratio)))
    nodes <- do.call(rbind, nodes)

    nfill <- length(flist$fill)

    plot <- ggplot(nodes, aes(x = x, y = y)) +
      geom_path() +
      facet_wrap(as.formula(paste("~", ancestors[1], "+", ancestors[2])))

    plot <- add_unit_fills(plot, flist, flvls, shapes, images, control)

    add_text(plot, text, nodes[[obs_label]])
  }
}


plot4 <- function(.edibble, flist, flvls, shapes, images, text, aspect_ratio, control, obsid, parentids) {
  prep <- cook_design(.edibble)
  parents <- rev(prep$fct_names(parentids))
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
  plot <- add_unit_fills(plot, flist, flvls, shapes, images, control)
  plot <- plot
  if(length(parentids)==3) {
    plot + facet_wrap(parse_expr(block))
  } else {
    plot + facet_wrap(parse_expr(block), scales = "free")
  }
}


plot5 <- function(.edibble, flist, flvls, shapes, images, text, aspect_ratio, control, obsid, parentids) {
  prep <- cook_design(.edibble)
  parent_labels <- prep$fct_names(parentids)
  parents <- names(sort(-lengths(flvls[parent_labels])))

  plot <- ggplot(.edibble, aes(!!parse_expr(parents[1]), !!parse_expr(parents[2])))
  plot <- add_unit_fills(plot, flist, flvls, shapes, images, control)
  plot +
    scale_x_discrete(limits = levels(.edibble[[parents[1]]])) +
    scale_y_discrete(limits = levels(.edibble[[parents[2]]])) +
    facet_grid(as.formula(paste(parents[[3]], "~", parents[[4]])))
}



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

add_unit_fills <- function(plot, flist, flvls, shapes, images, control) {
  nfill <- length(flist$fill)
  plot <- add_unit(plot, flist$fill[1], shapes[1], images[1], height = 1, width = 1, size = 0.1)

  nfill_max <- rep(control$nfill_max, length.out = nfill)
  random_fills <- rep(control$random_fills, length.out = nfill)
  fill_lvls <- list()
  for(ifill in seq_along(flist$fill)) {
    fill_lvls[[ifill]] <- lvl_lump_keep(flvls[[flist$fill[ifill]]],
                                        nfill_max[ifill],
                                        random_fills[ifill])
  }
  nmax_fills <- lengths(fill_lvls)

  opt_fills <- deggust_opt("discrete.fill")[rank(lengths(deggust_opt("discrete.fill")))]
  find_fill_scale <- function(i) {
    if(i <= length(opt_fills[[length(opt_fills)]])) {
      k <- 1
      while(i > length(opt_fills[[k]])) {
        k <- k + 1
      }
      return(k)
    } else {
      return(0)
    }
  }

  chosen_fills <- list()
  for(imax in seq_along(nmax_fills)) {
    k <- find_fill_scale(nmax_fills[imax])
    if(k > 0) {
      chosen_fills[[imax]] <- list(color = opt_fills[[k]],
                                   nlvl = length(flvls[[flist$fill[imax]]]))
      opt_fills[[k]] <- NULL
    } else {
      chosen_fills[[imax]] <- "scale"
    }
  }

  if(any(chosen_fills[[1]]=="scale")) {
    plot <- plot + scale_fill_viridis_d(option = "A")
  } else {
    plot <- plot + ggplot2::scale_fill_manual(breaks = fill_lvls[[1]],
                                              values = rep(chosen_fills[[1]]$color,
                                                           length.out = chosen_fills[[1]]$nlvl))
  }

  if(nfill > 1) {
    plot <- another_fill_scale(plot, 2, flist$fill[2], shape = shapes[2], image = images[2],
                               height = ifelse(nfill > 2, 0.66, 0.5),
                               width = ifelse(nfill > 2, 0.66, 0.5),
                               size = ifelse(nfill > 2, 0.066, 0.05))
    if(any(chosen_fills[[2]]=="scale")) {
      plot <- plot + scale_fill_viridis_d(option = "D")
    } else {
      plot <- plot + ggplot2::scale_fill_manual(breaks = fill_lvls[[2]],
                                                values = rep(chosen_fills[[2]]$color,
                                                             length.out = chosen_fills[[2]]$nlvl))
    }

    if(nfill > 2) {
      plot <- another_fill_scale(plot, 3, flist$fill[3], shape = shapes[3], image = images[3],
                                 height = 0.33, width = 0.33, size = 0.033)
      if(any(chosen_fills[[3]]=="scale")) {
        plot <- plot + scale_fill_viridis_d(option = "E")
      } else {
        plot <- plot + scale_fill_manual(breaks = fill_lvls[[3]],
                                                  values = rep(chosen_fills[[3]]$color,
                                                               length.out = chosen_fills[[3]]$nlvl))
      }

    }
  }
  plot + guides(fill_new_new = guide_legend(order = 1),
                fill_new = guide_legend(order = 2),
                fill = guide_legend(order = 3))
}

