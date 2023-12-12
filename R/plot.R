

plot1 <- function(.edibble, flist, flvls, shapes, images, text, aspect_ratio, control) {
  ufct <- .edibble[[flist$node[1]]]
  nfill <- length(flist$fill)
  N <- nrow(.edibble)
  plot <- switch(control$coord,
                 snake = {
                   nodes <- cbind(.edibble, coord_snake(N, aspect_ratio))
                   ggplot(nodes, aes(x, y)) +
                     geom_node_shape(data = data.frame(x = 0.4, y = 1), fill = "black",
                                     angle = 4 * pi/3, shape = "triangle", width = 0.7, height = 0.5) +
                     geom_path(linewidth = control$linewidth)
                 },
                 spiral = {
                   nodes <- cbind(.edibble, coord_spiral(N))
                   # Archimedian Spiral
                   theta <- seq(0, max(coord_theta(N)), 0.01)
                   r <- 0 + 0.2 * theta
                   ggplot(nodes, aes(x, y)) +
                     geom_path(data = data.frame(x = r * cos(theta), y = r * sin(theta)), linewidth = control$linewidth)
                 })
  plot <- add_unit_fills(plot, flist, flvls, shapes, images, control)
  plot <- plot + coord_equal()
  add_text(plot, text, .edibble[[flist$node[1]]])
}

plot2 <- function(.edibble, flist, flvls, shapes, images, text, aspect_ratio, control, obsid, parentids) {

  prov <- edibble::activate_provenance(.edibble)
  vlevs <- prov$fct_levels(return = "value")
  nlevels_units <- lengths(vlevs[flist$node])
  parent_label <- prov$fct_names(id = parentids)
  obs_label <- prov$fct_names(id = obsid)

  nodes <- split(.edibble, .edibble[[parent_label]])
  nodes_split <- switch(control$coord,
                  snake = lapply(nodes, function(df) cbind(df, coord_snake(nrow(df), aspect_ratio))),
                  spiral = lapply(nodes, function(df) cbind(df, coord_spiral(nrow(df)))))
  nodes <- do.call(rbind, nodes_split)

  nfill <- length(flist$fill)

  plot <- ggplot(nodes, aes(x = x, y = y)) +
    facet_wrap(parse_expr(parent_label))

  plot <- switch(control$coord,
                 snake = {
                   plot +
                     geom_node_shape(data = data.frame(x = 0.4, y = 1), fill = "black",
                                     angle = 4 * pi/3, shape = "triangle", width = 0.7, height = 0.5) +
                     geom_path(linewidth = control$linewidth)
                 },
                 spiral = {
                   # Archimedian Spiral
                   thetar <- map_dbl(nodes_split, function(df) max(coord_theta(nrow(df))))
                   theta <- seq(0, max(thetar), 0.01)
                   r <- 0 + 0.2 * theta
                   plot + geom_path(data = data.frame(x = r * cos(theta), y = r * sin(theta)),
                                    linewidth = control$linewidth)
                 })

  plot <- label_panel(flist, obsid, plot, flvls, shapes, images, control, nodes, prov, block = parent_label)
  add_text(plot, text, nodes[[obs_label]])
}

label_panel <- function(flist, obsid, plot, flvls, shapes, images, control, nodes, prov, block) {
  flist_node <- flist
  flist_node$fill <- flist_node$fill[flist$t2u[flist_node$fill] %in% prov$fct_names(id = obsid)]
  if(length(flist_node$fill) == 0) {
    flist_node$fill <- flist$fill[1]
  }
  trts_remaining <- setdiff(flist$fill, flist_node$fill)
  trts_block <- trts_remaining[flist$t2u[trts_remaining] == block]
  trts_remaining <- setdiff(trts_remaining, trts_block)

  plot <- add_unit_fills(plot, flist_node, flvls, shapes, images, control)
  plot <- plot + coord_equal()
  if(length(trts_block)) {
    plot <- plot +
      geom_text(x = (max(nodes$x) + 1)/2, y = 0.2,
                aes(label = ..label),
                size = control$label_size,
                data = ~{
                  data <- .x[trts_block]
                  .x$..label <- do.call(paste, c(data, list(sep = " / ")))
                  dplyr::slice(.x, 1, .by = parse_expr(block))
                })
    plot <- plot + ylim(0, max(nodes$y) + 0.5)
  }
  if(length(trts_remaining)) {
    plot <- plot + geom_text(aes(label = ..label),
                             size = control$label_size,
                             data = ~{
                               data <- lapply(.x[trts_remaining], function(x) abbreviate(x, minlength = 1))
                               .x$..label <- do.call(paste, c(data, list(sep = "/")))
                               .x
                             })
  }

  plot
}

plot3 <- function(.edibble, flist, flvls, shapes, images, text, aspect_ratio, control, obsid, parentids) {
  prov <- edibble::activate_provenance(.edibble)
  vlevs <- prov$fct_levels(return = "value")
  if(length(parentids)==2) {
    parent_labels <- prov$fct_names(parentids)
    parent1 <- parent_labels[which.max(lengths(vlevs[parent_labels]))]
    parent2 <- setdiff(parent_labels, parent1)

    plot <- ggplot(.edibble, aes(!!parse_expr(parent1), !!parse_expr(parent2)))
    plot <- add_unit_fills(plot, flist, flvls, shapes, images, control)
    plot +
      scale_x_discrete(limits = levels(.edibble[[parent1]])) +
      scale_y_discrete(limits = levels(.edibble[[parent2]]))
  } else if(length(parentids)==1) {
    aids <- intersect(prov$fct_id_ancestor(obsid), setdiff(prov$unit_ids, obsid))
    ancestors <- prov$fct_names(aids)
    nodes <- split(.edibble, paste0(.edibble[[ancestors[1]]], .edibble[[ancestors[2]]]))
    nodes <- lapply(nodes, function(df) cbind(df, coord_snake(nrow(df), aspect_ratio)))
    nodes <- do.call(rbind, nodes)

    nfill <- length(flist$fill)

    plot <- ggplot(nodes, aes(x = x, y = y)) +
      geom_path(linewidth = control$linewidth) +
      facet_wrap(as.formula(paste("~", ancestors[1], "+", ancestors[2])))

    plot <- add_unit_fills(plot, flist, flvls, shapes, images, control)

    add_text(plot, text, nodes[[obs_label]])
  }
}


plot4 <- function(.edibble, flist, flvls, shapes, images, text, aspect_ratio, control, obsid, parentids) {

  prov <- edibble::activate_provenance(.edibble)
  parents <- rev(prov$fct_names(id = parentids))
  unames <- prov$unit_names()
  ou <- prov$fct_names(id = obsid)
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
  plot + facet_wrap(parse_expr(block), scales = "free")
}


plot5 <- function(.edibble, flist, flvls, shapes, images, text, aspect_ratio, control, obsid, parentids) {
  prov <- edibble::activate_provenance(.edibble)
  parent_labels <- prov$fct_names(id = parentids)
  parents <- names(sort(-lengths(flvls[parent_labels])))

  plot <- ggplot(.edibble, aes(!!parse_expr(parents[1]), !!parse_expr(parents[2])))
  plot <- add_unit_fills(plot, flist, flvls, shapes, images, control)
  plot +
    scale_x_discrete(limits = levels(.edibble[[parents[1]]])) +
    scale_y_discrete(limits = levels(.edibble[[parents[2]]])) +
    facet_grid(as.formula(paste(parents[[3]], "~", parents[[4]])))
}

plot6 <- function(.edibble, flist, flvls, shapes, images, text, aspect_ratio, control, obsid, parentids) {
  prov <- edibble::activate_provenance(.edibble)
  subparent_labels <- prov$fct_names(id = parentids)
  subparents <- names(sort(-lengths(flvls[subparent_labels])))
  wholeunit <- flist$node[map_int(flist$node, function(node) length(prov$fct_id_child(id = prov$fct_id(name = node)))) == 3]
  wholeunitid <- prov$fct_id(name = wholeunit)
  parents <- prov$fct_names(id = prov$fct_id_parent(id = wholeunitid, role = "edbl_unit"))
  nodes <- data.frame(x = as.integer(.edibble[[subparents[1]]]),
                      y = as.integer(.edibble[[subparents[2]]]))

  plot <- ggplot(cbind(.edibble, nodes), aes(x, y))

  plot <- label_panel(flist, obsid, plot, flvls, shapes, images, control, nodes, prov, block = wholeunit)
  plot +
    facet_grid(as.formula(paste(parents[[1]], "~", parents[[2]])))
}


add_unit <- function(plot, fill, shape, image, height, width, size) {
  if(is.na(image) & !is.null(fill)) {
    plot <- plot + geom_unit_node(aes(fill = !!parse_expr(fill)), shape = shape, height = height, width = width)
  } else if(is.na(image) & is.null(fill)) {
    plot <- plot + geom_unit_node(shape = shape, height = height, width = width)
  } else if(!is.na(image)) {
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
      do.call("geom_text", c(list(mapping = aes(label = label),
                                  size = control$label_size),
                             text_aes))
  }
  plot
}

add_unit_fills <- function(plot, flist, flvls, shapes, images, control) {
  if(length(flist$fill)) {
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
    other_color <- "#FFFFFF"
    virdis_opts <- c("A", "D", "E")
    get_viridis_pal <- function(i) {
      viridis::viridis_pal(option = virdis_opts[1])(i)
    }

    chosen_fills <- list()
    for(imax in seq_along(nmax_fills)) {
      nmax_fill <- nmax_fills[imax]
      nlvl <- length(flvls[[flist$fill[imax]]])
      k <- find_fill_scale(nmax_fill)
      m <- ifelse(has_other <- nlvl > nmax_fill, nmax_fill - 1, nlvl)
      if(k > 0) {
        cols <- opt_fills[[k]]
        chosen_fills[[imax]] <- if(has_other) c(cols[1:m], other_color) else cols[1:m]
        opt_fills[[k]] <- NULL
      } else {
        cols <- get_viridis_pal(m)
        chosen_fills[[imax]] <- if(has_other) c(cols[1:m], other_color) else cols[1:m]
        virdis_opts <- virdis_opts[-1]
      }
    }

    plot <- plot + scale_fill_manual(limits = fill_lvls[[1]],
                                     values = chosen_fills[[1]],
                                     na.value = other_color)

    if(nfill == 1) {
      plot <- plot + geom_text(aes(label = abbreviate(.data[[flist$fill]], minlength = 1)),
                               size = control$label_size,
                               data = ~subset(., !.[[flist$fill]] %in% fill_lvls[[1]]))
    }

    if(nfill > 1) {
      plot <- another_fill_scale(plot, 2, flist$fill[2], shape = shapes[2], image = images[2],
                                 height = ifelse(nfill > 2, 0.66, 0.5),
                                 width = ifelse(nfill > 2, 0.66, 0.5),
                                 size = ifelse(nfill > 2, 0.066, 0.05))
      plot <- plot + scale_fill_manual(limits = fill_lvls[[2]],
                                       values = chosen_fills[[2]],
                                       na.value = other_color)

      if(nfill > 2) {
        plot <- another_fill_scale(plot, 3, flist$fill[3], shape = shapes[3], image = images[3],
                                   height = 0.33, width = 0.33, size = 0.033)
        plot <- plot + scale_fill_manual(limits = fill_lvls[[3]],
                                         values = chosen_fills[[3]],
                                         na.value = other_color)

      }
    }
    plot + guides(fill_new_new = guide_legend(order = 1),
                  fill_new = guide_legend(order = 2),
                  fill = guide_legend(order = 3))
  } else {
    add_unit(plot, fill = NULL, shapes[1], images[1], height = 1, width = 1, size = 0.1)
  }
}

