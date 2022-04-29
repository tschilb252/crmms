## Functions to adde tiers for Powell and Mead
bor_theme <- function() {
  theme_bw() + 
    theme(panel.background = element_rect(fill = "transparent", colour = NA),
    # plot.background = element_rect(fill = "transparent", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.box.background = element_rect(fill = "transparent", colour = NA)) 
}

eq_tier_df <- function() {
  Timestep = matrix(seq.Date(as.Date('2007-10-1'), 
                             as.Date('2026-09-1'), 'months') - 1,
                    byrow = T, ncol = 12)
  Timestep = as.Date(as.vector(t(cbind(Timestep[,1], Timestep))), 
                     origin = '1970-01-01')[-1]
  powell_line = data.frame(
    Date = as.POSIXct(Timestep),
    Eq_elev = rep(c(3636, 3639, 3642, 3643, 3645, 3646, 3648, 3649, 3651, 3652, 
                    3654, 3655, 3657, 3659, 3660, 3662, 3663, 3664, 3666),
                  each=13)[1:length(Timestep)],
    check.names = F,
    stringsAsFactors = FALSE) %>%
    mutate(Date = as.yearmon(Date)) 
  dates_2 = as.yearmon(seq.Date(as.Date('2026-09-1'), 
                                as.Date('2029-09-1'), 'months'))
  postEQ = data.frame(Date = dates_2,
                      Eq_elev = rep(3666, length(dates_2)))
  powell_line = rbind.data.frame(powell_line, postEQ)
  
  return(powell_line)
}



add_powell_tiers <- function(gg, xrange = NULL) {
  
  yrs <- gg_year_range(gg)
  
  if (2007 %in% yrs) {
    ig_start <- 2007
    ig_label <- 2007.1
  } else {
    ig_start <- min(yrs)
    ig_label <- ig_start + 0.1
  }
  ann_size <- 2.9
  line_size <- 0.6
  
  max_y <- ceiling(gg_y_range(gg)[2]/50)*50
  max_yrs = max(yrs)
  
  if (!is.null(xrange)) {
    xminI =  xrange[1]
    xmaxI =  xrange[2]
  } else {
    xminI = ig_start+.01
    xmaxI = max_yrs-.01
  }
  
  gg <- gg +
    geom_line(
      data = eq_tier_df(), 
      aes(x = Date, y=Eq_elev), 
      color ='#808080', linetype = 3
    ) +
    annotate(
      "text", x = ig_label, y = 3670, 
      label = "Equalization Tier (ET)", 
      size = ann_size, hjust = 0, fontface = "italic", color = '#505050'
    ) +
    geom_segment(aes(
      x = xminI, y = 3575, xend = xmaxI, yend = 3575), 
      size = line_size, 
      color ='#808080', 
      linetype = 3
    ) +
    annotate(
      "text", x = ig_label, y = 3620,
      label = "Upper Elevation Balancing Tier\n(3,575' to ET)", 
      size = ann_size, 
      hjust = 0, 
      fontface = "italic", 
      color = '#505050'
    ) +
    geom_segment(aes(
      x = xminI, y = 3525, xend = xmaxI, yend = 3525), 
      size = line_size, 
      color ='#808080', linetype = 3
    ) +
    annotate(
      "text", x = ig_label, y = 3544, 
      label = "Mid-Elevation Release Tier\n(3,525' to 3,575')", 
      size = ann_size, 
      hjust = 0, 
      fontface = "italic", 
      color = '#505050'
    ) +
    geom_segment(aes(
      x = xminI, y = 3490, xend = xmaxI, yend = 3490), 
      size = line_size, 
      color ='#808080', linetype = 3
    ) +
    annotate(
      "text", x = ig_label, y = 3510, 
      label = "Lower Elevation Balancing Tier\n(<3,525')", 
      size = ann_size, 
      hjust = 0, 
      fontface = "italic", 
      color = '#505050'
    ) +
    annotate(
      "text", x = ig_label, y = 3477, 
      label = "Minimum Power Pool\n(3,490')", 
      size = ann_size, 
      hjust = 0, 
      fontface = "italic", 
      color = '#505050'
    )
  
  gg
}

add_mead_tiers <- function(gg, xrange = NULL) {
  
  yrs <- gg_year_range(gg)
  
  if (2007 %in% yrs) {
    ig_start <- 2007
    ig_label <- 2007.1
  } else {
    ig_start <- min(yrs)
    ig_label <- ig_start + 0.1
  }
  ann_size <- 2.9
  line_size <- 0.6
  
  max_y <- ceiling(gg_y_range(gg)[2]/50)*50
  max_yrs = max(yrs)
  
  if (!is.null(xrange)) {
    xminI =  xrange[1]
    xmaxI =  xrange[2]
  } else {
    xminI = ig_start+.01
    xmaxI = max_yrs-.01
  }
  
  gg <- gg +
    geom_segment(aes(
      x = xminI, y = 1145, xend = xmaxI, yend = 1145), 
      size = line_size, 
      color ='#808080', linetype = 3
    ) + 
    annotate(
      "text", x = ig_label, y = 1152.5, 
      label = "Surplus Condition (>1,145')", 
      size = ann_size, hjust = 0, fontface = "italic", color = '#505050'
    ) +
    geom_segment(aes(
      x = xminI, y = 1075, xend = xmaxI, yend = 1075), 
      size = line_size, 
      color ='#808080', 
      linetype = 3
    ) +
    annotate(
      "text", x = ig_label, y = 1125,
      label = "Normal Condition\n(1,075' to 1,145')", 
      size = ann_size, 
      hjust = 0, 
      fontface = "italic", 
      color = '#505050'
    ) +
    geom_segment(aes(
      x = xminI, y = 1050, xend = xmaxI, yend = 1050), 
      size = line_size, 
      color ='#808080', linetype = 3
    ) +
    annotate(
      "text", x = ig_label, y = 1063, 
      label = "Level 1 Shortage Condition\n(1,050' to 1,075')", 
      size = ann_size, 
      hjust = 0, 
      fontface = "italic", 
      color = '#505050'
    ) +
    geom_segment(aes(
      x = xminI, y = 1025, xend = xmaxI, yend = 1025), 
      size = line_size, 
      color ='#808080', linetype = 3
    ) +
    annotate(
      "text", x = ig_label, y = 1037, 
      label = "Level 2 Shortage Condition\n(1,025' to 1,050')", 
      size = ann_size, 
      hjust = 0, 
      fontface = "italic", 
      color = '#505050'
    ) +
    annotate(
      "text", x = ig_label, y = 1015, 
      label = "Level 3 Shortage Condition\n(<1,025')", 
      size = ann_size, 
      hjust = 0, 
      fontface = "italic", 
      color = '#505050'
    )
  
  gg
}

gg_year_range <- function(gg) {
  tmp <- layer_scales(gg)$x$range$range
  tmp <- tmp[1]:tmp[2]
  
  tmp
}

gg_y_range <- function(gg) {
  layer_scales(gg)$y$range$range
}
