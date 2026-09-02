# =============================================================================
# crosssection_plot: draws the bathymetry cross-section with ocean shading,
# paleo-shoreline markers, and (if supplied) firebrick lines showing where
# the transect crosses the coastal waters limit.
# =============================================================================
crosssection_plot <- function(bath_df1, crosssection_labels, label_offset, segment_offset,
                              cwatr_crossings = NULL) {

  paleo <- data.frame(
    depth = c(-118, -94, -63, -41),
    label = c("20-30 Ka", "15-17 Ka", "12-13 Ka", "9-10 Ka")
  )

  for (i in 1:nrow(paleo)) {
    temp <- bath_df1 %>%
      dplyr::filter(abs(depth - paleo$depth[i]) == min(abs(depth - paleo$depth[i]))) %>%
      dplyr::select(depth, distance.from.coast) %>%
      dplyr::slice(1)
    if (i == 1) dat <- temp else dat <- bind_rows(dat, temp)
  }
  paleo$distance.from.coast <- dat$distance.from.coast

  p <- ggplot() +
    # water shading: wherever the seabed is below sea level
    geom_ribbon(
      data = bath_df1 %>% dplyr::filter(depth < 0),
      aes(x = distance.from.coast, ymin = depth, ymax = 0),
      fill = "#12a5db", alpha = 0.5
    ) +
    geom_line(data = bath_df1, aes(y = depth, x = distance.from.coast)) +
    geom_ribbon(data = bath_df1, aes(ymin = -Inf, ymax = depth, x = distance.from.coast), fill = "tan") +
    theme_classic() +
    scale_x_continuous(
      expand = c(0, 0),
      limits = c(min(bath_df1$distance.from.coast), max(bath_df1$distance.from.coast))
    ) +
    ylim(min(bath_df1$depth), max(bath_df1$depth) + 10) +
    labs(x = "Distance from coast (km)", y = "Elevation (m)") +
    geom_segment(
      data = paleo,
      aes(x = distance.from.coast, xend = distance.from.coast + segment_offset, y = depth, yend = depth),
      linetype = 2, alpha = 0.5
    ) +
    geom_text(
      data = paleo,
      aes(x = distance.from.coast + label_offset, y = depth, label = label),
      size = 3
    ) +
    annotate(
      geom = "text",
      x = crosssection_labels$x, y = crosssection_labels$y,
      label = crosssection_labels$label
    )

  if (!is.null(cwatr_crossings) && nrow(cwatr_crossings) > 0) {
    p <- p + geom_segment(
      data = cwatr_crossings,
      aes(x = distance.from.coast, xend = distance.from.coast, y = 0, yend = min(bath_df1$depth)),
      colour = "firebrick", linewidth = 0.5
    )
  }

  p
}
