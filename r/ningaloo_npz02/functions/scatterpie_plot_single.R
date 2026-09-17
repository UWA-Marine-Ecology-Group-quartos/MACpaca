scatterpie_plot_single <- function(benthos_year, site_limits, pie_radius = 0.004) {

  hab_cols_all <- c(
    "Sand" = "wheat",
    "Sessile invertebrates" = "plum",
    "Rock" = "grey40"
  )

  # Only draw pie slices (and legend entries) for habitats with some cover in
  # this data - an all-zero class would otherwise still show an empty slice
  present_habs <- names(hab_cols_all)[
    colSums(benthos_year[names(hab_cols_all)], na.rm = TRUE) > 0
  ]

  ggplot() +
    geom_contour_filled(
      data = bathy,
      aes(x, y, z = Depth, fill = after_stat(level)),
      color = "black",
      breaks = c(-30, -70, -200, -700, -2000, -4000),
      linewidth = 0.1
    ) +
    depth_fills +
    new_scale_fill() +
    geom_sf(data = ausc, fill = "seashell2", colour = "black", linewidth = 0.1) +
    geom_sf(data = wasanc, fill = "#bfd054", alpha = 2/5, colour = NA) +
    wampa_fills +
    labs(fill = "State Marine Parks") +
    new_scale_fill() +
    geom_sf(data = npz, fill = "#7bbc63", alpha = 2/5, colour = NA) +
    geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, linewidth = 0.3) +
    new_scale_fill() +
    geom_scatterpie(
      data = benthos_year,
      aes(x = longitude_dd, y = latitude_dd, r = pie_radius),
      cols = present_habs,
      colour = NA
    ) +
    labs(x = "Longitude", y = "Latitude", fill = NULL) +
    scale_fill_manual(values = hab_cols_all[present_habs]) +
    scale_x_continuous(breaks = scales::breaks_width(0.1)) +
    scale_y_continuous(breaks = scales::breaks_width(0.1)) +
    coord_sf(
      xlim = c(site_limits[1], site_limits[2]),
      ylim = c(site_limits[3], site_limits[4]),
      crs = 4326
    ) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "#b9d1d6", colour = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal"
    )
}
