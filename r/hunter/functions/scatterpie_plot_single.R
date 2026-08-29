scatterpie_plot_single <- function(benthos_year, site_limits, pie_radius = 0.004) {

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
    geom_sf(data = marine_parks_state, fill = NA, colour = "grey20") +
    geom_sf(data = marine_parks_amp, fill = NA, colour = "grey50") +
    geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, linewidth = 0.3) +
    new_scale_fill() +
    geom_scatterpie(
      data = benthos_year,
      aes(x = longitude_dd, y = latitude_dd, r = pie_radius),
      cols = c(
        "Sediment",
        "Sessile invertebrates",
        "Bare rock",
        "Macroalgae"
      ),
      colour = NA
    ) +
    labs(x = "Longitude", y = "Latitude", fill = "Habitat") +
    hab_fills +
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
