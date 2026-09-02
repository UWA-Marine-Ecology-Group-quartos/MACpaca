scatterpie_plot_single <- function(benthos_year, site_limits = NULL,
                                   pie_radius = 0.004, pad = 0.05) {

  # Frame the plot on the samples rather than a hardcoded extent. Pass
  # site_limits explicitly to override (e.g. to keep every year's panel on the
  # same window when this is called in a per-year loop).
  if (is.null(site_limits)) {
    site_limits <- c(
      range(benthos_year$longitude_dd, na.rm = TRUE),
      range(benthos_year$latitude_dd,  na.rm = TRUE)
    ) + c(-pad, pad, -pad, pad)
  }

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
      cols = c(
        "Sand",
        "Sessile invertebrates",
        "Rock",
        "Macroalgae",
        "Seagrass"
      ),
      colour = NA
    ) +
    labs(x = "Longitude", y = "Latitude", fill = "Habitat") +
    hab_fills +
    scale_x_continuous(breaks = scales::breaks_width(0.2)) +
    scale_y_continuous(breaks = scales::breaks_width(0.2)) +
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
      legend.box = "horizontal",
      legend.text = element_text(size = 10)
    )
}
