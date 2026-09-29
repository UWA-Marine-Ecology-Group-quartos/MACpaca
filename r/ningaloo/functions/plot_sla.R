plot_sla <- function(prediction_limits) {
  ggplot() +
    geom_spatraster(data = sla) +
    scale_fill_viridis_c(na.value = NA) +
    geom_sf(data = aus) +
    geom_sf(data = marine_parks, fill = NA, colour = "grey70", linewidth = 0.4) +
    facet_wrap(~lyr) +
    theme_minimal() +
    labs(fill = "SLA (m)") +
    scale_x_continuous(breaks = scales::breaks_width(0.8)) +
    scale_y_continuous(breaks = scales::breaks_width(0.5)) +
    coord_sf(xlim = c(prediction_limits[1], prediction_limits[2]),
             ylim = c(prediction_limits[3], prediction_limits[4]), crs = 4326)
}
