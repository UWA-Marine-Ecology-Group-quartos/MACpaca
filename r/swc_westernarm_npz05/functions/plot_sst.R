plot_sst <- function(prediction_limits) {
  ggplot() +
    geom_spatraster(data = sst) +
    scale_fill_viridis_c(na.value = NA) +
    geom_sf(data = ausc) +
    geom_sf(data = marine_parks, fill = NA, colour = "grey70", linewidth = 0.4) +
    facet_wrap(~lyr) +
    theme_minimal() +
    theme(axis.text = element_text(size = 10)) +
    labs(fill = "SST (°C)") +
    scale_x_continuous(breaks = seq(-180, 180, by = 0.4)) +
    scale_y_continuous(breaks = seq(-90, 90, by = 0.4)) +
    coord_sf(xlim = c(prediction_limits[1], prediction_limits[2]),
             ylim = c(prediction_limits[3], prediction_limits[4]), crs = 4326)
}
