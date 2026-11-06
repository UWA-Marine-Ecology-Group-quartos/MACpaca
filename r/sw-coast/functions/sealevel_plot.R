sealevel_plot <- function(plot_limits, annotation_labels) {
  ggplot() +
    geom_spatraster(data = clamp(bathy, upper = -50, values = F)) +
    scale_fill_gradient2(low = "royalblue4", mid = "lightskyblue1", high = "white", name = "Depth (m)",
                         na.value = "#f9ddb1") +
    new_scale_fill() +
    geom_spatraster_contour_filled(data = bathy,
                                   breaks = c(0, -40, -70, -125)) +
    depth_fills +
    new_scale_fill() +
    geom_sf(data = ausc, fill = "seashell2", colour = "grey62", size = 0.2) +
    geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 4/5, colour = NA, show.legend = F) +
    terr_fills +
    new_scale_fill() +

    # >>> NEW: X markers at annotation label coords <<<
    geom_point(data = annotation_labels,
               aes(x = x, y = y),
               shape = 4,
               size = 1,
               stroke = 0.5,
               colour = "black") +
    geom_text(data = annotation_labels,
              aes(x = x, y = y, label = label),
              size = 1.65,
              fontface = "italic",
              nudge_y = -0.02) +
    # <<< END NEW >>>

    coord_sf(xlim = c(plot_limits[1], plot_limits[2]), ylim = c(plot_limits[3], plot_limits[4]), crs = 4326) +
    labs(x = "Longitude", y = "Latitude") +
    theme_minimal()
}
