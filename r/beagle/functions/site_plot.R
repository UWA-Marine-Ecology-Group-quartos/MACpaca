site_plot <- function(site_limits, # Tighter zoom for this plot
                      annotation_labels) {
  ggplot() +
    geom_spatraster_contour_filled(data = bathy,
                                   breaks = c(0, -30, -70, -200, -700, -2000, -4000, -10000), alpha = 4/5) +
    scale_fill_manual(values = c("#FFFFFF", "#EFEFEF", "#DEDEDE", "#CCCCCC", "#B6B6B6", "#9E9E9E", "#808080"),
                      guide = "none") +
    geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.1) +
    new_scale_fill() +
    geom_sf(data = terrnp, aes(fill = TYPE), colour = NA, alpha = 0.8) +
    terr_fills +
    new_scale_fill() +
    geom_sf(data = marine_parks_state, aes(fill = zone), colour = NA, alpha = 0.4) +
    scale_fill_manual(name = "State Marine Parks",
                      values = with(marine_parks_state, setNames(colour, zone)),
                      guide  = guide_legend(order = 2)) +
    new_scale_fill() +
    geom_sf(data = marine_parks_amp, aes(fill = zone), colour = NA, alpha = 0.8) +
    scale_fill_manual(name = "Australian Marine Parks",
                      values = with(marine_parks_amp, setNames(colour, zone)),
                      guide  = guide_legend(order = 3)) +
    new_scale_fill() +
    labs(x = NULL, y = NULL) +
    new_scale_fill() +
    geom_sf(data = cwatr, colour = "firebrick", alpha = 1, size = 0.2, lineend = "round") +
    geom_sf(data = metadata, alpha = 1, shape = 16, size = 1, aes(colour = year)) +
    scale_colour_manual(values = c("2018" = "#5390d9",
                                   "2025" = "#593982"),
                        name = "Year", guide  = guide_legend(order = 1)) +

    geom_point(data = annotation_labels,
               aes(x = x, y = y),
               shape = 4,
               size = 1,
               stroke = 0.5,
               colour = "black") +
    geom_text(data = annotation_labels[1:2, ],
              aes(x = x, y = y, label = label),
              size = 1.65,
              fontface = "italic",
              nudge_y = -0.03) +
    geom_text(data = annotation_labels[3, ],
              aes(x = x, y = y, label = label),
              size = 1.65,
              fontface = "italic",
              hjust = 0,
              nudge_x = 0.03) +

    coord_sf(xlim = c(site_limits[1], site_limits[2]), ylim = c(site_limits[3], site_limits[4]), crs = 4326) +
    theme_minimal() +
    theme(panel.grid = element_blank())
}
