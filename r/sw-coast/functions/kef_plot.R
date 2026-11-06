kef_plot <- function(plot_limits, annotation_labels) {
  ggplot() +
    geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.1) +
    geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 4/5, colour = NA, show.legend = F) +
    labs(fill = "Terrestrial Managed Areas") +
    terr_fills +
    new_scale_fill() +
    geom_sf(data = kef, aes(fill = abbrv), alpha = 0.7, color = NA) +
    scale_fill_manual(name = "Key Ecological Features", guide = "legend",
                      values = with(kef, setNames(colour, abbrv))) +
    # kef_fills +
    new_scale_fill() +
    geom_sf(data = terrnp, aes(fill = leg_catego), colour = NA, alpha = 0.8, show.legend = F) +
    terr_fills +
    new_scale_fill() +
    geom_sf(data = marine_parks_state, aes(fill = zone), colour = NA) +
    scale_fill_manual(name = "State Marine Parks", guide = "legend",
                      values = with(marine_parks_state, setNames(colour, zone))) +
    new_scale_colour() +
    geom_sf(data = marine_parks_amp, aes(colour = zone), fill = NA, linewidth = 0.4, alpha = 0.3) +
    scale_colour_manual(name = "Australian Marine Parks", guide = "legend",
                      values = with(marine_parks_amp, setNames(colour, zone))) +
    new_scale_colour() +
    geom_sf(data = cwatr, colour = "firebrick", alpha = 1, linewidth = 0.4, lineend = "round") +
    labs(x = NULL, y = NULL,  fill = "Key Ecological Features") +

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
    theme_minimal() +
    theme(panel.grid = element_blank())
}
