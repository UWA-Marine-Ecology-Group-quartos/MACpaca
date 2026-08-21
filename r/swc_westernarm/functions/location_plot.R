location_plot <- function(plot_limits, study_limits, annotation_labels) {
  # 1. Location overview plot - includes parks zones and an aus inset
  require(tidyverse)
  require(tidyterra)
  require(patchwork)

  p1 <- ggplot() +
    geom_spatraster_contour_filled(data = bathy,
                                   breaks = c(0, -30, -70, -200, - 700, -2000 , -4000, -6000),
                                   colour = NA, show.legend = F) +
    scale_fill_manual(values = c("#FFFFFF", "#EFEFEF", "#DEDEDE", "#CCCCCC", "#B6B6B6", "#9E9E9E", "#808080")) +
    new_scale_fill() +
    geom_spatraster_contour(data = bathy,
                            breaks = c(-30, -70, -200, - 700, -2000 , -4000, -6000), colour = "white",
                            alpha = 3/5, linewidth = 0.1, show.legend = F) +
    geom_sf(data = ausc, fill = "seashell2", colour = "grey80", linewidth = 0.1) +
    geom_sf(data = terrnp, aes(fill = leg_catego), colour = NA, alpha = 0.8) +
    terr_fills +
    new_scale_fill() +
    geom_sf(data = marine_parks_state, aes(fill = zone), colour = NA, alpha = 0.4) +
    scale_fill_manual(name = "State Marine Parks", guide = "legend",
                      values = with(marine_parks_state, setNames(colour, zone))) +
    new_scale_fill() +
    geom_sf(data = marine_parks_amp, aes(fill = zone), colour = NA, alpha = 0.8) +
    scale_fill_manual(name = "Australian Marine Parks", guide = "legend",
                      values = with(marine_parks_amp, setNames(colour, zone))) +
    new_scale_fill() +
    geom_sf(data = cwatr, colour = "firebrick", alpha = 1, linewidth = 0.4, lineend = "round") +
    labs(x = NULL, y = NULL) +

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
              nudge_y = -0.03) +
    # <<< END NEW >>>

    annotate("rect", xmin = study_limits[1], xmax = study_limits[2], ymin = study_limits[3], ymax = study_limits[4],
             fill = NA, colour = "goldenrod2", linewidth = 0.4) +
    coord_sf(xlim = c(plot_limits[1], plot_limits[2]), ylim = c(plot_limits[3], plot_limits[4]), crs = 4326) +
    theme_minimal()

  # inset map
  p1.1 <- ggplot(data = aus) +
    geom_sf(fill = "seashell1", colour = "grey90", linewidth = 0.05, alpha = 4/5) +
    geom_sf(data = aus_marine_parks, alpha = 5/6, colour = "grey85", linewidth = 0.02) +
    coord_sf(xlim = c(110, 125), ylim = c(-37, -13)) + # This is constant for all plots - its just a map of WA
    annotate("rect", xmin = plot_limits[1], xmax = plot_limits[2], ymin = plot_limits[3], ymax = plot_limits[4],
             colour = "grey25", fill = "white", alpha = 1/5, linewidth = 0.2) +
    theme_bw() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major = element_blank(),
          panel.border = element_rect(colour = "grey70"))

  p1.1 + p1
}
