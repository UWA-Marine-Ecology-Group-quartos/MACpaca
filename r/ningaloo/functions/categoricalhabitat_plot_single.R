categoricalhabitat_plot_single <- function(pred_plot, prediction_limits, habitat_lookup) {

  # All available colours in canonical display order
  all_colours <- c(
    "Rock"                  = "grey40",
    "Sessile invertebrates" = "plum",
    "Macroalgae"            = "darkgoldenrod4",
    "Seagrass"              = "forestgreen",
    "Sand"                  = "wheat"
  )

  # Filter levels and colours to modelled taxa only, preserving canonical order
  all_levels  <- names(all_colours)
  modelled    <- names(habitat_lookup)
  hab_levels  <- all_levels[all_levels %in% modelled]
  hab_colours <- all_colours[hab_levels]

  pred_cat <- pred_plot %>%
    dplyr::mutate(
      dom_tag = as.character(dom_tag),
      dom_tag = dplyr::case_when(
        dom_tag %in% c("sand", "Sand")                                                        ~ "Sand",
        dom_tag %in% c("macro", "macroalgae", "Macroalgae")                                   ~ "Macroalgae",
        dom_tag %in% c("seagrass", "seagrasses", "Seagrass", "Seagrasses")                    ~ "Seagrass",
        dom_tag %in% c("rock", "Rock")                                                        ~ "Rock",
        dom_tag %in% c("sessile invertebrates", "Sessile Invertebrates", "inverts", "Inverts") ~ "Sessile invertebrates",
        TRUE ~ dom_tag
      ),
      dom_tag = factor(dom_tag, levels = hab_levels)
    )

  p_out <- ggplot() +
    geom_tile(data = pred_cat, aes(x = x, y = y, fill = dom_tag)) +
    scale_fill_manual(
      name     = "Habitat",
      limits   = hab_levels,
      values   = hab_colours,
      na.value = "transparent",
      drop     = FALSE
    ) +
    labs(x = NULL, y = NULL, fill = NULL) +
    new_scale_color() +
    geom_contour(
      data = bathy,
      aes(x = x, y = y, z = Depth),
      colour    = "black",
      breaks    = c(-30, -70, -200),
      linewidth = 0.2
    ) +
    geom_sf(data = ausc, fill = "seashell2", colour = "grey80", linewidth = 0.5) +
    geom_sf(
      data        = marine_parks_amp,
      aes(colour  = zone),
      fill        = NA,
      linewidth   = 1.2,
      show.legend = FALSE
    ) +
    scale_colour_manual(values = with(marine_parks_amp, setNames(colour, zone))) +
    new_scale_color() +
    geom_sf(
      data        = wasanc,
      aes(colour  = zone),
      fill        = NA,
      linewidth   = 0.7,
      show.legend = FALSE
    ) +
    scale_colour_manual(values = with(wasanc, setNames(colour, zone))) +
    new_scale_color() +
    geom_sf(data = cwatr, colour = "red", linewidth = 0.9) +
    coord_sf(
      xlim = c(prediction_limits[1], prediction_limits[2]),
      ylim = c(prediction_limits[3], prediction_limits[4]),
      crs  = 4326
    ) +
    theme_minimal() +
    theme(
      panel.background  = element_rect(fill = "white", colour = NA),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      legend.position   = "bottom",
      legend.direction  = "horizontal",
      legend.box        = "horizontal",
      legend.text       = element_text(size = 6),
      legend.title      = element_blank()
    )

  cowplot::plot_grid(p_out, marine_park_legend(), ncol = 1, rel_heights = c(1, 0.175))
}
