dominantbenthos_plot_single <- function(pred_plot, prediction_limits, habitat_lookup) {

  # Gradient high colours for each habitat (used in geom_tile fill gradient)
  grad_high <- c(
    "Sand"                  = "wheat",
    "Macroalgae"            = "darkorange4",
    "Seagrass"              = "forestgreen",
    "Rock"                  = "grey40",
    "Sessile invertebrates" = "deeppink3"
  )

  # Legend label (line break for long names)
  legend_names <- c(
    "Sand"                  = "Sand",
    "Macroalgae"            = "Macroalgae",
    "Seagrass"              = "Seagrass",
    "Rock"                  = "Rock",
    "Sessile invertebrates" = "Sessile\ninvertebrates"
  )

  # Canonical rendering order (bottom to top) — filter to modelled taxa only
  hab_order <- c("Sand", "Rock", "Macroalgae", "Seagrass", "Sessile invertebrates")
  modelled  <- hab_order[hab_order %in% names(habitat_lookup)]

  p <- ggplot()

  for (i in seq_along(modelled)) {
    hab       <- modelled[i]
    stub      <- habitat_lookup[[hab]]
    fit_col   <- paste0("p_", stub, ".fit")
    alpha_col <- paste0("p_", stub, ".alpha")

    if (i > 1) p <- p + new_scale_fill() + new_scale("alpha")

    p <- p +
      geom_tile(data = pred_plot,
                aes(x = x, y = y,
                    fill  = .data[[alpha_col]],
                    alpha = .data[[fit_col]])) +
      scale_alpha_continuous(range = c(0, 1), guide = "none", name = hab) +
      scale_fill_gradient(
        low      = "white",
        high     = grad_high[[hab]],
        name     = legend_names[[hab]],
        na.value = "transparent",
        breaks   = c(0, 0.5, 1),
        labels   = c("0", "0.5", "1")
      )
  }

  p_out <- p +
    geom_contour(
      data = bathy,
      aes(x = x, y = y, z = Depth),
      colour    = "black",
      breaks    = c(-30, -70, -200),
      linewidth = 0.1
    ) +
    geom_sf(data = ausc, fill = "seashell2", colour = "black", linewidth = 0.2) +
    geom_sf(
      data = wasanc,
      aes(colour = zone),
      fill        = NA,
      show.legend = FALSE,
      linewidth   = 0.6
    ) +
    scale_colour_manual(values = with(wasanc, setNames(colour, zone))) +
    ggnewscale::new_scale_color() +
    geom_sf(
      data = marine_parks_amp,
      aes(colour = zone),
      fill         = NA,
      show.legend  = FALSE,
      linewidth    = 0.6
    ) +
    geom_sf(data = cwatr, colour = "firebrick", linewidth = 0.6) +
    scale_colour_manual(

      values = with(marine_parks_amp, setNames(colour, zone))
    ) +
    coord_sf(
      xlim   = c(prediction_limits[1], prediction_limits[2]),
      ylim   = c(prediction_limits[3], prediction_limits[4]),
      crs    = 4326,
      expand = FALSE
    ) +
    labs(x = NULL, y = NULL, colour = NULL) +
    theme_minimal() +
    theme(
      axis.title        = element_blank(),
      axis.text         = element_text(size = 9),
      axis.ticks        = element_line(linewidth = 0.2),
      panel.grid.major  = element_line(linewidth = 0.2, colour = "grey85"),
      panel.grid.minor  = element_blank(),
      legend.title      = element_text(size = 9),
      legend.text       = element_text(size = 8),
      legend.key.height = unit(0.45, "cm"),
      legend.key.width  = unit(0.45, "cm"),
      plot.margin       = margin(2, 2, 2, 2, unit = "mm")
    )
  cowplot::plot_grid(p_out, marine_park_legend(), ncol = 1, rel_heights = c(1, 0.175))
}
