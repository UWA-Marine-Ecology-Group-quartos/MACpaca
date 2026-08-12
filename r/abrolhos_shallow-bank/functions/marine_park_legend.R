marine_park_legend <- function(wasanc = NULL) {

  plot_state <- !is.null(wasanc) && nrow(wasanc) > 0

  p <- ggplot()

  if (plot_state) {

    ngari_colours <- wasanc %>%
      sf::st_drop_geometry() %>%
      dplyr::distinct(zone, colour) %>%
      dplyr::arrange(zone) %>%
      dplyr::pull(colour)

    p <- p +
      geom_sf(data = wasanc, aes(colour = zone), fill = NA, linewidth = 0.8) +
      scale_colour_manual(
        name   = "State Marine Park",
        guide  = "legend",
        values = with(wasanc, setNames(colour, zone))
      ) +
      guides(
        colour = guide_legend(
          order        = 2,
          ncol         = 1,
          override.aes = list(colour = ngari_colours, fill = NA, linewidth = 1)
        )
      ) +
      ggnewscale::new_scale_color()
  }

  p <- p +
    geom_sf(data = marine_parks_amp, aes(colour = zone), fill = NA, linewidth = 0.8) +
    scale_colour_manual(
      name   = "Australian Marine Park",
      guide  = "legend",
      values = with(marine_parks_amp, setNames(colour, zone))
    ) +
    guides(
      colour = guide_legend(
        order        = 1,
        ncol         = if (plot_state) 2 else 3,
        override.aes = list(fill = NA, linewidth = 1)
      )
    ) +
    theme_minimal() +
    theme(
      legend.position  = "bottom",
      legend.direction = "vertical",
      legend.box       = "horizontal",
      legend.text      = element_text(size = 10),
      legend.title     = element_text(size = 10, face = "bold")
    )

  cowplot::get_legend(p)
}
