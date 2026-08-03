# Shared marine park legend attached under the plot grid.
# NOTE: state marine park (wasanc) legend removed - Eastern Recherche is
# Commonwealth only, so there are no state zones to list.
marine_park_legend <- function() {

  p <- ggplot() +
    geom_sf(data = marine_parks_amp, aes(colour = zone), fill = NA, linewidth = 0.8) +
    scale_colour_manual(
      name   = "Australian Marine Park",
      guide  = "legend",
      values = with(marine_parks_amp, setNames(colour, zone))
    ) +
    guides(
      colour = guide_legend(
        ncol         = 2,
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
