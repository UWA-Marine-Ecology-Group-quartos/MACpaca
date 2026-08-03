categoricalhabitat_plot_multi <- function(dat_list, prediction_limits, habitat_lookup) {

  yrs <- names(dat_list)

  if (is.null(yrs) || any(yrs == "")) {
    stop("dat_list must be a named list")
  }

  # All available colours in canonical display order
  all_colours <- c(
    "Rock"                  = "grey40",
    "Sessile invertebrates" = "plum",
    "Macroalgae"            = "darkgoldenrod4",
    "Seagrass"              = "forestgreen",
    "Sand"                  = "wheat"
  )

  # Filter to modelled taxa only, preserving canonical order
  all_levels      <- names(all_colours)
  modelled        <- names(habitat_lookup)
  hab_levels      <- all_levels[all_levels %in% modelled]
  habitat_colours <- all_colours[hab_levels]

  pred_cat <- purrr::map_dfr(seq_along(dat_list), function(i) {
    dat_list[[i]] %>%
      as.data.frame(xy = TRUE) %>%
      dplyr::mutate(year = yrs[i]) %>%
      normalise_se()
  }) %>%
    dplyr::mutate(
      year    = factor(year, levels = yrs),
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

  ngari_colours <- wasanc %>%
    st_drop_geometry() %>%
    distinct(zone, colour) %>%
    arrange(zone) %>%
    pull(colour)

  ggplot() +
    geom_tile(data = pred_cat, aes(x = x, y = y, fill = dom_tag)) +
    scale_fill_manual(
      name     = "Habitat",
      limits   = hab_levels,
      values   = habitat_colours,
      na.value = "transparent",
      drop     = FALSE
    ) +
    guides(
      fill = guide_legend(
        order         = 1,
        override.aes  = list(
          colour    = NA,
          fill      = unname(habitat_colours),
          linewidth = 0.5
        )
      )
    ) +
    labs(x = NULL, y = NULL) +
    geom_contour(
      data = bathy,
      aes(x = x, y = y, z = Depth),
      colour    = "black",
      breaks    = c(-30, -70, -200),
      linewidth = 0.2
    ) +
    geom_sf(data = ausc, fill = "seashell2", colour = "grey80", linewidth = 0.5) +
    new_scale_color() +
    geom_sf(
      data        = wasanc,
      aes(colour  = zone),
      fill        = NA,
      linewidth   = 0.8,
      show.legend = TRUE
    ) +
    scale_colour_manual(
      name  = "State Marine Park",
      guide = "legend",
      values = with(wasanc, setNames(colour, zone))
    ) +
    guides(
      colour = guide_legend(
        order        = 3,
        override.aes = list(
          colour    = ngari_colours,
          fill      = NA,
          linewidth = 1
        )
      )
    ) +
    new_scale_color() +
    geom_sf(
      data        = marine_parks_amp,
      aes(colour  = zone),
      fill        = NA,
      linewidth   = 0.8,
      show.legend = TRUE
    ) +
    scale_colour_manual(
      name  = "Australian Marine Park",
      guide = "legend",
      values = with(marine_parks_amp, setNames(colour, zone))
    ) +
    guides(
      colour = guide_legend(
        order        = 2,
        override.aes = list(fill = NA, linewidth = 1)
      )
    ) +
    geom_sf(
      data      = st_buffer(cwatr_offset, dist = 0.005),
      colour    = "red",
      linewidth = 0.5
    ) +
    coord_sf(
      xlim   = c(prediction_limits[1], prediction_limits[2]),
      ylim   = c(prediction_limits[3], prediction_limits[4]),
      crs    = 4326,
      expand = FALSE
    ) +
    facet_wrap(~year, nrow = 1) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", colour = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position  = "bottom",
      legend.direction = "vertical",
      legend.box       = "horizontal",
      legend.text      = element_text(size = 10),
      legend.title     = element_text(size = 10, face = "bold"),
      strip.text       = element_text(size = 12, face = "bold")
    )
}
