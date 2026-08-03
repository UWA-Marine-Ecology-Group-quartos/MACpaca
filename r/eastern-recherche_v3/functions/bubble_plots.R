# Combined bubble plot: species richness (top row) and total abundance
bubble_plots <- function(dat,
                         ausc,
                         cwatr,
                         marine_parks_amp,
                         wasanc,
                         prediction_limits,
                         size_range     = c(1, 6),
                         park_linewidth = 0.8,
                         break_width    = 0.4) {

  # Eastern Recherche has no state marine park in the extent, so wasanc can be
  # empty. Drawing an empty sf layer with a manual colour scale errors on the
  # zero-length override.aes, so the state layers are skipped when absent.
  has_state <- !is.null(wasanc) && nrow(wasanc) > 0

  state_colours <- if (has_state) {
    wasanc %>%
      st_drop_geometry() %>%
      distinct(zone, colour) %>%
      arrange(zone) %>%
      pull(colour)
  } else {
    character(0)
  }

  # ---- shared axis breaks (every `break_width` degrees) ----
  x_breaks <- seq(
    floor(prediction_limits[1] / break_width) * break_width,
    ceiling(prediction_limits[2] / break_width) * break_width,
    by = break_width
  )

  y_breaks <- seq(
    floor(prediction_limits[3] / break_width) * break_width,
    ceiling(prediction_limits[4] / break_width) * break_width,
    by = break_width
  )

  # ---- background map layer (drawn first, under the bubbles) ----
  build_background <- function() {
    list(
      geom_sf(data = ausc, fill = "seashell2", colour = "black", linewidth = 0.2)
    )
  }

  # ---- park boundary layer + zone-coloured legends (drawn last, on top) ----
  build_parks <- function(show_park_legend = FALSE) {

    state_layers <- if (has_state) {
      list(
        geom_sf(
          data        = wasanc,
          aes(colour  = zone),
          fill        = NA,
          linewidth   = park_linewidth,
          show.legend = show_park_legend
        ),
        scale_colour_manual(
          name   = "State Marine Park",
          guide  = "legend",
          values = with(wasanc, setNames(colour, zone))
        ),
        guides(colour = guide_legend(
          order          = 2,
          ncol           = 1,
          title.position = "top",
          override.aes   = list(colour = state_colours, fill = NA, linewidth = 1),
          title.theme    = element_text(size = 9, face = "bold")
        )),
        ggnewscale::new_scale_color()
      )
    } else {
      list()
    }

    c(
      state_layers,
      list(
        geom_sf(
          data        = marine_parks_amp,
          aes(colour  = zone),
          fill        = NA,
          show.legend = show_park_legend,
          linewidth   = park_linewidth
        ),
        geom_sf(data = cwatr, colour = "firebrick", linewidth = park_linewidth),
        scale_colour_manual(
          name   = "Australian Marine Parks",
          guide  = "legend",
          values = with(marine_parks_amp, setNames(colour, zone))
        ),
        guides(colour = guide_legend(
          order          = 1,
          ncol           = 2,
          title.position = "top",
          override.aes   = list(fill = NA, linewidth = 1),
          title.theme    = element_text(size = 9, face = "bold")
        )),
        ggnewscale::new_scale_color()
      )
    )
  }

  # ---- helper to build one metric's row (faceted by year) ----
  build_panel <- function(metric_response, legend_title) {

    plot_dat <- dat %>%
      dplyr::filter(response %in% metric_response) %>%
      dplyr::distinct(campaignid, sample, longitude_dd, latitude_dd,
                      status, year, count) %>%
      dplyr::filter(!is.na(count))

    ggplot() +
      build_background() +
      geom_point(
        data = plot_dat,
        aes(x = longitude_dd, y = latitude_dd, size = count, colour = count),
        alpha = 0.85,
        shape = 16
      ) +
      scale_colour_viridis_c(
        name   = legend_title,
        option = "viridis",
        guide  = guide_legend()
      ) +
      scale_size_continuous(
        name  = legend_title,
        range = size_range
      ) +
      ggnewscale::new_scale_color() +
      build_parks(show_park_legend = FALSE) +
      scale_x_continuous(breaks = x_breaks) +
      scale_y_continuous(breaks = y_breaks) +
      coord_sf(
        xlim   = c(prediction_limits[1], prediction_limits[2]),
        ylim   = c(prediction_limits[3], prediction_limits[4]),
        crs    = 4326,
        expand = FALSE
      ) +
      labs(x = NULL, y = NULL) +
      facet_wrap(~year, nrow = 1) +
      theme_bw() +
      theme(
        panel.grid       = element_blank(),
        axis.text        = element_text(size = 7),
        legend.position  = "right",
        strip.text       = element_text(size = 9, face = "bold")
      )
  }

  p_richness  <- build_panel("species_richness", "Species\nrichness")
  p_abundance <- build_panel("total_abundance",  "Total\nabundance\n(MaxN)")

  # ---- stack richness + abundance rows into the 2x2 grid ----
  combined_panels <- cowplot::plot_grid(
    p_richness, p_abundance,
    ncol   = 1,
    align  = "v",
    axis   = "lr",
    labels = c("a)", "b)")
  )

  # ---- attach the shared AMP/state park legend at the bottom ----
  cowplot::plot_grid(
    combined_panels,
    marine_park_legend(),
    ncol        = 1,
    rel_heights = c(1, 0.145)
  )
}
