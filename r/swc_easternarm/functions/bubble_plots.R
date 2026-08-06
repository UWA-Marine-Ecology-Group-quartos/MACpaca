###
# Project: NESP 4.21 - Australian Marine Parks Natural Values Reporting
# Task:    Bubble plots - species richness and total abundance
# Author:  Annika Leunig
# Date:    August 2026
###

# Combined bubble plot: species richness (top row) and total abundance
bubble_plots <- function(dat,
                         ausc,
                         cwatr,
                         marine_parks_amp,
                         wasanc,
                         prediction_limits,
                         size_range   = c(1, 6),
                         park_linewidth = 0.8,
                         # No-take boundaries are drawn last so they sit over
                         # the top of any zone they share an edge with
                         notake_zones = "National Park Zone|Sanctuary Zone|Marine National Park",
                         notake_linewidth = park_linewidth) {

  ngari_colours <- wasanc %>%
    st_drop_geometry() %>%
    distinct(zone, colour) %>%
    arrange(zone) %>%
    pull(colour)

  # ---- split each park layer into no-take and everything else ----
  # Within a single geom_sf, features draw in row order, so a National Park Zone
  # sitting early in the shapefile gets overdrawn by whatever zone abuts it.
  # Splitting into two layers makes the draw order explicit instead.
  is_notake <- function(x) {
    grepl(notake_zones, x$zone, ignore.case = TRUE)
  }

  wasanc_other  <- wasanc[!is_notake(wasanc), ]
  wasanc_notake <- wasanc[ is_notake(wasanc), ]
  amp_other     <- marine_parks_amp[!is_notake(marine_parks_amp), ]
  amp_notake    <- marine_parks_amp[ is_notake(marine_parks_amp), ]

  # TODO check this picks up the zones you expect - if either _notake object is
  # empty the zone names in the shapefile do not match notake_zones
  message("No-take zones drawn on top | state: ",
          paste(unique(wasanc_notake$zone), collapse = ", "),
          " | AMP: ",
          paste(unique(amp_notake$zone), collapse = ", "))

  # ---- background map layer (drawn first, under the bubbles) ----
  build_background <- function() {
    list(
      geom_sf(data = ausc, fill = "seashell2", colour = "black", linewidth = 0.2)
    )
  }

  # ---- park boundary layer + zone-coloured legends (drawn last, on top) ----
  build_parks <- function(show_park_legend = FALSE) {
    list(
      # State parks - other zones, then no-take over the top. Both layers share
      # one colour scale, so the legend is unaffected by the split
      geom_sf(
        data        = wasanc_other,
        aes(colour  = zone),
        fill        = NA,
        linewidth   = park_linewidth,
        show.legend = show_park_legend
      ),
      geom_sf(
        data        = wasanc_notake,
        aes(colour  = zone),
        fill        = NA,
        linewidth   = notake_linewidth,
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
        override.aes   = list(colour = ngari_colours, fill = NA, linewidth = 1),
        title.theme    = element_text(size = 9, face = "bold")
      )),
      ggnewscale::new_scale_color(),
      # Commonwealth parks - other zones, then National Park Zones over the top
      # of them. The coastal waters limit is drawn last of all, so it stays
      # visible where it runs along an NPZ boundary
      geom_sf(
        data        = amp_other,
        aes(colour  = zone),
        fill        = NA,
        show.legend = show_park_legend,
        linewidth   = park_linewidth
      ),
      geom_sf(
        data        = amp_notake,
        aes(colour  = zone),
        fill        = NA,
        show.legend = show_park_legend,
        linewidth   = notake_linewidth
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
