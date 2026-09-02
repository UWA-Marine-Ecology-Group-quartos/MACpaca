predictedreef_plot_multi <- function(dat_list, prediction_limits, se_limits = NULL) {

  yrs <- names(dat_list)

  if (is.null(yrs) || any(yrs == "")) {
    stop("dat_list must be a named list")
  }

  multi_year <- length(dat_list) > 1

  # ------------------------------------------------------------
  # Extract reef probability + SE rasters by year
  # ------------------------------------------------------------
  pred_list <- lapply(dat_list, function(x) x[["p_reef.fit"]])
  se_list   <- lapply(dat_list, function(x) x[["p_reef.se.fit"]])

  # Shared SE limits across years (probability stays fixed 0-1)
  if (is.null(se_limits)) {
    se_vals   <- unlist(lapply(se_list, terra::values))
    se_limits <- range(se_vals, na.rm = TRUE)
  }

  # ------------------------------------------------------------
  # Theme variants (matches dominantbenthos_plot_multi)
  # ------------------------------------------------------------
  theme_left <- theme(
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

  theme_inner <- theme_left +
    theme(
      axis.text.y  = element_blank(),
      axis.ticks.y = element_blank()
    )

  theme_top <- theme(
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()
  )

  ngari_colours <- wasanc %>%
    st_drop_geometry() %>%
    distinct(zone, colour) %>%
    arrange(zone) %>%
    pull(colour)

  # `col` is the grid column: 1 = prediction (keeps the y axis), 2 = SE.
  build_base <- function(col, show_x = TRUE, show_park_legend = TRUE) {

    y_theme <- if (col == 1) theme_left else theme_inner
    x_theme <- if (show_x) theme() else theme_top

    list(
      geom_contour(
        data = bathy,
        aes(x = x, y = y, z = Depth),
        colour    = "black",
        breaks    = c(-30, -70, -200),
        linewidth = 0.1
      ),
      geom_sf(data = ausc, fill = "seashell2", colour = "black", linewidth = 0.2),
      geom_sf(
        data        = wasanc,
        aes(colour  = zone),
        fill        = NA,
        linewidth   = 0.8,
        show.legend = show_park_legend
      ),
      scale_colour_manual(
        name   = "State Marine Park",
        guide  = "legend",
        values = with(wasanc, setNames(colour, zone))
      ),
      guides(colour = guide_legend(
        order        = 2,
        ncol         = 1,
        title.position = "top",
        override.aes = list(colour = ngari_colours, fill = NA, linewidth = 1),
        title.theme  = element_text(size = 9, face = "bold")
      )),
      ggnewscale::new_scale_color(),
      geom_sf(
        data        = marine_parks_amp,
        aes(colour  = zone),
        fill        = NA,
        show.legend = show_park_legend,
        linewidth   = 0.6
      ),
      geom_sf(data = cwatr, colour = "firebrick", linewidth = 0.6),
      scale_colour_manual(
        name   = "Australian Marine Parks",
        guide  = "legend",
        values = with(marine_parks_amp, setNames(colour, zone))
      ),
      guides(colour = guide_legend(
        order        = 1,
        ncol         = 2,
        title.position = "top",
        override.aes = list(fill = NA, linewidth = 1),
        title.theme  = element_text(size = 9, face = "bold")
      )),
      scale_x_continuous(breaks = scales::breaks_width(0.3)),
      scale_y_continuous(breaks = scales::breaks_width(0.04)),
      coord_sf(
        xlim   = c(prediction_limits[1], prediction_limits[2]),
        ylim   = c(prediction_limits[3], prediction_limits[4]),
        crs    = 4326,
        expand = FALSE
      ),
      labs(x = NULL, y = NULL, colour = NULL),
      theme_minimal(),
      y_theme,
      x_theme,
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 11)
      )
    )
  }

  # ------------------------------------------------------------
  # Top row: reef probability panels
  # ------------------------------------------------------------
  p_pred <- lapply(seq_along(yrs), function(i) {

    p <- ggplot() +
      geom_spatraster(data = pred_list[[i]], maxcell = Inf) +
      scale_fill_gradient(
        low      = "white",
        high     = "orange",
        name     = "Reef",
        na.value = "transparent",
        limits   = c(0, 1),
        breaks   = c(0, 0.5, 1),
        labels   = c("0", "0.5", "1"),
        oob      = scales::squish,
        guide    = guide_colorbar(title.hjust = 0, title.vjust = 0.5, label.hjust = 0)
      )

    if (multi_year) {
      p <- p + ggtitle(if (i == 1) "Predicted Reef Probability" else NULL)
    } else {
      p <- p + ggtitle("Predicted Reef Probability")
    }

    p + build_base(1, show_x = !multi_year || i == length(yrs),
                   show_park_legend = FALSE)
  })

  # ------------------------------------------------------------
  # Bottom row: reef SE panels
  # ------------------------------------------------------------
  p_se <- lapply(seq_along(yrs), function(i) {
    ggplot() +
      geom_spatraster(data = se_list[[i]], maxcell = Inf) +
      scale_fill_viridis_c(
        option   = "A",
        na.value = "transparent",
        name     = "Normalised\nSE",
        limits   = se_limits,
        breaks   = c(0, 0.03, 0.06),
        oob      = scales::squish
      ) +
      ggtitle(if (multi_year && i > 1) NULL else "Standard Error") +
      build_base(2, show_x = !multi_year || i == length(yrs),
                 show_park_legend = FALSE)
  })
  # ------------------------------------------------------------
  # Row labels (one per year)
  # ------------------------------------------------------------
  row_label_plot <- function(label) {
    ggplot() +
      theme_void() +
      annotate(
        "text", x = 0.5, y = 0.5,
        label = label, angle = 90,
        fontface = "bold", size = 3.5
      )
  }

  # ------------------------------------------------------------
  # Combine
  # ------------------------------------------------------------
  if (!multi_year) {

    p_out <- wrap_plots(c(p_pred, p_se), nrow = 1)

  } else {

    # The mapped area is a wide, shallow strip, so years run down the page as
    # rows and prediction/SE sit side by side as the two columns. The reverse
    # (a year per column) makes each panel far too short to read.
    year_rows <- lapply(seq_along(yrs), function(i) {
      row_label_plot(yrs[i]) + p_pred[[i]] + p_se[[i]] +
        plot_layout(widths = c(0.06, 1, 1))
    })

    p_out <- wrap_plots(year_rows, ncol = 1)
  }

  p_out <- p_out +
    plot_layout(guides = "collect") &
    theme(
      legend.position      = "bottom",
      legend.direction     = "horizontal",
      legend.box           = "horizontal",
      legend.box.just      = "centre",
      legend.justification = "centre",
      legend.title         = element_text(size = 9, margin = margin(b = 2, r = 3)),
      legend.text          = element_text(size = 8),
      legend.key.height    = unit(0.3, "cm"),
      legend.key.width     = unit(0.35, "cm"),
      legend.spacing.x     = unit(1, "mm"),
      legend.spacing.y     = unit(0.5, "mm"),
      legend.spacing       = unit(0.5, "mm"),
      legend.box.margin    = margin(0, 0, 0, 0),
      panel.spacing        = unit(0.5, "mm"),
      plot.margin           = margin(2, 2, 2, 2, unit = "mm")
    )

  p_out <- cowplot::plot_grid(p_out, marine_park_legend(), ncol = 1, rel_heights = c(1, 0.145))

  return(p_out)
}
