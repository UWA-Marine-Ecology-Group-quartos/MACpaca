individualbenthic_plot <- function(habitat_name,
                                   layer_stub,
                                   dat_list,
                                   prediction_limits,
                                   pred_limits = c(0, 1),
                                   se_limits = NULL) {

  yrs <- names(dat_list)

  if (is.null(yrs) || any(yrs == "")) {
    stop("dat_list must be a named list")
  }

  n_years <- length(dat_list)

  # ---- Extract rasters ----
  pred_list <- lapply(dat_list, function(x) x[[paste0("p_", layer_stub, ".fit")]])
  se_list   <- lapply(dat_list, function(x) x[[paste0("p_", layer_stub, ".se.fit")]])

  # ---- Shared limits ----
  if (is.null(pred_limits)) {
    pred_vals <- unlist(lapply(pred_list, terra::values))
    pred_limits <- range(pred_vals, na.rm = TRUE)
  }

  if (is.null(se_limits)) {
    se_vals <- unlist(lapply(se_list, terra::values))
    se_limits <- range(se_vals, na.rm = TRUE)
  }

  # ---- Split AMP zones so National Park Zones draw on top of the other zones ----
  # cwatr is added after both, so the coastal waters limit still sits above everything
  npz_rows      <- grepl("National Park", marine_parks_amp$zone)
  amp_other     <- marine_parks_amp[!npz_rows, ]
  amp_nationalp <- marine_parks_amp[npz_rows, ]

  # ---- Theme variants ----
  theme_left <- theme(
    axis.title = element_blank(),
    axis.text = element_text(size = 9),
    axis.ticks = element_line(linewidth = 0.2),
    panel.grid.major = element_line(linewidth = 0.2, colour = "grey85"),
    panel.grid.minor = element_blank(),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.key.height = unit(1.2, "lines"),
    legend.key.width = unit(1.2, "lines"),
    plot.margin = margin(2, 2, 2, 2, unit = "mm")
  )

  theme_inner <- theme_left +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )

  # No x axis (kept in case a stacked variant is needed again)
  theme_nox <- theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

  # ---- Base map builder ----
  # show_y is now passed explicitly - with the blocks side by side the first
  # panel of BOTH the prediction and the SE block needs latitude labels
  build_base <- function(show_y = TRUE, show_x = TRUE) {

    y_theme <- if (show_y) theme_left else theme_inner
    x_theme <- if (show_x) theme() else theme_nox

    list(
      geom_contour(
        data = bathy,
        aes(x = x, y = y, z = Depth),
        colour = "black",
        breaks = c(-30, -70, -200),
        linewidth = 0.1
      ),
      geom_sf(data = ausc, fill = "seashell2", colour = "black", linewidth = 0.2),
      geom_sf(
        data = wasanc,
        aes(colour = zone),
        fill = NA,
        show.legend = FALSE,
        linewidth = 0.6
      ),
      scale_colour_manual(values = with(wasanc, setNames(colour, zone))),
      ggnewscale::new_scale_color(),
      # All other AMP zones first...
      if (nrow(amp_other) > 0) {
        geom_sf(
          data = amp_other,
          aes(colour = zone),
          fill = NA,
          show.legend = FALSE,
          linewidth = 0.6
        )
      },
      # ...then National Park Zones on top of them
      if (nrow(amp_nationalp) > 0) {
        geom_sf(
          data = amp_nationalp,
          aes(colour = zone),
          fill = NA,
          show.legend = FALSE,
          linewidth = 0.6
        )
      },
      # Coastal waters limit stays above the National Park Zone outline
      geom_sf(data = cwatr, colour = "firebrick", linewidth = 0.6),
      scale_colour_manual(values = with(marine_parks_amp, setNames(colour, zone))),
      scale_x_continuous(breaks = scales::breaks_width(0.4)),
      scale_y_continuous(breaks = scales::breaks_width(0.4)),
      coord_sf(
        xlim = c(prediction_limits[1], prediction_limits[2]),
        ylim = c(prediction_limits[3], prediction_limits[4]),
        crs = 4326,
        expand = FALSE
      ),
      labs(x = NULL, y = NULL, colour = NULL),
      theme_minimal(),
      y_theme,
      x_theme,
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12)
      )
    )
  }

  # ---- Prediction panels ----
  p_pred <- lapply(seq_along(yrs), function(i) {
    ggplot() +
      geom_spatraster(data = pred_list[[i]]) +
      scale_fill_viridis_c(
        name = "Probability",
        direction = 1,
        na.value = "transparent",
        limits = pred_limits,
        oob = scales::squish
      ) +
      build_base(show_y = (i == 1), show_x = TRUE)
  })

  # ---- SE panels ----
  p_se <- lapply(seq_along(yrs), function(i) {
    ggplot() +
      geom_spatraster(data = se_list[[i]]) +
      scale_fill_viridis_c(
        option = "A",
        name = "SE",
        na.value = "transparent",
        limits = se_limits,
        oob = scales::squish
      ) +
      build_base(show_y = (i == 1), show_x = TRUE)
  })

  # ---- Block headers (horizontal, above each block) ----
  block_label_plot <- function(label) {
    ggplot() +
      theme_void() +
      annotate("text", x = 0.5, y = 0.5, label = label,
               fontface = "bold", size = 4.5)
  }

  # ---- Combine: prediction block and SE block side by side ----
  pred_block <- wrap_plots(p_pred, nrow = 1, guides = "collect")
  se_block   <- wrap_plots(p_se,   nrow = 1, guides = "collect")

  pred_col <- (block_label_plot("Prediction") / pred_block) +
    plot_layout(heights = c(0.06, 1))

  se_col <- (block_label_plot("Standard Error") / se_block) +
    plot_layout(heights = c(0.06, 1))

  p_out <- (pred_col | se_col) +
    plot_layout(widths = c(1, 1), guides = "collect") &
    theme(
      legend.position = "right",
      legend.box.margin = margin(l = 4, unit = "mm"),
      legend.margin = margin(l = 2, r = 4, unit = "mm"),
      panel.spacing = unit(0.5, "mm"),
      plot.margin = margin(2, 8, 2, 2, unit = "mm")
    )

  p_out <- cowplot::plot_grid(p_out, marine_park_legend(), ncol = 1, rel_heights = c(1, 0.25))

  return(p_out)
}
