fishmetric_plot <- function(metric_name,
                            layer_stub,
                            dat_list,
                            prediction_limits,
                            pred_limits = NULL,
                            se_limits = NULL) {

  yrs <- names(dat_list)

  if (is.null(yrs) || any(yrs == "")) {
    stop("dat_list must be a named list")
  }

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

  # ---- Metric-specific legend titles ----
  fill_title <- dplyr::case_when(
    layer_stub == "cti" ~ "CTI",
    layer_stub == "richness" ~ "Richness",
    layer_stub == "abundance" ~ "Abundance",
    layer_stub == "b20" ~ "B20*",
    TRUE ~ metric_name
  )

  # ---- Theme variants ----
  theme_left <- theme(
    axis.title = element_blank(),
    axis.text = element_text(size = 9),
    axis.ticks = element_line(linewidth = 0.2),
    panel.grid.major = element_line(linewidth = 0.2, colour = "grey85"),
    panel.grid.minor = element_blank(),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.key.height = unit(0.45, "cm"),
    legend.key.width = unit(0.45, "cm"),
    plot.margin = margin(2, 2, 2, 2, unit = "mm")
  )

  theme_inner <- theme_left +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )

  # Top row (no x axis)
  theme_top <- theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

  # ---- Base map builder ----
  build_base <- function(i, show_x = TRUE) {

    y_theme <- if (i == 1) theme_left else theme_inner
    x_theme <- if (show_x) theme() else theme_top

    list(
      geom_contour(
        data = bathy,
        aes(x = x, y = y, z = Depth),
        colour = "black",
        breaks = c(-30, -70, -200),
        linewidth = 0.1
      ),
      geom_sf(data = ausc, fill = "seashell2", colour = "black", linewidth = 0.2),
      ggnewscale::new_scale_color(),
      geom_sf(
        data = marine_parks_amp,
        aes(colour = zone),
        fill = NA,
        show.legend = FALSE,
        linewidth = 0.6
      ),
      geom_sf(data = cwatr, colour = "firebrick", linewidth = 0.6),
      geom_sf(
        data = marine_parks_state,
        aes(colour = zone),
        fill = NA,
        show.legend = FALSE,
        linewidth = 0.6
      ),
      scale_colour_manual(values = with(marine_parks, setNames(colour, zone))),
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
        plot.title = element_text(hjust = 0.5, face = "bold", size = 13)
      )
    )
  }

  # One column per year, prediction row above standard error row. When there is
  # only one year, prediction and SE are placed side by side and titled instead.
  single_panel <- length(yrs) == 1

  # ---- Prediction panels (top row) ----
  p_pred <- lapply(seq_along(yrs), function(i) {
    ggplot() +
      geom_spatraster(data = pred_list[[i]]) +
      scale_fill_viridis_c(
        name = fill_title,
        direction = 1,
        na.value = "transparent",
        limits = pred_limits,
        oob = scales::squish
      ) +
      ggtitle(if (single_panel) "Prediction" else yrs[i]) +
      build_base(i, show_x = single_panel)
  })

  # ---- SE panels (bottom row) ----
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
      ggtitle(if (single_panel) "Standard Error" else NULL) +
      build_base(if (single_panel) 2 else i, show_x = TRUE)
  })

  # ---- Row labels ----
  row_label_plot <- function(label) {
    ggplot() +
      theme_void() +
      annotate(
        "text",
        x = 0.5, y = 0.5,
        label = label,
        angle = 90,
        fontface = "bold",
        size = 5
      )
  }

  # ---- Combine ----
  if (single_panel) {

    p_out <- wrap_plots(c(p_pred, p_se), nrow = 1)

  } else {

    pred_label <- row_label_plot("Prediction")
    se_label   <- row_label_plot("Standard Error")

    pred_row <- pred_label + wrap_plots(p_pred, nrow = 1, guides = "collect") +
      plot_layout(widths = c(0.06, 1))

    se_row <- se_label + wrap_plots(p_se, nrow = 1, guides = "collect") +
      plot_layout(widths = c(0.06, 1))

    p_out <- (pred_row / se_row) +
      plot_layout(heights = c(1, 1))
  }

  p_out <- p_out +
    plot_layout(guides = "collect") &
    theme(
      legend.position = "right",
      panel.spacing = unit(0.5, "mm"),
      plot.margin = margin(2, 2, 2, 2, unit = "mm")
    )

  p_out <- cowplot::plot_grid(p_out, marine_park_legend(), ncol = 1, rel_heights = c(1, 0.145))

  return(p_out)
}
