controlplot_benthos <- function(data, taxa, amp_abbrv, state_abbrv,
                                        taxa_label = NULL,
                                        depth_levels = c("Shallow (0 - 30 m)",
                                                         "Mesophotic (30 - 70 m)",
                                                         "Rariphotic (70 - 200 m)")) {

  mean_col <- taxa
  se_col   <- paste0(taxa, "_se")

  if (is.null(taxa_label)) {
    taxa_label <- dplyr::case_when(
      taxa == "seagrass"   ~ "Seagrass",
      taxa == "macroalgae" ~ "Macroalgae",
      taxa == "rock"       ~ "Rock",
      taxa == "sand"       ~ "Sand",
      taxa == "inverts"    ~ "Sessile invertebrates",
      TRUE ~ stringr::str_to_title(taxa)
    )
  }

  req_cols <- c("year", "zone_new", "depth_class", mean_col, se_col)
  if (!all(req_cols %in% names(data))) {
    stop("Data is missing one or more required columns: ",
         paste(setdiff(req_cols, names(data)), collapse = ", "))
  }

  plot_dat <- data %>%
    dplyr::filter(!is.na(.data[[mean_col]])) %>%
    dplyr::mutate(
      depth_class = factor(depth_class, levels = depth_levels),
      zone_new = factor(
        zone_new,
        levels = c(
          paste(amp_abbrv, "HPZ"),
          paste(amp_abbrv, "NPZ (IUCN II)"),
          paste(amp_abbrv, "other zones"),
          paste(state_abbrv, "SZ (IUCN II)"),
          paste(state_abbrv, "other zones")
        )
      )
    )

  if (nrow(plot_dat) == 0) {
    message("No data available to plot for ", taxa_label)
    return(NULL)
  }

  fill_vals <- setNames(
    c("#fff8a3", "#7bbc63", "#b9e6fb", "#bfd054", "#bddde1"),
    c(
      paste(amp_abbrv, "HPZ"),
      paste(amp_abbrv, "NPZ (IUCN II)"),
      paste(amp_abbrv, "other zones"),
      paste(state_abbrv, "SZ (IUCN II)"),
      paste(state_abbrv, "other zones")
    )
  )

  shape_vals <- setNames(
    c(21, 21, 21, 25, 25),
    c(
      paste(amp_abbrv, "HPZ"),
      paste(amp_abbrv, "NPZ (IUCN II)"),
      paste(amp_abbrv, "other zones"),
      paste(state_abbrv, "SZ (IUCN II)"),
      paste(state_abbrv, "other zones")
    )
  )

  p <- ggplot(
    data = plot_dat,
    aes(x = year, y = .data[[mean_col]], fill = zone_new, shape = zone_new)
  ) +
    geom_errorbar(
      aes(
        ymin = pmax(.data[[mean_col]] - .data[[se_col]], 0),
        ymax = .data[[mean_col]] + .data[[se_col]]
      ),
      width = 0.8,
      position = position_dodge(width = 0.6)
    ) +
    geom_point(
      size = 3,
      position = position_dodge(width = 0.6),
      stroke = 0.2,
      color = "black",
      alpha = 0.8
    ) +
    geom_vline(
      xintercept = 2018, # TODO set to your zoning/reference year
      linetype = "dashed",
      color = "black",
      linewidth = 0.5,
      alpha = 0.5
    ) +
    facet_wrap(~depth_class, ncol = 1, scales = "free_y") +
    theme_classic() +
    scale_x_continuous(breaks = c(2014, 2024)) + # TODO set to your survey years
    coord_cartesian(xlim = c(2013, 2025), ylim = c(0, NA)) +
    scale_fill_manual(values = fill_vals, name = "Marine Parks", drop = FALSE) +
    scale_shape_manual(values = shape_vals, name = "Marine Parks", drop = FALSE) +
    labs(x = "Year", y = "Mean predicted probability") +
    theme(
      legend.position = "right",
      strip.background = element_blank(),
      strip.text = element_text(face = "bold")
    )

  return(p)
}
