normalise_se <- function(data) {
  if ("p_sand.se.fit" %in% colnames(data)) {
    data <- data %>%
      dplyr::mutate(p_sand.alpha = 1 - (p_sand.se.fit - min(p_sand.se.fit, na.rm = TRUE)) /
                      (max(p_sand.se.fit, na.rm = TRUE) - min(p_sand.se.fit, na.rm = TRUE)))
  }

  if ("p_rock.se.fit" %in% colnames(data)) {
    data <- data %>%
      dplyr::mutate(p_rock.alpha = 1 - (p_rock.se.fit - min(p_rock.se.fit, na.rm = TRUE)) /
                      (max(p_rock.se.fit, na.rm = TRUE) - min(p_rock.se.fit, na.rm = TRUE)))
  }

  if ("p_macro.se.fit" %in% colnames(data)) {
    data <- data %>%
      dplyr::mutate(p_macro.alpha = 1 - (p_macro.se.fit - min(p_macro.se.fit, na.rm = TRUE)) /
                      (max(p_macro.se.fit, na.rm = TRUE) - min(p_macro.se.fit, na.rm = TRUE)))
  }

  if ("p_seagrass.se.fit" %in% colnames(data)) {
    data <- data %>%
      dplyr::mutate(p_seagrass.alpha = 1 - (p_seagrass.se.fit - min(p_seagrass.se.fit, na.rm = TRUE)) /
                      (max(p_seagrass.se.fit, na.rm = TRUE) - min(p_seagrass.se.fit, na.rm = TRUE)))
  }

  if ("p_inverts.se.fit" %in% colnames(data)) {
    data <- data %>%
      dplyr::mutate(p_inverts.alpha = 1 - (p_inverts.se.fit - min(p_inverts.se.fit, na.rm = TRUE)) /
                      (max(p_inverts.se.fit, na.rm = TRUE) - min(p_inverts.se.fit, na.rm = TRUE)))
  }

  if ("p_black.se.fit" %in% colnames(data)) {
    data <- data %>%
      dplyr::mutate(p_black.alpha = 1 - (p_black.se.fit - min(p_black.se.fit, na.rm = TRUE)) /
                      (max(p_black.se.fit, na.rm = TRUE) - min(p_black.se.fit, na.rm = TRUE)))
  }

  return(data)
}
