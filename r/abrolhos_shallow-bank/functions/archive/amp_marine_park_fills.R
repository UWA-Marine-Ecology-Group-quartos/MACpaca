amp_marine_park_fills <- function(data) {
  amp_cols_all <- c("National Park Zone" = "#7bbc63",
                    "Habitat Protection Zone" = "#fff8a3",
                    "Multiple Use Zone" = "#b9e6fb",
                    "Recreational Use Zone" = "#ffb36b",
                    "Sanctuary Zone" = "#f7c0d8",
                    "Special Purpose Zone" = "#6daff4")

  scale_fill_manual(values = amp_cols_all[unique(data$zone)],
                    name = "Australian Marine Parks")
}
