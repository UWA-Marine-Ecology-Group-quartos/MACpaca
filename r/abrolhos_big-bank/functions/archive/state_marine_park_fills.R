state_marine_park_fills <- function(data) {
  state_cols_all <- c("Sanctuary Zone" = "#bfd054",
                      "Habitat Protection Zone" = "#fffbcc",
                      "General Use Zone" = "#bddde1",
                      "Recreational Use Zone" = "#f4e952",
                      "Special Purpose Zone" = "#c5bcc9")

  scale_fill_manual(values = state_cols_all[unique(data$zone)],
                    name = "State Marine Parks")
}
