###
# Project: North-west Network Report
# Data:    Marine parks, key ecological features, terrestrial parks, aus outline
# Task:    Create North-west network KEF map
# Author:  Annika Leunig
# Date:    July 2026
# Outputs: 1. North-west network KEF map
###

# Table of contents
#     1.  Set up and load data
#     2.  Recode and reorder KEF
#     3.  Colour palettes
#     4.  Plot inputs
#     5.  Map function
#     6.  FIGURE 1: North-west network KEF map


# ==============================================================================
# 1. LOAD DATA AND SETUP
# ==============================================================================
# Clear environment
rm(list = ls())

# Set the study name and marine park name (for folder structure)
name <- "north-west"
park <- "network"

# Load libraries
library(tidyverse)
library(sf)
library(rnaturalearth)
library(metR)
library(patchwork)
library(terra)
library(tidyterra)
library(ggnewscale)
library(CheckEM)
library(geosphere)

# Load functions
file.sources = list.files(pattern = "*.R", path = "functions/", full.names = T)
sapply(file.sources, source, .GlobalEnv)

# Set cropping extent (buffered slightly beyond the network so the
# terrestrial (seashell2) landmass isn't clipped flush against the panel edges)
e <- ext(106, 133, -28, -11)

# Load necessary spatial files
sf_use_s2(T)

# Standardise every layer on GDA2020 geographic (EPSG:7844)
aus_crs <- 7844

aus  <- st_read("data/north-west network/spatial/shapefiles/STE_2021_AUST_GDA2020.shp") %>%
  st_make_valid() %>% st_transform(aus_crs)
ausc <- st_crop(aus, e)

# KEF is a national dataset - not duplicated per network, so read from the
# shared north network data folder
KEF <- st_read("data/north-west network/spatial/shapefiles/Marine_Key_Ecological_Features.shp") %>%
  st_make_valid() %>%
  st_transform(aus_crs) %>%
  st_crop(e)

capad <- st_read("data/north-west network/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2024_-_Marine.shp") %>%
  st_transform(aus_crs)

marine_parks <- st_read("data/north-west network/spatial/shapefiles/nw-network-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c(# Commonwealth AMPs (North-west Network)
    "Argo-Rowley Terrace", "Ashmore Reef", "Carnarvon Canyon", "Cartier Island",
    "Dampier", "Eighty Mile Beach", "Gascoyne", "Kimberley", "Mermaid Reef",
    "Montebello", "Ningaloo", "Roebuck", "Shark Bay",
    # WA state marine parks (Gascoyne-Pilbara-Kimberley)
    "Hamelin Pool", "Muiron Islands", "Barrow Island", "Thevenard Island",
    "Montebello Islands", "Yawuru Nagulagun / Roebuck Bay", "Yawuru", # IPA
    "Nyangumarta Warrarn", # IPA
    "Bardi Jawi Gaarra", "North Kimberley", "Mayala",
    "Lalang-gaddam", "Rowley Shoals", "Scott Reef")) %>%
  st_transform(aus_crs) %>%
  glimpse()

marine_parks_amp <- marine_parks %>%
  dplyr::filter(epbc %in% "Commonwealth")


terrnp <- st_read("data/north-west network/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2024_-_Terrestrial__.shp") %>%  # Terrestrial reserves
  dplyr::filter(TYPE %in% c("Nature Reserve", "National Park")) %>%
  st_transform(aus_crs)


terr_fills <- scale_fill_manual(values = c("National Park" = "#c4cea6",          # Set the colours for terrestrial parks
                                           "Nature Reserve" = "#e4d0bb"),
                                name = "Terrestrial Parks")

# ==============================================================================
# 2. RECODE AND REORDER KEF
# ==============================================================================
# Shorten the (often very long) official KEF names for a readable legend

KEF$NAME <- dplyr::recode(KEF$NAME,
                          "Ancient coastline at 125 m depth contour"                                            = "Ancient Coastline (125 m)",
                          "Ancient coastline at 90-120m depth"                                                  = "Ancient Coastline (90-120 m)",
                          "Ashmore Reef and Cartier Island and surrounding Commonwealth waters"                 = "Ashmore Reef & Cartier Island",
                          "Canyons linking the Argo Abyssal Plain with the Scott Plateau"                       = "Argo Abyssal Plain Canyons",
                          "Canyons linking the Cuvier Abyssal Plain and the Cape Range Peninsula"                = "Cuvier Abyssal Plain Canyons",
                          "Carbonate bank and terrace system of the Sahul Shelf"                                = "Sahul Shelf",
                          "Carbonate bank and terrace system of the Van Diemen Rise"                            = "Van Diemen Rise",
                          "Commonwealth marine environment surrounding the Houtman Abrolhos Islands"            = "Houtman Abrolhos Islands",
                          "Commonwealth marine environment within and adjacent to the west coast inshore lagoons" = "West Coast Inshore Lagoons",
                          "Commonwealth waters adjacent to Ningaloo Reef"                                       = "Ningaloo Reef",
                          "Continental Slope Demersal Fish Communities"                                         = "Continental Slope Demersal Fish",
                          "Mermaid Reef and Commonwealth waters surrounding Rowley Shoals"                      = "Mermaid Reef & Rowley Shoals",
                          "Perth Canyon and adjacent shelf break, and other west coast canyons"                 = "Perth Canyon & West Coast Canyons",
                          "Seringapatam Reef and Commonwealth waters in the Scott Reef Complex"                 = "Seringapatam & Scott Reef Complex",
                          "Western demersal slope and associated fish communities"                              = "Western Demersal Slope"
)

# ==============================================================================
# 3. COLOUR PALETTES
# ==============================================================================

kef_names <- c(
  "Ancient Coastline (125 m)", "Ashmore Reef & Cartier Island",
  "Argo Abyssal Plain Canyons", "Cuvier Abyssal Plain Canyons",
  "Sahul Shelf", "Ningaloo Reef", "Continental Slope Demersal Fish",
  "Exmouth Plateau", "Glomar Shoals", "Mermaid Reef & Rowley Shoals",
  "Pinnacles of the Bonaparte Basin", "Seringapatam & Scott Reef Complex",
  "Wallaby Saddle", "Western Demersal Slope"
)

kef_colours <- c(
  "Ancient Coastline (125 m)"          = "#FCFF73",
  "Ashmore Reef & Cartier Island"      = "#F8B87A",
  "Argo Abyssal Plain Canyons"         = "#B56DFD",
  "Cuvier Abyssal Plain Canyons"       = "#7BB4FD",
  "Sahul Shelf"                        = "#8D4C0B",
  "Ningaloo Reef"                      = "#F772B6",
  "Continental Slope Demersal Fish"    = "#C97A3D",  # shifted, dup of Ashmore Reef & Cartier Island
  "Exmouth Plateau"                    = "#7E57C2",  # shifted, dup of Argo Abyssal Plain Canyons
  "Glomar Shoals"                      = "#2F6FAD",  # shifted, dup of Cuvier Abyssal Plain Canyons
  "Mermaid Reef & Rowley Shoals"       = "#77FD9D",
  "Pinnacles of the Bonaparte Basin"   = "#F6429A",
  "Seringapatam & Scott Reef Complex"  = "#BBDAFE",
  "Wallaby Saddle"                     = "#8C1108",
  "Western Demersal Slope"             = "#316BD9"
)

# ==============================================================================
# 4. PLOT INPUTS
# ==============================================================================

plot_limits     <- c(109, 130, -26.5, -12.0)
annotation_labels <- NULL

# ==============================================================================
# 5. MAP FUNCTION
# ==============================================================================

network_map <- function(plot_limits, annotation_labels = NULL) {
  require(tidyverse)
  require(tidyterra)
  require(patchwork)
  require(cowplot)

  p1 <- ggplot() +

    # Landmass
    geom_sf(data = ausc, fill = "seashell2", colour = "grey80", linewidth = 0.1) +

    # Key ecological features
    new_scale_fill() +
    geom_sf(data = KEF, aes(fill = NAME), colour = NA, alpha = 1) +
    scale_fill_manual(name   = "Key Ecological Features",
                      guide  = guide_legend(order = 1, ncol = 2,
                                            title.position = "top"),
                      values = kef_colours,
                      limits = kef_names) +

    # Terrestrial parks — no legend
    new_scale_fill() +
    geom_sf(data = terrnp, aes(fill = TYPE), colour = NA, alpha = 0.8) +
    scale_fill_manual(values = c("National Park" = "#c4cea6", "Nature Reserve" = "#e4d0bb"),
                      name   = "Terrestrial Parks",
                      guide  = guide_legend(order = 2, ncol = 1,
                                            title.position = "top")) +

    # Marine park boundaries only
    geom_sf(data = marine_parks_amp,   fill = NA, colour = "lightsteelblue3",  linewidth = 0.3, alpha = 0.5) +

    {if (!is.null(annotation_labels))
      list(
        geom_point(data = annotation_labels, aes(x = x, y = y),
                   shape = 4, size = 1, stroke = 0.5, colour = "black"),
        geom_text(data = annotation_labels, aes(x = x, y = y, label = label),
                  size = 1.65, fontface = "italic", nudge_y = -0.03)
      )} +

    coord_sf(xlim = c(plot_limits[1], plot_limits[2]),
             ylim = c(plot_limits[3], plot_limits[4]),
             crs  = aus_crs) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(legend.key.width    = unit(0.35, "cm"),
          legend.key.height   = unit(0.35, "cm"),
          legend.key.spacing.y = unit(0.05, "cm"),
          legend.spacing.y   = unit(0.01, "cm"),
          legend.text        = element_text(size = 8),
          legend.title       = element_text(size = 10),
          legend.position  = "bottom",
          legend.box       = "horizontal",
          legend.direction = "horizontal",
          panel.grid       = element_blank(),
          panel.background = element_rect(fill = "white", colour = NA),
          plot.background  = element_rect(fill = "white", colour = NA),
          panel.border     = element_rect(colour = "grey80", fill = NA, linewidth = 0.5),
          axis.ticks       = element_line(colour = "grey80", linewidth = 0.3))

  p1.1 <- ggplot(data = aus) +
    geom_sf(fill = "seashell1", colour = "grey90", linewidth = 0.05, alpha = 4/5) +
    geom_sf(data = capad, alpha = 5/6, colour = "grey85", linewidth = 0.02) +
    annotate("rect", xmin = plot_limits[1], xmax = plot_limits[2],
             ymin  = plot_limits[3], ymax = plot_limits[4],
             colour = "grey25", fill = "white", alpha = 1/5, linewidth = 0.2) +
    coord_sf(xlim = c(105, 160), ylim = c(-48, -8), crs = aus_crs) +
    theme_bw() +
    theme(axis.text        = element_blank(),
          axis.ticks       = element_blank(),
          panel.grid.major = element_blank(),
          panel.border     = element_rect(colour = "grey70"))

  legend <- cowplot::get_legend(p1 + theme(
    legend.text      = element_text(size = 7),
    legend.title     = element_text(size = 8),
    legend.key.width  = unit(0.3, "cm"),
    legend.key.height = unit(0.3, "cm"),
    legend.spacing.y  = unit(0.05, "cm"),
    legend.spacing.x  = unit(0.05, "cm"),
    legend.box        = "horizontal"
  ))

  p1_no_legend <- p1 + theme(legend.position = "none",
                             plot.margin    = margin(0, 0, 15, 0))

  (p1_no_legend) / (p1.1 + plot_spacer() + legend + plot_spacer() +
                      plot_layout(widths = c(0.3, 0.02, 1, 0.095))) +
    plot_layout(heights = c(4, 1))
}

# ==============================================================================
# 6. FIGURE 1: North-west network KEF map
# ==============================================================================

network_map(plot_limits)

ggsave(paste(paste0("plots/", park, "/spatial/", name), "network_KEFs.png", sep = "-"),
       dpi = 600, width = 7.5, height = 6.5, bg = "white")

# ==============================================================================
# End of script
# ==============================================================================
