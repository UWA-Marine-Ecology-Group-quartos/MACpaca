# =============================================================================
# dem_cross_section: builds the bathymetry profile along a transect, with
# distance measured ALONG the straight-line transect (not distance-to-coast),
# zeroed at the point the transect crosses the coastline. Optionally also
# returns where the transect crosses the coastal waters limit (cwatr), using
# the same distance measure so everything lines up on one x-axis.
# =============================================================================
dem_cross_section <- function(xstart, xend, ystart, yend, maxdist, cwatr = NULL) {
  require(sf)
  require(terra)
  require(tidyverse)
  sf_use_s2(FALSE)

  start_pt <- st_point(c(xstart, ystart)) %>% st_sfc(crs = 4326)

  points <- data.frame(x = c(xstart, xend), y = c(ystart, yend), id = 1)
  tran <- sfheaders::sf_linestring(obj = points, x = "x", y = "y", linestring_id = "id")
  st_crs(tran) <- 4326
  tranv <- vect(tran)

  # Distance along the transect for any point, measured from start_pt (km)
  dist_along_transect <- function(pts_sf) {
    as.numeric(st_distance(pts_sf, start_pt)) / 1000
  }

  # ---- Bathymetry profile ----
  topo <- rast("data/south-west network/spatial/rasters/AusBathyTopo__Australia__2024_250m_MSL_cog.tif")
  names(topo) <- "depth"
  batht <- terra::extract(topo, tranv, xy = TRUE, ID = FALSE)
  bath_cross <- st_as_sf(x = batht, coords = c("x", "y"), crs = 4326, remove = FALSE)

  # ---- Coastline, used to find the zero-point (where transect crosses coast) ----
  aus <- st_read("data/south-west network/spatial/shapefiles/aus-shapefile-w-investigator-stokes.shp") %>%
    dplyr::filter(FEAT_CODE %in% c("mainland")) %>%
    st_transform(4326) %>%
    sf::st_make_valid() %>%
    st_union()
  ausout <- st_cast(aus, "MULTILINESTRING")

  coast_crossing <- st_intersection(tran, ausout) %>% st_cast("POINT")
  coast_crossing_dist <- min(dist_along_transect(coast_crossing))

  bath_df <- bath_cross %>%
    dplyr::mutate(
      land = lengths(st_intersects(bath_cross, aus)) > 0,
      distance.from.coast = dist_along_transect(bath_cross) - coast_crossing_dist
    ) %>%
    as.data.frame() %>%
    dplyr::select(-geometry) %>%
    dplyr::filter(distance.from.coast < maxdist)

  # ---- Coastal waters limit crossings (optional) ----
  cwatr_crossings <- NULL
  if (!is.null(cwatr)) {
    cwatr <- st_transform(cwatr, 4326)
    cross_pts <- st_intersection(tran, st_geometry(cwatr)) %>% st_cast("POINT")

    if (length(cross_pts) > 0) {
      coords <- st_coordinates(cross_pts)
      cwatr_crossings <- data.frame(lon = coords[, "X"], lat = coords[, "Y"])
      cross_pts_sf <- st_as_sf(cwatr_crossings, coords = c("lon", "lat"), crs = 4326, remove = FALSE)

      cwatr_crossings <- cwatr_crossings %>%
        dplyr::mutate(
          distance.from.coast = dist_along_transect(cross_pts_sf) - coast_crossing_dist
        ) %>%
        dplyr::arrange(distance.from.coast)
    }
  }

  list(profile = bath_df, cwatr_crossings = cwatr_crossings, line = tran)
}
