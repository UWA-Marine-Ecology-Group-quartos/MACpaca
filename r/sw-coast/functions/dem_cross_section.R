dem_cross_section <- function(xstart, xend, ystart, yend, maxdist) {
  require(sf)
  require(terra)
  require(tidyverse)
  sf_use_s2(T)
  points <- data.frame(x = c(xstart, xend),
                       y = c(ystart, yend), id = 1)

  tran <- sfheaders::sf_linestring(obj = points,
                                   x = "x",
                                   y = "y",
                                   linestring_id = "id")
  st_crs(tran) <- 4326
  tranv <- vect(tran)

  topo <- rast("data/south-west network/spatial/rasters/AusBathyTopo__Australia__2024_250m_MSL_cog.tif")
  names(topo) <- "depth"
  batht <- terra::extract(topo, tranv, xy = T, ID = F)

  bath_cross <- st_as_sf(x = batht, coords = c("x", "y"), crs = 4326)

  aus <- st_read("data/south-west network/spatial/shapefiles/aus-shapefile-w-investigator-stokes.shp") %>%
    dplyr::filter(FEAT_CODE %in% "mainland") %>%
    st_transform(4326) %>%
    st_union()
  ausout <- st_cast(aus, "MULTILINESTRING")

  bath_cross %>%
    dplyr::mutate("distance.from.coast" = st_distance(bath_cross, bath_cross$geometry[which.min(st_distance(bath_cross, ausout))]),
                  land = lengths(st_intersects(bath_cross, aus)) > 0,
                  coast = bath_cross$geometry[which.min(st_distance(bath_cross, ausout))]) %>%
    bind_cols(st_coordinates(.)) %>%
    dplyr::rename(from_longitude = X, from_latitude = Y) %>%
    bind_cols(st_coordinates(.$coast)) %>%
    dplyr::rename(to_longitude = X, to_latitude = Y) %>%
    # dplyr::mutate(bearing = calculate_bearing(alon = .$from_longitude,
    #                                           alat = .$from_latitude,
    #                                           blon = .$to_longitude,
    #                                           blat = .$to_latitude)) %>%
    dplyr::mutate(distance.from.coast = ifelse(land == F, distance.from.coast * -1, distance.from.coast)) %>%
    as.data.frame() %>%
    dplyr::select(-geometry) %>%
    dplyr::mutate(distance.from.coast = as.numeric(distance.from.coast/1000)) %>%
    dplyr::filter(distance.from.coast < maxdist) %>%
    glimpse()

}
