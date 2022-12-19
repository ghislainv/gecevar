library(gecevar)
name <- "Reunion"
epsg <- 3337
all_extent <- transform_shp_country_extent(EPSG = epsg,
                                           country_name = name)
extent <- all_extent[1]
extent_latlon <- as.numeric(all_extent[2:5])
# for method="curl" in download.file
# sudo apt-get install curl
# Use downloader::download for portability ?
# for Open street Maps
# sudo apt-get install osmctools
environ_path <- get_env_variables(extent_latlon = extent_latlon,
                                  extent = extent,
                                  EPSG = epsg,
                                  country_name = "Reunion",
                                  destination = tempfile(),
                                  forest_year = 2010,
                                  resolution = 1000,
                                  rm_download = TRUE,
                                  gisBase = NULL)
env <- terra::rast(environ_path)
ext_out <- terra::ext(env)
ext <- as.numeric(strsplit(extent[1], " ")[[1]])
names(ext) <- c("xmin", "ymin", "xmax", "ymax")
test_that("get_env_variables works", {
  # Layers number and names
  expect_equal(names(env), c("aspect", "elevation", "roughness", "slope", "srad", "SoilType",
                             "forest", "distanceForest", "dist_sea", "dist_road", "dist_place",
                             "dist_water", "WDPA", "population"))
  # Resolution
  expect_equal(terra::res(env), c(1000,1000))
  # EPSG
  expect_equal(as.numeric(terra::crs(env, describe=TRUE)$code), epsg)
  # extent (km)
  expect_equal(trunc(ext/1000), trunc(c(ext_out$xmin, ext_out$ymin, ext_out$xmax, ext_out$ymax)/1000))
})

unlink(file.path(env_path))
