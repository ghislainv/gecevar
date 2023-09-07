library(gecevar)
iso <- "REU"
epsg <- 32740
r <- get_aoi_extent(EPSG = epsg,
                    country_iso = iso,
                    resol = 1000)
extent_latlon <- r$extent_latlon
extent_proj <- r$extent_proj

environ_path <- get_env_variables(extent_latlon = extent_latlon,
                                  extent_proj = extent_proj,
                                  EPSG = epsg,
                                  country_name = "Reunion",
                                  destination = tempdir(),
                                  forest_year = 2010,
                                  resol = 1000,
                                  rm_download = TRUE,
                                  gisBase = NULL)

environ <- terra::rast(environ_path)
ext_environ <- terra::ext(environ)
layer_names <- c("aspect", "elevation", "roughness", "slope", "srad", "soil_type",
               "dist_sea", "dist_road", "dist_place",
               "dist_water", "wdpa", "population", "forest", "dist_forest")

test_that("get_env_variables works", {
  # Layers number and names
  expect_equal(names(environ), layer_names)
  # Resolution
  expect_equal(terra::res(environ), c(1000,1000))
  # EPSG
  expect_equal(as.numeric(terra::crs(environ, describe=TRUE)$code), epsg)
  # extent (km)
  expect_equal(extent_proj, as.vector(ext_environ)[c(1, 3, 2, 4)])
})

unlink(file.path(environ_path))

# End
