library(gecevar)
name <- "Reunion"
epsg <- 3337
all_extent <- transform_shp_country_extent(EPSG = epsg,
                                           country_name = name)
extent <- all_extent[1]
extent_latlon <- as.numeric(all_extent[2:5])
clim_path <- get_chelsa_current(extent_latlon = extent_latlon,
                                  extent = extent,
                                  EPSG = epsg,
                                  destination = tempfile(),
                                  resolution = 1000,
                                  rm_download = TRUE)
clim <- terra::rast(clim_path)
ext_out <- terra::ext(clim)
ext <- as.numeric(strsplit(extent[1], " ")[[1]])
names(ext) <- c("xmin", "ymin", "xmax", "ymax")
test_that("get_chelsa_current works", {
  # Layers number and names
  expect_equal(names(clim), c(paste0(rep(c("tasmin","tasmax", "tas", "pr", "clt", "pet_penman"), each=12),
                                     rep(1:12,6)),
                              paste0("bio", 1:19),
                              "cwd_penman", "ndm_penman",
                              paste0("pet_thornthwaite_", 1:12),
                              "cwd_thornthwaite", "ndm_thornthwaite"))
  # Resolution
  expect_equal(terra::res(clim), c(1000,1000))
  # EPSG
  expect_equal(as.numeric(terra::crs(clim, describe=TRUE)$code), epsg)
  # extent (km)
  expect_equal(trunc(ext/1000), trunc(c(ext_out$xmin, ext_out$ymin, ext_out$xmax, ext_out$ymax)/1000))
})

unlink(file.path(clim_path))
