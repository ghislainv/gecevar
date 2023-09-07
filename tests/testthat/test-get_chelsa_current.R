library(gecevar)
iso <- "REU"
epsg <- 32740
r <- get_aoi_extent(EPSG_proj=epsg,
                    country_iso=iso)
extent_latlon <- r$extent_latlon
extent_proj <- r$extent_proj
resol <- 1000

clim_path <- get_chelsa_current(extent_latlon=extent_latlon,
                                extent_proj=extent_proj,
                                EPSG_proj=epsg,
                                destination=tempdir(),
                                resol=resol,
                                rm_download=TRUE)

clim <- terra::rast(clim_path)
ext_clim <- terra::ext(clim)
names_clim <- c(paste0(rep(c("tasmin","tasmax", "tas", "pr", "clt", "pet_penman"), each=12),
                                     rep(1:12,6)),
                              paste0("bio", 1:19),
                              "cwd_penman", "ndm_penman",
                              paste0("pet_thornthwaite_", 1:12),
                              "cwd_thornthwaite", "ndm_thornthwaite")

test_that("get_chelsa_current works", {
  # Layers number and names
  expect_equal(names(clim), names_clim)
  # Resolution
  expect_equal(terra::res(clim), c(1000,1000))
  # EPSG
  expect_equal(as.numeric(terra::crs(clim, describe=TRUE)$code), epsg)
  # extent (km)
  expect_equal(extent_proj, as.vector(ext_clim)[c(1, 3, 2, 4)])
})

unlink(file.path(clim_path))

# End
