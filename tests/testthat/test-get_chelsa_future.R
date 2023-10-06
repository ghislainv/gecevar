library(gecevar)
iso <- "REU"
epsg <- 32740

ifile <- system.file("extdata", "REU_borders.gpkg", package="gecevar")
r <- get_aoi_extent(EPSG_proj=epsg,
                    vector_file=ifile)

extent_latlon <- r$extent_latlon
extent_proj <- r$extent_proj
resol <- 1000

GCM <- c("GFDL-ESM4")
## GCM <- c("GFDL-ESM4", "IPSL-CM6A-LR",
##          "MPI-ESM1-2-HR", "MRI-ESM2-0",
##          "UKESM1-0-LL")

clim_path <- get_chelsa_future(extent_latlon=extent_latlon,
                               extent_proj=extent_proj,
                               EPSG=epsg,
                               destination=tempdir(),
                               resol=resol,
                               phase="2071-2100",
                               GCM=GCM,
                               SSP=585,
                               rm_download=TRUE)

clim <- terra::rast(clim_path)
ext_out <- terra::ext(clim)
names_clim <- rep(c(paste0(rep(c("tasmin","tasmax", "tas", "pr"), each=12),
                                     rep(1:12,4)),
                              paste0("bio", 1:19),
                              paste0("pet_thornthwaite_", 1:12),
                              "cwd_thornthwaite", "ndm_thornthwaite"), length(GCM))

test_that("get_chelsa_future works", {
  # Layers number and names
  expect_equal(names(clim), names_clim)
  # Resolution
  expect_equal(terra::res(clim), c(1000,1000))
  # EPSG
  expect_equal(as.numeric(terra::crs(clim, describe=TRUE)$code), epsg)
  # extent (km)
  expect_equal(extent_proj, as.vector(ext_out)[c(1, 3, 2, 4)])
})

unlink(file.path(clim_path))

# End
