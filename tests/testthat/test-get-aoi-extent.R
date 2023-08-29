test_that("get_aoi_extent works",
{
  expect_equal(get_aoi_extent(EPSG=4326, country_iso="FRA")$extent_latlon,
               c(lonmin=-6, latmin=41, lonmax=10, latmax=52))
  expect_equal(get_aoi_extent(EPSG=2154, country_iso="FRA")$extent_proj,
               c(xmin=-58254, ymin=6014745, xmax=1290258, ymax=7247373))
  expect_equal(get_aoi_extent(EPSG=2154, extent_latlon=c(-6, 41, 10, 52))$extent_proj,
               c(xmin=-58254, ymin=6014745, xmax=1290258, ymax=7247373))
  expect_equal(get_aoi_extent(EPSG=2154,
                              extent_proj=c(-58254, 6014745, 1290258, 7247373))$extent_latlon,
               c(lonmin=-8, latmin=40, lonmax=12, latmax=53))
  # Get France borders
  URL <- paste0("https://geodata.ucdavis.edu/gadm/gadm4.1/gpkg/gadm41_FRA.gpkg")
  dir_tmp <- tempdir()
  dest_file <- file.path(dir_tmp, "gadm41_FRA.gpkg")
  download.file(URL, quiet=TRUE, destfile=dest_file)
  expect_equal(get_aoi_extent(EPSG=4326, vector_file=dest_file)$extent_latlon,
               c(lonmin=-6, latmin=41, lonmax=10, latmax=52))
}
)

# End
