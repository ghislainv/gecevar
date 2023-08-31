test_that("get_aoi_extent works", {

  # Test with iso code
  r <- get_aoi_extent(EPSG_proj=4326, country_iso="REU", rm_output_dir=FALSE)
  expect_equal(round(r$extent_latlon_orig, 5),
               c(lonmin=55.21625, latmin=-21.38986,
                 lonmax=55.83736, latmax=-20.87181))
  expect_equal(r$extent_latlon,
               c(lonmin=55.21, latmin=-21.39,
                 lonmax=55.84, latmax=-20.87))
  expect_equal(r$extent_proj,
               c(xmin=55.21, ymin=-21.39,
                 xmax=55.84, ymax=-20.87))
  expect_equal(r$gpkg_file, "gadm/gadm41_REU.gpkg")

  # Test with file
  URL <- paste0("https://geodata.ucdavis.edu/gadm/gadm4.1/gpkg/gadm41_REU.gpkg")
  dir_tmp <- tempdir()
  dest_file <- file.path(dir_tmp, "gadm41_REU.gpkg")
  download.file(URL, quiet=TRUE, destfile=dest_file)
  r <- get_aoi_extent(EPSG_proj=32740, vector_file=dest_file)
  expect_equal(round(r$extent_latlon_orig, 5),
               c(lonmin=55.21625, latmin=-21.38986,
                 lonmax=55.83736, latmax=-20.87181))
  expect_equal(r$extent_latlon,
               c(lonmin=55.21, latmin=-21.40,
                 lonmax=55.86, latmax=-20.86))
  expect_equal(r$extent_proj,
               c(xmin=314436, ymin=7633603,
                 xmax=380436, ymax=7691603))
  expect_equal(r$gpkg_file, NULL)

  # Tes with e_latlon
  r <- get_aoi_extent(EPSG_proj=32740, extent_latlon=c(55.21625, -21.38986,
                                                       55.83736, -20.87181))
  expect_equal(round(r$extent_latlon_orig, 5),
               c(lonmin=55.21625, latmin=-21.38986,
                 lonmax=55.83736, latmax=-20.87181))
  expect_equal(r$extent_latlon,
               c(lonmin=55.21, latmin=-21.40,
                 lonmax=55.86, latmax=-20.86))
  expect_equal(r$extent_proj,
               c(xmin=314436, ymin=7633603,
                 xmax=380436, ymax=7691603))
  expect_equal(r$gpkg_file, NULL)
  
  # Tes with e_proj
  r <- get_aoi_extent(EPSG_proj=32740, extent_proj=c(314436, 7633603,
                                                     380436, 7691603))
  expect_equal(round(r$extent_latlon_orig, 5),
               c(lonmin=55.21003, latmin=-21.39584,
                 lonmax=55.85059, latmax=-20.86645))
  expect_equal(r$extent_latlon,
               c(lonmin=55.21, latmin=-21.40,
                 lonmax=55.86, latmax=-20.86))
  expect_equal(r$extent_proj,
               c(xmin=314436, ymin=7633603,
                 xmax=380436, ymax=7691603))
  expect_equal(r$gpkg_file, NULL)

})

# End
