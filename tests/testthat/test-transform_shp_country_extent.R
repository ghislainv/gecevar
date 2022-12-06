test_that("transform_shp_country_extent works", {
  expect_equal(transform_shp_country_extent(EPSG=4326, country_name="France")[[1]], "-6 41 10 52")
  expect_equal(transform_shp_country_extent(EPSG=2154, country_name="France")[[1]],  "-58254 6014745 1290258 7247373")
  expect_equal(as.numeric(transform_shp_country_extent(EPSG=2154, extent_latlon=c(-6,41,10,52))[2:5]),
               c(-58254,6014745,1290258,7247373))
  expect_equal(as.numeric(transform_shp_country_extent(EPSG=2154, extent_project=c(-58254,6014745,1290258,7247373))[2:5]),
               c(-8,40,12,53))
  # Get France shapefile
  country_name = "France"
  ISO_country_code <- countrycode::countryname(country_name, destination = "iso3c")
  URL <- paste0("https://geodata.ucdavis.edu/gadm/gadm3.6/gpkg/gadm36_", ISO_country_code, "_gpkg.zip")
  tempZip <- tempfile()
  download.file(URL, quiet = TRUE, destfile = tempZip)
  # Unzip
  unzip(tempZip, exdir = paste(getwd(), "gaul", sep = "/"), overwrite = TRUE)
  # Read vector (level 0 for country borders)
  shapefile <- sf::st_read(paste(getwd(), "gaul", paste0("gadm36_", ISO_country_code, ".gpkg"), sep = '/'),
                           layer = paste0("gadm36_", ISO_country_code, "_0"), quiet=TRUE)
  shapefile_path <- paste(getwd(), "gaul", paste0("gadm36_", ISO_country_code, "_0.gpkg"), sep = '/')
  st_write(shapefile, dsn=shapefile_path, quiet=TRUE)

  expect_equal(transform_shp_country_extent(EPSG=4326, shapefile_path=shapefile_path)[[1]], "-5 41 10 51")
  unlink(paste(getwd(), "gaul/", sep = "/"), recursive = TRUE)
  unlink(paste(getwd(), "gaul.zip", sep = "/"), recursive = TRUE)
})


