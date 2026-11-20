#' Create multilayer Tiff file with 14 environmental variables
#'
#' @description Variables are soil texture ??, type of soil,
#'   elevation, slope, aspect,
#'   roughness, distance to sea, protected areas,
#'   distance to roads, distance to cities and town, distance to
#'   rivers and waterbodies.
#'
#' @param extent_latlon vector. First output of `get_aoi_extent()`
#'   function.
#'
#' @param extent_proj vector. Second output of `get_aoi_extent()`
#'   function.
#'
#' @param EPSG int. to consider for this country/area.
#'
#' @param country_name character. country name (in English) which be
#'   use to collect protected areas. This country must be available in
#'   `https://www.protectedplanet.net/en/thematic-areas/wdpa?tab=WDPA`.
#'
#' @param destination character. absolute path where to download files
#'   like `here()` output.
#'
#' @param resol int. Resolution. If in meters, recommended resolutions
#'   are 250m, 500m, 1km, 2km or 5km. The resolution needs to be
#'   carefully chosen. If set too small (e.g. < 250m), raster file
#'   will be too big to fit in memory and R will crash. Default is
#'   1km.
#'
#' @param rm_download boolean. If TRUE remove download files and
#'   folders. Keep only environ.tif in `data_raw` folder, default is
#'   FALSE.
#'
#' @return character. Absolute path to `environ.tif` file.
#'
#' @details environ.tif.aux.xml is an extention of environ.tif, it
#'   allows to classify soilgrid variable with QGIS with
#'   RasterAttributeTable extension. Nevertheless it's cause problems
#'   to open it with `stars` package but not with `terra`. If you have
#'   any problems to open environ.tif, you can remove
#'   environ.tif.aux.xml. This solve all accessibility problems with
#'   `stars` and `terra` packages.
#'
#' Unit of each environ variable :
#'
#' | Name                                 | Unit          |
#' | ------------------------------------ | ------------- |
#' | Elevation                            | m             |
#' | Aspect                               | degrees       |
#' | Roughness                            | m             |
#' | Slope                                | degrees       |
#' | Soil category                        | category      |
#' | Forest                               | binary        |
#' | Distance to forest                   | m             |
#' | Distance sea                         | m             |
#' | Distance road                        | m             |
#' | Distance place                       | m             |
#' | Distance watering place              | m             |
#' | Protected Area (WDPA)                | binary        |
#' | Population density                   | people/km²    |
#' @md
#'
#' @importFrom glue glue
#' @importFrom utils download.file unzip
#' @importFrom RCurl url.exists
#' @import sf
#' @import geojsonsf
#' @import osmextract
#' @import RCurl
#' @import countrycode
#' @import stringr
#' @import httr
#' @import retry
#' @export

get_env_variables <- function(extent_latlon, extent_proj, EPSG,
                              country_name, destination, resol=1000,
                              rm_download=FALSE) {


 forest_path <- get_forest_var(extent_latlon, extent_proj, EPSG,
                       country_name, destination, resol,
                       rm_download)

 cat("Forest variables done. \n")

 osm_path <- get_osm_var(extent_latlon, extent_proj, EPSG,
                               country_name, destination, resol,
                               rm_download)

 cat("OSM variables done. \n")

 pop_path <- get_population(extent_latlon, extent_proj, EPSG,
                         country_name, destination, resol,
                         rm_download)

 cat("Population done. \n")

 wdpa_path <- get_protected_area(extent_latlon, extent_proj, EPSG,
                                 country_name, destination, resol,
                                 rm_download)

 cat("Protected areas done. \n")

 soilgrid_path <- get_soil_grid(extent_latlon, extent_proj, EPSG,
                                country_name, destination, resol,
                                rm_download)

 cat("Soil variables done. \n")

 srtm_path <- get_srtm(extent_latlon, extent_proj, EPSG,
                       country_name, destination, resol,
                       rm_download)

 cat("Elevation and derived variables done. \n")

 distsea_path <- get_dist_to_sea(extent_latlon, extent_proj, EPSG,
                          country_name, destination, resol,
                          rm_download)

 cat("Distance to sea done. \n")

  ##=====================================
  ##
  ## Merge environmental variables in one .tif
  ##
  ##=====================================

  # Load all rasters
  forest <- terra::rast(forest_path)
  osm <- terra::rast(osm_path)
  pop <- terra::rast(pop_path)
  wdpa <- terra::rast(wdpa_path)
  soil_grid <- terra::rast(soilgrid_path)
  srtm <- terra::rast(srtm_path)
  dist_sea <- terra::rast(distsea_path)

  # Create environ raster with all layers

  environ <- c(forest, osm, pop, wdpa, soil_grid, srtm, dist_sea)

  # Write to disk
  ofile <- file.path(destination, "data_raw", "environ.tif")
  terra::writeRaster(environ, filename=ofile,
                     gdal=c("COMPRESS=LZW", "PREDICTOR=2"),
                     progress=FALSE, overwrite=TRUE, datatype="INT4S")

  # Return absolute path of environ.tif
  return(file.path(destination, "data_raw", "environ.tif"))

}

# End
