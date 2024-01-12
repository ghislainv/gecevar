#' Create multilayer Tiff file with 11 environmental variables
#'
#' @description Variables are type of soil, elevation, slope, aspect,
#'   roughness, solar radiation, distance to sea, protected areas,
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
#' @param forest_year int. Forest at the decade chosen. Must be one of
#'   2000, 2010 or 2020, default is 2010.
#'
#' @param gisBase NULL or character. Parameter `gisBase` for
#'   `rgrass::initGRASS()`. The directory path to GRASS binaries and
#'   libraries, containing bin and lib subdirectories among others; if
#'   NULL, system("grass --config path") is tried.
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
#' | Solar radiation                      | Wh.m^{-2}.day |
#' | Soilgrids                            | category      |
#' | Forest                               | binary        |
#' | Distance to forest                   | m             |
#' | Distance sea                         | m             |
#' | Distance road                        | m             |
#' | Distance place                       | m             |
#' | Distance watering place              | m             |
#' | Protected Area (WDPA)                | category      |
#' | Population density                   | people/km²    |
#' @md
#'
#' @importFrom glue glue
#' @importFrom utils download.file unzip
#' @importFrom RCurl url.exists
#' @import sf
#' @import rgrass
#' @import osmextract
#' @import RCurl
#' @import countrycode
#' @import stringr
#' @import httr
#' @import retry
#' @export

get_env_variables <- function(extent_latlon, extent_proj, EPSG,
                              country_name, destination, resol=1000,
                              rm_download=FALSE, forest_year=2010,
                              gisBase=NULL) {

 srad_path <- get_srad(extent_latlon, extent_proj, EPSG,
                       country_name, destination, resol,
                       rm_download, gisBase)

 forest_path <- get_forest_var(extent_latlon, extent_proj, EPSG,
                       country_name, destination, resol,
                       rm_download, forest_year)

 osm_path <- get_osm_var(extent_latlon, extent_proj, EPSG,
                               country_name, destination, resol,
                               rm_download)

 pop_path <- get_population(extent_latlon, extent_proj, EPSG,
                         country_name, destination, resol,
                         rm_download)

 wdpa_path <- get_protected_area(extent_latlon, extent_proj, EPSG,
                                 country_name, destination, resol,
                                 rm_download)

 soilgrid_path <- get_soil_grid(extent_latlon, extent_proj, EPSG,
                                country_name, destination, resol,
                                rm_download)

 srtm_path <- get_srtm(extent_latlon, extent_proj, EPSG,
                       country_name, destination, resol,
                       rm_download)

 distsea_path <- get_dist_to_sea(extent_latlon, extent_proj, EPSG,
                          country_name, destination, resol,
                          rm_download)

  ##=====================================
  ##
  ## Merge environmental variables in one .tif
  ##
  ##=====================================

  # Load all rasters
  srad <- terra::rast(srad_path)
  forest <- terra::rast(forest_path)
  osm <- terra::rast(osm_path)
  pop <- terra::rast(pop_path)
  wdpa <- terra::rast(wdpa_path)
  soil_grid <- terra::rast(soilgrid_path)
  srtm <- terra::rast(srtm_path)
  dist_sea <- terra::rast(distsea_path)

  # Create environ raster with all layers
  environ <- terra::c(srad, forest, osm, pop, wdpa, soil_grid, srtm, dist_sea)

  # Write to disk
  ofile <- file.path(destination, "data_raw", "environ.tif")
  terra::writeRaster(environ, filename=ofile,
                     gdal=c("COMPRESS=LZW", "PREDICTOR=2"),
                     progress=FALSE, overwrite=TRUE, datatype="INT4S")


  # Return absolute path of environ.tif
  return(file.path(destination, "data_raw", "environ.tif"))

}

# End
