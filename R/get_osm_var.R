#' Create multilayer Tiff file with 3 environmental variables
#'
#' @description Variables are distance to roads, distance to cities and town, distance to
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
#' | Distance road                        | m             |
#' | Distance place                       | m             |
#' | Distance watering place              | m             |
#' @md
#'
#' @importFrom glue glue
#' @importFrom utils download.file unzip
#' @importFrom RCurl url.exists
#' @import sf
#' @import osmextract
#' @import RCurl
#' @import countrycode
#' @import stringr
#' @import httr
#' @import retry
#' @export

get_osm_var <- function(extent_latlon, extent_proj, EPSG,
                              country_name, destination, resol=1000,
                              rm_download=FALSE) {

  # Round extent_latlon to nearest degree
  extent_latlon_1d <- c(floor(extent_latlon["lonmin"]), floor(extent_latlon["latmin"]),
                        ceiling(extent_latlon["lonmax"]), ceiling(extent_latlon["latmax"]))

  # Extent for gdal_translate
  # /!\ with gdal_translate: c(xmin, ymax, xmax, ymin) corresponding to <ulx> <uly> <lrx> <lry>
  extent_gdal_translate <- c(extent_latlon[1], extent_latlon[4],
                             extent_latlon[3], extent_latlon[2])

  # Transform extent_proj from vector to string
  extent_proj_string <- paste(extent_proj, collapse=" ")

  options(warn=-1)
  dir.create(path=destination, recursive=TRUE, showWarnings=FALSE)
  nodata_Int16 <- nodata_INT2S <- -32768
  nodata_Int32 <- nodata_INT4S <- -2147483648
  proj_s <- "EPSG:4326"
  proj_t <- paste0("EPSG:", EPSG)
  ISO_country_code <- countrycode::countryname(country_name, destination="iso3c")
  options(download.file.method="auto")

  ##=========================
  ##
  ## Open Street Map : distance from cities, roads, rivers
  ##
  ##=========================

  # Download data with osmextract
  dir.create(file.path(destination, "data_raw", "OSM", "temp"), recursive=TRUE, showWarnings=FALSE)
  osm_country <- osmextract::oe_match(country_name, quiet=TRUE)

  if (httr::http_error(osm_country$url)) {
    message("There appears to be a problem reaching the website.")
    return(invisible(NULL))
  }

  osmpbf_file <- osmextract::oe_download(
    file_url=osm_country$url,
    file_size=osm_country$file_size,
    force_download=TRUE,
    max_file_size=osm_country$file_size + 1,
    download_directory=file.path(destination, "data_raw", "OSM", "temp"),
    quiet=TRUE)

  # Features
  feature_name <- c("road", "place", "river", "waterbody")
  layer_osm <- c("lines", "points", "lines", "multipolygons")

  # Where SQL request
  # See example: https://github.com/ropensci/osmextract/blob/master/R/get-network.R
  where_road <- c("(highway IS NOT NULL) AND (highway IN ('motorway', 'trunk', 'primary', 'secondary', 'tertiary'))")
  where_place <- c("(place IS NOT NULL) AND (place IN ('city', 'town', 'village'))")
  where_river <- c("(waterway IS NOT NULL) AND (waterway='river')")
  where_waterbody <- c("(natural IS NOT NULL) AND (natural='water')") # To be modified so that it is more precise
  where_request <- c(where_road, where_place, where_river, where_waterbody)

  # Basic options for sf::gdal_utils(util="vectortranslate")
  # See: https://docs.ropensci.org/osmextract/articles/osmextract.html#vectortranslate_options-argument
  osmconf_ini <- system.file("osmconf.ini", package="osmextract")
  opts_base <- c("-oo", paste0("CONFIG_FILE=", osmconf_ini),
                 "-overwrite", "-f", "GPKG", "-skipfailures", "-progress",
                 "-lco", "GEOMETRY_NAME=geometry")

  # Loop on features
  for (i in 1:length(feature_name)) {

    # File names
    gpkg_file  <- file.path(destination, "data_raw", "OSM" , "temp", paste0(feature_name[i], ".gpkg"))
    gpkg_proj  <- file.path(destination, "data_raw", "OSM" , "temp", paste0(feature_name[i], "_proj.gpkg"))
    tif_file <- file.path(destination, "data_raw", "OSM" , "temp", paste0(feature_name[i], ".tif"))
    dist_file <- file.path(destination, "data_raw", "OSM", paste0(feature_name[i], "_distance", ".tif"))
    dist_file_res <- file.path(destination, "data_raw", "OSM", paste0(feature_name[i], "_distance_res.tif"))

    # Convert .osm.pbf to gpkg
    opts <- c(opts_base, "-where", where_request[i], layer_osm[i])
    sf::gdal_utils(util="vectortranslate", source=osmpbf_file, destination=gpkg_file,
                   options=opts, quiet=TRUE)

    # Reproject
    opts <- c("-overwrite", "-f", "GPKG", "-lco", "ENCODING=UTF-8", "-s_srs", "EPSG:4326", "-t_srs", proj_t)
    sf::gdal_utils(util="vectortranslate", source=gpkg_file, destination=gpkg_proj,
                   options=opts, quiet=TRUE)

    # Rasterize at 100 m
    opts <- c("-te", unlist(strsplit(extent_proj_string, " ")),
              "-tap", "-burn", "1",
              "-co", "COMPRESS=LZW", "-co", "PREDICTOR=2",
              "-ot", "Byte", "-of", "GTiff", "-a_nodata", "255", "-a_srs", proj_t,
              "-tr", "100", "100")
    sf::gdal_utils(util="rasterize", source=gpkg_proj, destination=tif_file,
                   options=opts, quiet=TRUE)

    # Computes distances
    # To be improved with exclude argument and land area to avoid computing distance on every pixels.
    r <- terra::rast(tif_file)
    dist <- terra::distance(r, unit="m", filename=dist_file,
                            gdal=c("COMPRESS=LZW", "PREDICTOR=2"),
                            progress=0, overwrite=TRUE, datatype="INT4S")

    # Resample at the requested resolution computing the average
    opts <- glue("-overwrite -t_srs {proj_t} -dstnodata {nodata_Int32} ",
                 "-r average -tr {resol} {resol} -te {extent_proj_string} ",
                 "-ot Int32 -of GTiff ",
                 "-co COMPRESS=LZW -co PREDICTOR=2")
    sf::gdal_utils(util="warp", source=dist_file, destination=dist_file_res,
                   options=unlist(strsplit(opts, " ")),
                   quiet=TRUE)
  }

  # Minimal distance to water (to be improved, see average distance computation in preceding step...).
  water_files <- list.files(file.path(destination, "data_raw","OSM"),
                            pattern="^(river|waterbody)_distance_res\\.tif$", full.names=TRUE)
  water_dist <- terra::rast(water_files[1])
  values(water_dist) <- pmin(values(terra::rast(water_files[1])),
                             values(terra::rast(water_files[2])))
  ##datatype arg missing ?
  terra::writeRaster(water_dist, gdal=c("COMPRESS=LZW","PREDICTOR=2"),
                     file.path(destination, "data_raw", "OSM", "water_distance_res.tif"),
                     progress=FALSE, overwrite=TRUE)

  ##=====================================
  ##
  ## Merge environmental variables in one .tif
  ##
  ##=====================================

  # Load all rasters
  dist_road <- terra::rast(file.path(destination, "data_raw", "OSM", "road_distance_res.tif"))
  dist_place <- terra::rast(file.path(destination, "data_raw", "OSM", "place_distance_res.tif"))
  dist_water <- terra::rast(file.path(destination, "data_raw", "OSM", "water_distance_res.tif"))

  # Create environ raster with all layers
  environ <- c(dist_road, dist_place, dist_water)
  layer_names <- c("dist_road", "dist_place", "dist_water")
  names(environ) <- layer_names

  # Write to disk
  ofile <- file.path(destination, "data_raw", "osm_variables.tif")
  terra::writeRaster(environ, filename=ofile,
                     gdal=c("COMPRESS=LZW", "PREDICTOR=2"),
                     progress=FALSE, overwrite=TRUE, datatype="INT4S")

  if (rm_download) {
    unlink(file.path(destination, "data_raw", "OSM"), recursive=TRUE)
  }

  # Return absolute path of environ.tif
  return(file.path(destination, "data_raw", "osm_variables.tif"))

}

# End
