#' Create multilayer Tiff file with 2 environmental variables
#'
#' @description Variables are forest and distance to forest.
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
#' | Forest                               | binary        |
#' | Distance to forest                   | m             |
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

get_forest_var <- function(extent_latlon, extent_proj, EPSG,
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

  ##===========================
  ##
  ## Forest cover
  ## https://forobs.jrc.ec.europa.eu/GFC/v4
  ##
  ##===========================

  dir.create(file.path(destination, "data_raw", "forest_var", "temp"), showWarnings=FALSE, recursive=TRUE)

  # Download raw data
  forest_url <- "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/FOREST/GFC2020/LATEST/single-cog/"
  if (httr::http_error(forest_url)) {
    message("There appears to be a problem reaching the website.")
    return(invisible(NULL))
  }
  ifile <- glue::glue("/vsicurl/{forest_url}","JRC_GFC2020_V4_COG.tif")
  ofile <- file.path(destination, "data_raw", "forest_var", "temp","forest_raw.tif")
  gdal_utils_translate(ifile, ofile, extent_gdal_translate)

  # Reproject at 10m resolution
  res1 <- 10
  ofile_reproj <- file.path(destination, "data_raw", "forest_var", "temp", "forest_raw_m.tif")
  opts <- glue("-overwrite -t_srs {proj_t} -dstnodata 255 ",
               "-r near -tr {res1} {res1} -te {extent_proj_string} ",
               "-ot Byte -of GTiff ",
               "-co COMPRESS=LZW -co PREDICTOR=2")
  sf::gdal_utils(util="warp", source=ofile, destination=ofile_reproj,
                 options=unlist(strsplit(opts, " ")),
                 quiet=TRUE)

  # Sum at user resolution
  ofile_sum <- file.path(destination, "data_raw", "forest_var", "temp", "forest_sum.tif")
  opts <- glue("-overwrite -t_srs {proj_t} -dstnodata {nodata_Int32} ",
               "-r sum -tr {resol} {resol} -te {extent_proj_string} ",
               "-ot Int32 -of GTiff ",
               "-co COMPRESS=LZW -co PREDICTOR=2")
  sf::gdal_utils(util="warp", source=ofile_reproj, destination=ofile_sum,
                 options=unlist(strsplit(opts, " ")),
                 quiet=TRUE)

  # Compute percentage of forest cover and save on disk
  sum_raster <- terra::rast(ofile_sum)
  facteur <- (resol / res1)^2
  ofile <- file.path(destination, "data_raw", "forest_var", "forest_percent.tif")
  pct_raster <- terra::app(sum_raster, fun=function(x) round((x/facteur)*100),
                           filename=ofile, overwrite=TRUE,
                           wopt=list(datatype="INT1U", NAflag=255,
                                     gdal=c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES"))
  )

  ##===========================
  ##
  ## Distance to forest
  ##
  ##===========================

  # Compute distance to forest from reproj 10m resol forest raster
  sourcefile <- file.path(destination, "data_raw", "forest_var", "temp", "forest_raw_m.tif")
  destfile <- file.path(destination, "data_raw", "forest_var", "temp", "dist_forest_raw.tif")
  cmd <- glue("gdal_proximity.py {sourcefile} {destfile} ",
              "-ot Int32 -of GTiff -nodata {nodata_Int32} ",
              "-values {1} -distunits GEO -use_input_nodata NO")
  system(cmd, ignore.stdout=TRUE, ignore.stderr=TRUE)

  # Set resol and save to disk
  ofile <- file.path(destination, "data_raw", "forest_var", "dist_to_forest.tif")
  opts <- glue("-overwrite -t_srs {proj_t} -dstnodata {nodata_Int32} ",
               "-r average -tr {resol} {resol} -te {extent_proj_string} ",
               "-ot Int32 -of GTiff ",
               "-co COMPRESS=LZW -co PREDICTOR=2")
  sf::gdal_utils(util="warp", source=destfile, destination=ofile,
                 options=unlist(strsplit(opts, " ")),
                 quiet=TRUE)

  ##=====================================
  ##
  ## Merge environmental variables in one .tif
  ##
  ##=====================================

  # Load all rasters
  forest_percent <- terra::rast(file.path(destination, "data_raw", "forest_var", "forest_percent.tif"))
  dist_forest <- terra::rast(file.path(destination, "data_raw", "forest_var", "dist_to_forest.tif"))
  environ <- c(forest_percent, dist_forest)
  layer_names <- c("forest_percent", "dist_forest")
  names(environ) <- layer_names

  # Write to disk
  ofile <- file.path(destination, "data_raw", "forest_variables.tif")
  terra::writeRaster(environ, filename=ofile,
                     gdal=c("COMPRESS=LZW", "PREDICTOR=2"),
                     progress=FALSE, overwrite=TRUE, datatype="INT4S")

  if (rm_download) {
    unlink(file.path(destination, "data_raw", "dist_forest"), recursive=TRUE)
  }

  # Return absolute path of .tif
  return(file.path(destination, "data_raw", "forest_variables.tif"))

}

# End
