#' Create multilayer Tiff file with 1 environmental variable
#'
#' @description Variable is protected areas.
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
#' | Protected Area (WDPA)                | category      |
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
#' @import wdpar
#' @export

get_protected_area <- function(extent_latlon, extent_proj, EPSG,
                              country_name, destination, resol=1000,
                              rm_download=FALSE, gisBase=NULL){

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
  ## WDPA : World Database Protected Areas
  ## International Union for Conservation of Nature
  ## UNEP-WCMC (2022). Protected Area Profile from the World Database of Protected Areas, May 2022.
  ## Available at: www.protectedplanet.net
  ##
  ##=========================

  dir.create(file.path(destination, "data_raw", "WDPA"), showWarnings=FALSE)

  raw_data <- wdpar::wdpa_fetch(country_name, wait = TRUE,
                                download_dir = file.path(destination, "data_raw", "WDPA"))

  ##issue in cleaning data : reprojecting in EPSG:4326 gives the following error :
  ##"Erreur dans st_snap_to_grid.sfc(st_geometry(x), size, origin) :
  ##!isTRUE(st_is_longlat(x)) n'est pas TRUE"

  # clean_data <- wdpar::wdpa_clean(x = raw_data, erase_overlaps = F,
  #               crs = st_crs(proj_s)$proj4string
  #    #,crs = paste("+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs")
  #    #,crs = paste("+proj=cea +datum=WGS84 +ellps=WGS84 +units=m +no_defs")
  #     )

  clean_data <- raw_data
  #from "sf" to SpatVector
  v1 <- terra::vect(clean_data)
  #create an "empty" raster
  r1 <- terra::rast(v1)
  rast_marin <- terra::rasterize(v1, r1, field = as.factor(clean_data$MARINE),
                                 touches = F, background = NA)
  terra::values(rast_marin) <- replace(x = terra::values(rast_marin),
                                       list = is.na(terra::values(rast_marin)),
                                       values = NA)
  ifile <- file.path(destination, "data_raw", "WDPA", "WDPA_raw.tif")
  terra::writeRaster(rast_marin, filename=ifile,
                     gdal=c("COMPRESS=LZW", "PREDICTOR=2"),
                     progress=FALSE, overwrite=TRUE, datatype="INT2S")

  ofile <- file.path(destination, "data_raw", "WDPA", "WDPA_raw_resol.tif")
  gdal_utils_translate(ifile=ifile,
                       ofile=ofile, proj_s = proj_s,
                       extent_gdal_translate)

  opts <- glue("-tr {resol} {resol} -te {extent_proj_string} ",
               "-t_srs {proj_t} -s_srs {proj_s} -overwrite -r near -dstnodata {nodata_Int16} ",
               "-ot Int16 -of GTiff -co COMPRESS=LZW -co PREDICTOR=2")
  sf::gdal_utils(util="warp",
                 source = ofile,
                 destination = file.path(destination, "data_raw", "WDPA", "WDPA_res.tif"),
                 options = unlist(strsplit(opts, " ")))

   ##=====================================
  ##
  ## Merge environmental variables in one .tif
  ##
  ##=====================================

  # Load all rasters
  wdpa <- terra::rast(file.path(destination, "data_raw", "WDPA", "WDPA_res.tif"))

  # Create environ raster with all layers
  environ <- c(wdpa)
  layer_names <- c("wdpa")
  names(environ) <- layer_names

  # Write to disk
  ofile <- file.path(destination, "data_raw", "environ.tif")
  terra::writeRaster(environ, filename=ofile,
                     gdal=c("COMPRESS=LZW", "PREDICTOR=2"),
                     progress=FALSE, overwrite=TRUE, datatype="INT4S")


  if (rm_download) {
    unlink(file.path(destination, "data_raw", "WDPA"), recursive=TRUE)
  }

  # Return absolute path of environ.tif
  return(file.path(destination, "data_raw", "environ.tif"))

}

# End
