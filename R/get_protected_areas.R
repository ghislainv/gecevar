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
                              rm_download=FALSE){

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

  dir.create(file.path(destination, "data_raw", "WDPA"), showWarnings=FALSE, recursive=TRUE)

  raw_data <- wdpar::wdpa_fetch(country_name, wait = TRUE)
  clean_data <- wdpar::wdpa_clean(x = raw_data, erase_overlaps = F)

  # create an empty raster at 90m
  extent_rast <- ext(c(extent_proj[1], extent_proj[3], extent_proj[2], extent_proj[4]))
  #ici est-ce que l'extent correspond bien à la proj_t ?
  r1 <- rast(nlyrs=1, crs=proj_t, extent=extent_rast, resolution=90)

  # rasterize wdpa geometries
  wdpa <- terra::rasterize(terra::project(vect(clean_data$geometry), proj_t),
                           r1, field=1, touches=F, background=0)
  names(wdpa) <- "wdpa"

  ifile <- file.path(destination, "data_raw", "WDPA", "WDPA_raw.tif")
  terra::writeRaster(wdpa, filename=ifile,
                     gdal=c("COMPRESS=LZW", "PREDICTOR=2"),
                     progress=FALSE, overwrite=TRUE, datatype="INT2S")

  ofile <- file.path(destination, "data_raw", "WDPA.tif")
  res_out <- resol
  opts <- glue("-overwrite -t_srs {proj_t} -tap -r bilinear -dstnodata {nodata_Int16} ",
               "-te {extent_proj_string} -ot Int16 -of GTiff ",
               "-tr {res_out} {res_out} -co COMPRESS=LZW -co PREDICTOR=2")
  sf::gdal_utils(util="warp", source=ifile, destination=ofile,
                 options=unlist(strsplit(opts, " ")),
                 quiet=TRUE)


  if (rm_download) {
    unlink(file.path(destination, "data_raw", "WDPA"), recursive=TRUE)
  }

  # Return absolute path of .tif
  return(file.path(destination, "data_raw", "WDPA.tif"))

}

# End
