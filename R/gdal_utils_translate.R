#' Download data with gdal_translate.
#'
#' Use gdal_translate to download raster data from a Cloud Optimized
#' Geotiffs.
#'
#' @param ifile character. Input file
#' @param ofile character. Output file
#' @param ullr_extent vector. Extent c(ulx, uly, lrx, lry) which
#'   corresponds to c(xmin, ymax, xmax, ymin).
#' @param proj_s character. EPSG code of the input file. Default to
#'   "EPSG:4326".
#' @param overwite boolean. If FALSE, do not overwrite raster if it
#'   exists. Default to TRUE.
#' @param opts vector. Additional GDAL options.
#'
#' @return NULL
#' @keywords internal
#' 
gdal_utils_translate <- function(ifile, ofile, ullr_extent,
                                 proj_s="EPSG:4326",
                                 overwrite=TRUE, opts=NULL) {
  
  if (!file.exists(ofile) | overwrite) {
    opts <- c(opts, "-projwin", ullr_extent, "-projwin_srs", proj_s,
              "-co", "COMPRESS=LZW", "-co", "PREDICTOR=2")
    sf::gdal_utils(util="translate", source=ifile, destination=ofile,
                   options=opts,
                   config_options=c(GTIFF_SRS_SOURCE="EPSG"),
                   quiet=TRUE)
  }
}

# End
