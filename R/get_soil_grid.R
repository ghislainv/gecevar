#' Create multilayer Tiff file with 1 environmental variable
#'
#' @description Variable is type of soil.
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
#' | Soilgrids                            | category      |
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

get_soil_grid <- function(extent_latlon, extent_proj, EPSG,
                              country_name, destination, resol=1000,
                              rm_download=FALSE, gisBase=NULL) {
  
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
  
  ##==============================
  ##
  ## Soilgrids
  ##
  ##==============================
  
  dir.create(file.path(destination, "data_raw"), showWarnings=FALSE)
  dir.create(file.path(destination, "data_raw", "soilgrids250_v2_0"), showWarnings=FALSE)
  dir.create(file.path(destination, "data_raw", "soilgrids250_v2_0", "temp"), showWarnings=FALSE)
  
  
  
  
  # soilgrid_base_url <- paste0("https://maps.isric.org/mapserv?map=/map/wrb.map&SERVICE=WCS&VERSION=2.0.1",
  #                             "&REQUEST=GetCoverage&COVERAGEID=MostProbable&FORMAT=image/tiff&SUBSET=long(")
  # soilgrid_end_url <- paste0(".0000)&SUBSETTINGCRS=http://www.opengis.net/def/crs/EPSG/0/4326&OUTPUTCRS=",
  #                            "http://www.opengis.net/def/crs/EPSG/0/4326")
  # # Files need to be downloaded degree per degree for Soilgrids
  # # Loop on longitude
  # for (i in extent_latlon_1d[1]:extent_latlon_1d[3]) {
  #   alpha <- TRUE
  #   # Loop on latitude
  #   for (j in extent_latlon_1d[2]:extent_latlon_1d[4]) {
  #     url=paste0(soilgrid_base_url, i, ".0000,", i + 1, ".0000)&SUBSET=lat(", j, ".0000,", j + 1, soilgrid_end_url)
  #     
  #     if (httr::http_error(url)) {
  #       message("There appears to be a problem reaching the website.")
  #       return(invisible(NULL))
  #     }
  #     
  #     dest=file.path(destination, "data_raw", "soilgrids250_v2_0", "temp", paste0("soilgrids_", j, "_", i, ".tif"))
  #     download.file(url=url, destfile=dest, quiet=TRUE)
  #   }
  # }
  # vrtfile <- file.path(destination, "data_raw", "soilgrids250_v2_0", "soilgrids.vrt")
  # file <- file(file.path(destination, "data_raw", "soilgrids250_v2_0", "temp", "files_list.txt"))
  # writeLines(list.files(file.path(destination, "data_raw", "soilgrids250_v2_0", "temp"),
  #                       pattern="soilgrids_", full.names=TRUE), file)
  # close(file)
  # 
  # # build vrt
  # file_list <- readLines(file.path(destination, "data_raw", "soilgrids250_v2_0", "temp", "files_list.txt"))
  # sf::gdal_utils(util="buildvrt", source=file_list, destination=vrtfile, quiet=TRUE)
 
  # # warp
  # ofile <- file.path(destination, "data_raw", "soilgrids250_v2_0", "soilgrids_res.tif")
  # opts <- glue("-tr {resol} {resol} -te {extent_proj_string} ",
  #              "-s_srs {proj_s} -t_srs {proj_t} -overwrite ",
  #              "-r mode ",
  #              "-ot Byte -of GTiff -co COMPRESS=LZW -co PREDICTOR=2")
  # sf::gdal_utils(util="warp", source=vrtfile, destination=ofile,
  #                options=unlist(strsplit(opts, " ")),
  #                quiet=TRUE)
  
  
  gdalwarp("./crop_roi_igh_r.vrt",
           "./crop_roi_ll_r.vrt", 
           s_srs=igh, 
           t_srs="EPSG:4326", 
           of="VRT",
           overwrite = TRUE)
  
  
  ##=====================================
  ##
  ## Merge environmental variables in one .tif
  ##
  ##=====================================
  
  # Load all rasters
  soilgrids <- terra::rast(file.path(destination, "data_raw", "soilgrids250_v2_0", "soilgrids_res.tif"))
  
  # Create environ raster with all layers                         
  environ <- c(soilgrids)
  layer_names <- c("soil_type")
  names(environ) <- layer_names
 
  # Write to disk
  ofile <- file.path(destination, "data_raw", "environ.tif")
  terra::writeRaster(environ, filename=ofile,
                     gdal=c("COMPRESS=LZW", "PREDICTOR=2"),
                     #NAflag=-2147483648,
                     progress=FALSE, overwrite=TRUE, datatype="INT4S")
  
  # Modify legend for soil_type
  ifile <- file.path(destination, "data_raw", "environ.tif")
  unique_values <- unique(c(values(terra::rast(ifile)[[1]])))
  create_xml_legend(unique_values=unique_values, output_dir=file.path(destination, "data_raw"), file_name="environ")
  
  if (rm_download) {
    unlink(file.path(destination, "data_raw", "soilgrids250_v2_0"), recursive=TRUE)
  }
  
  # Return absolute path of environ.tif 
  return(file.path(destination, "data_raw", "environ.tif"))
  
}

# End
