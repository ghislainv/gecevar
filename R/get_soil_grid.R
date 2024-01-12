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
#' @import osmextract
#' @import RCurl
#' @import countrycode
#' @import stringr
#' @import httr
#' @import retry
#' @export

get_soil_grid <- function(extent_latlon, extent_proj, EPSG,
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


  dir.create(file.path(destination, "data_raw"), showWarnings=FALSE)
  dir.create(file.path(destination, "data_raw", "soilgrids250_v2_0"), showWarnings=FALSE)
  sg_url <- "https://files.isric.org/soilgrids/latest/data/"
  if (httr::http_error(sg_url)) {
    message("There appears to be a problem reaching the website.")
    return(invisible(NULL))
  }

  ##==============================
  ##
  ## Soil texture
  ##
  ##==============================
  # projection of vrt files Interrupted Goode Homolosine
  proj_vrt_init <- '+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs'

  # clay
  if (httr::http_error(paste0(sg_url,'clay/clay_0-5cm_mean.vrt'))) {
    message("There appears to be a problem reaching the website.")
    return(invisible(NULL))
  }

  ifile <- paste0(sg_url,'clay/clay_0-5cm_mean.vrt')
  ofile_vrt <- file.path(destination, "data_raw", "soilgrids250_v2_0", "soilgrids_res.vrt")
  ofile <- file.path(destination, "data_raw", "soilgrids250_v2_0", "soilgrids_res.tif")

  # warp to vrt with final resol/crop/proj
  opts <- glue("-tr {resol} {resol} -te {extent_proj_string} ",
               "-s_srs {proj_vrt_init} -t_srs {proj_t} -overwrite ",
               "-r bilinear -dstnodata {nodata_Int16} ",
               "-ot Int16 -of VRT -co COMPRESS=LZW -co PREDICTOR=2")
  sf::gdal_utils(util="warp", source=ifile, destination=ofile_vrt,
                 options=unlist(strsplit(opts, " ")),
                 quiet=TRUE)
  #here : rast(ofile_vrt) has no values at all + values(rast(ofile_vrt)) returns
  #"Erreur : [readValues] cannot read values"

  #& still needs to be converted to tif
  gdal_utils_translate(ifile = ofile_vrt, ofile = ofile,
                       proj_s = proj_t, ullr_extent = extent_gdal_translate,
                       opts = c("-of", "GTiff"))
  #returns an error "an error occured"


  # # sand
  # if (httr::http_error(paste0(sg_url,'sand/sand_0-5cm_mean.vrt'))) {
  #   message("There appears to be a problem reaching the website.")
  #   return(invisible(NULL))
  # }
  #
  #
  # # silt
  # if (httr::http_error(paste0(sg_url,'silt/silt_0-5cm_mean.vrt'))) {
  #   message("There appears to be a problem reaching the website.")
  #   return(invisible(NULL))
  # }



  ##==============================
  ##
  ## Soil category
  ##
  ##==============================

  if (httr::http_error(paste0(sg_url,'wrb/MostProbable.vrt'))) {
    message("There appears to be a problem reaching the website.")
    return(invisible(NULL))
  }

  ifile <- paste0(sg_url,'wrb/MostProbable.vrt')
  vrt_crop_file <- file.path(destination, "data_raw", "soilgrids250_v2_0", "soilcategory_crop.vrt")

   gdal_utils_translate(ifile=ifile,
                        ofile=vrt_crop_file, proj_s = proj_s,
                        extent_gdal_translate)
   # here : rast(ofile) values -> all NAs and values() returns the following error :
   #"Erreur : [readValues] cannot read values"
   # -> cropped but resol and proj still need to be updated & then tif conversion



  ##=====================================
  ##
  ## Merge environmental variables in one .tif
  ##
  ##=====================================

  # Load all rasters
  soilsand <- terra::rast(file.path(destination, "data_raw", "soilgrids250_v2_0", "soilsand_res.tif"))
  soilsilt <- terra::rast(file.path(destination, "data_raw", "soilgrids250_v2_0", "soilsilt_res.tif"))
  soilclay <- terra::rast(file.path(destination, "data_raw", "soilgrids250_v2_0", "soilclay_res.tif"))
  #soiltype

  # Create environ raster with all layers
  environ <- c(soilsand, soilsilt, soilclay)
  layer_names <- c("soil_sand", "soil_silt", "soil_clay")
  names(environ) <- layer_names

  # Write to disk
  ofile <- file.path(destination, "data_raw", "soil_variables.tif")
  terra::writeRaster(environ, filename=ofile,
                     gdal=c("COMPRESS=LZW", "PREDICTOR=2"),
                     progress=FALSE, overwrite=TRUE, datatype="INT2S")

  ## Modify legend for soil_type
  #ifile <- file.path(destination, "data_raw", "environ.tif")
  #unique_values <- unique(c(values(terra::rast(ifile)[[1]])))
  #create_xml_legend(unique_values=unique_values, output_dir=file.path(destination, "data_raw"), file_name="environ")

  if (rm_download) {
    unlink(file.path(destination, "data_raw", "soilgrids250_v2_0"), recursive=TRUE)
  }

  # Return absolute path of environ.tif
  return(file.path(destination, "data_raw", "soil_variables.tif"))

}

# End
