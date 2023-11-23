#' Create multilayer Tiff file with 1 environmental variable
#'
#' @description Variable is distance to sea.
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
#' | Distance sea                         | m             |
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

get_dist_to_sea <- function(extent_latlon, extent_proj, EPSG,
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
  ## SRTM at 90m resolution from
  ## Elevation, slope aspect, roughness
  ##
  ## https://dwtkns.com/srtm/ version 4.1
  ##
  ##==============================
  
  # Compute tiles
  dir.create(file.path(destination, "data_raw", "srtm_v1_4_90m", "temp"), showWarnings=FALSE, recursive=TRUE)
  tiles_srtm <- c(floor(extent_latlon[1] / 5) * 5, ceiling(extent_latlon[2] / 5) * 5,
                  floor(extent_latlon[3] / 5) * 5, ceiling(extent_latlon[4] / 5) * 5)
  lat <- stringr::str_pad(seq(tiles_srtm[1], tiles_srtm[3], 5) / 5 + 37, width=2, pad="0")
  lon <- stringr::str_pad(-seq(tiles_srtm[2], tiles_srtm[4], 5) / 5 + 13, width=2, pad="0")
  tiles <- NULL
  for (i in lon) {
    tiles <- c(tiles, paste(lat, i, sep="_"))
  }
  
  # Donwload tiles and unzip
  for (i in tiles) {
    options(warn=-1)
    dst <- paste0(file.path(destination, "data_raw", "srtm_v1_4_90m", "temp", "srtm_"), i, ".zip")
    url.tile <- paste0("https://srtm.csi.cgiar.org/wp-content/uploads/files/srtm_5x5/TIFF/srtm_", i, ".zip")
    
    if (httr::http_error(url.tile)) {
      message("There appears to be a problem reaching the website.")
      return(invisible(NULL))
    }
    
    download.file(url=url.tile, destfile=dst, quiet=TRUE,
                  method="curl", extra="-k")
    unzip(dst, exdir=file.path(destination, "data_raw", "srtm_v1_4_90m", "temp"), overwrite=TRUE)
  }
  
  # Build vrt
  file_list <- list.files(file.path(destination, "data_raw", "srtm_v1_4_90m", "temp"),
                          pattern="^srtm_.*\\.tif$", full.names=TRUE)
  vrtfile <- file.path(destination, "data_raw", "srtm_v1_4_90m", "temp", "srtm.vrt")
  sf::gdal_utils(util="buildvrt", source=file_list, destination=vrtfile,
                 options=c("-vrtnodata", nodata_Int16),
                 quiet=TRUE)
  
  # Resample with gdal_warp
  ofile <- file.path(destination, "data_raw", "srtm_v1_4_90m", "temp", "elevation.tif")
  res_out <- 90 # resample at 90 m
  # /!\ Creation options (-co) must not be quoted for sf::gdal_utils as in GDAL command)
  opts <- glue("-overwrite -t_srs {proj_t} -tap -r bilinear -dstnodata {nodata_Int16} ",
               "-te {extent_proj_string} -ot Int16 -of GTiff ",
               "-tr {res_out} {res_out} -co COMPRESS=LZW -co PREDICTOR=2")
  sf::gdal_utils(util="warp", source=vrtfile, destination=ofile,
                 options=unlist(strsplit(opts, " ")),
                 quiet=TRUE)
  
  
  # Resolution from res_out to chosen resolution using gdalwarp
  # elevation
  ifile <- file.path(destination, "data_raw", "srtm_v1_4_90m", "temp", "elevation.tif")
  ofile <- file.path(destination, "data_raw", "srtm_v1_4_90m", "elevation_res.tif")
  opts <- glue("-overwrite -r bilinear -tr {resol} {resol} -te {extent_proj_string} ",
               "-ot Int16 -of GTiff -dstnodata {nodata_Int16} ",
               "-co COMPRESS=LZW -co PREDICTOR=2")
  sf::gdal_utils(util="warp", source=ifile, destination=ofile,
                 options=unlist(strsplit(opts, " ")),
                 quiet=TRUE)
  
  
  ##===========================
  ##
  ## Distance to Sea
  ##
  ##===========================
  
  # Land area
  dir.create(file.path(destination, "data_raw", "dist_sea"), showWarnings=FALSE)
  # The following line should be modified as it implies loading the raster in memory.
  seaBool <- (terra::rast(file.path(destination, "data_raw", 
                                    "srtm_v1_4_90m", "elevation_res.tif")) == nodata_Int16)
  #datatype arg missing ?
  terra::writeRaster(seaBool, gdal = c("COMPRESS=LZW", "PREDICTOR=2"), overwrite = TRUE, 
                     filename = file.path(destination, "data_raw", "dist_sea", "sea_res.tif"))
 
  # Distance to sea
  sourcefile <- file.path(destination, "data_raw", "dist_sea", "sea_res.tif")
  destfile <- file.path(destination, "data_raw", "dist_sea", "dist_sea.tif")
  # cmd <- glue("gdal_proximity.py -ot Int32 -of GTiff -nodata {nodata_Int32} ",
  #             "-values {nodata_Int16} -distunits GEO -use_input_nodata NO {sourcefile} {destfile}")
  distance(terra::rast(sourcefile), target = nodata_Int16,  unit = "m", filename = destfile, 
           overwrite = TRUE, gdal=c("COMPRESS=LZW", "PREDICTOR=2"))
  
  
  # Replace 0 with NA
  dist_sea <- terra::rast(file.path(destination, "data_raw", "dist_sea", "dist_sea.tif"))
  values(dist_sea)[values(dist_sea) == 0] <- NA
  ofile <- file.path(destination, "data_raw", "dist_sea", "dist_sea.tif")
  terra::writeRaster(dist_sea, filename=ofile,
                     gdal=c("COMPRESS=LZW","PREDICTOR=2"),
                     progress=FALSE, overwrite=TRUE, datatype="INT4S")

  ##=====================================
  ##
  ## Merge environmental variables in one .tif
  ##
  ##=====================================
  
  # Load all rasters
  dist_sea <- terra::rast(file.path(destination, "data_raw", "dist_sea", "dist_sea.tif"))
 
  # Create environ raster with all layers                         
  environ <- c(dist_sea)
  layer_names <- c("dist_sea")
  names(environ) <- layer_names
  
  # Write to disk
  ofile <- file.path(destination, "data_raw", "environ.tif")
  terra::writeRaster(environ, filename=ofile,
                     gdal=c("COMPRESS=LZW", "PREDICTOR=2"),
                     #NAflag=-2147483648,
                     progress=FALSE, overwrite=TRUE, datatype="INT4S")
  
  if (rm_download) {
    unlink(file.path(destination, "data_raw", "dist_sea"), recursive=TRUE)
    unlink(file.path(destination, "data_raw", "grassdata"), recursive=TRUE)
    unlink(file.path(destination, "data_raw", "soilgrids250_v2_0"), recursive=TRUE)
    unlink(file.path(destination, "data_raw", "srtm_v1_4_90m"), recursive=TRUE)
  }
  
  # Return absolute path of environ.tif 
  return(file.path(destination, "data_raw", "environ.tif"))
  
}

# End
