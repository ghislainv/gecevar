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
#' @export

get_env_variables <- function(extent_latlon, extent_proj, EPSG,
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
  soilgrid_base_url <- paste0("https://maps.isric.org/mapserv?map=/map/wrb.map&SERVICE=WCS&VERSION=2.0.1",
                              "&REQUEST=GetCoverage&COVERAGEID=MostProbable&FORMAT=image/tiff&SUBSET=long(")
  soilgrid_end_url <- paste0(".0000)&SUBSETTINGCRS=http://www.opengis.net/def/crs/EPSG/0/4326&OUTPUTCRS=",
                             "http://www.opengis.net/def/crs/EPSG/0/4326")
  # Files need to be downloaded degree per degree for Soilgrids
  # Loop on longitude
  for (i in extent_latlon_1d[1]:extent_latlon_1d[3]) {
    alpha <- TRUE
    # Loop on latitude
    for (j in extent_latlon_1d[2]:extent_latlon_1d[4]) {
      url=paste0(soilgrid_base_url, i, ".0000,", i + 1, ".0000)&SUBSET=lat(", j, ".0000,", j + 1, soilgrid_end_url)
      
      if (httr::http_error(url)) {
        message("There appears to be a problem reaching the website.")
        return(invisible(NULL))
      }
      
      dest=file.path(destination, "data_raw", "soilgrids250_v2_0", "temp", paste0("soilgrids_", j, "_", i, ".tif"))
      download.file(url=url, destfile=dest, quiet=TRUE)
    }
  }
  vrtfile <- file.path(destination, "data_raw", "soilgrids250_v2_0", "soilgrids.vrt")
  file <- file(file.path(destination, "data_raw", "soilgrids250_v2_0", "temp", "files_list.txt"))
  writeLines(list.files(file.path(destination, "data_raw", "soilgrids250_v2_0", "temp"),
                        pattern="soilgrids_", full.names=TRUE), file)
  close(file)
  
  # build vrt
  file_list <- readLines(file.path(destination, "data_raw", "soilgrids250_v2_0", "temp", "files_list.txt"))
  sf::gdal_utils(util="buildvrt", source=file_list, destination=vrtfile, quiet=TRUE)
  
  # warp
  ofile <- file.path(destination, "data_raw", "soilgrids250_v2_0", "soilgrids_res.tif")
  opts <- glue("-tr {resol} {resol} -te {extent_proj_string} ",
               "-s_srs {proj_s} -t_srs {proj_t} -overwrite ",
               "-r mode ",
               "-ot Byte -of GTiff -co COMPRESS=LZW -co PREDICTOR=2")
  sf::gdal_utils(util="warp", source=vrtfile, destination=ofile,
                 options=unlist(strsplit(opts, " ")),
                 quiet=TRUE)
  
  ##==============================
  ##
  ## SRTM at 90m resolution from
  ## Elevation, slope aspect, roughness
  ##
  ## https://dwtkns.com/srtm/ version 4.1
  ##
  ##==============================
  
  # Compute tiles
  dir.create(file.path(destination, "data_raw", "srtm_v1_4_90m"), showWarnings=FALSE)
  dir.create(file.path(destination, "data_raw", "srtm_v1_4_90m", "temp"), showWarnings=FALSE)
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
  
  ## Compute slope, aspect and roughness using gdaldem
  # compute slope
  ifile <- file.path(destination, "data_raw", "srtm_v1_4_90m", "temp", "elevation.tif")
  ofile <- file.path(destination, "data_raw", "srtm_v1_4_90m", "temp", "slope.tif")
  opts <- glue("-co COMPRESS=LZW -co PREDICTOR=2 -compute_edges")
  sf::gdal_utils(util="demprocessing", processing="slope", source=ifile, destination=ofile,
                 options=unlist(strsplit(opts, " ")),
                 quiet=TRUE)
  
  # compute aspect
  ofile <- file.path(destination, "data_raw", "srtm_v1_4_90m", "temp", "aspect.tif")
  opts <- glue("-co COMPRESS=LZW -co PREDICTOR=2 -compute_edges")
  sf::gdal_utils(util="demprocessing", processing="aspect", source=ifile, destination=ofile,
                 options=unlist(strsplit(opts, " ")),
                 quiet=TRUE)
  
  # compute roughness
  ofile <- file.path(destination, "data_raw", "srtm_v1_4_90m", "temp", "roughness.tif")
  opts <- glue("-co COMPRESS=LZW -co PREDICTOR=2 -compute_edges")
  sf::gdal_utils(util="demprocessing", processing="roughness", source=ifile, destination=ofile,
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
  
  # aspect
  ifile <- file.path(destination, "data_raw", "srtm_v1_4_90m", "temp", "aspect.tif")
  ofile <-file.path(destination, "data_raw", "srtm_v1_4_90m", "aspect_res.tif")
  sf::gdal_utils(util="warp", source=ifile, destination=ofile,
                 options=unlist(strsplit(opts, " ")),
                 quiet=TRUE)
  
  # slope
  ifile <- file.path(destination, "data_raw", "srtm_v1_4_90m", "temp", "slope.tif")
  ofile <- file.path(destination, "data_raw", "srtm_v1_4_90m", "slope_res.tif")
  sf::gdal_utils(util="warp", source=ifile, destination=ofile,
                 options=unlist(strsplit(opts, " ")),
                 quiet=TRUE)
  
  # roughness
  ifile <- file.path(destination, "data_raw", "srtm_v1_4_90m", "temp", "roughness.tif")
  ofile <- file.path(destination, "data_raw", "srtm_v1_4_90m", "roughness_res.tif")
  sf::gdal_utils(util="warp", source=ifile, destination=ofile,
                 options=unlist(strsplit(opts, " ")),
                 quiet=TRUE)

  
  ##=========================
  ##
  ## WDPA : World Database Protected Areas
  ## International Union for Conservation of Nature
  ## UNEP-WCMC (2022). Protected Area Profile from the World Database of Protected Areas, May 2022.
  ## Available at: www.protectedplanet.net
  ##
  ##=========================
  
  ## /!\ This needs to be rewritten using a wdpa token and something like worlpa https://frbcesab.github.io/worldpa/
  
  dir.create(file.path(destination, "data_raw", "WDPA"), showWarnings=FALSE)
  
  # Temporary
  ifile <- file.path(destination, "data_raw", "soilgrids250_v2_0", "soilgrids_res.tif")
  ofile <- file.path(destination, "data_raw", "WDPA", "WDPA_resBool.tif")
  file.copy(ifile, ofile)
  
  
  ##=====================================
  ##
  ## Merge environmental variables in one .tif
  ##
  ##=====================================
  
  # Load all rasters
  aspect <- terra::rast(file.path(destination, "data_raw", "srtm_v1_4_90m", "aspect_res.tif"))
  elevation <- terra::rast(file.path(destination, "data_raw", "srtm_v1_4_90m", "elevation_res.tif"))
  roughness <- terra::rast(file.path(destination, "data_raw", "srtm_v1_4_90m", "roughness_res.tif"))
  slope <- terra::rast(file.path(destination, "data_raw", "srtm_v1_4_90m", "slope_res.tif"))
  srad <- terra::rast(file.path(destination, "data_raw", "srtm_v1_4_90m", "srad_res.tif"))
  soilgrids <- terra::rast(file.path(destination, "data_raw", "soilgrids250_v2_0", "soilgrids_res.tif"))
  dist_sea <- terra::rast(file.path(destination, "data_raw", "dist_sea", "dist_sea.tif"))
  dist_road <- terra::rast(file.path(destination, "data_raw", "OSM", "road_distance_res.tif"))
  dist_place <- terra::rast(file.path(destination, "data_raw", "OSM", "place_distance_res.tif"))
  dist_water <- terra::rast(file.path(destination, "data_raw", "OSM", "water_distance_res.tif"))
  wdpa <- terra::rast(file.path(destination, "data_raw", "WDPA", "WDPA_resBool.tif"))
  population <- terra::rast(file.path(destination, "data_raw", "world_pop", paste0(ISO_country_code, "_pop_res.tif")))
  
  # Create environ raster with all layers                         
  environ <- c(aspect, elevation, roughness, slope, srad, soilgrids,
               dist_sea, dist_road, dist_place, dist_water, wdpa, population)
  layer_names <- c("aspect", "elevation", "roughness", "slope", "srad", "soil_type",
                   "dist_sea", "dist_road", "dist_place", "dist_water", "wdpa", "population")
  names(environ) <- layer_names
  
  # Update if forest
  if (forest) {
    forest <- terra::rast(file.path(destination, "data_raw", "forestatrisk", "forest.tif"))
    dist_forest <- terra::rast(file.path(destination, "data_raw", "dist_forest", "dist_forest.tif"))
    environ <- c(environ, forest, dist_forest)
    names(environ) <- c(layer_names, "forest", "dist_forest")
  }
  
  # Write to disk
  ofile <- file.path(destination, "data_raw", "environ.tif")
  terra::writeRaster(environ, filename=ofile,
                     gdal=c("COMPRESS=LZW", "PREDICTOR=2"),
                     #NAflag=-2147483648,
                     progress=FALSE, overwrite=TRUE, datatype="INT4S")
  
  # Modify legend for soil_type
  ifile <- file.path(destination, "data_raw", "environ.tif")
  unique_values <- unique(c(values(terra::rast(ifile)[[6]])))
  create_xml_legend(unique_values=unique_values, output_dir=file.path(destination, "data_raw"), file_name="environ")
  
  if (rm_download) {
    unlink(file.path(destination, "data_raw", "dist_sea"), recursive=TRUE)
    unlink(file.path(destination, "data_raw", "grassdata"), recursive=TRUE)
    unlink(file.path(destination, "data_raw", "OSM"), recursive=TRUE)
    unlink(file.path(destination, "data_raw", "soilgrids250_v2_0"), recursive=TRUE)
    unlink(file.path(destination, "data_raw", "srtm_v1_4_90m"), recursive=TRUE)
    unlink(file.path(destination, "data_raw", "WDPA"), recursive=TRUE)
    unlink(file.path(destination, "data_raw", "forestatrisk"), recursive=TRUE)
    unlink(file.path(destination, "data_raw", "dist_forest"), recursive=TRUE)
    unlink(file.path(destination, "data_raw", "world_pop"), recursive=TRUE)
  }
  
  # Return absolute path of environ.tif 
  return(file.path(destination, "data_raw", "environ.tif"))
  
}

# End
