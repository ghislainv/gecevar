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
#' @param forest_year int. Forest at the decade chosen. Must be one of
#'   2000, 2010 or 2020, default is 2010.
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
                              rm_download=FALSE, forest_year=2010) {

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

  #https://forobs.jrc.ec.europa.eu/GFC/v4
  # tiles 10 * 10 degre
  # JRC_GFC2020V3[lat]_[lon].tif, where
  # [lat]: latitude of the tile's top-left corner (tuile 1 : N80 W180)
  # [lon]: longitude of the tile's top left corner
  # WGS84 datum

  # 1) find tiles qui appartiennent à l'extent
  # 2) dl tiles
  # 3) build vrt : ok, en fait V4 monde entier dc vsicurl + gdal translate w/ extent
  # 4) resample + resol + tif : juste resol du coup

  # pourcentage de forest cover dans la cellule + dist

  dir.create(file.path(destination, "data_raw", "forest_var", "temp"), showWarnings=FALSE, recursive=TRUE)
  # tiles_forest <- c(floor(extent_latlon[1] / 10) * 10, ceiling(extent_latlon[2] / 10) * 10,
  #                 floor(extent_latlon[3] / 10) * 10, ceiling(extent_latlon[4] / 10) * 10)
  #
  # # ici on construit le code des tiles à récup en fonction de tiles_forest
  # lat_seq <- stringr::str_c("N", as.character(seq(tiles_forest[2], tiles_forest[4], 10)))
  # lat_str <- stringr::str_replace_all(lat_seq, "N-", "S")
  # lon_seq <- stringr::str_c("E", as.character(seq(tiles_forest[1], tiles_forest[3], 10)))
  # lon_str <- stringr::str_replace_all(lon_seq, "E-", "W")
  #
  # tiles <- NULL
  # for (i in lon_str) {
  #   tiles <- c(tiles, paste(lat_str, i, sep="_"))
  # }

  forest_url <- "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/FOREST/GFC2020/LATEST/single-cog/"
  if (httr::http_error(forest_url)) {
    message("There appears to be a problem reaching the website.")
    return(invisible(NULL))
  }
  ifile <- glue::glue("/vsicurl/{forest_url}","JRC_GFC2020_V4_COG.tif")
  ofile <- file.path(destination, "data_raw", "forest_var", "temp","forest.tif")
  gdal_utils_translate(ifile, ofile, extent_gdal_translate)




  ##===========================
  ##
  ## Forest area from Forest At Risk
  ## if available
  ##===========================

  # forest <- FALSE
  # continent_name <- countrycode::countrycode(sourcevar=country_name,
  #                                            origin="country.name",
  #                                            destination="continent")
  # if (continent_name == "Oceania") {
  #   continent_name="Asia"
  # }
  # continent_short <- substr(toupper(continent_name), 1, 3)
  # if (RCurl::url.exists(paste0("https://forestatrisk.cirad.fr/tropics/tif/fcc_123_", continent_short, "_aea.tif"))) {
  #   dir.create(file.path(destination, "data_raw", "forestatrisk"), showWarnings=FALSE)
  #   url_far <- paste0("https://forestatrisk.cirad.fr/tropics/tif/fcc_123_", continent_short, "_aea.tif")
  #
  #   if (httr::http_error(url_far)) {
  #     message("There appears to be a problem reaching the website.")
  #     return(invisible(NULL))
  #   }
  #
  #   ofile <- file.path(destination, "data_raw", "forestatrisk", "forest_nocrop.tif")
  #   gdal_utils_translate(ifile=paste0("/vsicurl/", url_far),
  #                        ofile=ofile,
  #                        extent_gdal_translate)
  #   raw_resol <- res(terra::rast(ofile))
  #
  #   ifile <- file.path(destination, "data_raw", "forestatrisk", "forest_nocrop.tif")
  #   ofile <- file.path(destination, "data_raw", "forestatrisk", "forest_crop_raw_resol.tif")
  #   opts <- glue("-overwrite -t_srs {proj_t} -dstnodata 255 ",
  #                "-r near -tr {raw_resol[1]} {raw_resol[2]} -te {extent_proj_string} ",
  #                "-ot Byte -of GTiff ",
  #                "-co COMPRESS=LZW -co PREDICTOR=2")
  #   sf::gdal_utils(util="warp", source=ifile, destination=ofile,
  #                  options=unlist(strsplit(opts, " ")),
  #                  quiet=TRUE)
  #
  #   unlink(file.path(destination, "data_raw", "forestatrisk", "forest_nocrop.tif"))
  #
  #   forest_crop_raw_resol <- terra::rast(ofile)
  #   if (forest_year == 2000) {
  #     # 1 is deforestation during 2000-2010
  #     values(forest_crop_raw_resol) <- values(forest_crop_raw_resol) >= 1
  #   } else if (forest_year == 2010) {
  #     # 2 is deforestation during 2010-2020
  #     values(forest_crop_raw_resol) <- values(forest_crop_raw_resol) >= 2
  #   } else {
  #     # 3 is forest in 2020
  #     values(forest_crop_raw_resol) <- values(forest_crop_raw_resol) == 3
  #   }
  #   terra::writeRaster(forest_crop_raw_resol,gdal = c("COMPRESS=LZW", "PREDICTOR=2"),
  #                      overwrite = TRUE, datatype = "Byte", filename = ofile)
  #
  #   ifile <- file.path(destination, "data_raw", "forestatrisk", "forest_crop_raw_resol.tif")
  #   ofile <- file.path(destination, "data_raw", "forestatrisk", "forest.tif")
  #   opts <- glue("-overwrite -t_srs {proj_t} -dstnodata 255 ",
  #                "-r average -tr {resol} {resol} -te {extent_proj_string} ",
  #                "-ot Byte -of GTiff ",
  #                "-co COMPRESS=LZW -co PREDICTOR=2 ")
  #   sf::gdal_utils(util="warp", source=ifile, destination=ofile,
  #                  options=unlist(strsplit(opts, " ")),
  #                  quiet=TRUE)
  #
  #   unlink(file.path(destination, "data_raw", "forestatrisk", "forest_crop_raw_resol.tif"))
  #
  #   forest <- TRUE
  # } else {
  #   print("Forest layer is not available for your country")
  # }


  ##===========================
  ##
  ## Distance to forest
  ## if forest is available
  ##===========================

  if (forest) {

    dir.create(file.path(destination, "data_raw", "dist_forest"), showWarnings=FALSE)
    sourcefile <- file.path(destination, "data_raw", "forestatrisk", "forest.tif")
    destfile <- file.path(destination, "data_raw", "dist_forest", "dist_forest.tif")
    dist_forest <- distance(terra::rast(sourcefile), exclude = 0,  unit = "m",
                            filename = destfile, overwrite = TRUE,
                            datatype = nodata_Int32, gdal=c("COMPRESS=LZW", "PREDICTOR=2"))

    # cmd <- glue("gdal_proximity.py {sourcefile} {destfile} ",
    #             "-ot Int32 -of GTiff -nodata {nodata_Int32} ",
    #             "-values {1} -distunits GEO -use_input_nodata NO")
  }

  ##=====================================
  ##
  ## Merge environmental variables in one .tif
  ##
  ##=====================================

  # Load all rasters
  forest <- terra::rast(file.path(destination, "data_raw", "forestatrisk", "forest.tif"))
  dist_forest <- terra::rast(file.path(destination, "data_raw", "dist_forest", "dist_forest.tif"))
  environ <- c(forest, dist_forest)
  layer_names <- c("forest", "dist_forest")
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
