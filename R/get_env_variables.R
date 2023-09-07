#' Create multilayer Tiff file with 11 environmental variables
#'
#' @description Variables are type of soil, elevation, slope, aspect,
#'   roughness, solar radiation, distance to sea, protected areas,
#'   distance to roads, distance to cities and town, distance to
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
#' @param forest_year int. Forest at the decade chosen. Must be one of
#'   2000, 2010 or 2020, default is 2010.
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
#' | Elevation                            | m             |
#' | Aspect                               | degrees       |
#' | Roughness                            | m             |
#' | Slope                                | degrees       |
#' | Solar radiation                      | Wh.m^{-2}.day |
#' | Soilgrids                            | category      |
#' | Forest                               | binary        |
#' | Distance to forest                   | m             |
#' | Distance sea                         | m             |
#' | Distance road                        | m             |
#' | Distance place                       | m             |
#' | Distance watering place              | m             |
#' | Protected Area (WDPA)                | category      |
#' | Population density                   | people/km²    |
#' @md
#'
#' @importFrom glue glue
#' @importFrom utils download.file unzip
#' @importFrom RCurl url.exists
#' @import sf
#' @import stars
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
                              rm_download=FALSE, forest_year=2010,
                              gisBase=NULL) {

  # Round extent_latlon to nearest degree
  extent_latlon_1d <- c(floor(extent_latlon["lonmin"]), floor(extent_latlon["latmin"]),
                        ceiling(extent_latlon["lonmax"]), ceiling(extent_latlon["latmax"]))
    
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

  ##==============================
  ##
  ## Solar radiation
  ##
  ## with r.sun at 90m resolution
  ## Solar radiation (in Wh.m-2.day-1) was computed from altitude,
  ## slope and aspect using the function r.sun from the GRASS GIS software.
  ## We incorporated the shadowing effect of terrain to compute the solar radiation.
  ## Solar radiation was computed for the Julian day 79 (20th of March for regular years=equinox).
  ##
  ##==============================

  ## Initialize GRASS
  # get gisBase directory
  if (is.null(gisBase)) {
    gisBase <- system("grass --config path", intern=TRUE)
  }
  # Set library path
  Sys.setenv(LD_LIBRARY_PATH=paste(file.path(gisBase, "lib"), Sys.getenv("LD_LIBRARY_PATH"), sep=":"))
  # use a georeferenced raster
  elevation <- file.path(destination, "data_raw/srtm_v1_4_90m/temp/elevation.tif")
  cmd <- glue('grass -c {elevation} -e {destination}/data_raw/grassdata/environ')
  system(cmd, ignore.stdout=TRUE, ignore.stderr=TRUE)
  # connect to grass database
  rgrass::initGRASS(gisBase=gisBase,
                    gisDbase=file.path(destination, "data_raw", "grassdata"), home=file.path(destination, "data_raw"),
                    location="environ", mapset="PERMANENT",
                    override=TRUE)
  
  ## Import raster in grass
  cmd <- glue("r.in.gdal -e -o input={elevation} output=elevation")
  system(cmd, ignore.stdout=TRUE, ignore.stderr=TRUE)
  slope <- file.path(destination, "data_raw", "srtm_v1_4_90m", "temp", "slope.tif")
  cmd <- glue("r.in.gdal -e --o input={slope} output=slope")
  system(cmd, ignore.stdout=TRUE, ignore.stderr=TRUE)
  aspect <- file.path(destination, "data_raw", "srtm_v1_4_90m", "temp", "aspect.tif")
  cmd <- glue("r.in.gdal -e --o input={aspect} output=aspect")
  system(cmd, ignore.stdout=TRUE, ignore.stderr=TRUE)
  
  # Compute radiation
  cmd <- glue("r.sun --overwrite --verbose elevation=elevation aspect=aspect ",
              "slope=slope day=79 glob_rad=global_rad")
  system(cmd, ignore.stdout=TRUE, ignore.stderr=TRUE)
  # Export
  ofile <- file.path(destination, 'data_raw', 'srtm_v1_4_90m', 'temp', 'srad.tif')
  cmd <- glue("r.out.gdal -f --verbose --overwrite input=global_rad ",
              "createopt='COMPRESS=LZW' nodata={nodata_Int16} output={ofile} type=Int16")
  system(cmd, ignore.stdout=TRUE, ignore.stderr=TRUE)
  
  # Resolution from 90m x 90m to chosen resolution using gdalwarp
  ifile <- file.path(destination, "data_raw", "srtm_v1_4_90m", "temp", "srad.tif")
  ofile <- file.path(destination, "data_raw", "srtm_v1_4_90m", "srad_res.tif")
  opts <- glue("-overwrite -t_srs {proj_t} -dstnodata {nodata_Int16} ",
               "-r bilinear -tr {resol} {resol} -te {extent_proj_string} ",
               "-ot Int16 -of GTiff ",
               "-co COMPRESS=LZW -co PREDICTOR=2")
  sf::gdal_utils(util="warp", source=ifile, destination=ofile,
                 options=unlist(strsplit(opts, " ")),
                 quiet=TRUE)

  ##===========================
  ##
  ## Forest area from Forest At Risk
  ## if available
  ##===========================

  forest <- FALSE
  continent_name <- countrycode::countrycode(sourcevar=country_name,
                                             origin="country.name",
                                             destination="continent")
  if (continent_name == "Oceania") {
    continent_name="Asia"
  }
  continent_short <- substr(toupper(continent_name), 1, 3)
  if (RCurl::url.exists(paste0("https://forestatrisk.cirad.fr/tropics/tif/fcc_123_", continent_short, "_aea.tif"))) {
    dir.create(file.path(destination, "data_raw", "forestatrisk"), showWarnings=FALSE)
    url_far <- paste0("https://forestatrisk.cirad.fr/tropics/tif/fcc_123_", continent_short, "_aea.tif")
    ofile <- file.path(destination, "data_raw", "forestatrisk", "forest_nocrop.tif")
    opts <- glue("-projwin {extent_latlon[1]} {extent_latlon[4]} {extent_latlon[3]} {extent_latlon[2]} ",
                "-projwin_srs EPSG:4326 -co COMPRESS=LZW -co PREDICTOR=2")
    sf::gdal_utils(util="translate", source=paste0("/vsicurl/", url_far), destination=ofile,
                   options=unlist(strsplit(opts, " ")),
                   quiet=TRUE)
    
    ifile <- file.path(destination, "data_raw", "forestatrisk", "forest_nocrop.tif")
    ofile <- file.path(destination, "data_raw", "forestatrisk", "forest.tif")
    opts <- glue("-overwrite -t_srs {proj_t} -dstnodata 255 ",
                 "-r near -tr {resol} {resol} -te {extent_proj_string} ",
                 "-ot Byte -of GTiff ",
                 "-co COMPRESS=LZW -co PREDICTOR=2")
    sf::gdal_utils(util="warp", source=ifile, destination=ofile,
                   options=unlist(strsplit(opts, " ")),
                   quiet=TRUE)
    
    unlink(file.path(destination, "data_raw", "forestatrisk", "forest_nocrop.tif"))
    forest_stars <- stars::read_stars(ofile)
    if (forest_year == 2000) {
      # 1 is deforestation during 2000-2010
      forest_stars[[1]] <- forest_stars[[1]] >= 1
    } else if (forest_year == 2010) {
        # 2 is deforestation during 2010-2020
        forest_stars[[1]] <- forest_stars[[1]] >= 2
    } else {
        # 3 is forest in 2020
        forest_stars[[1]] <- forest_stars[[1]] == 3
    }
    stars::write_stars(forest_stars, dsn=ofile, update=TRUE, type="Byte", options=c("COMPRESS=LZW", "PREDICTOR=2"))
    forest <- TRUE
  } else {
    print("Forest layer is not available for your country")
  }

  ##===========================
  ##
  ## Distance to forest
  ## if forest is available
  ##===========================

  if (forest) {
    dir.create(file.path(destination, "data_raw", "dist_forest"), showWarnings=FALSE)
    sourcefile <- file.path(destination, "data_raw", "forestatrisk", "forest.tif")
    destfile <- file.path(destination, "data_raw", "dist_forest", "dist_forest.tif")
    cmd <- glue("gdal_proximity.py {sourcefile} {destfile} ",
                "-ot Int32 -of GTiff -nodata {nodata_Int32} ",
                "-values {1} -distunits GEO -use_input_nodata NO")
    system(cmd, ignore.stdout=TRUE, ignore.stderr=TRUE)
  }

  ##===========================
  ##
  ## Distance to Sea
  ##
  ##===========================

  # Land area
  dir.create(file.path(destination, "data_raw", "dist_sea"), showWarnings=FALSE)
  # The following line should be modified as it implies loading the raster in memory.
  seaBool <- (stars::read_stars(file.path(destination, "data_raw", "srtm_v1_4_90m", "srad_res.tif")) == nodata_Int16)
  stars::write_stars(seaBool, options=c("COMPRESS=LZW", "PREDICTOR=2"), NA_value=,
                     dsn=file.path(destination, "data_raw", "dist_sea", "sea_res.tif"))

  # Distance to sea
  sourcefile <- file.path(destination, "data_raw", "dist_sea", "sea_res.tif")
  destfile <- file.path(destination, "data_raw", "dist_sea", "dist_sea.tif")
  cmd <- glue("gdal_proximity.py -ot Int32 -of GTiff -nodata {nodata_Int32} ",
              "-values {nodata_Int16} -distunits GEO -use_input_nodata NO {sourcefile} {destfile}")
  system(cmd, ignore.stdout=TRUE, ignore.stderr=TRUE)

  # Replace 0 with NA
  dist_sea <- terra::rast(file.path(destination, "data_raw", "dist_sea", "dist_sea.tif"))
  values(dist_sea)[values(dist_sea) == 0] <- NA
  ofile <- file.path(destination, "data_raw", "dist_sea", "dist_sea.tif")
  terra::writeRaster(dist_sea, filename=ofile,
                     gdal=c("COMPRESS=LZW","PREDICTOR=2"),
                     progress=FALSE, overwrite=TRUE, datatype="INT4S")


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
  ## dir.create(file.path(destination, "data_raw", "WDPA", "temp"), showWarnings=FALSE)
  ## # Date in english
  ## Sys.setlocale("LC_TIME", "C")
  ## date <- stringr::str_remove(stringr::str_to_title(format(Sys.Date(), format="%b%Y")), "\\.")
  ## Sys.setlocale("LC_TIME", "")
  ## httr::POST(url="https://www.protectedplanet.net/downloads", encode="json", body=list("domain"="general", "format"="gdb", "token"=ISO_country_code))
  ## retry::wait_until(RCurl::url.exists(paste0("https://d1gam3xoknrgr2.cloudfront.net/current/WDPA_WDOECM_", date, "_Public_", ISO_country_code, ".zip")), timeout=60)
  ## download.file(paste0("https://d1gam3xoknrgr2.cloudfront.net/current/WDPA_WDOECM_", date, "_Public_", ISO_country_code, ".zip"),
  ##               destfile=file.path(destination, "data_raw", "WDPA","temp", paste0("WDPA_WDOECM_", date,"_Public_", ISO_country_code, ".zip")),
  ##               method='auto', mode="wb", quiet=TRUE)
  ## unzip(file.path(destination, "data_raw", "WDPA","temp", paste0("WDPA_WDOECM_", date,"_Public_", ISO_country_code, ".zip")),
  ##       exdir=file.path(destination, 'data_raw', 'WDPA', 'temp'))
  ## WDPA <- terra::vect(file.path(destination, "data_raw", "WDPA", "temp", paste0("WDPA_WDOECM_", date, "_Public_", ISO_country_code, ".gdb/")),
  ##                     layer=paste0("WDPA_WDOECM_poly_", date, "_", ISO_country_code))
  ## WDPA <- sf::st_as_sf(WDPA)[3]
  ## WDPA <- sf::st_transform(WDPA, EPSG)
  ## WDPA <- stars::st_rasterize(WDPA, dx=resol, dy=resol)
  ## WDPA[[1]] <- WDPA[[1]] != 0
  ## stars::write_stars(WDPA, options=c("COMPRESS=LZW", "PREDICTOR=2"), NA_value=255,
  ##                    dsn=file.path(destination, "data_raw", "WDPA", "WDPA_resBool.tif"))

  # Temporary
  ifile <- file.path(destination, "data_raw", "soilgrids250_v2_0", "soilgrids_res.tif")
  ofile <- file.path(destination, "data_raw", "WDPA", "WDPA_resBool.tif")
  file.copy(ifile, ofile)

  ##=========================
  ##
  ## Open Street Map : distance from cities, roads, rivers
  ##
  ##=========================

  # Download data with osmextract
  dir.create(file.path(destination, "data_raw", "OSM", "temp"), recursive=TRUE, showWarnings=FALSE)
  osm_country <- osmextract::oe_match(country_name, quiet=TRUE)
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
  # Also, remove dependence to stars here.
  water_files <- list.files(file.path(destination, "data_raw","OSM"),
                            pattern="^(river|waterbody)_distance_res\\.tif$", full.names=TRUE)
  water_dist <- stars::read_stars(water_files[1])
  water_dist[[1]] <- pmin(stars::read_stars(water_files[1])[[1]],
                          stars::read_stars(water_files[2])[[1]])
  stars::write_stars(water_dist, file.path(destination, "data_raw", "OSM", "water_distance_res.tif"))

  ##===================
  ##
  ## Population
  ##
  ##===================

  # Directory
  dir.create(path=file.path(destination, "data_raw", "world_pop"), showWarnings=FALSE)
  dir.create(path=file.path(destination, "data_raw", "world_pop", "temp"), showWarnings=FALSE)

  # Download
  dest <- file.path(destination, "data_raw", "world_pop", "temp", paste0(ISO_country_code, "_pop.tif"))
  # url depends of the chosen country (different for New Caledonia or Madagascar)
  URL_maxar_v1 <- paste0("https://data.worldpop.org/GIS/Population/Global_2000_2020_Constrained/2020/maxar_v1/",
                         ISO_country_code, "/",tolower(ISO_country_code),"_ppp_2020_UNadj_constrained.tif")
  URL_BSGM <- paste0("https://data.worldpop.org/GIS/Population/Global_2000_2020_Constrained/2020/BSGM/",
                     ISO_country_code, "/",tolower(ISO_country_code),"_ppp_2020_UNadj_constrained.tif")
  if (RCurl::url.exists(URL_maxar_v1)) {
    download.file(URL_maxar_v1, destfile=dest, quiet=TRUE)
  } else {
    download.file(URL_BSGM, destfile=dest, quiet=TRUE)
  }

  # Unit set to pop/km²
  pop <- round(rast(dest) * 100)
  terra::writeRaster(pop, filename=file.path(destination, "data_raw", "world_pop", "temp", paste0(ISO_country_code, "_pop_km.tif")),
                     gdal=c("COMPRESS=LZW","PREDICTOR=2"), progress=0, overwrite=TRUE, datatype="INT2S")

  # Reproject
  ifile <- file.path(destination, "data_raw", "world_pop", "temp", paste0(ISO_country_code, "_pop_km.tif"))
  ofile <- file.path(destination, "data_raw", "world_pop", paste0(ISO_country_code, "_pop_res.tif"))
  opts <- glue("-overwrite -s_srs {proj_s} -t_srs {proj_t} ",
               "-srcnodata {nodata_Int16} -dstnodata {nodata_Int16} ",
               "-r bilinear -tr {resol} {resol} -te {extent_proj_string} ",
               "-ot Int16 -of GTiff ",
               "-co COMPRESS=LZW -co PREDICTOR=2")
  sf::gdal_utils(util="warp", source=ifile, destination=ofile,
                 options=unlist(strsplit(opts, " ")),
                 quiet=TRUE)
  
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

  # Create envrion raster with all layers                         
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
