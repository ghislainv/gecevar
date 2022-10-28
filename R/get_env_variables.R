get_env_variables <- function(extent_latlon, extent, EPSG, country_name, destination, resolution = 1000, rm_download = FALSE, forest_year = 2010, gisBase = NULL){
  #' Create multilayer Tiff file with 11 environmental variables
  #'
  #' @description
  #' Variables are type of soil, elevation, slope, aspect, roughness, solar radiation, distance to sea,
  #' protected areas, distance to roads, distance to cities and town, distance to rivers & lake.
  #'
  #' @param extent_latlon int vector. in this order c(lon_min, lat_min, lon_max, lat_max).
  #' @param extent character. First output of `transform_shp_country_extent` function.
  #' @param EPSG int. to consider for this country/area.
  #' @param country_name character. country name (in English) which be use to collect protected areas. This country must be available in `https://www.protectedplanet.net/en/thematic-areas/wdpa?tab=WDPA`.
  #' @param destination character. absolute path where to download files like `here()` output.
  #' @param resolution int. in meters, recommended resolution are 250m, 500m, 1km, 2km or 5km, default is 1km. See more in details.
  #' @param rm_download boolean. If TRUE remove download files and folders. Keep only environ.tif in `data_raw` folder, default is FALSE.
  #' @param forest_year int. Forest at the decade chosen. Must be one of 2000, 2010 or 2020, default is 2010.
  #' @param gisBase NULL or character. Parameter `gisBase` for `rgrass::initGRASS()`. The directory path to GRASS binaries and libraries, containing bin and lib subdirectories among others; if NULL, system("grass --config path") is tried.
  #' @return character. Absolute path to `environ.tif` file.
  #' @details `resolution` need to be carefully chosen because if .tif file is too large, R can crash.
  #'
  #' @details
  #'
  #' environ.tif.aux.xml is an extention of environ.tif, it allows to classify soilgrid variable with QGIS with RasterAttributeTable extension.
  #' Nevertheless it's cause problems to open it with `stars` package but not with `terra`. If you have any problems to open environ.tif, you can remove environ.tif.aux.xml.
  #' This solve all accessibility problems with `stars` and `terra` packages.
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

  options(warn = -1)
  dir.create(path = destination, recursive = TRUE, showWarnings = FALSE)
  destination <- paste0(destination, "/")
  nodat <- -32768
  proj.s <- "EPSG:4326"
  proj.t <- paste0("EPSG:", EPSG)
  ISO_country_code <- countryname(country_name, destination = "iso3c")
  extent_num <- as.numeric(strsplit(extent, split = " ")[[1]])

  ##==============================
  ##
  ## Soilgrids
  ##
  ##==============================

  dir.create(paste(destination, "data_raw", sep = "/"), showWarnings = FALSE)
  dir.create(paste(destination, "data_raw", "soilgrids250_v2_0", sep = "/"), showWarnings = FALSE)
  dir.create(paste(destination, "data_raw", "soilgrids250_v2_0", "temp", sep = "/"), showWarnings = FALSE)
  # file available only by 2° x 2° for soilgrids250
  merge_lat <- "c("
  for (i in extent_latlon[1]:extent_latlon[3]) # longitude
  {
    alpha <- TRUE
    for (j in extent_latlon[2]:extent_latlon[4]) # latitude
    {
      url = paste0("https://maps.isric.org/mapserv?map=/map/wrb.map&SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage&COVERAGEID=MostProbable&FORMAT=image/tiff&SUBSET=long(",i, ".0000,", i + 1, ".0000)&SUBSET=lat(", j, ".0000,", j + 1, ".0000)&SUBSETTINGCRS=http://www.opengis.net/def/crs/EPSG/0/4326&OUTPUTCRS=http://www.opengis.net/def/crs/EPSG/0/4326")
      dest = paste(destination, "data_raw", "soilgrids250_v2_0", "temp", paste0("soilgrids_", j, "_", i, ".tif"), sep = "/")
      download.file(url = url, destfile = dest, quiet = TRUE)
    }
  }
  destfile <- paste(destination, "data_raw", "soilgrids250_v2_0", "soilgrids.vrt", sep = "/")
  file <- file(paste(destination, "data_raw", "soilgrids250_v2_0", "temp", "files_list.txt", sep = "/"))
  writeLines(list.files(paste(destination, "data_raw", "soilgrids250_v2_0", "temp", sep = "/"), pattern = "soilgrids_", full.names = TRUE),
             file)
  close(file)

  system(glue('gdalbuildvrt {destfile} -input_file_list {paste(destination, "data_raw", "soilgrids250_v2_0", "temp", "files_list.txt", sep = "/")}'), ignore.stdout = TRUE, ignore.stderr = TRUE)

  sourcefile <- destfile
  destfile <- paste(destination, "data_raw", "soilgrids250_v2_0", "soilgrids.tif", sep = "/")
  system(glue('gdal_translate -of GTiff  -r bilinear \\
              {sourcefile} {destfile}'), ignore.stdout = TRUE, ignore.stderr = TRUE)
  system(glue('gdalwarp -tr {resolution} {resolution} -te {extent} -s_srs {proj.s} -t_srs {proj.t} -overwrite {destfile} \\
              -r mode {paste(destination, "data_raw", "soilgrids250_v2_0", "soilgrids_res.tif", sep = "/")}'), ignore.stdout = TRUE, ignore.stderr = TRUE)

  ##==============================
  ##
  ## SRTM at 90m resolution from
  ## Elevation, slope aspect, roughness
  ##
  ## https://dwtkns.com/srtm/ version 4.1
  ##
  ##==============================

  dir.create(paste(destination, "data_raw", "srtm_v1_4_90m", sep = "/"), showWarnings = FALSE)
  dir.create(paste(destination, "data_raw", "srtm_v1_4_90m", "temp", sep = "/"), showWarnings = FALSE)
  tiles_srtm <- c(floor(extent_latlon[1] / 5) * 5, ceiling(extent_latlon[2] / 5) * 5,
                  floor(extent_latlon[3] / 5) * 5, ceiling(extent_latlon[4] / 5) * 5)
  lat <- str_pad(seq(tiles_srtm[1], tiles_srtm[3], 5) / 5 + 37, width = 2, pad = "0")
  lon <- str_pad(-seq(tiles_srtm[2], tiles_srtm[4], 5) / 5 + 13, width = 2, pad = "0")
  tiles <- NULL
  for (i in lon)
  {
    tiles <- c(tiles, paste(lat, i, sep = "_"))
  }

  for (i in tiles) {
    if (url.exists(paste0("https://srtm.csi.cgiar.org/wp-content/uploads/files/srtm_5x5/TIFF/srtm_", i, ".zip")))
    {
      dst <- paste0(paste(destination, "data_raw", "srtm_v1_4_90m", "temp", "srtm_", sep = "/"), i, ".zip")
      url.tile <- paste0("https://srtm.csi.cgiar.org/wp-content/uploads/files/srtm_5x5/TIFF/srtm_", i, ".zip")
      download.file(url = url.tile, destfile = dst, method = "wget", quiet = TRUE)
      unzip(dst, exdir = paste(destination, "data_raw", "srtm_v1_4_90m", "temp", sep = "/"), overwrite = TRUE)
    }
  }

  # Merge and Reproject with EPSG
  sourcefile <- list.files(paste(destination, "data_raw", "srtm_v1_4_90m", "temp", sep = "/"), pattern = "*.tif", full.names = TRUE)
  file <- file(paste(destination, "data_raw", "srtm_v1_4_90m", "temp", "sourcefilevrt.txt", sep = "/"))
  writeLines(sourcefile, file)
  close(file)

  destfile <- paste(destination, "data_raw", "srtm_v1_4_90m", "temp", "srtm.vrt", sep = "/")
  system(glue('gdalbuildvrt {destfile} -vrtnodata {nodat} -input_file_list {paste(destination, "data_raw", "srtm_v1_4_90m", "temp", "sourcefilevrt.txt", sep = "/")}'), ignore.stdout = TRUE, ignore.stderr = TRUE)
  system(glue('gdalwarp -overwrite -t_srs {proj.t} -tap -r bilinear -dstnodata {nodat} \\
            -co "COMPRESS=LZW" -co "PREDICTOR=2" -te {extent} -ot Int16 -of GTiff \\
            -tr {resolution / 2} {resolution / 2} {destfile} {paste(destination, "data_raw", "srtm_v1_4_90m", "temp", "elevation.tif", sep = "/")}'), ignore.stdout = TRUE, ignore.stderr = TRUE)

  ## Compute slope, aspect and roughness using gdaldem
  # compute slope
  in_f <- paste(destination, "data_raw", "srtm_v1_4_90m", "temp", "elevation.tif", sep = "/")
  out_f <- paste(destination, "data_raw", "srtm_v1_4_90m", "temp", "slope.tif", sep = "/")
  system(glue('gdaldem slope {in_f} {out_f} -co "COMPRESS=LZW" -co "PREDICTOR=2"'), ignore.stdout = TRUE, ignore.stderr = TRUE)
  # compute aspect
  out_f <- paste(destination, "data_raw", "srtm_v1_4_90m", "temp", "aspect.tif", sep = "/")
  system(glue('gdaldem aspect {in_f} {out_f} -co "COMPRESS=LZW" -co "PREDICTOR=2"'), ignore.stdout = TRUE, ignore.stderr = TRUE)
  # compute roughness
  out_f <- paste(destination, "data_raw", "srtm_v1_4_90m", "temp", "roughness.tif", sep = "/")
  system(glue('gdaldem roughness {in_f} {out_f} -co "COMPRESS=LZW" -co "PREDICTOR=2"'), ignore.stdout = TRUE, ignore.stderr = TRUE)

  # Resolution from resolution/2 to chosen resolution using gdalwarp
  # elevation
  out_f <- paste(destination, "data_raw", "srtm_v1_4_90m", "elevation_res.tif", sep = "/")
  system(glue('gdalwarp -r bilinear -tr {resolution} {resolution} -ot Int16 -of GTiff -dstnodata {nodat} \\
        -co "COMPRESS=LZW" -co "PREDICTOR=2" -overwrite {in_f} {out_f}'), ignore.stdout = TRUE, ignore.stderr = TRUE)
  # aspect
  in_f <- paste(destination, "data_raw", "srtm_v1_4_90m", "temp", "aspect.tif", sep = "/")
  out_f <-paste(destination, "data_raw", "srtm_v1_4_90m", "aspect_res.tif", sep = "/")
  system(glue('gdalwarp -r bilinear -tr {resolution} {resolution} -ot Int16 -of GTiff -dstnodata {nodat} \\
        -co "COMPRESS=LZW" -co "PREDICTOR=2" -overwrite {in_f} {out_f}'), ignore.stdout = TRUE, ignore.stderr = TRUE)
  # slope
  in_f <- paste(destination, "data_raw", "srtm_v1_4_90m", "temp", "slope.tif", sep = "/")
  out_f <- paste(destination, "data_raw", "srtm_v1_4_90m", "slope_res.tif", sep = "/")
  system(glue('gdalwarp -r bilinear -tr {resolution} {resolution} -ot Int16 -of GTiff \\
        -co "COMPRESS=LZW" -co "PREDICTOR=2" -overwrite {in_f} {out_f}'), ignore.stdout = TRUE, ignore.stderr = TRUE)
  # roughness
  in_f <- paste(destination, "data_raw", "srtm_v1_4_90m", "temp", "roughness.tif", sep = "/")
  out_f <- paste(destination, "data_raw", "srtm_v1_4_90m", "roughness_res.tif", sep = "/")
  system(glue('gdalwarp -r bilinear -tr {resolution} {resolution} -ot Int16 -of GTiff \\
        -co "COMPRESS=LZW" -co "PREDICTOR=2" -overwrite {in_f} {out_f}'), ignore.stdout = TRUE, ignore.stderr = TRUE)

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
    gisBase <- system("grass --config path", intern = TRUE)
  }
  # set library path
  setwd(paste(destination, "data_raw", sep = "/"))
  Sys.setenv(LD_LIBRARY_PATH = paste(file.path(gisBase, "lib"), Sys.getenv("LD_LIBRARY_PATH"), sep = ":"))
  # use a georeferenced raster
  elevation <- paste(destination, "data_raw/srtm_v1_4_90m/temp/elevation.tif", sep = "/")
  system(glue('grass -c {elevation} -e grassdata/environ'), ignore.stdout = TRUE, ignore.stderr = TRUE)
  # connect to grass database
  initGRASS(gisBase = gisBase,
            gisDbase = "grassdata", home = tempdir(),
            location = "environ", mapset = "PERMANENT",
            override = TRUE)
  ## Import raster in grass
  system(glue("r.in.gdal -e -o input={elevation} output=elevation"), ignore.stdout = TRUE, ignore.stderr = TRUE)
  slope <- paste(destination, "data_raw", "srtm_v1_4_90m", "temp", "slope.tif", sep = "/")
  system(glue("r.in.gdal -e --o input={slope} output=slope"), ignore.stdout = TRUE, ignore.stderr = TRUE)
  aspect <- paste(destination, "data_raw", "srtm_v1_4_90m", "temp", "aspect.tif", sep = "/")
  system(glue("r.in.gdal -e --o input={aspect} output=aspect"), ignore.stdout = TRUE, ignore.stderr = TRUE)
  # Compute radiation
  system(glue("r.sun --overwrite --verbose elevation=elevation aspect=aspect slope=slope day=79 glob_rad=global_rad"), ignore.stdout = TRUE, ignore.stderr = TRUE)
  # Export
  system(glue("r.out.gdal -f --verbose --overwrite input=global_rad createopt='COMPRESS=LZW' nodata={nodat} \\
  			 output={paste(destination, 'data_raw', 'srtm_v1_4_90m', 'temp', 'srad.tif', sep = '/')} type=Int16"), ignore.stdout = TRUE, ignore.stderr = TRUE)
  # Resolution from 90m x 90m to chosen resolution using gdalwarp
  # srad
  in_f <- paste(destination, "data_raw", "srtm_v1_4_90m", "temp", "srad.tif", sep = "/")
  out_f <- paste(destination, "data_raw", "srtm_v1_4_90m", "srad_res.tif", sep = "/")
  system(glue('gdalwarp  -t_srs {proj.t} -dstnodata {nodat} \\
        -r bilinear -tr {resolution} {resolution} -ot Int16 -of GTiff \\
        -co "COMPRESS=LZW" -co "PREDICTOR=2" -overwrite {in_f} {out_f}'), ignore.stdout = TRUE, ignore.stderr = TRUE)

  ##===========================
  ##
  ## Forest area from Forest At Risk
  ## if available
  ##===========================

  forest = FALSE
  continent_name <-countrycode(sourcevar = country_name,
                               origin = "country.name",
                               destination = "continent")
  if (continent_name == "Oceania"){
    continent_name = "Asia"
  }
  continent_short <- substr(toupper(continent_name), 1, 3)
  if (url.exists(paste0("https://forestatrisk.cirad.fr/tropics/tif/fcc_123_", continent_short, "_aea.tif"))){
    dir.create(paste(destination, "data_raw", "forestatrisk", sep = "/"), showWarnings = FALSE)
    system(glue('gdal_translate -projwin {extent_latlon[1]} {extent_latlon[4]} {extent_latlon[3]} {extent_latlon[2]} -projwin_srs EPSG:4326 \\
                /vsicurl/{paste0("https://forestatrisk.cirad.fr/tropics/tif/fcc_123_", continent_short, "_aea.tif")} \\
                -co "COMPRESS=LZW" -co "PREDICTOR=2" {paste(destination, "data_raw", "forestatrisk", "forest_nocrop.tif", sep = "/")}'), ignore.stdout = TRUE, ignore.stderr = TRUE)
    sourcefile <- paste(destination, "data_raw", "forestatrisk", "forest_nocrop.tif", sep = "/")
    destfile <- paste(destination, "data_raw", "forestatrisk", "forest.tif", sep = "/")
    system(glue("gdalwarp -overwrite -t_srs {proj.t} \\
        -r bilinear -tr {resolution} {resolution} -te {extent} -ot Int16 -of GTiff \\
        {sourcefile} {destfile}"), ignore.stdout = TRUE, ignore.stderr = TRUE)
    unlink(paste(destination, "data_raw", "forestatrisk", "forest_nocrop.tif", sep = "/"))
    forest_stars <- read_stars(destfile)
    if (forest_year == 2000){
      # 1 is deforestation during 2000-2010
      forest_stars[[1]] <- forest_stars[[1]] >= 1
    }else{
      if (forest_year == 2010){
        # 2 is deforestation during 2010-2020
        forest_stars[[1]] <- forest_stars[[1]] >= 2
      }else{
        # 3 is forest in 2020
        forest_stars[[1]] <- forest_stars[[1]] == 3
      }
    }
    write_stars(forest_stars, dsn = destfile, update = TRUE, type = "Int16", options = c("COMPRESS=LZW", "PREDICTOR=2"))
    forest <- TRUE
  }else{
    print("Forest layer is not available for your country")
  }

  ##===========================
  ##
  ## Distance to forest
  ## if forest is available
  ##===========================

  if (forest){
    dir.create(paste(destination, "data_raw", "distForest", sep = "/"), showWarnings = FALSE)
    sourcefile <- paste(destination, "data_raw", "forestatrisk", "forest.tif", sep = "/")
    destfile <- paste(destination, "data_raw", "distForest", "distForest.tif", sep = "/")
    system(glue("gdal_proximity.py {sourcefile} {destfile} -ot Int16 -of GTiff -nodata {nodat} \\
        -values {1} -distunits GEO -use_input_nodata NO "), ignore.stdout = TRUE, ignore.stderr = TRUE)
  }

  ##===========================
  ##
  ## Distance to Sea
  ##
  ##===========================

  dir.create(paste(destination, "data_raw", "distSea", sep = "/"), showWarnings = FALSE)
  seaBool <- read_stars(paste(destination, "data_raw", "srtm_v1_4_90m", "srad_res.tif", sep = "/")) == nodat
  write_stars(seaBool, options = c("COMPRESS=LZW", "PREDICTOR=2"), NA_value = nodat,
              dsn = paste(destination, "data_raw", "distSea", "Sea_resBool.tif", sep = "/"))
  sourcefile <- paste(destination, "data_raw", "distSea", "Sea_resBool.tif", sep = "/")
  destfile <- paste(destination, "data_raw", "distSea", "distSea.tif", sep = "/")
  system(glue("gdal_proximity.py -ot Int16 -of GTiff -nodata {nodat} \\
        -values {nodat} -distunits GEO -use_input_nodata NO {sourcefile} {destfile}"), ignore.stdout = TRUE, ignore.stderr = TRUE)
  distSea <- read_stars(paste(destination, "data_raw", "distSea", "distSea.tif", sep = "/"))
  distSea[[1]][distSea[[1]] == 0] <- NA
  write_stars(distSea, paste(destination, "data_raw", "distSea", "distSea.tif", sep = "/"), NA_value = nodat,
              options = c("COMPRESS=LZW", "PREDICTOR=2"))

  ##=========================
  ##
  ## WDPA : World Database Protected Areas
  ## International Union for Conservation of Nature
  ## UNEP-WCMC (2022). Protected Area Profile for New Caledonia from the World Database of Protected Areas, May 2022.
  ## Available at: www.protectedplanet.net
  ##
  ##=========================

  dir.create(paste(destination, "data_raw", "WDPA", sep = "/"), showWarnings = FALSE)
  dir.create(paste(destination, "data_raw", "WDPA", "temp", sep = "/"), showWarnings = FALSE)
  date <- str_remove(str_to_title(format(Sys.Date(), format = "%b%Y")), "\\.")
  POST(url = "https://www.protectedplanet.net/downloads", encode = "json", body = list("domain" = "general", "format" = "gdb", "token" = ISO_country_code))
  wait_until(url.exists(paste0("https://d1gam3xoknrgr2.cloudfront.net/current/WDPA_WDOECM_", date, "_Public_", ISO_country_code, ".zip")), timeout = 60)
  download.file(paste0("https://d1gam3xoknrgr2.cloudfront.net/current/WDPA_WDOECM_", date, "_Public_", ISO_country_code, ".zip"),
                destfile = paste(destination, "data_raw", "WDPA","temp", paste0("WDPA_WDOECM_", date,"_Public_", ISO_country_code, ".zip"), sep = "/"), method = 'auto', mode = "wb", quiet = TRUE)
  unzip(paste(destination, "data_raw", "WDPA","temp", paste0("WDPA_WDOECM_", date,"_Public_", ISO_country_code, ".zip"), sep = "/"),
        exdir = paste(destination, 'data_raw', 'WDPA', 'temp', sep = '/'))
  WDPA <- vect(paste(destination, "data_raw", "WDPA", "temp", paste0("WDPA_WDOECM_", date, "_Public_", ISO_country_code, ".gdb/"), sep = "/"), layer = paste0("WDPA_WDOECM_poly_", date, "_", ISO_country_code))
  WDPA <- st_as_sf(WDPA)[3]
  WDPA <- st_transform(WDPA, EPSG)
  WDPA <- st_rasterize(WDPA, dx = resolution, dy = resolution)
  WDPA[[1]] <- WDPA[[1]] != 0
  write_stars(WDPA, options = c("COMPRESS=LZW", "PREDICTOR=2"), NA_value = nodat,
              dsn = paste(destination, "data_raw", "WDPA", "WDPA_resBool.tif", sep = "/"))

  ##=========================
  ##
  ## Open Street Map : distance from cities, roads, rivers
  ##
  ##=========================

  dir.create(paste(destination, "data_raw", "OSM", sep = "/"), showWarnings = FALSE)
  dir.create(paste(destination, "data_raw", "OSM", "temp", sep = "/"), showWarnings = FALSE)
  osm_country <- oe_match(country_name, quiet = TRUE)
  oe_download(
    file_url = osm_country$url,
    file_size = osm_country$file_size,
    force_download = TRUE,
    max_file_size = osm_country$file_size + 1,
    download_directory = paste(destination, "data_raw", "OSM", "temp", sep = "/"),
    quiet = TRUE)

  download_file <- list.files(paste(destination, "data_raw", "OSM", "temp", sep = "/"), pattern = "osm.pbf", full.names = TRUE)
  type_object <- c("lines", "points", "lines", "multipolygons", "multipolygons")
  file_name <- c("roads", "place", "river", "lake", "reservoir")
  osm_key <- c("highway", "place", "waterway", "natural", "natural")
  osm_value <- c( 'highway=motorway or highway=trunk or highway=primary or highway=secondary or highway=primary_link or highway=secondary_link or highway=tertiary or highway=motorway_link)',
                  'place=city or place=town or place=village',
                  'waterway=river',
                  'water=lake',
                  'water=reservoir and reservoir_type!=sewage and reservoir_type!=water_storage')
  destfile <- paste0(substring(download_file, 1, nchar(download_file) - 8), ".o5m")
  system(glue('osmconvert {download_file} -o={destfile}'))
  for (i in 1:length(osm_key))
  {
    osm_file <- paste(destination, "data_raw", "OSM" , "temp", paste0(file_name[i], ".osm"), sep = "/")
    shpfile  <- paste(destination, "data_raw", "OSM" , "temp", paste0(file_name[i], "NoProj.shp"), sep = "/")
    projshp  <- paste(destination, "data_raw", "OSM" , "temp", paste0(file_name[i], ".shp"), sep = "/")
    file.tif <- paste(destination, "data_raw", "OSM" , "temp", paste0(file_name[i], ".tif"), sep = "/")
    distance.tif <- paste(destination, "data_raw", "OSM", paste0(file_name[i], "distance", ".tif"), sep = "/")
    distance_res.tif <- paste(destination, "data_raw", "OSM", paste0(file_name[i], "distance_res.tif"), sep = "/")
    system(glue("osmfilter {destfile}  --keep='{osm_value[i]}' > {osm_file}"))
    system(glue("ogr2ogr -overwrite -skipfailures -f 'ESRI Shapefile' -progress \\
              -sql 'SELECT osm_id, name,{osm_key[i]}  FROM {type_object[i]} WHERE {osm_key[i]} IS NOT NULL' \\
              -lco ENCODING=UTF-8  {shpfile} {osm_file}"), ignore.stdout = TRUE, ignore.stderr = TRUE)
    system(glue("ogr2ogr -overwrite -s_srs EPSG:4326 -t_srs {proj.t} -f 'ESRI Shapefile' \\
              -lco ENCODING=UTF-8 {projshp} {shpfile} "), ignore.stdout = TRUE, ignore.stderr = TRUE)
    system(glue("gdal_rasterize  {projshp} -te {extent} -tap -burn 1 -co 'COMPRESS=LZW' -co 'PREDICTOR=2' \\
              -ot Byte -of GTiff -a_nodata {nodat} -a_srs {proj.t} -tr 100 100 {file.tif}"), ignore.stdout = TRUE, ignore.stderr = TRUE)
    system(glue("gdal_proximity.py {file.tif} {distance.tif} -f -overwrite -co 'COMPRESS=LZW' -co 'PREDICTOR=2' \\
              -values 1 -ot Int16 -of GTiff -distunits GEO -use_input_nodata NO"), ignore.stdout = TRUE, ignore.stderr = TRUE)
    system(glue("gdalwarp -overwrite -r average -tr {resolution} {resolution} -ot Int16 -srcnodata {nodat} -of GTiff \\
              -dstnodata {nodat} {distance.tif} {distance_res.tif}"), ignore.stdout = TRUE, ignore.stderr = TRUE)
  }

  file.remove(list.files(paste(destination, "data_raw", "OSM", sep = "/"), pattern = "distance.tif", full.names = TRUE)) # delete temporary files
  water <- paste("lake", "reservoir", "river", sep = "|")
  watering_place <- list.files(paste(destination, "data_raw","OSM", sep = "/"), pattern = water, full.names = TRUE)
  dim_matrix <- dim(read_stars(watering_place[1])[[1]])[1]
  watering_place.tif <- read_stars(watering_place[1])
  watering_place.tif[[1]] <- pmin(read_stars(watering_place[1])[[1]],
                                  read_stars(watering_place[2])[[1]],
                                  read_stars(watering_place[3])[[1]])
  write_stars(watering_place.tif, paste(destination, "data_raw", "OSM", "wateringplacedistance_res.tif", sep = "/"))
  file.remove(list.files(paste(destination, "data_raw","OSM", sep = "/"), pattern = water, full.names = TRUE))

  for (j in list.files(path = paste(destination, "data_raw","OSM", sep = "/"), pattern = ".tif", full.names = TRUE))
  {
    osm_dist <- st_crop(read_stars(j), st_as_sf(seaBool))
    write_stars(osm_dist, j, options = c("COMPRESS=LZW","PREDICTOR=2"))
  }

  ##===================
  ##
  ## Population
  ##
  ##===================

  dir.create(path = paste(destination, "data_raw", "world_pop", sep = "/"), showWarnings = FALSE)
  dir.create(path = paste(destination, "data_raw", "world_pop", "temp", sep = "/"), showWarnings = FALSE)

  dest <- paste(destination, "data_raw", "world_pop", "temp", paste0(ISO_country_code, "_pop.tif"), sep = "/")
  URL <- paste0("https://data.worldpop.org/GIS/Population/Global_2000_2020_Constrained/2020/BSGM/",ISO_country_code, "/",tolower(ISO_country_code),"_ppp_2020_constrained.tif")
  download.file(URL, destfile = dest, quiet = TRUE)
  # unit set to pop/km²
  pop <- round(rast(dest) * 100)
  writeRaster(pop, filename = paste(destination, "data_raw", "world_pop", "temp", paste0(ISO_country_code, "_pop_km.tif"), sep = "/"),
              gdal = c("COMPRESS=LZW","PREDICTOR=2"), progress = 0, overwrite = TRUE, datatype = "Int16")
  sourcefile <- paste(destination, "data_raw", "world_pop", "temp", paste0(ISO_country_code, "_pop_km.tif"), sep = "/")
  destfile <- paste(destination, "data_raw", "world_pop", paste0(ISO_country_code, "_pop_res.tif"), sep = "/")
  system(glue('gdalwarp -tr {resolution} {resolution} -te {extent} -s_srs {proj.s} -t_srs {proj.t}  \\
              -r bilinear -ot Int16 -overwrite -srcnodata -32768 -dstnodata -32768 {sourcefile} {destfile}'), ignore.stdout = TRUE, ignore.stderr = TRUE)

  ##=====================================
  ##
  ## Merge environmental variables in one .tif
  ##
  ##=====================================

  if (forest){
    system(glue('gdal_merge.py -ot Int16 -of GTiff -o {paste(destination, "data_raw", "environ_no_name.tif", sep = "/")} -a_nodata {nodat} -separate \\
            -co "COMPRESS=LZW" -co "PREDICTOR=2" \\
            {paste(destination, "data_raw", "srtm_v1_4_90m", "aspect_res.tif", sep = "/")} \\
            {paste(destination, "data_raw", "srtm_v1_4_90m", "elevation_res.tif", sep = "/")} \\
            {paste(destination, "data_raw", "srtm_v1_4_90m", "roughness_res.tif", sep = "/")} \\
            {paste(destination, "data_raw", "srtm_v1_4_90m", "slope_res.tif", sep = "/")} \\
            {paste(destination, "data_raw", "srtm_v1_4_90m", "srad_res.tif", sep = "/")} \\
            {paste(destination, "data_raw", "soilgrids250_v2_0", "soilgrids_res.tif", sep = "/")} \\
            {paste(destination, "data_raw", "forestatrisk", "forest.tif", sep = "/")} \\
            {paste(destination, "data_raw", "distForest", "distForest.tif", sep = "/")} \\
            {paste(destination, "data_raw", "distSea", "distSea.tif", sep = "/")} \\
            {paste(destination, "data_raw", "OSM", "roadsdistance_res.tif", sep = "/")} \\
            {paste(destination, "data_raw", "OSM", "placedistance_res.tif", sep = "/")} \\
            {paste(destination, "data_raw", "OSM", "wateringplacedistance_res.tif", sep = "/")} \\
            {paste(destination, "data_raw", "WDPA", "WDPA_resBool.tif", sep = "/")} \\
            {paste(destination, "data_raw", "world_pop", paste0(ISO_country_code, "_pop_res.tif"), sep = "/")}'),
           ignore.stdout = TRUE, ignore.stderr = TRUE)
    environ <- rast(paste(destination, "data_raw", "environ_no_name.tif", sep = "/"))
    names(environ) <- c( "aspect", "elevation", "roughness", "slope", "srad", "soilgrids", "forest", "distanceForest",
                         "dist_sea", "dist_road", "dist_place", "dist_water", "WDPA", "population")
  }else{
    system(glue('gdal_merge.py -ot Int16 -of GTiff -o {paste(destination, "data_raw", "environ_no_name.tif", sep = "/")} -a_nodata {nodat} -separate \\
            -co "COMPRESS=LZW" -co "PREDICTOR=2" \\
            {paste(destination, "data_raw", "srtm_v1_4_90m", "aspect_res.tif", sep = "/")} \\
            {paste(destination, "data_raw", "srtm_v1_4_90m", "elevation_res.tif", sep = "/")} \\
            {paste(destination, "data_raw", "srtm_v1_4_90m", "roughness_res.tif", sep = "/")} \\
            {paste(destination, "data_raw", "srtm_v1_4_90m", "slope_res.tif", sep = "/")} \\
            {paste(destination, "data_raw", "srtm_v1_4_90m", "srad_res.tif", sep = "/")} \\
            {paste(destination, "data_raw", "soilgrids250_v2_0", "soilgrids_res.tif", sep = "/")}  \\
            {paste(destination, "data_raw", "distSea", "distSea.tif", sep = "/")} \\
            {paste(destination, "data_raw", "OSM", "roadsdistance_res.tif", sep = "/")} \\
            {paste(destination, "data_raw", "OSM", "placedistance_res.tif", sep = "/")} \\
            {paste(destination, "data_raw", "OSM", "wateringplacedistance_res.tif", sep = "/")} \\
            {paste(destination, "data_raw", "WDPA", "WDPA_resBool.tif", sep = "/")} \\
            {paste(destination, "data_raw", "world_pop", paste0(ISO_country_code, "_pop_res.tif"), sep = "/")}'), ignore.stdout = TRUE, ignore.stderr = TRUE)
    environ <- rast(paste(destination, "data_raw", "environ_no_name.tif", sep = "/"))
    names(environ) <- c( "aspect", "elevation", "roughness", "slope", "srad", "soilgrids",
                         "dist_sea", "dist_road", "dist_place", "dist_water", "WDPA", "population")
  }

  writeRaster(environ, filename = paste(destination, "data_raw", "environ_nocrop.tif", sep = "/"),
              gdal = c("COMPRESS=LZW","PREDICTOR=2"), progress = 0, overwrite = TRUE, datatype = "Int16")
  system(glue("gdal_translate -projwin {extent_num[1]} {extent_num[4]} {extent_num[3]} {extent_num[2]} -projwin_srs {proj.t} {paste(destination, 'data_raw', 'environ_nocrop.tif', sep = '/')} \\
              {paste(destination, 'data_raw', 'environ.tif', sep = '/')}"), ignore.stdout = TRUE, ignore.stderr = TRUE)

  unique_values <- unique(c(values(rast(paste(destination, "data_raw", "environ.tif", sep = "/"))[[6]])))
  create_xml_legend(unique_values = unique_values, destination = paste(destination, "data_raw", sep = "/"), name_file = "environ")

  unlink(paste(destination, "data_raw", "environ_nocrop.tif", sep = "/"), recursive = TRUE)
  unlink(paste(destination, "data_raw", "environ_no_name.tif", sep = "/"), recursive = TRUE)

  if (rm_download){
    unlink(paste(destination, "data_raw", "distSea", sep = "/"), recursive = TRUE)
    unlink(paste(destination, "data_raw", "grassdata", sep = "/"), recursive = TRUE)
    unlink(paste(destination, "data_raw", "OSM", sep = "/"), recursive = TRUE)
    unlink(paste(destination, "data_raw", "soilgrids250_v2_0", sep = "/"), recursive = TRUE)
    unlink(paste(destination, "data_raw", "srtm_v1_4_90m", sep = "/"), recursive = TRUE)
    unlink(paste(destination, "data_raw", "WDPA", sep = "/"), recursive = TRUE)
    unlink(paste(destination, "data_raw", "forestatrisk", sep = "/"), recursive = TRUE)
    unlink(paste(destination, "data_raw", "distForest", sep = "/"), recursive = TRUE)
    unlink(paste(destination, "data_raw", "world_pop", sep = "/"), recursive = TRUE)

  }


  return(paste(destination, "data_raw", "environ.tif", sep = "/"))
}

# End of file
