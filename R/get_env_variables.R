get_env_variables <- function(extent_latlon, extent, EPSG, country_name, destination){
  #' Create multilayer Tiff file with 11 variables
  #' Variables are type of soil, elevation, slope, aspect, roughness, solar radiation, distance to sea,
  #' protected areas, distance to roads, distance to cities and town, distance to rivers & lake.
  #'
  #' @param extent_latlon int vector. in this order c(lon_min, lat_min, lon_max, lat_max).
  #' @param extent character. First output of `transform_shp_country_extent` function.
  #' @param EPSG int. to consider for this country/area.
  #' @param country_name character. country name (in english) to considers.
  #' @param destination character. absolute path where to download files like `here()` output.
  #' @return character. absolute path to environ.tif file.
  #'
  #' @import glue
  #' @import here
  #' @import sf
  #' @import stars
  #' @import rgrass7
  #' @import osmextract
  #' @import RCurl
  #' @import wdpar
  #' @import countrycode
  #' @import stringr
  #' @import httr
  #' @import retry
  #'

  dir.create(path = destination, recursive = TRUE, showWarnings = FALSE)
  destination <- paste0(destination, "/")
  nodat <- -9999
  proj.s <- "EPSG:4326"
  proj.t <- paste0("EPSG:", EPSG)
  ISO_country_code <- countryname(country_name, destination = "iso3c")

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
      download.file(url = url, destfile = dest, verbose = FALSE)
    }
    list <- list.files(paste(destination, "data_raw", "soilgrids250_v2_0", "temp", sep = "/"), pattern = as.character(i), full.names = TRUE)
    merge_lon <- "c("
    for (k in 1:length(list)){
      if ( k == length(list)){
        merge_lon <- paste0(merge_lon, "read_stars('", list[k], "'), along = 2)")
      }else{
        merge_lon <- paste0(merge_lon, "read_stars('", list[k], "'), ")
      }
    }
    assign(paste0("merge", abs(i)), eval(parse(text = merge_lon)))
    if (i == extent_latlon[3]){
      merge_lat <- paste0(merge_lat, paste0("merge", abs(i), ", along = 1)"))
    }else{
      merge_lat <- paste0(merge_lat, paste0("merge", abs(i), ","))
    }
  }
  merge_all <- eval(parse(text = merge_lat))
  write_stars(merge_all, paste(destination, "data_raw", "soilgrids250_v2_0", "temp", "soilgridNoExtent.tif", sep = "/"),
              options = c("COMPRESS=LZW","PREDICTOR=2"))
  sourcefile <- paste(destination, "data_raw", "soilgrids250_v2_0", "temp", "soilgridNoExtent.tif", sep = "/")
  destfile <- paste(destination, "data_raw", "soilgrids250_v2_0", "soilgrids_1km.tif", sep = "/")
  system(glue("gdalwarp -overwrite -s_srs {proj.s} -t_srs {proj.t} \\
        -r bilinear -tr 1000 1000 -ot Int16 -srcnodata {nodat} -of GTiff \\
        {sourcefile} \\
        {destfile}"))

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
  system(glue('gdalbuildvrt {destfile} -input_file_list {paste(destination, "data_raw", "srtm_v1_4_90m", "temp", "sourcefilevrt.txt", sep = "/")}'))
  system(glue('gdalwarp -overwrite -t_srs {proj.t} -tap -r bilinear \\
            -co "COMPRESS=LZW" -co "PREDICTOR=2" -te {extent} -ot Int16 -of GTiff \\
            -tr 90 90 {destfile} {paste(destination, "data_raw", "srtm_v1_4_90m", "temp", "elevation.tif", sep = "/")}'))

  ## Compute slope, aspect and roughness using gdaldem
  # compute slope
  in_f <- paste(destination, "data_raw", "srtm_v1_4_90m", "temp", "elevation.tif", sep = "/")
  out_f <- paste(destination, "data_raw", "srtm_v1_4_90m", "temp", "slope.tif", sep = "/")
  system(glue('gdaldem slope {in_f} {out_f} -co "COMPRESS=LZW" -co "PREDICTOR=2"'))
  # compute aspect
  out_f <- paste(destination, "data_raw", "srtm_v1_4_90m", "temp", "aspect.tif", sep = "/")
  system(glue('gdaldem aspect {in_f} {out_f} -co "COMPRESS=LZW" -co "PREDICTOR=2"'))
  # compute roughness
  out_f <- paste(destination, "data_raw", "srtm_v1_4_90m", "temp", "roughness.tif", sep = "/")
  system(glue('gdaldem roughness {in_f} {out_f} -co "COMPRESS=LZW" -co "PREDICTOR=2"'))

  # Resolution from 90m x 90m to 1000m x 1000m using gdalwarp
  # elevation
  out_f <- paste(destination, "data_raw", "srtm_v1_4_90m", "elevation_1km.tif", sep = "/")
  system(glue('gdalwarp -r bilinear -tr 1000 1000 -ot Int16 -of GTiff \\
        -co "COMPRESS=LZW" -co "PREDICTOR=2" -overwrite {in_f} {out_f}'))
  # aspect
  in_f <- paste(destination, "data_raw", "srtm_v1_4_90m", "temp", "aspect.tif", sep = "/")
  out_f <-paste(destination, "data_raw", "srtm_v1_4_90m", "aspect_1km.tif", sep = "/")
  system(glue('gdalwarp -r bilinear -tr 1000 1000 -ot Int16 -of GTiff \\
        -co "COMPRESS=LZW" -co "PREDICTOR=2" -overwrite {in_f} {out_f}'))
  # slope
  in_f <- paste(destination, "data_raw", "srtm_v1_4_90m", "temp", "slope.tif", sep = "/")
  out_f <- paste(destination, "data_raw", "srtm_v1_4_90m", "slope_1km.tif", sep = "/")
  system(glue('gdalwarp -r bilinear -tr 1000 1000 -ot Int16 -of GTiff \\
        -co "COMPRESS=LZW" -co "PREDICTOR=2" -overwrite {in_f} {out_f}'))
  # roughness
  in_f <- paste(destination, "data_raw", "srtm_v1_4_90m", "temp", "roughness.tif", sep = "/")
  out_f <- paste(destination, "data_raw", "srtm_v1_4_90m", "roughness_1km.tif", sep = "/")
  system(glue('gdalwarp -r bilinear -tr 1000 1000 -ot Int16 -of GTiff \\
        -co "COMPRESS=LZW" -co "PREDICTOR=2" -overwrite {in_f} {out_f}'))

  ##==============================
  ##
  ## Solar radiation
  ##
  #### with r.sun at 90m resolution
  ## Solar radiation (in Wh.m-2.day-1) was computed from altitude,
  ## slope and aspect using the function r.sun from the GRASS GIS software.
  ## We incorporated the shadowing effect of terrain to compute the solar radiation.
  ## Solar radiation was computed for the Julian day 79 (20th of March for regular years=equinox).
  ##
  ##==============================

  ## Initialize GRASS
  setwd(paste(destination, "data_raw", sep = "/"))
  Sys.setenv(LD_LIBRARY_PATH = paste("/usr/lib/grass80/lib", Sys.getenv("LD_LIBRARY_PATH"), sep = ":"))
  # use a georeferenced raster
  elevation <- paste(destination, "data_raw", "srtm_v1_4_90m", "temp", "elevation.tif", sep = "/")
  system(glue('grass -c {elevation} grassdata/environ'))
  # connect to grass database
  initGRASS(gisBase = "/usr/lib/grass80",
            gisDbase = "grassdata", home = tempdir(),
            location = "environ", mapset = "PERMANENT",
            override = TRUE)
  ## Import raster in grass
  system(glue("r.in.gdal -e --o input={elevation} output=elevation"))
  slope <- paste(destination, "data_raw", "srtm_v1_4_90m", "temp", "slope.tif", sep = "/")
  system(glue("r.in.gdal -e --o input={slope} output=slope"))
  aspect <- paste(destination, "data_raw", "srtm_v1_4_90m", "temp", "aspect.tif", sep = "/")
  system(glue("r.in.gdal -e --o input={aspect} output=aspect"))
  # Compute radiation
  system(glue("r.sun --overwrite --verbose elevation=elevation aspect=aspect slope=slope day=79 glob_rad=global_rad"))

  # Export
  system(glue("r.out.gdal -f --verbose --overwrite input=global_rad \\
  			 output={paste(destination, 'data_raw', 'srtm_v1_4_90m', 'temp', 'srad.tif', sep = "/")} type=Int16  \\
  			 createopt='COMPRESS=LZW' nodata={nodat}"))

  # Resolution from 90m x 90m to 1000m x 1000m using gdalwarp
  # srad
  in_f <- paste(destination, "data_raw", "srtm_v1_4_90m", "temp", "srad.tif", sep = "/")
  out_f <- paste(destination, "data_raw", "srtm_v1_4_90m", "srad_1km.tif", sep = "/")
  system(glue('gdalwarp  -t_srs {proj.t} \\
        -r bilinear -tr 1000 1000 -ot Int16 -of GTiff \\
        -co "COMPRESS=LZW" -co "PREDICTOR=2" -overwrite {in_f} {out_f}'))

  # unlink(paste(destination, "data_raw", "srtm_v1_4_90m", "temp", sep = "/"), recursive = TRUE)

  ##===========================
  ##
  ## Distance to Sea
  ##
  ##===========================

  dir.create(paste(destination, "data_raw", "distSea", sep = "/"), showWarnings = FALSE)
  seaBool <- read_stars(paste(destination, "data_raw", "srtm_v1_4_90m", "srad_1km.tif", sep = "/")) == nodat
  write_stars(seaBool, options = c("COMPRESS=LZW", "PREDICTOR=2"), NA_value = nodat,
              dsn = paste(destination, "data_raw", "distSea", "Sea_1kmBool.tif", sep = "/"))
  sourcefile <- paste(destination, "data_raw", "distSea", "Sea_1kmBool.tif", sep = "/")
  destfile <- paste(destination, "data_raw", "distSea", "distSea.tif", sep = "/")
  system(glue("gdal_proximity.py -ot Int16 -of GTiff -nodata {nodat} \\
        -values {nodat} -distunits GEO -use_input_nodata YES {sourcefile} {destfile}"))
  distSea <- read_stars(paste(destination, "data_raw", "distSea", "distSea.tif", sep = "/"))
  distSea[[1]][distSea[[1]] == 0] <- NA
  write_stars(distSea, paste(destination, "data_raw", "distSea", "distSea.tif", sep = "/"), options = c("COMPRESS=LZW", "PREDICTOR=2"))

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
  POST(url = "https://www.protectedplanet.net/downloads", encode = "json", body = list("domain" = "general", "format" = "gdb", "token" = ISO_country_code))
  wait_until(url.exists(paste0("https://d1gam3xoknrgr2.cloudfront.net/current/WDPA_WDOECM_", str_remove(str_to_title(format(Sys.Date(), format = "%b%Y")), "\\."), "_Public_", ISO_country_code, ".zip")), timeout = 60)
  download.file(paste0("https://d1gam3xoknrgr2.cloudfront.net/current/WDPA_WDOECM_", str_remove(str_to_title(format(Sys.Date(), format = "%b%Y")), "\\."), "_Public_", ISO_country_code, ".zip"),
                destfile = paste(destination, "data_raw", "WDPA","temp", paste0("WDPA_WDOECM_", str_remove(str_to_title(format(Sys.Date(), format = "%b%Y")), "\\."),"_Public_", ISO_country_code, ".zip"), sep = "/"), method = 'auto', mode = "wb")
  WDPA <- wdpa_read(paste(destination, "data_raw", "WDPA", "temp", paste0("WDPA_WDOECM_", str_remove(str_to_title(format(Sys.Date(), format = "%b%Y")), "\\."), "_Public_", ISO_country_code, ".zip"), sep = "/"))
  WDPA = st_as_stars(WDPA[3])
  st_set_crs(WDPA, EPSG)
  WDPA <- st_transform_proj(WDPA, proj.t)
  WDPA <- st_combine(WDPA)
  WDPA <- st_rasterize(st_as_sf(WDPA), dx = 1000, dy = 1000)
  write_stars(WDPA, options = c("COMPRESS=LZW", "PREDICTOR=2"), NA_value = nodat,
              dsn = paste(destination, "data_raw", "WDPA", "WDPA_1kmBool.tif", sep = "/"))

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
    download_directory = paste(destination, "data_raw", "OSM", "temp", sep = "/"))

  download_file <- list.files(paste(destination, "data_raw", "OSM", "temp", sep = "/"), pattern = "osm.pbf", full.names = TRUE)
  type_object <- c("lines", "points", "lines", "multipolygons", "multipolygons")
  file_name <- c("roads", "place", "river", "lake", "reservoir")
  osm_key <- c("highway", "place", "waterway", "natural", "natural")
  osm_value <- c( 'highway=motorway or highway=trunk or highway=primary or highway=secondary or highway=primary_link or highway=secondary_link or highway=tertiary or highway=motorway_link)',
                  'place=city or place=town or place=village',
                  'waterway=river',
                  'water=lake',
                  'water=reservoir and reservoir_type!=sewage and reservoir_type!=water_storage')
  destfile <- paste0(substring(download_file, 1, nchar(download_file)-8), ".o5m")
  system(glue('osmconvert {download_file} -o={destfile}'))
  for (i in 1:length(osm_key))
  {
    osm_file <- paste(destination, "data_raw", "OSM" , "temp", paste0(file_name[i], ".osm"), sep = "/")
    shpfile  <- paste(destination, "data_raw", "OSM" , "temp", paste0(file_name[i], "NoProj.shp"), sep = "/")
    projshp  <- paste(destination, "data_raw", "OSM" , "temp", paste0(file_name[i], ".shp"), sep = "/")
    file.tif <- paste(destination, "data_raw", "OSM" , "temp", paste0(file_name[i], ".tif"), sep = "/")
    distance.tif <- paste(destination, "data_raw", "OSM", paste0(file_name[i], "distance", ".tif"), sep = "/")
    distance_1km.tif <- paste(destination, "data_raw", "OSM", paste0(file_name[i], "distance_1km.tif"), sep = "/")
    system(glue("osmfilter {destfile}  --keep='{osm_value[i]}' > {osm_file}"))
    system(glue("ogr2ogr -overwrite -skipfailures -f 'ESRI Shapefile' -progress \\
              -sql 'SELECT osm_id, name,{osm_key[i]}  FROM {type_object[i]} WHERE {osm_key[i]} IS NOT NULL' \\
              -lco ENCODING=UTF-8  {shpfile} {osm_file}"))
    system(glue("ogr2ogr -overwrite -s_srs EPSG:4326 -t_srs {proj.t} -f 'ESRI Shapefile' \\
              -lco ENCODING=UTF-8 {projshp} {shpfile} "))
    system(glue("gdal_rasterize  {projshp} -te {extent} -tap -burn 1 -co 'COMPRESS=LZW' -co 'PREDICTOR=2' \\
              -ot Byte -of GTiff -a_nodata {nodat} -a_srs {proj.t} -tr 100 100 {file.tif}"))
    system(glue("gdal_proximity.py {file.tif} {distance.tif} -f -overwrite -co 'COMPRESS=LZW' -co 'PREDICTOR=2' \\
              -values 1 -ot Int16 -of GTiff -distunits GEO "))
    system(glue("gdalwarp -overwrite -r average -tr 1000 1000 -ot Int16 -srcnodata {nodat} -of GTiff \\
              -dstnodata {nodat} {distance.tif} {distance_1km.tif}"))
  }
  # unlink(paste(destination, "data_raw", "OSM", "temp", sep = "/"), recursive = TRUE) # delete temporary files

  file.remove(list.files(paste(destination, "data_raw", "OSM", sep = "/"), pattern = "distance.tif", full.names = TRUE)) # delete temporary files
  water <- paste("lake", "reservoir", "river", sep = "|")
  watering_place <- list.files(paste(destination, "data_raw","OSM", sep = "/"), pattern = water, full.names = TRUE)
  dim_matrix <- dim(read_stars(watering_place[1])[[1]])[1]
  watering_place.tif <- pmin(read_stars(watering_place[1])[[1]],
                             read_stars(watering_place[2])[[1]],
                             read_stars(watering_place[3])[[1]])
  lake <- read_stars(paste(destination, "data_raw", "OSM", "lakedistance_1km.tif", sep = "/"))
  watering_place.tif <- st_as_stars(watering_place.tif, dimension = st_dimensions(lake))
  write_stars(watering_place.tif, paste(destination, "data_raw", "OSM", "wateringplacedistance_1km.tif", sep = "/"))
  file.remove(list.files(paste(destination, "data_raw","OSM", sep = "/"), pattern = water, full.names = TRUE))

  ##=====================================
  ##
  ## Merge environmental variables in one .tif
  ##
  ##=====================================

  system(glue('gdal_merge.py -ot Int16 -of GTiff -o {paste(destination, "data_raw", "environ.tif", sep = "/")} -a_nodata {nodat} -separate \\
            -co "COMPRESS=LZW" -co "PREDICTOR=2" \\
            {paste(destination, "data_raw", "srtm_v1_4_90m", "aspect_1km.tif", sep = "/")} \\
            {paste(destination, "data_raw", "srtm_v1_4_90m", "elevation_1km.tif", sep = "/")} {paste(destination, "data_raw", "srtm_v1_4_90m", "roughness_1km.tif", sep = "/")} \\
            {paste(destination, "data_raw", "srtm_v1_4_90m", "slope_1km.tif", sep = "/")} {paste(destination, "data_raw", "srtm_v1_4_90m", "srad_1km.tif", sep = "/")} \\
            {paste(destination, "data_raw", "soilgrids250_v2_0", "soilgrids_1km.tif", sep = "/")}  \\
            {paste(destination, "data_raw", "distSea", "distSea.tif", sep = "/")} {paste(destination, "data_raw", "OSM", "roadsdistance_1km.tif", sep = "/")} \\
            {paste(destination, "data_raw", "OSM", "placedistance_1km.tif", sep = "/")} {paste(destination, "data_raw", "OSM", "wateringplacedistance_1km.tif", sep = "/")} \\
            {paste(destination, "data_raw", "WDPA", "WDPA_1kmBool.tif", sep = "/")} '))
  environ <- split(read_stars(paste(destination, "data_raw", "environ.tif", sep = "/")))
  names(environ) <- c( "aspect", "elevation", "roughness", "slope", "srad", "soilgrids",
                       "distanceSea", "distanceRoad", "distancePlace", "distancewater", "WDPA")
  write_stars(merge(environ), dsn = paste(destination, "data_raw", "environ.tif", sep = "/"), options = c("COMPRESS=LZW","PREDICTOR=2"))

  return(paste(destination, "data_raw", "environ.tif", sep = "/"))
}





