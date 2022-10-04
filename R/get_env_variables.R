get_env_variables <- function(extent_latlon, extent, EPSG, country_name){
  #' Get variables from many sites.
  #'
  #' @param extent_latlon int vector. in this order c(lon_min, lat_min, lon_max, lat_max).
  #' @param extent character. First output of `transform_shp_country_extent` function.
  #' @param EPSG int. to consider for this country/area.
  #' @param country_name character. country name (in english) to considers.
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

  nodat <- -9999
  proj.s <- "EPSG:4326"
  proj.t <- paste0("EPSG:", EPSG)
  ISO_country_code <- countryname(country_name, destination = "iso3c")
  ##==============================
  ##
  ## Soilgrids
  ##
  ##==============================

  dir.create(here("data_raw"), showWarnings = FALSE)
  dir.create(here("data_raw", "soilgrids250_v2_0"), showWarnings = FALSE)
  dir.create(here("data_raw", "soilgrids250_v2_0", "temp"), showWarnings = FALSE)
  # file available only by 2° x 2° for soilgrids250
  merge_lat <- "c("
  for (i in extent_latlon[1]:extent_latlon[3]) # longitude
  {
    alpha <- TRUE
    for (j in extent_latlon[2]:extent_latlon[4]) # latitude
    {
      url = paste("https://maps.isric.org/mapserv?map=/map/wrb.map&SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage&COVERAGEID=MostProbable&FORMAT=image/tiff&SUBSET=long(",i, ".0000,", i + 1, ".0000)&SUBSET=lat(", j, ".0000,", j + 1, ".0000)&SUBSETTINGCRS=http://www.opengis.net/def/crs/EPSG/0/4326&OUTPUTCRS=http://www.opengis.net/def/crs/EPSG/0/4326", sep = "")
      dest = here("data_raw", "soilgrids250_v2_0", "temp", paste("soilgrids_", j, "_", i, ".tif", sep = ""))
      download.file(url = url, destfile = dest, verbose = FALSE)
    }
    list <- list.files(here("data_raw", "soilgrids250_v2_0", "temp"), pattern = as.character(i), full.names = TRUE)
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
  write_stars(merge_all, here("data_raw", "soilgrids250_v2_0", "temp", "soilgridNoExtent.tif"),
              options = c("COMPRESS=LZW","PREDICTOR=2"))
  sourcefile <- here("data_raw", "soilgrids250_v2_0", "temp", "soilgridNoExtent.tif")
  destfile <- here("data_raw", "soilgrids250_v2_0", "soilgrids_1km.tif")
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

  dir.create(here("data_raw", "srtm_v1_4_90m"), showWarnings = FALSE)
  dir.create(here("data_raw", "srtm_v1_4_90m", "temp"), showWarnings = FALSE)
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
      dst <- paste0(here("data_raw", "srtm_v1_4_90m", "temp", "srtm_"), i, ".zip")
      url.tile <- paste0("https://srtm.csi.cgiar.org/wp-content/uploads/files/srtm_5x5/TIFF/srtm_", i, ".zip")
      download.file(url = url.tile, destfile = dst, method = "wget", quiet = TRUE)
      unzip(dst, exdir = here("data_raw", "srtm_v1_4_90m", "temp"), overwrite = TRUE)
    }
  }

  # Merge and Reproject with EPSG
  sourcefile <- list.files(here("data_raw", "srtm_v1_4_90m", "temp"), pattern = "*.tif", full.names = TRUE)
  file <- file(here("data_raw", "srtm_v1_4_90m", "temp", "sourcefilevrt.txt"))
  writeLines(sourcefile, file)
  close(file)

  destfile <- here("data_raw", "srtm_v1_4_90m", "temp", "srtm.vrt")
  system(glue('gdalbuildvrt {destfile} -input_file_list {here("data_raw", "srtm_v1_4_90m", "temp", "sourcefilevrt.txt")}'))
  system(glue('gdalwarp -overwrite -t_srs {proj.t} -tap -r bilinear \\
            -co "COMPRESS=LZW" -co "PREDICTOR=2" -te {extent} -ot Int16 -of GTiff \\
            -tr 90 90 {destfile} {here("data_raw", "srtm_v1_4_90m", "temp", "elevation.tif")}'))

  ## Compute slope, aspect and roughness using gdaldem
  # compute slope
  in_f <- here("data_raw", "srtm_v1_4_90m", "temp", "elevation.tif")
  out_f <- here("data_raw", "srtm_v1_4_90m", "temp", "slope.tif")
  system(glue('gdaldem slope {in_f} {out_f} -co "COMPRESS=LZW" -co "PREDICTOR=2"'))
  # compute aspect
  out_f <- here("data_raw", "srtm_v1_4_90m", "temp", "aspect.tif")
  system(glue('gdaldem aspect {in_f} {out_f} -co "COMPRESS=LZW" -co "PREDICTOR=2"'))
  # compute roughness
  out_f <- here("data_raw", "srtm_v1_4_90m", "temp", "roughness.tif")
  system(glue('gdaldem roughness {in_f} {out_f} -co "COMPRESS=LZW" -co "PREDICTOR=2"'))

  # Resolution from 90m x 90m to 1000m x 1000m using gdalwarp
  # elevation
  out_f <- here("data_raw", "srtm_v1_4_90m", "elevation_1km.tif")
  system(glue('gdalwarp -r bilinear -tr 1000 1000 -ot Int16 -of GTiff \\
        -co "COMPRESS=LZW" -co "PREDICTOR=2" -overwrite {in_f} {out_f}'))
  # aspect
  in_f <- here("data_raw", "srtm_v1_4_90m", "temp", "aspect.tif")
  out_f <-here("data_raw", "srtm_v1_4_90m", "aspect_1km.tif")
  system(glue('gdalwarp -r bilinear -tr 1000 1000 -ot Int16 -of GTiff \\
        -co "COMPRESS=LZW" -co "PREDICTOR=2" -overwrite {in_f} {out_f}'))
  # slope
  in_f <- here("data_raw", "srtm_v1_4_90m", "temp", "slope.tif")
  out_f <- here("data_raw", "srtm_v1_4_90m", "slope_1km.tif")
  system(glue('gdalwarp -r bilinear -tr 1000 1000 -ot Int16 -of GTiff \\
        -co "COMPRESS=LZW" -co "PREDICTOR=2" -overwrite {in_f} {out_f}'))
  # roughness
  in_f <- here("data_raw", "srtm_v1_4_90m", "temp", "roughness.tif")
  out_f <- here("data_raw", "srtm_v1_4_90m", "roughness_1km.tif")
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
  setwd(here("data_raw"))
  Sys.setenv(LD_LIBRARY_PATH = paste("/usr/lib/grass80/lib", Sys.getenv("LD_LIBRARY_PATH"), sep = ":"))
  # use a georeferenced raster
  elevation <- here("data_raw", "srtm_v1_4_90m", "temp", "elevation.tif")
  system(glue('grass -c {elevation} grassdata/environ'))
  # connect to grass database
  initGRASS(gisBase = "/usr/lib/grass80",
            gisDbase = "grassdata", home = tempdir(),
            location = "environ", mapset = "PERMANENT",
            override = TRUE)
  ## Import raster in grass
  system(glue("r.in.gdal -e --o input={elevation} output=elevation"))
  slope <- here("data_raw", "srtm_v1_4_90m", "temp", "slope.tif")
  system(glue("r.in.gdal -e --o input={slope} output=slope"))
  aspect <- here("data_raw", "srtm_v1_4_90m", "temp", "aspect.tif")
  system(glue("r.in.gdal -e --o input={aspect} output=aspect"))
  # Compute radiation
  system(glue("r.sun --overwrite --verbose elevation=elevation aspect=aspect slope=slope day=79 glob_rad=global_rad"))

  # Export
  system(glue("r.out.gdal -f --verbose --overwrite input=global_rad \\
  			 output={here('data_raw', 'srtm_v1_4_90m', 'temp', 'srad.tif')} type=Int16  \\
  			 createopt='COMPRESS=LZW' nodata={nodat}"))

  # Resolution from 90m x 90m to 1000m x 1000m using gdalwarp
  # srad
  in_f <- here("data_raw", "srtm_v1_4_90m", "temp", "srad.tif")
  out_f <- here("data_raw", "srtm_v1_4_90m", "srad_1km.tif")
  system(glue('gdalwarp  -t_srs {proj.t} \\
        -r bilinear -tr 1000 1000 -ot Int16 -of GTiff \\
        -co "COMPRESS=LZW" -co "PREDICTOR=2" -overwrite {in_f} {out_f}'))

  # unlink(here("data_raw", "srtm_v1_4_90m", "temp"), recursive = TRUE)

  ##===========================
  ##
  ## Distance to Sea
  ##
  ##===========================

  dir.create(here("data_raw", "distSea"), showWarnings = FALSE)
  seaBool <- read_stars(here("data_raw", "srtm_v1_4_90m", "srad_1km.tif")) == nodat
  write_stars(seaBool, options = c("COMPRESS=LZW", "PREDICTOR=2"), NA_value = nodat,
              dsn = here("data_raw", "distSea", "Sea_1kmBool.tif"))
  sourcefile <- here("data_raw", "distSea", "Sea_1kmBool.tif")
  destfile <- here("data_raw", "distSea", "distSea.tif")
  system(glue("gdal_proximity.py -ot Int16 -of GTiff -nodata {nodat} \\
        -values {nodat} -distunits GEO -use_input_nodata YES {sourcefile} {destfile}"))
  distSea <- read_stars(here("data_raw", "distSea", "distSea.tif"))
  distSea[[1]][distSea[[1]] == 0] <- NA
  write_stars(distSea, here("data_raw", "distSea", "distSea.tif"), options = c("COMPRESS=LZW", "PREDICTOR=2"))

  ##=========================
  ##
  ## WDPA : World Database Protected Areas
  ## International Union for Conservation of Nature
  ## UNEP-WCMC (2022). Protected Area Profile for New Caledonia from the World Database of Protected Areas, May 2022.
  ## Available at: www.protectedplanet.net
  ##
  ##=========================

  dir.create(here("data_raw", "WDPA"), showWarnings = FALSE)
  dir.create(here("data_raw", "WDPA", "temp"), showWarnings = FALSE)
  POST(url = "https://www.protectedplanet.net/downloads", encode = "json", body = list("domain" = "general", "format" = "gdb", "token" = ISO_country_code))
  wait_until(url.exists(paste("https://d1gam3xoknrgr2.cloudfront.net/current/WDPA_WDOECM_", str_remove(str_to_title(format(Sys.Date(), format = "%b%Y")), "\\."), "_Public_", ISO_country_code, ".zip", sep = "")), timeout = 60)
  download.file(paste("https://d1gam3xoknrgr2.cloudfront.net/current/WDPA_WDOECM_", str_remove(str_to_title(format(Sys.Date(), format = "%b%Y")), "\\."), "_Public_", ISO_country_code, ".zip", sep = ""),
                destfile = here("data_raw", "WDPA","temp", paste("WDPA_WDOECM_", str_remove(str_to_title(format(Sys.Date(), format = "%b%Y")), "\\."),"_Public_", ISO_country_code, ".zip")), method = 'auto', mode = "wb")
  WDPA <- wdpa_read(here("data_raw", "WDPA", "temp", paste("WDPA_WDOECM_", str_remove(str_to_title(format(Sys.Date(), format = "%b%Y")), "\\."), "_Public_", ISO_country_code, ".zip")))
  WDPA = st_as_stars(WDPA[3])
  st_set_crs(WDPA, EPSG)
  WDPA <- st_transform_proj(WDPA, proj.t)
  WDPA <- st_combine(WDPA)
  WDPA <- st_rasterize(st_as_sf(WDPA), dx = 1000, dy = 1000)
  write_stars(WDPA, options = c("COMPRESS=LZW", "PREDICTOR=2"), NA_value = nodat,
              dsn = here("data_raw", "WDPA", "WDPA_1kmBool.tif" ))

  ##=========================
  ##
  ## Open Street Map : distance from cities, roads, rivers
  ##
  ##=========================

  dir.create(here("data_raw", "OSM"), showWarnings = FALSE)
  dir.create(here("data_raw", "OSM", "temp"), showWarnings = FALSE)
  osm_country <- oe_match(country_name, quiet = TRUE)
  oe_download(
    file_url = osm_country$url,
    file_size = osm_country$file_size,
    force_download = TRUE,
    max_file_size = osm_country$file_size + 1,
    download_directory = here("data_raw", "OSM", "temp"))

  download_file <- list.files(here("data_raw", "OSM", "temp"), pattern = "osm.pbf", full.names = TRUE)
  type_object <- c("lines", "points", "lines", "multipolygons", "multipolygons")
  file_name <- c("roads", "place", "river", "lake", "reservoir")
  osm_key <- c("highway", "place", "waterway", "natural", "natural")
  osm_value <- c( 'highway=motorway or highway=trunk or highway=primary or highway=secondary or highway=primary_link or highway=secondary_link or highway=tertiary or highway=motorway_link)',
                  'place=city or place=town or place=village',
                  'waterway=river',
                  'water=lake',
                  'water=reservoir and reservoir_type!=sewage and reservoir_type!=water_storage')
  destfile <- paste(substring(download_file, 1, nchar(download_file)-8), ".o5m", sep ="")
  system(glue('osmconvert {download_file} -o={destfile}'))
  for (i in 1:length(osm_key))
  {
    osm_file <- here("data_raw", "OSM" , "temp", paste(file_name[i], ".osm", sep = ""))
    shpfile  <- here("data_raw", "OSM" , "temp", paste(file_name[i], "NoProj.shp", sep = ""))
    projshp  <- here("data_raw", "OSM" , "temp", paste(file_name[i], ".shp", sep = ""))
    file.tif <- here("data_raw", "OSM" , "temp", paste(file_name[i], ".tif", sep = ""))
    distance.tif <- here("data_raw", "OSM", paste(file_name[i], "distance", ".tif", sep = ""))
    distance_1km.tif <- here("data_raw", "OSM", paste(file_name[i], "distance_1km.tif", sep = ""))
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
  # unlink(here("data_raw", "OSM", "temp"), recursive = TRUE) # delete temporary files

  file.remove(list.files(here("data_raw", "OSM"), pattern = "distance.tif", full.names = TRUE)) # delete temporary files
  water <- paste("lake", "reservoir", "river", sep = "|")
  watering_place <- list.files(here("data_raw","OSM"), pattern = water, full.names = TRUE)
  dim_matrix <- dim(read_stars(watering_place[1])[[1]])[1]
  watering_place.tif <- pmin(read_stars(watering_place[1])[[1]],
                             read_stars(watering_place[2])[[1]],
                             read_stars(watering_place[3])[[1]])
  lake <- read_stars(here("data_raw", "OSM", "lakedistance_1km.tif"))
  watering_place.tif <- st_as_stars(watering_place.tif, dimension = st_dimensions(lake))
  write_stars(watering_place.tif, here("data_raw", "OSM", "wateringplacedistance_1km.tif"))
  file.remove(list.files(here("data_raw","OSM"), pattern = water, full.names = TRUE))

  ##=====================================
  ##
  ## Merge environmental variables in one .tif
  ##
  ##=====================================

  system(glue('gdal_merge.py -ot Int16 -of GTiff -o {here("data_raw", "environ.tif")} -a_nodata {nodat} -separate \\
            -co "COMPRESS=LZW" -co "PREDICTOR=2" \\
            {here("data_raw", "srtm_v1_4_90m", "aspect_1km.tif")} \\
            {here("data_raw", "srtm_v1_4_90m", "elevation_1km.tif")} {here("data_raw", "srtm_v1_4_90m", "roughness_1km.tif")} \\
            {here("data_raw", "srtm_v1_4_90m", "slope_1km.tif")} {here("data_raw", "srtm_v1_4_90m", "srad_1km.tif")} \\
            {here("data_raw", "soilgrids250_v2_0", "soilgrids_1km.tif")}  \\
            {here("data_raw", "distSea", "distSea.tif")} {here("data_raw", "OSM", "roadsdistance_1km.tif")} \\
            {here("data_raw", "OSM", "placedistance_1km.tif")} {here("data_raw", "OSM", "wateringplacedistance_1km.tif")} \\
            {here("data_raw", "WDPA", "WDPA_1kmBool.tif")} '))
  environ <- split(read_stars(here("data_raw", "environ.tif")))
  names(environ) <- c( "aspect", "elevation", "roughness", "slope", "srad", "soilgrids",
                       "distanceSea", "distanceRoad", "distancePlace", "distancewater", "WDPA")
  write_stars(merge(environ), dsn = here("data_raw", "environ.tif"), options = c("COMPRESS=LZW","PREDICTOR=2"))

}





