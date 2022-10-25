transform_shp_country_extent <- function(EPSG, country_name = NULL, shapefile_path = NULL, extent_short = NULL){
  #' Create extent & extent in latitude and longitude
  #'
  #' @description
  #' With a country name, a shapefile or a extent and an EPSG value.
  #' Succed to give extent in EPSG reproject and extent with default EPSG also knows as latitude and longitude coordinates.
  #'
  #' @param country_name character. English country name, default is NULL.
  #' @param shapefile_path character. Path to a .shp file, default is NULL.
  #' @param extent_short int vector. length 4, with this order c(xmin, ymin, xmax, ymax), default is NULL.
  #' @param EPSG int. to consider for this country/area.
  #'
  #' @return character vector. with extent of the area in one character, other are latlon coord of area
  #' @import sf
  #' @importFrom utils download.file unzip
  #' @import terra
  #' @import countrycode
  #' @export

  if ((as.numeric(!is.null(country_name)) + as.numeric(!is.null(shapefile_path)) + as.numeric(!is.null(extent_short))) != 1){
    stop("One attribute among country_name, shapefile_path and extent_short")

  }
  if (!is.null(country_name)){
    dir.create(paste(getwd(), "gaul", sep = "/"), showWarnings = FALSE)
    ISO_country_code <- countryname(country_name, destination = "iso3c")
    URL <- paste0("https://geodata.ucdavis.edu/gadm/gadm3.6/gpkg/gadm36_", ISO_country_code, "_gpkg.zip")
    tempZip <- tempfile()
    download.file(URL, quiet = TRUE, destfile = tempZip)
    # Unzip
    unzip(tempZip, exdir = paste(getwd(), "gaul", sep = "/"), overwrite = TRUE)
    # Read vector (level 0 for country borders)
    coord <- st_bbox(sf::st_read(paste(getwd(), "gaul", paste0("gadm36_", ISO_country_code, ".gpkg"), sep = '/'),
                           layer = paste0("gadm36_", ISO_country_code, "_0"), quiet = TRUE))
    unlink(paste(getwd(), "gaul/", sep = "/"), recursive = TRUE)
    unlink(paste(getwd(), "gaul.zip", sep = "/"), recursive = TRUE)
    lonmin <- floor(coord[1])
    lonmax <- ceiling(coord[3])
    latmin <- floor(coord[2])
    latmax <- ceiling(coord[4])
    extent_latlon <- c(lonmin = lonmin, latmin = latmin, lonmax = lonmax, latmax = latmax)
    e <- ext(extent_latlon[1], extent_latlon[3], extent_latlon[2], extent_latlon[4])
    e <- as.polygons(e)
    crs(e) <- "epsg:4326"
    extent <- st_bbox(project(e, paste0("epsg:", EPSG)))
    extent <- c(floor(extent[1]), floor(extent[2]), ceiling(extent[3]), ceiling(extent[4]))

  }
  if (!is.null(shapefile_path)){
    map <- read_sf(shapefile_path)
    map <- st_transform(map, EPSG)
    extent <- round(st_bbox(map))
    coord <- st_coordinates(map)
    lonmin <- floor(min(coord[,1]))
    lonmax <- ceiling(max(coord[,1]))
    latmin <- floor(min(coord[,2]))
    latmax <- ceiling(max(coord[,2]))
    extent_latlon <- c(lonmin = lonmin, lonmax = lonmax, latmin = latmin, latmax = latmax)
    extent_latlon <- as.polygons(ext(extent_latlon))
    crs(extent_latlon) <- paste0("epsg:", EPSG)
    extent_latlon <- st_bbox(project(extent_latlon, "epsg:4326"))
    extent_latlon <- c(floor(extent_latlon[1]), floor(extent_latlon[2]), ceiling(extent_latlon[3]), ceiling(extent_latlon[4]))
  }
  if (!is.null(extent_short)){
    extent <- round(extent_short)
    e <- ext(extent[1], extent[3], extent[2], extent[4])
    e <- as.polygons(e)
    crs(e) <- paste0("epsg:", EPSG)
    extent_latlon <- st_bbox(project(e, "epsg:4326"))
    extent_latlon <- c(floor(extent_latlon[1]), floor(extent_latlon[2]), ceiling(extent_latlon[3]), ceiling(extent_latlon[4]))
  }
  extent <- paste(extent[1], extent[2], extent[3], extent[4], sep = " ")
  return(c(extent, extent_latlon))
}
