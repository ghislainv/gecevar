transform_shp_country_extent <- function(EPSG, country_name = NULL, shapefile_path = NULL, extent_short = NULL){
  #' Create extent & extent in latitude and longitude
  #'
  #' @description
  #' With a country name, a shapefile or a extent and an EPSG value.
  #' Succed to give extent in EPSG reproject and extent with default EPSG also knows as latitude and longitude coordinates.
  #'
  #' @param country_name character. English country name. Check `details` for more information, default is NULL.
  #' @param shapefile_path character. Path to a .shp file, default is NULL.
  #' @param extent_short int vector. length 4, with this order c(xmin, ymin, xmax, ymax), default is NULL.
  #' @param EPSG int. to consider for this country/area.
  #'
  #' @return character vector. with extent of the area in one character, other are latlon coord of area
  #' @details `country_name` must be available on `https://www.naturalearthdata.com/` website.
  #' @import sf
  #' @import rnaturalearth
  #' @import rnaturalearthdata
  #' @import rnaturalearthhires
  #' @import terra

  if ((!is.null(country_name) + !is.null(shapefile_path) + !is.null(extent_short)) != 1){
    print("only one attribute among country_name, shapefile_path and extent_short")
    break
  }
  if (!is.null(country_name)){
    map <- ne_countries(scale = 10, country = country_name, returnclass = "sf")
    coord <- st_coordinates(map)
    lonmin <- floor(min(coord[,1]))
    lonmax <- ceiling(max(coord[,1]))
    latmin <- floor(min(coord[,2]))
    latmax <- ceiling(max(coord[,2]))
    extent_latlon <- c(lonmin = lonmin, latmin = latmin, lonmax = lonmax, latmax = latmax)
    map <- st_transform(map, EPSG)
    extent <- round(st_bbox(map))
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
    extent_latlon <- c(lonmin = lonmin, latmin = latmin, lonmax = lonmax, latmax = latmax)
  }
  if (!is.null(extent_short)){
    extent <- round(extent_short)
    e <- ext(extent[1], extent[3], extent[2], extent[4])
    e <- as.polygons(e)
    crs(e) <- paste0("epsg:", EPSG)
    extent_latlon <- st_bbox(project(e, "epsg:4326"))
  }
  extent <- paste(extent[1], extent[2], extent[3], extent[4], sep = " ")
  return(c(extent, extent_latlon))
}
