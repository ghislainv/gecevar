#!/usr/bin/R

## ==============================================================================
## authors         :Pierre Guillaumont, Ghislain Vieilledent
## emails          :pierreguillaumont@gmail.com,
##                  ghislain.vieilledent@cirad.fr, ghislainv@gmail.com
## web             :https://ecology.ghislainv.fr
## license         :GPLv3
## ==============================================================================

#' Get the extent of an area of interest (AOI) in GDAL format (xmin, ymin, xmax, ymax).
#'
#' This function gets the extent of an area of interest (AOI) in GDAL
#'   format (xmin, ymin, xmax, ymax). The extent is provided both in
#'   latitude and longitude and in the projected coordinate reference
#'   system (CRS) specified by the user. The area of interest can be
#'   specified either by a country iso3 code, a vector file, or an
#'   extent (in latlong or projected CRS). When the area of interest
#'   is specified with a country iso3 code, the country borders are
#'   downloaded from the GADM (Global ADMinistrative areas database)
#'   at \url{https://gadm.org}.
#'
#' The extent is always enlarged to the nearest degrees in lat
#'   long. As a consequence, when providing an extent in a projected
#'   CRS as an input, the extent in the output can be substantially
#'   enlarged and different from the extent in input.
#'
#' @param country_iso character. Country iso3 code used to download
#'   the country borders from the GADM database. To be used when the
#'   AOI is a country. Default is NULL.
#'
#' @param vector_file character. Path to a vector file of polygons
#'   (such as .shp or .gpkg file) with AOI borders. Default is NULL.
#'
#' @param extent_proj int vector. Extent of the AOI in the projected
#'   coordinate reference system (see EPSG_proj). Vector of length 4,
#'   in this order c(xmin, ymin, xmax, ymax). Default is NULL.
#'
#' @param extent_latlon int vector. Extent of the AOI with latitudes
#'   and longitudes. Vector of length 4, in this order c(lonmin,
#'   latmin, lonmax, latmax). Default is NULL.
#'
#' @param EPSG_proj int. EPSG code of the projected coordinate
#'   reference system for the AOI. Default is NULL.
#'
#' @param output_dir character. Output directory where the vector file
#'   for the country borders is downloaded. Default is `"gadm"`.
#'
#' @param rm_output_dir boolean. If FALSE, does not remove the output
#'   directory. Default is TRUE.
#'
#' @return list. With `e_proj`: projected extent, `e_latlong`: extent
#'   in latlong, and `gpkg_file`: path to .gpkg country vector file if
#'   downloaded.
#'
#' @import sf
#' @importFrom utils download.file
#' @import terra
#' @export

get_aoi_extent <- function(country_iso=NULL,
                           vector_file=NULL,
                           extent_proj=NULL,
                           extent_latlon=NULL,
                           EPSG_proj=NULL,
                           output_dir="gadm",
                           rm_output_dir=TRUE) {

  # Initial check
  if ((as.numeric(!is.null(country_iso)) +
         as.numeric(!is.null(vector_file)) +
         as.numeric(!is.null(extent_proj)) +
         as.numeric(!is.null(extent_latlon))) != 1) {
    stop("One attribute among 'country_iso', 'vector_file', 'extent_proj', or 'exent_latlong' must be specified") 
  }

  # Set gpkg_file to NULL
  gpkg_file <- NULL

  # With country iso code
  if (!is.null(country_iso)) {
    # Download country borders
    options(timeout=300)
    dir.create(output_dir, showWarnings=FALSE)
    URL <- paste0("https://geodata.ucdavis.edu/gadm/gadm4.1/gpkg/gadm41_", country_iso, ".gpkg")
    gpkg_file <- file.path(output_dir, paste0("gadm41_", country_iso, ".gpkg"))
    download.file(URL, quiet=TRUE, destfile=gpkg_file)
    # Read vector file (level 0 for country borders)
    borders <- sf::st_read(gpkg_file, layer="ADM_ADM_0", quiet=TRUE)
    # Get bounding box
    bbox <- sf::st_bbox(borders)
    # e_latlong extending to nearest degree
    lonmin <- floor(as.numeric(bbox$xmin))
    lonmax <- ceiling(as.numeric(bbox$xmax))
    latmin <- floor(as.numeric(bbox$ymin))
    latmax <- ceiling(as.numeric(bbox$ymax))
    e_latlong <- c(lonmin=lonmin, latmin=latmin, lonmax=lonmax, latmax=latmax)
    # e_proj from e_latlong
    e <- terra::ext(lonmin, lonmax, latmin, latmax)
    e <- terra::as.polygons(e, crs="epsg:4326")
    e_proj <- sf::st_bbox(terra::project(e, paste0("epsg:", EPSG_proj)))
    e_proj <- c(floor(e_proj$xmin), floor(e_proj$ymin), ceiling(e_proj$xmax), ceiling(e_proj$ymax))
    # Remove directory if requested
    if(rm_output_dir) {
      unlink(output_dir, recursive=TRUE)
    }
  }

  # With AOI vector file
  if (!is.null(vector_file)) {
    # Reproject in latlong
    borders <- sf::st_read(vector_file, quiet=TRUE) 
    borders <- sf::st_transform(borders, "EPSG:4326")
    # Get bounding box
    bbox <- sf::st_bbox(borders)
    # e_latlong extending to nearest degree
    lonmin <- floor(as.numeric(bbox$xmin))
    lonmax <- ceiling(as.numeric(bbox$xmax))
    latmin <- floor(as.numeric(bbox$ymin))
    latmax <- ceiling(as.numeric(bbox$ymax))
    e_latlong <- c(lonmin=lonmin, latmin=latmin, lonmax=lonmax, latmax=latmax)
    # e_proj from e_latlong
    e <- terra::ext(lonmin, lonmax, latmin, latmax)
    e <- terra::as.polygons(e, crs="epsg:4326")
    e_proj <- sf::st_bbox(terra::project(e, paste0("epsg:", EPSG_proj)))
    e_proj <- c(floor(e_proj$xmin), floor(e_proj$ymin), ceiling(e_proj$xmax), ceiling(e_proj$ymax))
  }

  # With projected extent
  if (!is.null(extent_proj)) {
    # Get the bounding box in latlong
    e <- terra::ext(as.numeric(extent_proj[1]),
                    as.numeric(extent_proj[3]),
                    as.numeric(extent_proj[2]),
                    as.numeric(extent_proj[4]))
    e <- terra::as.polygons(e, crs=paste0("epsg:", EPSG_proj))
    bbox <-  sf::st_bbox(terra::project(e, "epsg:4326"))
     # e_latlong extending to nearest degree
    lonmin <- floor(as.numeric(bbox$xmin))
    lonmax <- ceiling(as.numeric(bbox$xmax))
    latmin <- floor(as.numeric(bbox$ymin))
    latmax <- ceiling(as.numeric(bbox$ymax))
    e_latlong <- c(lonmin=lonmin, latmin=latmin, lonmax=lonmax, latmax=latmax)
    # e_proj from e_latlong
    e <- terra::ext(lonmin, lonmax, latmin, latmax)
    e <- terra::as.polygons(e, crs="epsg:4326")
    e_proj <- sf::st_bbox(terra::project(e, paste0("epsg:", EPSG_proj)))
    e_proj <- c(floor(e_proj$xmin), floor(e_proj$ymin), ceiling(e_proj$xmax), ceiling(e_proj$ymax))
  }

  # With extent in lat long
  if (!is.null(extent_latlon)) {
    # e_latlong extending to nearest degree
    lonmin <- floor(as.numeric(extent_latlon[1]))
    lonmax <- ceiling(as.numeric(extent_latlon[3]))
    latmin <- floor(as.numeric(extent_latlon[2]))
    latmax <- ceiling(as.numeric(extent_latlon[4]))
    e_latlong <- c(lonmin=lonmin, latmin=latmin, lonmax=lonmax, latmax=latmax)
    # e_proj from e_latlong
    e <- terra::ext(lonmin, lonmax, latmin, latmax)
    e <- terra::as.polygons(e, crs="epsg:4326")
    e_proj <- sf::st_bbox(terra::project(e, paste0("epsg:", EPSG_proj)))
    e_proj <- c(floor(e_proj$xmin), floor(e_proj$ymin), ceiling(e_proj$xmax), ceiling(e_proj$ymax))
  }

  # Collapse vectors to characters
  e_latlong <- paste(e_latlong, collapse=" ")
  e_proj <- paste(e_proj, collapse=" ")

  # Return results
  return(list(e_proj=e_proj, e_latlong=e_latlong, gpkg_file=gpkg_file))
  
}

# End
