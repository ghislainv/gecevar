#' Get extents from e_latlong.
#' 
#' @param e_latlon num vector. Named vector of length 4, in this order c(lonmin,
#'   latmin, lonmax, latmax).
#'
#' @param EPSG_proj int. EPSG code.
#'
#' @param resol num. Resolution used to align the extent.
#' 
#' @return list. With projected extent and latlon extent.
#'
#' @import sf
#' @import terra
get_extents_from_e_latlon <- function(e_latlon_orig, EPSG_proj, resol) {
  
  # e_proj from e_latlon_orig
  e <- terra::ext(e_latlon_orig["lonmin"], e_latlon_orig["lonmax"],
                  e_latlon_orig["latmin"], e_latlon_orig["latmax"])
  e <- terra::as.polygons(e, crs="epsg:4326")
  e_proj <- sf::st_bbox(terra::project(e, paste0("epsg:", EPSG_proj)))
  xmin <- ifelse(EPSG_proj != 4326, floor(e_proj$xmin), floor(e_proj$xmin * 100) / 100)
  ymax <- ifelse(EPSG_proj != 4326, ceiling(e_proj$ymax), ceiling(e_proj$ymax * 100) / 100)
  
  # tap
  xmax <- xmin + ceiling((e_proj$xmax - xmin) / resol) * resol
  ymin <- ymax - ceiling((ymax - e_proj$ymin) / resol) * resol
  e_proj <- c(xmin, ymin, xmax, ymax)
  names(e_proj) <- c("xmin", "ymin", "xmax", "ymax")
  
  # New e_latlon from e_proj to ensure that the extended aoi (cf. tap) is included in e_latlong.
  e <- terra::ext(xmin, xmax, ymin, ymax)
  e <- terra::as.polygons(e, crs=paste0("epsg:", EPSG_proj))
  bbox <- sf::st_bbox(terra::project(e, "epsg:4326"))
  
  # Coordinates are extended to nearest 0.01 degree (about 1km).
  e_latlon <- c(floor(bbox$xmin * 100) / 100, floor(bbox$ymin * 100) / 100,
                ceiling(bbox$xmax * 100) / 100, ceiling(bbox$ymax * 100) / 100)
  names(e_latlon) <- c("lonmin", "latmin", "lonmax", "latmax")
  
  return(list(e_proj=e_proj, e_latlon=e_latlon))
  
}

#' Get the extent of an area of interest (AOI) in GDAL format (xmin,
#' ymin, xmax, ymax).
#'
#' This function gets the extent of an area of interest (AOI) in GDAL
#' format (xmin, ymin, xmax, ymax). The extent is provided both in
#' latitude and longitude and in the projected coordinate reference
#' system (CRS) specified by the user. The area of interest can be
#' specified either by a country iso3 code, a vector file, or an
#' extent (in latlon or projected CRS). When the area of interest is
#' specified with a country iso3 code, the country borders are
#' downloaded from the GADM (Global ADMinistrative areas database) at
#' \url{https://gadm.org}.
#'
#' The projected extent (`extent_proj`) is always greater than the
#' original extent and aligned with pixels (see `tap` parameter in
#' GDAL for target aligned pixels). The extent in latitude and
#' longitude (`extent_latlon`) is also aligned to the nearest 0.01
#' degree (about 1.1 km at the equator) and is always greater than the
#' projected extent so that the projected extent is included in the
#' latlon extent. This is done to reduce edge effects in later
#' computations.
#' 
#' @param country_iso character. Country iso3 code used to download
#'   the country borders from the GADM database. To be used when the
#'   AOI is a country. Default is NULL.
#'
#' @param vector_file character. Path to a vector file of polygons
#'   (such as .shp or .gpkg file) with AOI borders. The first layer
#'   will be used. Default is NULL.
#'
#' @param extent_proj num vector. Extent of the AOI in the projected
#'   coordinate reference system (see EPSG_proj). Vector of length 4,
#'   in this order c(xmin, ymin, xmax, ymax). Default is NULL.
#'
#' @param extent_latlon num vector. Extent of the AOI with latitudes
#'   and longitudes. Vector of length 4, in this order c(lonmin,
#'   latmin, lonmax, latmax). Default is NULL.
#'
#' @param EPSG_proj int. EPSG code of the projected coordinate
#'   reference system for the AOI. Default is 4326.
#'
#' @param resol num. Resolution used to align the coordinates of the
#'   extent (see `tap` parameter in GDAL for target aligned
#'   pixel). The resolution unit is the one of the EPSG code defined
#'   with `EPSG_proj`. Default is 1 degree for EPSG:4326 and 1000
#'   meters otherwise.
#' 
#' @param output_dir character. Output directory where the vector file
#'   for the country borders is downloaded. Default is `"gadm"`.
#'
#' @param rm_output_dir boolean. If FALSE, does not remove the output
#'   directory. Default is TRUE.
#'
#' @return list. With `extent_latlon`: extent in latitude and
#'   longitude (lonmin, latmin, lonmax, latmax), `extent_proj`:
#'   projected extent (xmin, ymin, xmax, ymax), `extent_latlon_orig`:
#'   original extent (without tap) in latlon, and `gpkg_file`: path to
#'   .gpkg country vector file if downloaded.
#'
#' @import sf
#' @importFrom utils download.file
#' @import terra
#' @export
get_aoi_extent <- function(country_iso=NULL,
                           vector_file=NULL,
                           extent_proj=NULL,
                           extent_latlon=NULL,
                           EPSG_proj=4326,
                           resol=ifelse(EPSG_proj==4326, 0.01, 1000),
                           output_dir="gadm",
                           rm_output_dir=TRUE) {

  # Initial check
  if ((as.numeric(!is.null(country_iso)) +
         as.numeric(!is.null(vector_file)) +
         as.numeric(!is.null(extent_proj)) +
         as.numeric(!is.null(extent_latlon))) != 1) {
    stop("One attribute among 'country_iso', 'vector_file', 'extent_proj', or 'exent_latlon' must be specified") 
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
    # e_latlon_orig
    e_latlon_orig <- c(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax)
    names(e_latlon_orig) <- c("lonmin", "latmin", "lonmax", "latmax")
    # Get extents
    extents <- get_extents_from_e_latlon(e_latlon_orig, EPSG_proj, resol)
    e_proj <- extents$e_proj
    e_latlon <- extents$e_latlon
    # Remove directory if requested
    if(rm_output_dir) {
      unlink(output_dir, recursive=TRUE)
      gpkg_file <- NULL
    }
  }

  # With AOI vector file in any projection
  if (!is.null(vector_file)) {
    # Reproject in latlon
    layer1_name <- sf::st_layers(vector_file)$name[1]
    borders <- sf::st_read(vector_file, layer=layer1_name, quiet=TRUE)
    borders <- sf::st_transform(borders, "EPSG:4326")
    # Get bounding box
    bbox <- sf::st_bbox(borders)
    # e_latlon_orig
    e_latlon_orig <- c(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax)
    names(e_latlon_orig) <- c("lonmin", "latmin", "lonmax", "latmax")
    # Get extents
    extents <- get_extents_from_e_latlon(e_latlon_orig, EPSG_proj, resol)
    e_proj <- extents$e_proj
    e_latlon <- extents$e_latlon
  }

  # With projected extent (that should be different from latlon)
  if (!is.null(extent_proj)) {
    # e_latlong_orig
    e <- terra::ext(as.numeric(extent_proj[1]), as.numeric(extent_proj[3]),
                    as.numeric(extent_proj[2]), as.numeric(extent_proj[4]))
    e <- terra::as.polygons(e, crs=paste0("epsg:", EPSG_proj))
    bbox <- sf::st_bbox(terra::project(e, "epsg:4326"))
    e_latlon_orig <- c(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax)
    names(e_latlon_orig) <- c("lonmin", "latmin", "lonmax", "latmax")
    # tap
    xmin <- floor(as.numeric(extent_proj[1]))
    ymax <- ceiling(as.numeric(extent_proj[4]))
    # tap
    xmax <- xmin + ceiling((as.numeric(extent_proj[3]) - xmin) / resol) * resol
    ymin <- ymax - ceiling((ymax - as.numeric(extent_proj[2])) / resol) * resol
    e_proj <- c(xmin, ymin, xmax, ymax)
    names(e_proj) <- c("xmin", "ymin", "xmax", "ymax")
    # New e_latlon from e_proj to ensure that the extended aoi (cf. tap) is included in e_latlong.
    e <- terra::ext(xmin, xmax, ymin, ymax)
    e <- terra::as.polygons(e, crs=paste0("epsg:", EPSG_proj))
    bbox <- sf::st_bbox(terra::project(e, "epsg:4326"))
    # Coordinates are extended to nearest 0.01 degree (about 1km).
    e_latlon <- c(floor(bbox$xmin * 100) / 100, floor(bbox$ymin * 100) / 100,
                  ceiling(bbox$xmax * 100) / 100, ceiling(bbox$ymax * 100) / 100)
    names(e_latlon) <- c("lonmin", "latmin", "lonmax", "latmax")
  }

  # With extent in latlon
  if (!is.null(extent_latlon)) {
    # e_latlon_orig
    lonmin <- as.numeric(extent_latlon[1])
    lonmax <- as.numeric(extent_latlon[3])
    latmin <- as.numeric(extent_latlon[2])
    latmax <- as.numeric(extent_latlon[4])
    e_latlon_orig <- c(lonmin, latmin, lonmax, latmax)
    names(e_latlon_orig) <- c("lonmin", "latmin", "lonmax", "latmax")
    # Get extents
    extents <- get_extents_from_e_latlon(e_latlon_orig, EPSG_proj, resol)
    e_proj <- extents$e_proj
    e_latlon <- extents$e_latlon
  }

  # Return results
  return(list(extent_latlon=e_latlon, extent_proj=e_proj,
              extent_latlon_orig=e_latlon_orig, gpkg_file=gpkg_file))
  
}

# End
