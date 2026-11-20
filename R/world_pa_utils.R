#' Download Protected Areas by Country
#'
#' This function downloads protected areas for one country using the WDPA API.
#'
#' @param isocode a character specifying the ISO-3 code of the country
#' @param path the path (directory) to save spatial dataset (`.gpkg`). Default
#' is: `wdpa/`.
#' @inheritParams get_token
#'
#' @return A `sf` object (**GEOMETRY**) of protected areas defined in the
#'   EPSG 4326 with the following 19 attributes:
#' \describe{
#'   \item{wdpa_id}{the ID of the protected area on the WDPA database}
#'   \item{pa_name}{the name of the protected area on the WDPA database}
#'   \item{original_name}{the original name of the protected area}
#'   \item{country_iso3}{the ISO-3 code of the country}
#'   \item{country}{the name of the country}
#'   \item{owner_type}{the owner of the protected area}
#'   \item{is_marine}{a boolean: `TRUE` for marine and `FALSE` for terrestrial
#'   protected area}
#'   \item{designation}{the designation of the protected area}
#'   \item{iucn_category}{the IUCN category}
#'   \item{no_take_status}{the no take status}
#'   \item{reported_area}{the (reported) area of the protected area}
#'   \item{reported_marine_area}{the (reported) area of the marine part of the
#'   protected area}
#'   \item{no_take_area}{the (reported) area of the no take part of the
#'   protected area}
#'   \item{legal_status}{the legal status of the protected area}
#'   \item{management_authority}{the management authority}
#'   \item{governance}{the type of gouvernance}
#'   \item{management_plan}{the URL of the management plan}
#'   \item{protected_planet}{the URL of the protected area page on the Protected
#'   Planet website}
#'   \item{legal_status_updated}{the date of the last update of the legal status}
#' }
#'
#' Spatial data are also written on the hard drive in the folder `path` as a
#' **geopackage**. Filename is structured as **XXX_protectedareas.gpkg** (where
#' `XXX` is the ISO-3 code of the country). If the `path` folder does not exist
#' it will created relatively to the current directory.
#'
#' @note Note that some geometries are **POINTS** not **(MULTIPOLYGONS)**.
#'
#' @author Nicolas Casajus
#'
#' @keywords internal
#'
#' @import geojsonsf jsonlite
#' @noRd

get_wdpa <- function(isocode, path = "wdpa", key = "WDPA_KEY") {


  ## Checks inputs ----

  if (missing(isocode)) {
    stop("Please provide one country ISO-3 code.\n",
         "Type `data(wdpa_countries)` or `wdpa_countries` to search for ",
         "ISO-3 codes.")
  }

  if (length(isocode) > 1) {
    stop("This function only works with one ISO-3 code.")
  }

  wdpa_token <- get_token(key)


  ## Is ISO-3 code valid? ----

  response <- httr::GET(wdpa_fullurl("v3/countries/", isocode, "?token=",
                                     wdpa_token))

  if (response$"status" == 404) {
    stop("Invalid ISO-3 code.\n",
         "Type `data(wdpa_countries)` or `wdpa_countries` to search for ",
         "ISO-3 codes.")
  }

  httr::stop_for_status(response)


  ## Get Total Number of Pages ----

  response <- httr::content(response, as = "text")
  response <- jsonlite::fromJSON(response)

  pas_count <- response$"country"$"pas_count"
  pages     <- seq_len(ceiling(pas_count / 50))


  ## Download Data ----

  if (pas_count) {

    for (page in pages) {

      request <- wdpa_fullurl("v3/protected_areas/search/?token=", wdpa_token,
                              "&with_geometry=true", "&country=", isocode,
                              "&per_page=50", "&page=", page)

      response <- httr::GET(request)
      response <- httr::content(response, as = "text")
      response <- jsonlite::fromJSON(response)
      response <- response$"protected_areas"

      pa_json  <- jsonlite::toJSON(response$"geojson"$"geometry")
      pa_sf    <- geojsonsf::geojson_sf(pa_json)

      attributs <- data.frame(
        wdpa_id              = response$"id",
        pa_name              = response$"name",
        original_name        = response$"original_name",
        country_iso3         = isocode,
        country              = unlist(lapply(response$"countries",
                                             function(x) paste0(x[["name"]],
                                                                collapse = "; "))),
        owner_type           = response$"owner_type",
        is_marine            = response$"marine",
        designation          = response$"designation"$"name",
        iucn_category        = response$"iucn_category"$"name",
        no_take_status       = response$"no_take_status"$"name",
        reported_area        = as.numeric(response$"reported_area"),
        reported_marine_area = as.numeric(response$"reported_marine_area"),
        no_take_area         = as.numeric(response$"no_take_status"$"area"),
        legal_status         = response$"legal_status"$"name",
        management_authority = response$"management_authority"$"name",
        governance           = response$"governance"$"governance_type",
        management_plan      = response$"management_plan",
        protected_planet     = response$"links"$"protected_planet",
        legal_status_updated = response$"legal_status_updated_at"
      )

      pa_sf <- sf::st_sf(attributs, geom = sf::st_geometry(pa_sf))

      if (page == 1) {

        all_pa <- pa_sf

      } else {

        all_pa <- rbind(all_pa, pa_sf)
      }
    }


    ## Write Layer ----

    if (!dir.exists(path)) dir.create(path, recursive = TRUE)

    sf::st_write(
      obj    = all_pa,
      dsn    = file.path(path, paste0(isocode, "_protectedareas.gpkg")),
      layer  = paste0(isocode, "_protectedareas"),
      append = FALSE,
      quiet = TRUE)

    return(all_pa)

  } else {

    stop("The WDPA database does not contain any protected areas for ", isocode)
  }
}


#' Check Protected Planet API Token
#'
#' This function checks if the user has stored a valid Protected Planet API
#'   token in the R environment file (`.Renviron`).
#'
#' @param key a character providing the WDPA token name stored in the
#'   `.Renviron` file. Default is `WDPA_KEY`.
#'
#' @return A vector of length one with the value of the API token.
#'
#' @details Before using this package for the first time, the user must follow
#'   these steps:
#' \itemize{
#'   \item Fill in the form available at:
#'         \url{https://api.protectedplanet.net/request} to obtain a personal
#'         API token;
#'   \item Store the token in the `.Renviron` file under the key `WDPA_KEY`.
#'         User can use the function `usethis::edit_r_environ` to open this
#'         file;
#'   \item Restart R.
#' }
#'
#' @author Nicolas Casajus
#'
#' @keywords internal
#' @noRd


get_token <- function(key = "WDPA_KEY") {

  wdpa_token <- Sys.getenv(key)

  if (wdpa_token == "") {
    stop("Missing WDPA API Token.\n",
         "Please make sure you:\n",
         " 1. have completed this form ",
         "<https://api.protectedplanet.net/request> ",
         "to get your own token, and\n",
         " 2. have stored the value in the `.Renviron` file with the name ",
         "WDPA_KEY.")
  }

  response <- httr::GET(wdpa_fullurl("test?token=", wdpa_token))

  if (response$"status" == 401) {
    stop("Invalid WDPA API Token.\n",
         "Please make sure you:\n",
         " 1. have completed this form ",
         "<https://api.protectedplanet.net/request> ",
         "to get your own token, and\n",
         " 2. have stored the value in the `.Renviron` file with the name ",
         "WDPA_KEY.")
  }

  httr::stop_for_status(response)

  return(wdpa_token)
}


#' Helpers Functions
#'
#' `wdpa_baseurl()`: Set the base URL of the WDPA API
#' `wdpa_fullurl()`: Add parameters to the base URL of the WDPA API
#'
#' @author Nicolas Casajus
#'
#' @keywords internal
#'
#' @noRd

wdpa_baseurl <- function() "https://api.protectedplanet.net/"

wdpa_fullurl <- function(...) paste0(wdpa_baseurl(), ...)
