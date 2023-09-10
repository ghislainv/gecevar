#' Get nodata value and data type for Chelsa variables.
#'
#' Ues gdalinfo and regular expression to extract nodata values and
#' data types for Chelsa climatic variables. The following monthly
#' climatic variables are considered: clt, pet, pr, tas, tasmax,
#' tasmin. 
#'
#' @param nmonths numeric. Number of months to consider. Must be
#'   between 1 and 12. Default to 1.
#'
#' @param biovar boolean. If TRUE (the default), consider the 19
#'   bioclimatic variables in addition to monthly variables.
#' 
#' @param verbose boolean. If FALSE, do not print message. Default to
#'   TRUE.
#' 
#' @return dataframe. The dataframe includes nodata value and data
#'   type for each variable.
#' 
#' @author Ghislain Vieilledent <ghislain.vieilledent@cirad.fr>
#' 
#' @importFrom glue glue
#' @importFrom sf gdal_utils
#' @importFrom stringr str_pad
#' @export
#' @examples
#' \dontrun{
#' df <- get_nodata_dtype_chelsa(nmonths=1, biovar=TRUE, verbose=TRUE)
#' print(df)
#' }
#' 
get_nodata_dtype_chelsa <- function (nmonths=1, biovar=TRUE, verbose=TRUE) {

  # Check month
  if (!(nmonths %in% 1:12)) {
    stop("Argument \"nmonths\" must be an integer between 1 and 12.")
  }

  # Bioclimatic variables
  bio <- bioo <- NULL
  if (biovar) {
    bio <- rep("bio", 19)
    bioo <- paste0("bio", 1:19)
  }
  
  # Climatic variables
  var <- c("clt", "pet", "pr", "tas", "tasmax", "tasmin", bio)
  varr <- c("clt", "pet_penman", "pr", "tas", "tasmax", "tasmin", bioo)
  nvar <- length(var)

  # Base url for Chelsa
  url_base_chelsa <- paste0(
    "https://os.zhdk.cloud.switch.ch/",
    "envicloud/chelsa/chelsa_V2/GLOBAL/",
    "climatologies/1981-2010")

  # Dataframe to store results
  df <- data.frame()
 
  # Loop on variables
  for (v in 1:nvar) {
    # Message
    if (verbose) {
      cat(glue("Get nodata value and data type for: \"{varr[v]}\"."), "\n")
    }
    months <- stringr::str_pad(1:nmonths, width = 2, pad = "0")
    if (varr[v] %in% c("clt", "pet_penman", "pr", "tas", "tasmax", "tasmin")) {
      for (m in months) {
        # Long url
        ifile <- glue::glue("/vsicurl/{url_base_chelsa}/{var[v]}/",
                            "CHELSA_{varr[v]}_{m}_1981-2010_V.2.1.tif")
        # Get metadata using gdalinfo and regular expressions
        metadata <- sf::gdal_utils("gdalinfo", ifile,
                                   config_options=c(GTIFF_SRS_SOURCE="EPSG"),
                                   quiet=TRUE)
        data_type <- regmatches(metadata, regexpr("Type=[[:alnum:]]+", metadata))
        data_type <- sub("Type=", "", data_type)
        reg_expr <- regexpr("NoData[[:space:]]Value=[[:graph:]]+", metadata)
        nodata_val <- regmatches(metadata, reg_expr)
        nodata_val <- sub("NoData Value=", "", nodata_val)
        df <- rbind(df, c(varr[v], m, data_type, nodata_val))
      }
    }
    if (varr[v] %in% bioo) {
      # Long url
      ifile <- glue::glue("/vsicurl/{url_base_chelsa}/{var[v]}/",
                          "CHELSA_{varr[v]}_1981-2010_V.2.1.tif")
      # Get metadata using gdalinfo and regular expressions
      metadata <- sf::gdal_utils("gdalinfo", ifile,
                                 config_options=c(GTIFF_SRS_SOURCE="EPSG"),
                                 quiet=TRUE)
      data_type <- regmatches(metadata, regexpr("Type=[[:alnum:]]+", metadata))
      data_type <- sub("Type=", "", data_type)
      reg_expr <- regexpr("NoData[[:space:]]Value=[[:graph:]]+", metadata)
      nodata_val <- regmatches(metadata, reg_expr)
      nodata_val <- sub("NoData Value=", "", nodata_val)
      df <- rbind(df, c(varr[v], NA, data_type, nodata_val))
    }
  }
  # Rename columns
  names(df) <- c("var", "month", "dtype", "nodata")
  df$m <- as.numeric(df$m)
  df$nodata <- as.numeric(df$nodata)
  
  # Return dataframe with results
  return(df)
}

# End
