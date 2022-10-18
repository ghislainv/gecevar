#' Template of soilgrids legend to add to output files `gecevar.tif` and `environ.tif`
#'
#' @name xml
#'




xml <- as.character(read_xml("data-raw/legend_soilgrids.tif.aux.xml"))
usethis::use_data(xml, overwrite = TRUE)
