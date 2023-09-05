#' Create a multilayer Tiff file with environ & climat variables.
#'
#' @description
#' Use output of `get_env_variables` and `get_chelsa_current` to create an unique file with more than 100 variables.
#' Can be use with files created by `get_chelsa_futur`.
#'
#' @param environ_path character. Absolute path to `environ.tif` file, output of `get_env_variables`.
#' @param climate_path  character. Absolute path to `current_chelsa.tif` file, output of `get_chelsa_variables`.
#' @param destination character. Absolute path for the output directory.
#' @return character. Absolute path to `gecevar.tif`.
#'
#' @details
#'
#' gecevar.tif.aux.xml is an extention of gecevar.tif, it allows to classify soilgrid variable with QGIS with RasterAttributeTable extension.
#' Nevertheless it's cause problems to open it with `stars` package but not with `terra`. If you have any problems to open gecevar.tif, you can remove gecevar.tif.aux.xml.
#' This solve all accessibility problems with `stars` and `terra` packages.
#'
#' @import terra
#' @export

merge_files <- function (environ_path, climate_path, destination) {

  nodata_value=-32768
  
  environ <- terra::rast(environ_path)
  climate <- terra::rast(climate_path)
  gecevar <- c(environ, climate)

  ofile <- file.path(destination, "gecevar.tif")
  terra::writeRaster(gecevar, filename=ofile,
                     NAflag=nodata_value,
                     gdal=c("COMPRESS=LZW", "PREDICTOR=2"),
                     progress=0, overwrite=TRUE, datatype="INT2S")

  unique_values <- unique(values(terra::rast(ofile)[[6]]))
  create_xml_legend(unique_values=unique_values, output_dir=destination, file_name="gecevar")

}

# End
