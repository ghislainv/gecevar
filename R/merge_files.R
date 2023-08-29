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
#' @importFrom glue glue
#' @import terra
#' @export

merge_files <- function (environ_path, climate_path, destination) {

  nodat = -32768
  ofile_noname <- file.path(destination, "gecevar_noname.tif")
  system(glue('gdal_merge.py -ot Int16 -of GTiff -o {ofile_noname} -a_nodata {nodat} -separate \\
            -co "COMPRESS=LZW" -co "PREDICTOR=2" {environ_path} {climate_path}'), ignore.stdout = TRUE, ignore.stderr = TRUE)
  all_var <- rast(ofile_noname)
  names(all_var) <-  c(names(rast(environ_path)), names(rast(climate_path)))

  ofile <- file.path(destination, "gecevar.tif")
  writeRaster(x = all_var, filename = ofile, overwrite = TRUE, datatype = "INT2S")

  unique_values <- unique(values(rast(ofile)[[6]]))
  create_xml_legend(unique_values = unique_values, destination = destination, name_file = "gecevar")
  unlink(ofile_noname)

}

# End
