merge_files <- function(environ_path, climat_path, destination){
  #' Create a multilayer Tiff file with environ & climat variables.
  #'
  #' @description
  #' Use output of `get_env_variables` and `get_chelsa_current` to create an unique file with more than 100 variables.
  #' Can be use with files created by `get_chelsa_futur`.
  #'
  #' @param environ_path character. absolute path to environ.tif file, output of `get_env_variables`.
  #' @param climat_path  character. absolute path to current_chelsa.tif.tif file, output of `get_chelsa_variables`
  #' @param destination character. absolute path where to download files like `here()` output.
  #' @return character. absolute path to gecevar.tif.
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

  nodat = -32768
  system(glue('gdal_merge.py -ot Int16 -of GTiff -o {paste(destination, "gecevar_noname.tif", sep = "/")} -a_nodata {nodat} -separate \\
            -co "COMPRESS=LZW" -co "PREDICTOR=2" {environ_path} {climat_path}'), ignore.stdout = TRUE, ignore.stderr = TRUE)
  all_var <- rast(paste(destination,  "gecevar_noname.tif", sep = "/"))
  names(all_var) <-  c(names(rast(environ_path)), names(rast(climat_path)))
  writeRaster(x = all_var, filename = paste(destination, "gecevar.tif", sep = "/"), overwrite = TRUE)

  unique_values <- unique(values(rast(paste(destination, "gecevar.tif", sep = "/"))[[6]]))
  create_xml_legend(unique_values = unique_values, destination = destination, name_file = "gecevar")
  unlink(paste(destination, "gecevar_noname.tif", sep = "/"))

  }
