merge_files <- function(environ_path, climat_path, destination){
  #' Create a multilayer Tiff file with environ & climat variables.
  #'
  #' @description
  #' Use output of `get_env_variables` and `get_chelsa_variables` to create an unique file with more than 100 variables.
  #'
  #' @param environ_path character. absolute path to environ.tif file, output of `get_env_variables`.
  #' @param climat_path  character. absolute path to current_chelsa.tif.tif file, output of `get_chelsa_variables`
  #' @param destination character. absolute path where to download files like `here()` output.
  #' @return character. absolute path to gecevar.tif.
  #'
  #' @import glue
  #' @import rgdal
  #' @import stars

  nodat = -9999
  system(glue('gdal_merge.py -ot Int16 -of GTiff -o {paste(destination, "output", "gecevar.tif", sep = "/")} -a_nodata {nodat} -separate \\
            -co "COMPRESS=LZW" -co "PREDICTOR=2" {environ_path} {climat_path}'))
  all_var <- split(st_as_stars(read_stars(paste(destination, "output", "gecevar.tif", sep = "/"))))
  names(all_var) <-  c(names(split(read_stars(environ_path))), names(split(read_stars(climat_path))))
  write_stars(merge(all_var), paste(destination, "output", "gecevar.tif", sep = "/"), options = c("COMPRESS=LZW","PREDICTOR=2"))

  }
