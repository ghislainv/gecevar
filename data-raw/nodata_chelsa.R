## code to prepare `nodata_chelsa` dataset goes here
nodata_chelsa <- gecevar::get_nodata_dtype_chelsa(nmonths = 1, biovar = TRUE, verbose = TRUE)
usethis::use_data(nodata_chelsa, overwrite = TRUE)
