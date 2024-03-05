#' Create multilayer Tiff file with 107 variables from chelsa-climate.org
#'
#' @description Gives the average values on the data recovered between
#'   1981 and 2010. Monthly variables are average temperatures, min
#'   temperatures, max temperatures, precipitation, potential
#'   evapotranspiration (with Penman formula and with Thornthwaite
#'   formula), and total cloud cover. Others variables are climatic
#'   water deficit (with Penman and Thornthwaite), number of dry month
#'   (with Penman and Thornthwaite) and 19 bio variables (more
#'   information in chelsa documentation).
#' 
#' @param extent_latlon num vector. Must be in the form c(lonmin, latmin,
#'   lonmax, latmax). This extent is used to download climatic data
#'   using GDAL function `gdal_translate` and properties of COG
#'   files. This extent can be obtained with the `get_aoi_extent()`
#'   function.
#'
#' @param extent_proj num vector. Must be in the form c(xmin, ymin, xmax,
#'   ymax). This extent is used for the output raster file. This
#'   extent can be obtained with the `get_aoi_extent()` function.
#'
#' @param EPSG_proj int. EPSG code used to define the coordinate
#'   reference system to project the output raster file.
#' 
#' @param destination character. Directory path for outputs.
#' 
#' @param resol int. Resolution. If in meters, recommended
#'   resolutions are 250m, 500m, 1km, 2km or 5km. The resolution needs
#'   to be carefully chosen. If set too small (e.g. < 250m), raster
#'   file will be too big to fit in memory and R will crash. Default
#'   is 1km.
#' 
#' @param rm_download boolean. If TRUE, remove downloaded files and
#'   folders except `current_chelsa.tif` file in the `data_raw`
#'   folder. Default is FALSE.
#' 
#' @return character. The absolute path to the `current_chelsa.tif` file.
#' 
#' @details
#' Unit of each climatic variable :
#'
#' | Name                                  | Unit                 |
#' | ------------------------------------- | -------------------- |
#' | Temperature average (tas)             | °C x 10              |
#' | Temperature min (tasmin)              | °C x 10              |
#' | Temperature max (tasmax)              | °C x 10              |
#' | Precipitation                         | kg.m^{-2}            |
#' | Cloud area fraction (clt)             | %                    |
#' | PET Penman                            | kg.m^{-2}            |
#' | PET Thornthwaite                      | kg.m^{-2}            |
#' | Climatic water deficit (Penman)       | kg.m^{-2}            |
#' | Climatic dater deficit (Thornthwaite) | kg.m^{-2}            |
#' | Number of dry months (Penman)         | month                |
#' | Number of dry months (Thornthwaite)   | month                |
#' | bio1                                  | °C x 10              |
#' | bio2                                  | °C x 10              |
#' | bio3                                  | °C x 10              |
#' | bio4                                  | °C x 10              |
#' | bio5                                  | °C x 10              |
#' | bio6                                  | °C x 10              |
#' | bio7                                  | °C x 10              |
#' | bio8                                  | °C x 10              |
#' | bio9                                  | °C x 10              |
#' | bio10                                 | °C x 10              |
#' | bio11                                 | °C x 10              |
#' | bio12                                 | kg.m^{-2}.year^{-1}  |
#' | bio13                                 | kg.m^{-2}.month^{-1} |
#' | bio14                                 | kg.m^{-2}.month^{-1} |
#' | bio15                                 | kg.m^{-2}            |
#' | bio16                                 | kg.m^{-2}.month^{-1} |
#' | bio17                                 | kg.m^{-2}.month^{-1} |
#' | bio18                                 | kg.m^{-2}.month^{-1} |
#' | bio19                                 | kg.m^{-2}.month^{-1} |
#' @md
#'
#' @import stringr
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @import terra
#' @import httr
#' @importFrom glue glue
#' @export
#' 
get_chelsa_current <- function(extent_latlon, extent_proj, EPSG_proj, destination,
                               resol = 1000, rm_download = FALSE) {

  # Extent for gdal_translate
  # /!\ with gdal_translate: c(xmin, ymax, xmax, ymin) corresponding to <ulx> <uly> <lrx> <lry> 
  extent_gdal_translate <- c(extent_latlon[1], extent_latlon[4],
                             extent_latlon[3], extent_latlon[2])
  
  # Transform extent_proj from vector to string
  extent_proj_string <- paste(extent_proj, collapse=" ")
  
  nodata_Int16 <- -32768
  nodata_Int32 <- -2147483648
  nodata_UInt16 <- 65535
  
  proj_s <- "EPSG:4326"
  proj_t <- paste0("EPSG:", EPSG_proj)
  dir.create(file.path(destination, "data_raw", "chelsa_v2_1", "temp"),
             recursive = TRUE, showWarnings = FALSE)

  ## ==============================
  ## Download
  ## ==============================

  ## Base url for Chelsa
  ## More easy to modify here if this url changes in the future
  url_base_chelsa <- paste0(
    "https://os.zhdk.cloud.switch.ch/",
    "envicloud/chelsa/chelsa_V2/GLOBAL/",
    "climatologies/1981-2010")
 
  ## Climatic variables
  var <- c("clt", "pet", "pr", "tas", "tasmax", "tasmin")
  varr <- c("clt", "pet_penman", "pr", "tas", "tasmax", "tasmin")
  nvar <- length(var)

  iter_bar <- 0
  nb_var_download <- nvar * 12
  cat("Downloading clt, pet_penman, pr, tas, tasmax, tasmin", "\n")
  pb = txtProgressBar(min = 1, max = nb_var_download, width=30, style=3)
  ## Loop on variables
  for (v in 1:nvar) {
    ## Loop on months
    for (m in stringr::str_pad(1:12, width = 2, pad = "0")) {
      ifile <- glue::glue("/vsicurl/{url_base_chelsa}/{var[v]}/",
                          "CHELSA_{varr[v]}_{m}_1981-2010_V.2.1.tif")
      ofile <- file.path(destination, "data_raw", "chelsa_v2_1", "temp",
                         paste0(varr[v], m, ".tif"))
      gdal_utils_translate(ifile, ofile, extent_gdal_translate,
                           opts=c("-ot", "Int32"))
      iter_bar <- iter_bar + 1
      setTxtProgressBar(pb, iter_bar)
    }
  }
  close(pb)

  ## Bioclimatic variables
  ## https://chelsa-climate.org/wp-admin/download-page/CHELSA_tech_specification_V2.pdf
  cat("Downloading bioclimatic variables", "\n")
  pb = txtProgressBar(min = 1, max = 19, width=30, style=3)
  for (i in 1:19) {
    ifile <- glue::glue("/vsicurl/{url_base_chelsa}/bio/",
                        "CHELSA_bio{i}_1981-2010_V.2.1.tif")
    ofile <- file.path(destination, 'data_raw', 'chelsa_v2_1', 'temp',
                       paste0('bio', stringr::str_pad(i, width = 2, pad = '0'), '.tif'))
    # Data type and nodata
    if (i %in% c(8:11, 16:19)) {dtype <- "UInt16"}  # nodata_val == 65535
    if (i %in% c(1, 2, 4:7, 12:14)) {dtype <- "Int32"}  # nodata_val == -99999
    if (i %in% c(3, 15)) {dtype <- "Float32"}  # nodata_val == 3.4028234663852886e+38
    # Download
    gdal_utils_translate(ifile, ofile, extent_gdal_translate,
                         opts=c("-ot", dtype))
    setTxtProgressBar(pb, i)
  }
  close(pb)

  ## ================================
  ## Reproject
  ## ================================

  cat("Reprojecting rasters", "\n")

  var_list <- c("tas", "tasmin", "tasmax", "pr", "pet_penman", "clt", "bio")
  for (var in var_list) {
    idir <- file.path(destination, "data_raw", "chelsa_v2_1", "temp")
    tif_files <- list.files(idir, pattern = paste0(var, "[0-9]{2}\\.tif"),
                            full.names = TRUE)
    for (i in 1:length(tif_files)) {
      ifile <- tif_files[i]
      ofile <- gsub(".tif", "_res.tif", tif_files[i])
      opts <- glue("-tr {resol} {resol} -te {extent_proj_string} ",
                   "-s_srs {proj_s} -t_srs {proj_t} -overwrite ",
                   "-r bilinear -dstnodata {nodata_Int16} ",
                   "-ot Int16 -of GTiff -co COMPRESS=LZW -co PREDICTOR=2")
      sf::gdal_utils(util="warp", source=ifile, destination=ofile,
                     options=unlist(strsplit(opts, " ")),
                     quiet=TRUE)
    }
  }

  ## ================================
  ## Stack per variable
  ## ================================

  cat("Stack per variable", "\n")
  for (var in var_list) {
    idir <- file.path(destination, "data_raw", "chelsa_v2_1", "temp")
    tif_files_res <- list.files(idir, pattern = paste0(var, "[0-9]{2}_res\\.tif"),
                                full.names = TRUE)
    r <- terra::rast(sort(tif_files_res))
    names(r) <- paste0(var, 1:length(tif_files_res))

    # Export
    ofile <- file.path(destination, "data_raw", "chelsa_v2_1", paste0(var, "_res.tif"))
    terra::writeRaster(r, gdal = c("COMPRESS=LZW","PREDICTOR=2"),
                       progress = 0, overwrite = TRUE,
                       datatype = "INT2S", filename = ofile)
  }

  ## ==============================
  ## Create raster stack
  ## ==============================

  cat("Create raster stack of monthly variables and bioclimatic variables", "\n")
  # Stack tasmin, tasmax, tas, pr, clt, pet_penman, and bio
  tif_files <- file.path(destination, "data_raw", "chelsa_v2_1",
                         paste0(var_list, "_res.tif"))
  r <- terra::rast(tif_files)
  ofile <- file.path(destination, "data_raw", "chelsa_v2_1", "clim_res.tif")
  terra::writeRaster(r, gdal = c("COMPRESS=LZW","PREDICTOR=2"),
                     progress = 0, overwrite = TRUE,
                     datatype = "INT2S", filename = ofile)

  ## ===================================================
  ## Compute water deficit (cwd and ndw) with Penman ETP
  ## ===================================================

  cat("Compute water deficit (cwd and ndm) with Penman ETP", "\n")
  ## cwd: climatic water deficit
  ## ndm: number of dry months
  pr_file <- file.path(destination, "data_raw", "chelsa_v2_1", "pr_res.tif")
  pet_penman_file <- file.path(destination, "data_raw", "chelsa_v2_1", "pet_penman_res.tif")
  r_pet_penman <- terra::rast(pet_penman_file)
  r_pr <- terra::rast(pr_file)

  # CWD = min(pet_penman_i - pr_i, 0)
  # CWD is positive and indicates a deficit of water
  cwd <- r_pet_penman
  values(cwd) <- pmax(values(r_pet_penman) - values(r_pr), 0)
  names(cwd) <- paste0("cwd", 1:12)
  
  cwd_annual <- cwd[[1,]]
  values(cwd_annual) <- rowSums(values(cwd))
  names(cwd_annual) <- "cwd"
  ofile <- file.path(destination, "data_raw", "chelsa_v2_1", "cwd_res.tif")
  terra::writeRaster(cwd_annual, gdal = c("COMPRESS=LZW","PREDICTOR=2"),
                     overwrite = TRUE, datatype = "INT2S", filename = ofile)
 
  ndm <- cwd_annual
  ndm_per_month <- ifelse(values(cwd) > 0, 1, 0)
  values(ndm) <- rowSums(ndm_per_month)
  names(ndm) <- "ndm"
  ofile <- file.path(destination, "data_raw", "chelsa_v2_1", "ndm_res.tif")
  terra::writeRaster(ndm, gdal = c("COMPRESS=LZW","PREDICTOR=2"),
                     overwrite = TRUE, datatype = "INT2S", filename = ofile)
  rm(ndm_per_month, ndm, cwd, cwd_annual)

  ## =========================================================
  ## Compute water deficit (cwd and ndw) with Thornthwaite ETP
  ## =========================================================

  cat("Compute water deficit and number of dry months (cwd and ndm) with Thornthwaite ETP", "\n")
  ## PET with Thornthwaite formula
  tas <- terra::rast(file.path(destination, "data_raw", "chelsa_v2_1", "tas_res.tif"))
  # Keep only latitude coordinates
  extent_tas <- sf::st_bbox(tas)
  e <- terra::ext(extent_tas[1], extent_tas[3], extent_tas[2], extent_tas[4])
  e <- terra::as.polygons(e)
  terra::crs(e) <- paste0("epsg:", EPSG_proj)
  ext_ll <- sf::st_bbox(terra::project(e, "epsg:4326"))
  lat <- seq(ext_ll[4], ext_ll[2], length.out = dim(tas)[1])
  lat <- rep(lat, each = dim(tas)[2])
  tas_matrix <- values(tas)/10
  I <- (tas_matrix / 5)^1.514
  alpha <- (6.75e-7) * I^3 - (7.71e-5) * I^2 + (1.792e-2) * I + 0.49239
  L <- NULL
  month_length <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  # Mid-month julian day: 15 of each month for a regular year
  mid_month_jday <- c(15, 46, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349)
  for (i in 1:12){
    day_lengths <- daylength(lat=lat, long=0, jd=mid_month_jday[i], tmz=0)[, 3]
    L <- cbind(L, day_lengths)
  }
  PET_Thornthwaite <- 16 * (L / 12) * (10 * tas_matrix / I)^alpha
  pet <- tas
  for (i in 1:12) {
    values(pet)[, i] <- PET_Thornthwaite[, i] * (month_length[i] / 30)
  }
  rm(PET_Thornthwaite, tas_matrix, lat, I, L)
  ## Set attribute name and dimension values
  names(pet) <- paste0("pet_thornthwaite", 1:12)
  ofile <- file.path(destination, "data_raw", "chelsa_v2_1", "pet_thornthwaite_res.tif")
  terra::writeRaster(pet, gdal = c("COMPRESS=LZW","PREDICTOR=2"),
                     overwrite = TRUE, datatype = "INT2S", filename = ofile)

  ## CWD with Thornthwaite PET
  ifile <- file.path(destination, "data_raw", "chelsa_v2_1", "pr_res.tif")
  pr <- terra::rast(ifile)
  cwd_thornthwaite <- pet
  values(cwd_thornthwaite) <- pmax(values(pet) - values(pr), 0)
  names(cwd_thornthwaite) <- paste0("cwd_thornthwaite", 1:12)
  
  cwd_annual <- cwd_thornthwaite[[1,]]
  values(cwd_annual) <- rowSums(values(cwd_thornthwaite))
  names(cwd_annual) <- "cwd_thornthwaite"
  ofile <- file.path(destination, "data_raw", "chelsa_v2_1", "cwd_thornthwaite_res.tif")
  terra::writeRaster(cwd_annual, gdal = c("COMPRESS=LZW","PREDICTOR=2"),
                     overwrite = TRUE, datatype = "INT2S", filename = ofile)
  
  ## NDM with Thornthwaite
  ndm <- cwd_annual
  ndm_per_month <- ifelse(values(cwd_thornthwaite) > 0, 1, 0)
  values(ndm) <- rowSums(ndm_per_month)
  names(ndm) <- "ndm_thornthwaite"
  ofile <- file.path(destination, "data_raw", "chelsa_v2_1", "ndm_thornthwaite_res.tif")
  terra::writeRaster(ndm, gdal = c("COMPRESS=LZW","PREDICTOR=2"),
                     overwrite = TRUE, datatype = "INT2S", filename = ofile)
  rm(ndm_per_month, ndm, cwd_thornthwaite, cwd_annual)

  ## =========================================================
  ## Creating final raster stack
  ## =========================================================

  # Message
  cat("Creating final raster stack", "\n")
  
  # Output file
  ofile <- file.path(destination, "data_raw", "current_chelsa_no_name.tif")

  # Load all files
  clim <- terra::rast(file.path(destination, "data_raw", "chelsa_v2_1", "clim_res.tif"))
  cwd <- terra::rast(file.path(destination, "data_raw", "chelsa_v2_1", "cwd_res.tif"))
  ndm <- terra::rast(file.path(destination, "data_raw", "chelsa_v2_1", "ndm_res.tif"))
  pet_t <- terra::rast(file.path(destination, "data_raw", "chelsa_v2_1", "pet_thornthwaite_res.tif"))
  cwd_t <- terra::rast(file.path(destination, "data_raw", "chelsa_v2_1", "cwd_thornthwaite_res.tif"))
  ndm_t <- terra::rast(file.path(destination, "data_raw", "chelsa_v2_1", "ndm_thornthwaite_res.tif"))

  # Create current raster with all climatic layers
  current <- c(clim, cwd, ndm, pet_t, cwd_t, ndm_t)
  layer_names <- c(names(clim), "cwd_penman", "ndm_penman",
                   paste0("pet_thornthwaite_", 1:12), "cwd_thornthwaite", "ndm_thornthwaite")
  names(current) <- layer_names

  # Write to disk
  ofile <- file.path(destination, "data_raw", "current_chelsa.tif")
  terra::writeRaster(current, filename=ofile,
                     gdal=c("COMPRESS=LZW", "PREDICTOR=2"),
                     progress=FALSE, overwrite=TRUE, datatype="INT4S")  
  cat("File ", ofile, " has been created", "\n")

  ## ========================================
  ## Clean and return results
  ## ========================================

  if (rm_download) {
    input_dir <- file.path(destination, "data_raw", "chelsa_v2_1")
    cat("Removing folder ", input_dir, "\n")
    unlink(file.path(destination, "data_raw", "chelsa_v2_1"), recursive = TRUE)
    unlink(file.path(destination, "data_raw", "current_chelsa_no_name.tif"))
  }

  return(file.path(destination, "data_raw", "current_chelsa.tif"))
}

# End
