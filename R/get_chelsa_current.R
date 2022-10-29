#!/usr/bin/R

## ==============================================================================
## authors         :Pierre Guillaumont, Ghislain Vieilledent
## emails          :pierreguillaumont@gmail.com,
##                  ghislain.vieilledent@cirad.fr, ghislainv@gmail.com
## web             :https://ecology.ghislainv.fr
## license         :GPLv3
## ==============================================================================

get_chelsa_current <- function(extent, extent_latlon, EPSG, destination, resolution = 1000, rm_download = FALSE) {
  #' Create multilayer Tiff file with 107 variables from chelsa-climate.org
  #'
  #' @description
  #' Gives the average values on the data recovered between 1981 and 2010.
  #' Monthly variables are average temperatures, min temperatures, max temperatures, precipitation, potential evapotranspiration (with Penman formula and with Thornthwaite formula),
  #' and total cloud cover.
  #' Others variables are climatic water deficit (with Penman and Thornthwaite), number of dry month (with Penman and Thornthwaite) and 19 bio variables (more information in chelsa documentation).
  #'
  #' @param extent character. First output of `transform_shp_coutry_extent`.
  #' @param extent_latlon character. Second output of `transform_shp_country_extent`
  #' @param EPSG int. to consider for this country/area.
  #' @param destination character. absolute path where to download files like `here()` output.
  #' @param resolution int. in meters, recommended resolution are 250m, 500m, 1km, 2km or 5km, default is 1km. See more in details.
  #' @param rm_download boolean. If TRUE remove download files and folders. Keep only environ.tif in `data_raw` folder, default is FALSE.
  #' @return character. absolute path to climate_chelsa.tif.
  #' @details `resolution` need to be carefully choosen because if Tiff file is too big, R can crash.
  #'
  #' @details
  #' Unit of each climatic variable :
  #'
  #' | Name                                  | Unit                 |
  #' | ------------------------------------- | -------------------- |
  #' | Temperature average (tas)             | °C x 10              |
  #' | Temperature min (tasmin)              | °C x 10              |
  #' | Temperature max (tasmax)              | °C x 10              |
  #' | Precipitation                         | kg.m^{-1}            |
  #' | Cloud area fraction (clt)             | %                    |
  #' | PET Penman                            | kg.m^{-1}            |
  #' | PET Thornthwaite                      | kg.m^{-1}            |
  #' | Climatic water deficit (Penman)       | kg.m^{-1}            |
  #' | Climatic dater deficit (Thornthwaite) | kg.m^{-1}            |
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
  #' @import stars
  #' @import stringr
  #' @importFrom utils txtProgressBar setTxtProgressBar
  #' @import geosphere
  #' @import terra
  #' @import sp
  #' @importFrom glue glue
  #' @export

  nodat <- -9999
  proj.s <- "EPSG:4326"
  proj.t <- paste0("EPSG:", EPSG)
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

  ## Extent for gdal
  ext_gdal <- paste(extent_latlon[1], extent_latlon[4],
                    extent_latlon[3], extent_latlon[2], sep=" ")

  progress_bar <- 0
  nb_var_download <- 12 * 6
  print("Downloading tasmin, tasmax, tas, pr, clt, and pet_penman")
  pb = txtProgressBar(min = 0, max = nb_var_download, initial = 0)
  for (m in str_pad(1:12, width = 2, pad = "0")) {
    ## Monthly minimum temperature (°C).
    ifile <- glue("{url_base_chelsa}/tasmin/CHELSA_tasmin_{m}_1981-2010_V.2.1.tif")
    ofile <- file.path(destination, 'data_raw', 'chelsa_v2_1', 'temp',
                       paste0('tasmin', m, '.tif'))
    if (!file.exists(ofile)) {
      system(glue("gdal_translate -projwin {ext_gdal} -projwin_srs {proj.s} \\
                  /vsicurl/{ifile} {ofile}"), ignore.stdout = TRUE, ignore.stderr = TRUE)
    }
    progress_bar <- progress_bar + 1
    setTxtProgressBar(pb, progress_bar)
    ## Monthly maximum temperature (°C).
    ifile <- glue("{url_base_chelsa}/tasmax/CHELSA_tasmax_{m}_1981-2010_V.2.1.tif")
    ofile <- file.path(destination, 'data_raw', 'chelsa_v2_1', 'temp',
                       paste0('tasmax', m, '.tif'))
    if (!file.exists(ofile)) {
      system(glue("gdal_translate -projwin {ext_gdal} -projwin_srs {proj.s} \\
                  /vsicurl/{ifile} {ofile}"), ignore.stdout = TRUE, ignore.stderr = TRUE)
    }
    progress_bar <- progress_bar + 1
    setTxtProgressBar(pb, progress_bar)
    ## Monthly average temperature (°C).
    ifile <- glue("{url_base_chelsa}/tas/CHELSA_tas_{m}_1981-2010_V.2.1.tif")
    ofile <- file.path(destination, 'data_raw', 'chelsa_v2_1', 'temp',
                       paste0('tas', m, '.tif'))
    if (!file.exists(ofile)) {
      system(glue("gdal_translate -projwin {ext_gdal} -projwin_srs {proj.s} \\
                  /vsicurl/{ifile} {ofile}"), ignore.stdout = TRUE, ignore.stderr = TRUE)
    }
    progress_bar <- progress_bar + 1
    setTxtProgressBar(pb, progress_bar)
    ## Monthly precipitation (mm ~ kg/m2).
    ifile <- glue("{url_base_chelsa}/pr/CHELSA_pr_{m}_1981-2010_V.2.1.tif")
    ofile <- file.path(destination, 'data_raw', 'chelsa_v2_1', 'temp',
                       paste0('pr', m, '.tif'))
    if (!file.exists(ofile)) {
      system(glue("gdal_translate -projwin {ext_gdal} -projwin_srs {proj.s} \\
                  /vsicurl/{ifile} {ofile}"), ignore.stdout = TRUE, ignore.stderr = TRUE)
    }
    progress_bar <- progress_bar + 1
    setTxtProgressBar(pb, progress_bar)
    ## Monthly cloud area fraction
    ifile <- glue("{url_base_chelsa}/clt/CHELSA_clt_{m}_1981-2010_V.2.1.tif")
    ofile <- file.path(destination, 'data_raw', 'chelsa_v2_1', 'temp',
                       paste0('clt', m, '.tif'))
    if (!file.exists(ofile)) {
      system(glue("gdal_translate -projwin {ext_gdal} -projwin_srs {proj.s} \\
                  /vsicurl/{ifile} {ofile}"), ignore.stdout = TRUE, ignore.stderr = TRUE)
    }
    progress_bar <- progress_bar + 1
    setTxtProgressBar(pb, progress_bar)
    ## Monthly pet_penman
    # https://www.fao.org/3/x0490e/x0490e06.htm
    ifile <- glue("{url_base_chelsa}/pet/CHELSA_pet_penman_{m}_1981-2010_V.2.1.tif")
    ofile <- file.path(destination, 'data_raw', 'chelsa_v2_1', 'temp',
                       paste0('pet_penman', m, '.tif'))
    if (!file.exists(ofile)) {
      system(glue("gdal_translate -projwin {ext_gdal} -projwin_srs {proj.s} \\
                  /vsicurl/{ifile} {ofile}"), ignore.stdout = TRUE, ignore.stderr = TRUE)
    }
    progress_bar <- progress_bar + 1
    setTxtProgressBar(pb, progress_bar)
  }
  close(pb)

  ## Bioclimatic variables
  ## See https://chelsa-climate.org/wp-admin/download-page/CHELSA_tech_specification_V2.pdf for details
  print("Downloading bioclimatic variables")
  for(i in 1:19){
    ifile <- glue("{url_base_chelsa}/bio/CHELSA_bio{i}_1981-2010_V.2.1.tif")
    ofile <- file.path(destination, 'data_raw', 'chelsa_v2_1', 'temp',
                       paste0('bio', str_pad(i, width = 2, pad = '0'), '.tif'))
    if (!file.exists(ofile)) {
      system(glue("gdal_translate -projwin {ext_gdal} -projwin_srs {proj.s} \\
                  /vsicurl/{ifile} {ofile}"), ignore.stdout = TRUE, ignore.stderr = TRUE)
    }
  }

  ## ================================
  ## Reproject and stack per variable
  ## ================================

  print("Reprojecting rasters and stacking monthly rasters per variable")
  for(var in c("tasmin", "tasmax", "tas", "pr", "bio", "clt", "pet_penman")) {
    ifile <- file.path(destination, "data_raw", "chelsa_v2_1", "temp")
    files.tif <- list.files(ifile, pattern = paste0(var, "[0-9]{2}\\.tif"), full.names = TRUE)
    for (i in 1:length(files.tif)) {
      sourcefile <- files.tif[i]
      destfile <- gsub(".tif", "_res.tif", files.tif[i])
      system(glue("gdalwarp -overwrite -s_srs {proj.s} -t_srs {proj.t} \\
        -r bilinear -tr {resolution} {resolution} -te {extent} -ot Int16 -of GTiff -srcnodata 0 -dstnodata {nodat} \\
        {sourcefile} {destfile}"), ignore.stdout = TRUE, ignore.stderr = TRUE)
      if (var %in% c("tasmin", "tasmax", "tas") | (var == "bio" & i <= 11)) {
        # Stock °C as integer to reduce size
        # °C * 10 to keep information
        change_scale <- round(read_stars(destfile) * 10)
        write_stars(obj = change_scale, options = c("COMPRESS=LZW", "PREDICTOR=2"), NA_value = nodat,
                    type = "Int16", dsn = destfile)
        rm(change_scale)
      }
    }
    ifile <- file.path(destination, "data_raw", "chelsa_v2_1", "temp")
    files.tif <- list.files(ifile, pattern = paste0(var, "[0-9]{2}_res\\.tif"), full.names = TRUE)
    r <- read_stars(sort(files.tif), along = "band")
    r <- split(r)
    names(r) <- c(paste0(var, 1:length(names(r))))
    r <- merge(r)
    ofile <- file.path(destination, "data_raw", "chelsa_v2_1",
                       paste0(var, "_res.tif"))
    write_stars(obj = r, options = c("COMPRESS=LZW", "PREDICTOR=2"),
                type = "Int16", dsn = ofile)
  }

  ## ==============================
  ## Create raster stack
  ## ==============================

  print("Create raster stack of monthly variables and bioclimatic variables")
  # Stack tasmin, tasmax, tas, pr, clt, pet_penman, and bio
  files.tif <- file.path(destination, "data_raw", "chelsa_v2_1",
                         paste0(c("tasmin", "tasmax", "tas", "pr", "clt", "pet_penman", "bio"), "_res.tif"))
  r <- c(read_stars(files.tif[1]), read_stars(files.tif[2]), read_stars(files.tif[3]), read_stars(files.tif[4]),
         read_stars(files.tif[5]), read_stars(files.tif[6]), read_stars(files.tif[7]), along = "band")
  ofile <- file.path(destination, "data_raw", "chelsa_v2_1", "clim_res.tif")
  write_stars(obj = r, options = c("COMPRESS=LZW", "PREDICTOR=2"), NA_value = nodat,
              type = "Int16", dsn = ofile)
  rm(r)

  ## ===================================================
  ## Compute water deficit (cwd and ndw) with Penman ETP
  ## ===================================================

  print("Compute water deficit (cwd and ndw) with Penman ETP")
  ## cwd: climatic water deficit
  ## ndm: number of dry months
  pr_file <- file.path(destination, "data_raw", "chelsa_v2_1", "pr_res.tif")
  pet_penman_file <- file.path(destination, "data_raw", "chelsa_v2_1", "pet_penman_res.tif")

  ## Monthly values of cdw and ndm
  for (i in 1:12) {
    # CWD = min(pet_penman_i - pr_i, 0)
    # CWD is positive and indicates a deficit of water
    cwd_file <- file.path(destination, "data_raw", "chelsa_v2_1", paste0("cwd", i, "_res.tif"))
    system(glue('gdal_calc.py -A {pet_penman_file} --A_band={i} -B {pr_file} --B_band={i} --quiet --type=Int16 \\
                --creation-option="COMPRESS=LZW" --creation-option="PREDICTOR=2"  --calc="A-B" --NoDataValue={nodat} \\
                --outfile={cwd_file} --overwrite'),
           ignore.stdout = TRUE, ignore.stderr = TRUE)
    # Number of dry months, ie sum(CWD > 0)
    ndm_file <- file.path(destination, "data_raw", "chelsa_v2_1", "temp",
                          paste0("ndm", i, "_res.tif"))
    system(glue('gdal_calc.py -A {cwd_file} --A_band={1} --quiet --type=Int16 \\
                --creation-option="COMPRESS=LZW" --creation-option="PREDICTOR=2" \\
                --outfile={ndm_file} --calc="A>0" --overwrite --NoDataValue={nodat}'),
           ignore.stdout = TRUE, ignore.stderr = TRUE)
  }

  ifile <- file.path(destination, "data_raw", "chelsa_v2_1", "temp")
  ndm_files <- list.files(ifile, pattern = "ndm[0-9]{1,2}_res.tif", full.names = TRUE)
  ofile <- file.path(destination, "data_raw", "chelsa_v2_1", "ndm_res.tif")
  system(glue('gdal_calc.py -A {ndm_files[1]} -B {ndm_files[2]} -C {ndm_files[3]} -D {ndm_files[4]}  -E {ndm_files[5]} \\
            -F {ndm_files[6]} -G {ndm_files[7]} -H {ndm_files[8]} -I {ndm_files[9]} -J {ndm_files[10]} -K {ndm_files[11]} \\
            -L {ndm_files[12]} --quiet --type=Int16 --creation-option="COMPRESS=LZW" --creation-option="PREDICTOR=2" \\
            --outfile={ofile} --NoDataValue={nodat} \\
            --calc="A+B+C+D+E+F+G+H+I+J+K+L" --overwrite'), ignore.stdout = TRUE, ignore.stderr = TRUE)

  ifile <- file.path(destination, "data_raw", "chelsa_v2_1")
  cwd_files <- list.files(ifile, pattern = "cwd[0-9]{1,2}_res.tif", full.names = TRUE)
  ofile <- file.path(destination, "data_raw", "chelsa_v2_1", "cwd_res.tif")
  system(glue('gdal_calc.py -A {cwd_files[1]} -B {cwd_files[2]} -C {cwd_files[3]} -D {cwd_files[4]} -E {cwd_files[5]} \\
            -F {cwd_files[6]} -G {cwd_files[7]} -H {cwd_files[8]} -I {cwd_files[9]} -J {cwd_files[10]} -K {cwd_files[11]} \\
            -L {cwd_files[12]} --quiet --type=Int16 --creation-option="COMPRESS=LZW" --creation-option="PREDICTOR=2" \\
            --outfile={ofile} --NoDataValue={nodat} \\
            --calc="numpy.maximum(A,0)+numpy.maximum(B,0)+numpy.maximum(C,0)+numpy.maximum(D,0)+numpy.maximum(E,0)+numpy.maximum(F,0) \\
            +numpy.maximum(G,0)+numpy.maximum(H,0)+numpy.maximum(I,0)+numpy.maximum(J,0)+numpy.maximum(K,0)+numpy.maximum(L,0)" --overwrite'), ignore.stdout = TRUE, ignore.stderr = TRUE)

  ## =========================================================
  ## Compute water deficit (cwd and ndw) with Thornthwaite ETP
  ## =========================================================

  print("Compute water deficit (cwd and ndw) with Thornthwaite ETP")
  ## PET with Thornthwaite formula
  tas <- read_stars(file.path(destination, "data_raw", "chelsa_v2_1", "tas_res.tif"))
  # keep only latitude coordinates
  extent <- st_bbox(tas)
  e <- ext(extent[1], extent[3], extent[2], extent[4])
  e <- terra::as.polygons(e)
  crs(e) <- paste0("epsg:", EPSG)
  extent_latlon <- st_bbox(terra::project(e, "epsg:4326"))
  lat <- seq(extent_latlon[4], extent_latlon[2], length.out = dim(tas)[2])
  lat <- rep(lat, each = dim(tas)[1])
  tas_matrix <- NULL
  for (month in 1:12) {
    tas_matrix <- cbind(tas_matrix, c(tas[[1]][,, month] / 10))
  }
  I <- (tas_matrix / 5)^1.514
  alpha <- (6.75e-7) * I^3 - (7.71e-5) * I^2 + (1.792e-2) * I + 0.49239
  L <- NULL
  month_length <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  mid_month_day <- c(15, 43, 74, 104, 135, 165, 196, 227, 257, 288, 318, 349)
  for (i in 1:12){
    L <- cbind(L, daylength(lat, doy = mid_month_day[i]))
  }
  PET_Thornthwaite <- 16 * (L / 12) * (10 * tas_matrix / I)^alpha
  pet_stars <- tas
  for (i in 1:12){
    pet_stars[[1]][,, i][!is.na(pet_stars[[1]][,, i])] <- PET_Thornthwaite[, i] * (month_length[i] / 30)
  }
  rm(PET_Thornthwaite, tas_matrix, lat, I, L)
  pet_stars <- split(pet_stars)
  names(pet_stars) <- paste0("pet_thornthwaite", 1:12)
  ofile <- file.path(destination, "data_raw", "chelsa_v2_1", "pet_thornthwaite_res.tif")
  write_stars(merge(pet_stars) , dsn = ofile,
              options = c("COMPRESS=LZW","PREDICTOR=2"), type = "Int16")

  ## CWD with Thornthwaite PET
  ifile <- file.path(destination, "data_raw", "chelsa_v2_1", "pr_res.tif")
  pr <- read_stars(ifile)
  cwd_thornthwaite <- merge(pet_stars) - pr
  rm(pet_stars)
  cwd_thornthwaite[[1]] <- pmax(cwd_thornthwaite[[1]], 0)
  cwd_thornthwaite <- split(cwd_thornthwaite)
  names(cwd_thornthwaite) <- "cwd_thornthwaite"
  cwd_annual <- split(tas)[1,,]
  cwd_annual[[1]] <- rowSums(merge(cwd_thornthwaite)[[1]], dims = 2)
  names(cwd_annual) <- "cwd_thornthwaite"
  ofile <- file.path(destination, "data_raw", "chelsa_v2_1", "cwd_thornthwaite_res.tif")
  write_stars(cwd_annual, dsn = ofile,
              options = c("COMPRESS=LZW","PREDICTOR=2"), type = "Int16")
  rm(cwd_annual)

  ## NDM with Thornthwaite
  ndm_stars <- split(tas)[1,,]
  ndm_stars[[1]] <- rowSums(merge(cwd_thornthwaite)[[1]] > 0, dims = 2)
  rm(cwd_thornthwaite)
  names(ndm_stars) <- "ndm_thornthwaite"
  ofile <- file.path(destination, "data_raw", "chelsa_v2_1", "ndm_thornthwaite_res.tif")
  write_stars(ndm_stars , dsn = ofile,
              options = c("COMPRESS=LZW","PREDICTOR=2"))
  rm(ndm_stars)

  ## =========================================================
  ## Creating final raster stack
  ## =========================================================

  print("Creating final raster stack")
  ofile <- file.path(destination, "data_raw", "current_chelsa_no_name.tif")
  system(glue('gdal_merge.py -o {ofile} -of GTiff -ot Int16 -co "COMPRESS=LZW" \\
            -co "PREDICTOR=2" -separate -a_nodata {nodat} \\
            {file.path(destination, "data_raw", "chelsa_v2_1", "clim_res.tif")} \\
            {file.path(destination, "data_raw", "chelsa_v2_1", "cwd_res.tif")} \\
            {file.path(destination, "data_raw", "chelsa_v2_1", "ndm_res.tif")} \\
            {file.path(destination, "data_raw", "chelsa_v2_1", "pet_thornthwaite_res.tif")} \\
            {file.path(destination, "data_raw", "chelsa_v2_1", "cwd_thornthwaite_res.tif")} \\
            {file.path(destination, "data_raw", "chelsa_v2_1", "ndm_thornthwaite_res.tif")}'), ignore.stdout = TRUE, ignore.stderr = TRUE)
  current <- rast(file.path(destination, "data_raw",  "current_chelsa_no_name.tif"))
  clim_res_file <- file.path(destination, "data_raw", "chelsa_v2_1", "clim_res.tif")
  names(current) <- c(names(rast(clim_res_file)), "cwd_penman", "ndm_penman", paste0("pet_thornthwaite_", 1:12), "cwd_thornthwaite", "ndm_thornthwaite")
  ofile <- file.path(destination, "data_raw", "current_chelsa.tif")
  writeRaster(current, datatype = "Int16", filename = ofile,
              overwrite = TRUE, gdal = c("COMPRESS=LZW","PREDICTOR=2"), progress = 0)
  rm(current)
  cat("File ", ofile, " has been created\n")

  ## ========================================
  ## Clean and return results
  ## ========================================

  if (rm_download) {
    ifile <- file.path(destination, "data_raw", "chelsa_v2_1")
    cat("Removing folder ", ifile, "\n")
    unlink(file.path(destination, "data_raw", "chelsa_v2_1"), recursive = TRUE)
  }
  return(file.path(destination, "data_raw", "current_chelsa.tif"))

}

## End of file
