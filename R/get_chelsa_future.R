#' Create several multilayer Tiff files with 81 variables from
#' chelsa-climate.org with future climat variables.
#'
#' @description Gives predictions on future values for the choosen
#'   phase. Prediction are from multiple models (GFDL-ESM4,
#'   IPSL-CM6A-LR, MPI-ESM1-2-HR, MRI-ESM2-0, UKESM1-0-LL).  Creates
#'   folder for each model and a folder with mean of this five models.
#'   Monthly variables are average temperatures, min temperatures, max
#'   temperatures, precipitation, potential evapotranspiration with
#'   Thornthwaite formula.  Others variables are climatic water
#'   deficit with Thornthwaite, number of dry month with Thornthwaite
#'   and 19 bio variables (more information in chelsa documentation).
#'
#' @param extent_latlon vector. First output of `get_aoi_extent()` function.
#' 
#' @param extent_proj vector. Second output of `get_aoi_extent()` function.
#' 
#' @param EPSG int. to consider for this country/area.
#' 
#' @param destination character. absolute path where to download files
#'   like `here()` output.
#' 
#' @param resol int. Resolution. If in meters, recommended resolutions
#'   are 250m, 500m, 1km, 2km or 5km. The resolution needs to be
#'   carefully chosen. If set too small (e.g. < 250m), raster file
#'   will be too big to fit in memory and R will crash. Default is
#'   1km.
#' 
#' @param phase character. Must be in c("2041-2070", "2071-2100")
#'   match to years to download, default is "2071-2100".
#' 
#' @param GCM character. Global Climatic Models considered. Must be
#'   in c("GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "MRI-ESM2-0",
#'   "UKESM1-0-LL"). By default climate simulated by the five GCMs are
#'   downloaded.
#' 
#' @param SSP character or int. Scenario specifier must be in c(126,
#'   370, 585). Corresponding to the the Shared Socio-economic
#'   Pathways you want to download, default is 585 for SSP5-RCP8.5
#'   climate as simulated by the GCMs.
#' 
#' @param rm_download boolean. If TRUE remove download files and
#'   folders. Keep only future_chelsa.tif in `data_raw` folder,
#'   default is FALSE.
#' 
#' @return character. absolute path to future_chelsa.tif.
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
#' | PET Thornthwaite                      | kg.m^{-2}            |
#' | Climatic Water Deficit (Thornthwaite) | kg.m^{-2}            |
#' | Number of dry month (Thornthwaite)    | month                |
#' | bio 1                                 | °C x 10              |
#' | bio 2                                 | °C x 10              |
#' | bio 3                                 | °C x 10              |
#' | bio 4                                 | °C x 10              |
#' | bio 5                                 | °C x 10              |
#' | bio 6                                 | °C x 10              |
#' | bio 7                                 | °C x 10              |
#' | bio 8                                 | °C x 10              |
#' | bio 9                                 | °C x 10              |
#' | bio 10                                | °C x 10              |
#' | bio 11                                | °C x 10              |
#' | bio 12                                | kg.m^{-2}.year^{-1}  |
#' | bio 13                                | kg.m^{-2}.month^{-1} |
#' | bio 14                                | kg.m^{-2}.month^{-1} |
#' | bio 15                                | kg.m^{-2}            |
#' | bio 16                                | kg.m^{-2}.month^{-1} |
#' | bio 17                                | kg.m^{-2}.month^{-1} |
#' | bio 18                                | kg.m^{-2}.month^{-1} |
#' | bio 19                                | kg.m^{-2}.month^{-1} |
#' @md
#'
#' @import stringr
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom glue glue
#' @import terra
#' @import httr
#' @export
#' 
get_chelsa_future <- function(extent_latlon, extent_proj, EPSG,
                              destination, resol = 1000,
                              phase = "2071-2100",
                              GCM = c("GFDL-ESM4", "IPSL-CM6A-LR",
                                      "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL"),
                              SSP = 585,
                              rm_download = FALSE) {

  # Extent for gdal_translate
  # /!\ with gdal_translate: c(xmin, ymax, xmax, ymin) corresponding to <ulx> <uly> <lrx> <lry> 
  extent_gdal_translate <- c(extent_latlon[1], extent_latlon[4],
                             extent_latlon[3], extent_latlon[2])

  # Transform extent_proj from vector to string
  extent_proj_string <- paste(extent_proj, collapse=" ")
  
  dir.create(path = destination, recursive = TRUE, showWarnings = FALSE)
  nodat <- -9999
  proj_s <- "EPSG:4326"
  proj_t <- paste0("EPSG:", EPSG)
  dir.create(file.path(destination, "data_raw"), showWarnings = FALSE)
  dir.create(file.path(destination, "data_raw", "future_chelsa"), showWarnings = FALSE) ## folder for climatic data
  for (model in GCM) {
    dir.create(file.path(destination, "data_raw", "future_chelsa", paste("climat", phase, model, "ssp", SSP, sep = "_"), "temp"),
               showWarnings = FALSE, recursive = TRUE)
  }
  progress_bar <- 0
  nb_var_download <- 12 * 5 * length(GCM)
  cat("Downloading tasmin, tasmax, tas and pr\n")
  pb = txtProgressBar(min = 0, max = nb_var_download, initial = 0, width=30, style=3)
  for (m in stringr::str_pad(1:12, width = 2, pad = "0")) {
    for (model in GCM) {
      ## Monthly minimum temperature (°C).
      url_tasmin <- paste0('https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/',
                           phase, '/', model, '/ssp', SSP, '/tasmin/CHELSA_',
                           tolower(model), '_r1i1p1f1_w5e5_ssp', SSP, '_tasmin_',
                           m, '_', stringr::str_replace(phase, pattern = '-', '_'), '_norm.tif')
      
      if (httr::http_error(url_tasmin)) {
        message("There appears to be a problem reaching the website.")
        return(invisible(NULL))
      }
      
      tasmin_file <- file.path(destination, 'data_raw', 'future_chelsa',
                               paste('climat', phase, model, 'ssp', SSP, sep = '_'),
                               'temp', paste0('tasmin', m, '.tif'))
      gdal_utils_translate(ifile=paste0("/vsicurl/", url_tasmin),
                           ofile=tasmin_file,
                           extent_gdal_translate)
      progress_bar <- progress_bar + 1
      setTxtProgressBar(pb, progress_bar)

      ## Monthly maximum temperature (°C).
      url_tasmax <- paste0('https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/',
                           phase, '/', model, '/ssp', SSP, '/tasmax/CHELSA_', tolower(model),
                           '_r1i1p1f1_w5e5_ssp', SSP, '_tasmax_', m, '_',
                           stringr::str_replace(phase, pattern = '-', '_'), '_norm.tif')
      
      if (httr::http_error(url_tasmax)) {
        message("There appears to be a problem reaching the website.")
        return(invisible(NULL))
      }
      
      tasmax_file <- file.path(destination, 'data_raw', 'future_chelsa', paste('climat', phase, model, 'ssp', SSP, sep = '_'),
                               'temp', paste0('tasmax', m, '.tif'))
      gdal_utils_translate(ifile=paste0("/vsicurl/", url_tasmax),
                           ofile=tasmax_file,
                           extent_gdal_translate)
      progress_bar <- progress_bar + 1
      setTxtProgressBar(pb, progress_bar)

      ## Monthly average temperature (°C).
      url_tas <- paste0('https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/',
                        phase, '/', model, '/ssp', SSP, '/tas/CHELSA_', tolower(model), '_r1i1p1f1_w5e5_ssp', SSP, '_tas_', m, '_',
                        stringr::str_replace(phase, pattern = '-', '_'), '_norm.tif')
      
      if (httr::http_error(url_tas)) {
        message("There appears to be a problem reaching the website.")
        return(invisible(NULL))
      }
      
      tas_file <- file.path(destination, 'data_raw', 'future_chelsa', paste('climat', phase, model, 'ssp', SSP, sep = '_'),
                            'temp', paste0('tas', m, '.tif'))
      gdal_utils_translate(ifile=paste0("/vsicurl/", url_tas),
                           ofile=tas_file,
                           extent_gdal_translate)
      progress_bar <- progress_bar + 1
      setTxtProgressBar(pb, progress_bar)

      ## Monthly precipitation (mm ~ kg/m2).
      url_pr <- paste0('https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/',
                       phase, '/', model, '/ssp', SSP, '/pr/CHELSA_', tolower(model), '_r1i1p1f1_w5e5_ssp', SSP, '_pr_', m, '_',
                       stringr::str_replace(phase, pattern = '-', '_'), '_norm.tif')
      
      if (httr::http_error(url_pr)) {
        message("There appears to be a problem reaching the website.")
        return(invisible(NULL))
      }
      
      pr_file <- file.path(destination, 'data_raw', 'future_chelsa', paste('climat', phase, model, 'ssp', SSP, sep = '_'),
                           'temp', paste0('pr', m, '.tif'))
      gdal_utils_translate(ifile=paste0("/vsicurl/", url_pr),
                           ofile=pr_file,
                           extent_gdal_translate)
      progress_bar <- progress_bar + 1
      setTxtProgressBar(pb, progress_bar)
    }
  }
  close(pb)

  progress_bar <- 0
  nb_var_download <- 19 *  length(GCM)
  cat("Downloading bioclimatic variables\n")
  pb = txtProgressBar(min=0, max=nb_var_download, initial=0, width=30, style=3)
  for(i in 1:19){
    for(model in GCM){

      # 19 Bioclimatic variables
      # See https://chelsa-climate.org/wp-admin/download-page/CHELSA_tech_specification_V2.pdf for details
      url_bio <- paste0('https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/', phase, '/', model, '/ssp', SSP, '/bio/CHELSA_bio', i, '_', phase, '_', tolower(model), '_ssp', SSP, '_V.2.1.tif')
      
      if (httr::http_error(url_bio)) {
        message("There appears to be a problem reaching the website.")
        return(invisible(NULL))
      }
      
      bio_file <- file.path(destination, 'data_raw', 'future_chelsa', paste('climat', phase, model, 'ssp', SSP, sep = '_'), 'temp', paste0('bio', i, '.tif'))
      gdal_utils_translate(ifile=paste0("/vsicurl/", url_bio),
                           ofile=bio_file,
                           extent_gdal_translate)
      progress_bar <- progress_bar + 1
      setTxtProgressBar(pb, progress_bar)
    }
  }
  close(pb)

  cat("Reprojecting rasters and stacking monthly rasters per variable\n")
  for(var in c("tasmin", "tasmax", "tas", "pr", "bio")) {
    for (model in GCM){
      files.tif <- list.files(file.path(destination, "data_raw", "future_chelsa",
                                        paste('climat', phase, model, 'ssp', SSP, sep = '_'), "temp"),
                              pattern = paste0(var, "[0-9]"), full.names = TRUE)
      for(i in 1:length(files.tif)) {
        sourcefile <- files.tif[i]
        destfile <- gsub(".tif", "_res.tif", files.tif[i])
        opts <- glue("-overwrite -s_srs {proj_s} -t_srs {proj_t} -r bilinear -tr {resol} {resol} ", 
                     "-te {extent_proj_string} -ot Int16 -of GTiff -srcnodata 0 -dstnodata {nodat}")
        sf::gdal_utils(util = "warp", source = sourcefile, destination = destfile,
                       options = unlist(strsplit(opts, " ")), quiet = TRUE)

        # Remove the following if block as Chelsa use offset and scale=0.1 in raster metadata to avoid the problem
        ## if (var %in% c("tasmin", "tasmax", "tas") | (var == "bio" & i <= 11)) {
        ##   # stock °C as integer to reduce size
        ##   # °C * 10 to keep information
        ##   change_scale <- round(stars::read_stars(destfile) * 10)
        ##   stars::write_stars(obj = change_scale, options = c("COMPRESS=LZW","PREDICTOR=2"), NA_value = nodat,
        ##                      type = "Int16", dsn = destfile)
        ##   rm(change_scale)
        ## }
      }
      files.tif <- list.files(file.path(destination, "data_raw", "future_chelsa", paste('climat', phase, model, 'ssp', SSP, sep = '_'), "temp"),
                              pattern = paste0(var, "[0-9]"), full.names = TRUE)
      files.tif <- files.tif[grep("[[:digit:]]_res", files.tif)] # remove original file but not delete it
      r <- terra::rast(sort(files.tif))
      names(r) <-  paste0(var, 1:length(files.tif))
      terra::writeRaster(x = r, gdal = c("COMPRESS=LZW","PREDICTOR=2"),
                         datatype = "INT2S", overwrite = TRUE,
                         filename = file.path(destination, "data_raw","future_chelsa",
                                              paste('climat', phase, model, 'ssp', SSP, sep = '_'),
                                              paste0(var,"_res.tif")))
    }
  }
  rm(r)

  cat("Create raster stack of monthly variables and bioclimatic variables\n")
  for (model in GCM){
    # Stack Tasmin, Tasmax, Tas, Pr & bio
    files.tif <- file.path(destination, "data_raw", "future_chelsa", paste('climat', phase, model, 'ssp', SSP, sep = '_'),
                           paste0(c("tasmin","tasmax","tas","pr", "bio"), "_res.tif"))
    
    r <- terra::rast(files.tif)
    terra::writeRaster(x = r, overwrite = TRUE, 
                       filename = file.path(destination, "data_raw","future_chelsa", 
                                            paste('climat', phase, model, 'ssp', SSP, sep = '_'), "clim_res.tif"),
                       gdal = c("COMPRESS=LZW","PREDICTOR=2"), datatype = "Int16")
     rm(r)
  }

  ## PET with Thornthwaite formula
  cat("Compute water deficit and number of dry months (cwd and ndm) with Thornthwaite ETP\n")
  for (model in GCM){
    tas <- terra::rast(file.path(destination, "data_raw", "future_chelsa",
                                 paste('climat', phase, model, 'ssp', SSP, sep = '_'), "tas_res.tif"))
    # Keep only latitude coordinates
    lat <- seq(extent_latlon[4], extent_latlon[2], length.out = dim(tas)[2])
    lat <- rep(lat, each = dim(tas)[1])
    tas_matrix <- NULL
    for (month in 1:12) {
      tas_matrix <- cbind(tas_matrix, terra::values(tas)[,month]/10)
    }
    I <- (tas_matrix / 5)^1.514
    alpha <- (6.75e-7) * I^3 - (7.71e-5) * I^2 + (1.792e-2) * I + 0.49239
    L <- NULL
    month_length <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    # Mid-month julian day: 15 of each month for a regular year
    mid_month_jday <- c(15, 46, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349)
    for (i in 1:12){
      day_lengths <- daylength(lat = lat, long = 0, jd = mid_month_jday[i], tmz = 0)[, 3]
      L <- cbind(L, day_lengths)
    }
    PET_Thornthwaite <- 16 * (L / 12) * (10 * tas_matrix / I)^alpha
    pet <- tas
    for (i in 1:12){
      terra::values(pet)[,i] <- PET_Thornthwaite[, i] * (month_length[i] / 30)
    }
    rm(PET_Thornthwaite, tas_matrix, alpha, I, L, lat)
    names(pet) <- paste0("pet_thornthwaite", 1:12)
    terra::writeRaster(x = pet, overwrite=TRUE,
                       filename = file.path(destination, "data_raw", "future_chelsa",
                                           paste('climat', phase, model, 'ssp', SSP, sep = '_'), "pet_thornthwaite_res.tif"),
                       gdal = c("COMPRESS=LZW","PREDICTOR=2"), datatype = "INT2S")

    ## CWD with Thornthwaite PET

    pr <- terra::rast(file.path(destination, "data_raw", "future_chelsa",
                                paste('climat', phase, model, 'ssp', SSP, sep = '_'), "pr_res.tif"))
    cwd_thornthwaite <- pet - pr
    terra::values(cwd_thornthwaite) <- pmax(terra::values(cwd_thornthwaite), 0.0)
    names(cwd_thornthwaite) <- paste0("cwd_thornthwaite", 1:12)
    cwd_annual <- pet[[1]]
    rm(pet)
    terra::values(cwd_annual) <- rowSums(terra::values(cwd_thornthwaite))
    names(cwd_annual) <- "cwd_thornthwaite"

    terra::writeRaster(x = cwd_annual, overwrite=TRUE,
                       filename = file.path(destination, "data_raw", "future_chelsa",
                                           paste('climat', phase, model, 'ssp', SSP, sep = '_'), "cwd_thornthwaite_res.tif"),
                       gdal = c("COMPRESS=LZW","PREDICTOR=2"), datatype = "INT2S")
    rm(cwd_annual)

    ## NDM with Thornthwaite

    ndm <- sum(cwd_thornthwaite > 0)
    rm(cwd_thornthwaite)
    names(ndm) <- "ndm_thornthwaite"
    terra::writeRaster(x = ndm,
                       filename = file.path(destination, "data_raw", "future_chelsa",
                                            paste('climat', phase, model, 'ssp', SSP, sep = '_'),
                                            "ndm_thornthwaite_res.tif"),
                       gdal = c("COMPRESS=LZW","PREDICTOR=2"), datatype = "INT2S")
    rm(ndm)
  }

  cat("Creating final raster stack\n")
  for (model in GCM){
    clim_file <- file.path(destination, "data_raw", "future_chelsa",
                           paste("climat", phase, model, "ssp", SSP, sep = "_"), "clim_res.tif")
    pet_t_file <- file.path(destination, "data_raw", "future_chelsa",
                            paste("climat", phase, model, "ssp", SSP, sep = "_"), "pet_thornthwaite_res.tif")
    cwd_t_file <- file.path(destination, "data_raw", "future_chelsa",
                            paste("climat", phase, model, "ssp", SSP, sep = "_"), "cwd_thornthwaite_res.tif")
    ndm_t_file <- file.path(destination, "data_raw", "future_chelsa",
                            paste("climat", phase, model, "ssp", SSP, sep = "_"), "ndm_thornthwaite_res.tif")
    destfile <- file.path(destination, "data_raw", "future_chelsa",
                          paste("climat", phase, model, "ssp", SSP, sep = "_"), "future_chelsa_no_name.tif")
  
    # system(glue('gdal_merge.py -o {destfile} -of GTiff -ot Int16 -co "COMPRESS=LZW" \\
    #             -co "PREDICTOR=2" -separate -a_nodata {nodat} \\
    #             {clim_file} {pet_t_file} {cwd_t_file} {ndm_t_file}'), ignore.stdout = TRUE, ignore.stderr = TRUE)

    future <- c(terra::rast(clim_file), terra::rast(pet_t_file), terra::rast(cwd_t_file), terra::rast(ndm_t_file))

    # future <- terra::rast(file.path(destination, "data_raw", "future_chelsa", paste("climat", phase, model, "ssp", SSP, sep = "_"),
    #                                 "future_chelsa_no_name.tif"))
    
    names(future) <- c(names(terra::rast(file.path(destination, "data_raw", "future_chelsa",
                                                   paste("climat", phase, model, "ssp", SSP, sep = "_"),
                                                   "clim_res.tif"))), paste0("pet_thornthwaite_", 1:12), "cwd_thornthwaite", "ndm_thornthwaite")
    terra::writeRaster(future, filename = file.path(destination, "data_raw", "future_chelsa",
                                                    paste("climat", phase, model, "ssp", SSP, sep = "_"),
                                                    paste0(paste("climat", phase, model, "ssp", SSP, sep = "_"),".tif")),
                       overwrite = TRUE, datatype = "INT2S", gdal = c("COMPRESS=LZW","PREDICTOR=2"), progress = 0)

    rm(future)
  }
  if (rm_download) {
    cat("Removing intermediate data\n")
    for (model in GCM){

      unlink(file.path(destination, "data_raw", "future_chelsa",
                       paste("climat", phase, model, "ssp", SSP, sep = "_"),
                       "temp"), recursive = TRUE)
      unlink(list.files(file.path(destination, "data_raw", "future_chelsa",
                                  paste("climat", phase, model, "ssp", SSP, sep = "_")),
                        pattern = "res", full.names = TRUE))
      unlink(file.path(destination, "data_raw", "future_chelsa",
                       paste("climat", phase, model, "ssp", SSP, sep = "_"),
                       "future_chelsa_no_name.tif"))
    }
  }

  model_files <- file.path(destination, "data_raw", "future_chelsa", paste("climat", phase, GCM, "ssp", SSP, sep = "_"),
                           paste0(paste("climat", phase, GCM, "ssp", SSP, sep = "_"),".tif"))
  ## Mean of the five models
  # Average_model <- rast(model_files[1]) + rast(model_files[2]) + rast(model_files[3]) + rast(model_files[4]) + rast(model_files[5])
  # Average_model <- round(Average_model / 5)
  # terra::writeRaster(Average_model, filename = file.path(destination, "data_raw", "future_chelsa", paste("climat", phase, "average", "ssp", SSP, sep = "_"),
  #                                                        paste0(paste("climat", phase, "average", "ssp", SSP, sep = "_"), ".tif")),
  #                    datatype = "INT2S", gdal = c("COMPRESS=LZW","PREDICTOR=2"), progress = 0)

  return(model_files)
}

# End
