get_chelsa_future <- function(extent, extent_latlon, EPSG, destination, resolution = 1000, phase = "2071-2100", ssp = 585){
  #' Create several multilayer Tiff files with 81 variables from chelsa-climate.org with future climat variables.
  #'
  #' @description
  #' Gives predictions on future values for the choosen phase. Prediction are from multiple models (GFDL-ESM4, IPSL-CM6A-LR, MPI-ESM1-2-HR, MRI-ESM2-0, UKESM1-0-LL).
  #' Creates folder for each model and a folder with mean of this five models.
  #' Monthly variables are average temperatures, min temperatures, max temperatures, precipitation, potential evapotranspiration with Thornthwaite formula.
  #' Others variables are climatic water deficit with Thornthwaite, number of dry month with Thornthwaite and 19 bio variables (more information in chelsa documentation).
  #'
  #' @param extent character. First output of `transform_shp_coutry_extent`.
  #' @param extent_latlon character. Second output of `transfomr_shp_country_extent`
  #' @param EPSG int. to consider for this country/area.
  #' @param destination character. absolute path where to download files like `here()` output.
  #' @param resolution int. in meters, recommended resolution are 250m, 500m, 1km, 2km or 5km, default is 1km. See more in details.
  #' @param phase character. Must be in c("2041-2070", "2071-2100") match to years to download, default is "2071-2100".
  #' @param ssp character or int. Must be in c(126, 370, 585). It's the Shared Socio-economic Pathways you want to download.
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
  #' | PET Thornthwaite                      | kg.m^{-1}            |
  #' | Climatic Water Deficit (Thornthwaite) | kg.m^{-1}            |
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
  #' @import stars
  #' @import stringr
  #' @importFrom utils txtProgressBar setTxtProgressBar
  #' @importFrom glue glue
  #' @import geosphere
  #' @import terra
  #' @import sp
  #' @export



  dir.create(path = destination, recursive = TRUE, showWarnings = FALSE)
  nodat <- -9999
  proj.s <- "EPSG:4326"
  proj.t <- paste0("EPSG:", EPSG)
  dir.create(file.path(destination, "data_raw"), showWarnings = FALSE)
  dir.create(file.path(destination, "data_raw","future_chelsa"), showWarnings = FALSE) ## folder for climatic data
  dir.create(file.path(destination, "data_raw","future_chelsa", paste("climat", phase, "GFDL-ESM4", "ssp", ssp, sep = "_"), "temp"), showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(destination, "data_raw","future_chelsa", paste("climat", phase, "IPSL-CM6A-LR", "ssp", ssp, sep = "_"), "temp"), showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(destination, "data_raw","future_chelsa", paste("climat", phase, "MPI-ESM1-2-HR", "ssp", ssp, sep = "_"), "temp"), showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(destination, "data_raw","future_chelsa", paste("climat", phase, "MRI-ESM2-0", "ssp", ssp, sep = "_"), "temp"), showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(destination, "data_raw","future_chelsa", paste("climat", phase, "UKESM1-0-LL", "ssp", ssp, sep = "_"), "temp"), showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(destination, "data_raw","future_chelsa", paste("climat", phase, "average", "ssp", ssp, sep = "_"),  sep = "/"), showWarnings = FALSE, recursive = TRUE)
  progress_bar <- 0
  nb_var_download <- (12 * 5 + 19) * 5
  print("Download in progress")
  pb = txtProgressBar(min = 0, max = nb_var_download, initial = 0)
  for (model in c("GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL")){
    for(m in str_pad(1:12, width = 2, pad = "0")){

      ## Monthly minimum temperature (°C).
      url_tasmin <- paste0('https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/', phase, '/', model, '/ssp', ssp, '/tasmin/CHELSA_', tolower(model), '_r1i1p1f1_w5e5_ssp', ssp, '_tasmin_', m, '_', str_replace(phase, pattern = '-', '_'), '_norm.tif')
      tasmin_file <- file.path(destination, 'data_raw', 'future_chelsa', paste('climat', phase, model, 'ssp', ssp, sep = '_'), 'temp', paste0('tasmin', m, '.tif'), sep = '/')
      system(glue("gdal_translate -projwin {extent_latlon[1]} {extent_latlon[4]} {extent_latlon[3]} {extent_latlon[2]} -projwin_srs EPSG:4326 \\
                /vsicurl/{url_tasmin} {tasmin_file}"), ignore.stdout = TRUE, ignore.stderr = TRUE)
      progress_bar <- progress_bar + 1
      setTxtProgressBar(pb, progress_bar)

      ## Monthly maximum temperature (°C).
      url_tasmax <- paste0('https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/', phase, '/', model, '/ssp', ssp, '/tasmax/CHELSA_', tolower(model), '_r1i1p1f1_w5e5_ssp', ssp, '_tasmax_', m, '_', str_replace(phase, pattern = '-', '_'), '_norm.tif')
      tasmax_file <- file.path(destination, 'data_raw', 'future_chelsa', paste('climat', phase, model, 'ssp', ssp, sep = '_'), 'temp', paste0('tasmax', m, '.tif'), sep = '/')
      system(glue("gdal_translate -projwin {extent_latlon[1]} {extent_latlon[4]} {extent_latlon[3]} {extent_latlon[2]} -projwin_srs EPSG:4326 \\
                /vsicurl/{url_tasmax} {tasmax_file}"), ignore.stdout = TRUE, ignore.stderr = TRUE)
      progress_bar <- progress_bar + 1
      setTxtProgressBar(pb, progress_bar)

      ## Monthly average temperature (°C).
      url_tas <- paste0('https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/', phase, '/', model, '/ssp', ssp, '/tas/CHELSA_', tolower(model), '_r1i1p1f1_w5e5_ssp', ssp, '_tas_', m, '_', str_replace(phase, pattern = '-', '_'), '_norm.tif')
      tas_file <- file.path(destination, 'data_raw', 'future_chelsa', paste('climat', phase, model, 'ssp', ssp, sep = '_'), 'temp', paste0('tas', m, '.tif'), sep = '/')
      system(glue("gdal_translate -projwin {extent_latlon[1]} {extent_latlon[4]} {extent_latlon[3]} {extent_latlon[2]} -projwin_srs EPSG:4326 \\
                /vsicurl/{url_tas} {tas_file}"), ignore.stdout = TRUE, ignore.stderr = TRUE)
      progress_bar <- progress_bar + 1
      setTxtProgressBar(pb, progress_bar)

      ## Monthly precipitation (mm ~ kg/m2).
      url_pr <- paste0('https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/', phase, '/', model, '/ssp', ssp, '/pr/CHELSA_', tolower(model), '_r1i1p1f1_w5e5_ssp', ssp, '_pr_', m, '_', str_replace(phase, pattern = '-', '_'), '_norm.tif')
      pr_file <- file.path(destination, 'data_raw', 'future_chelsa', paste('climat', phase, model, 'ssp', ssp, sep = '_'), 'temp', paste0('pr', m, '.tif'), sep = '/')
      system(glue("gdal_translate -projwin {extent_latlon[1]} {extent_latlon[4]} {extent_latlon[3]} {extent_latlon[2]} -projwin_srs EPSG:4326 \\
                /vsicurl/{url_pr} {pr_file}"), ignore.stdout = TRUE, ignore.stderr = TRUE)
      progress_bar <- progress_bar + 1
      setTxtProgressBar(pb, progress_bar)
    }

    for(i in 1:19){

      # 19 Bioclimatic variables
      # See https://chelsa-climate.org/wp-admin/download-page/CHELSA_tech_specification_V2.pdf for details
      url_bio <- paste0('https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/', phase, '/', model, '/ssp', ssp, '/bio/CHELSA_bio', i, '_', phase, '_', tolower(model), '_ssp', ssp, '_V.2.1.tif')
      bio_file <- file.path(destination, 'data_raw', 'future_chelsa', paste('climat', phase, model, 'ssp', ssp, sep = '_'), 'temp', paste0('bio', i, '.tif'), sep = '/')
      system(glue("gdal_translate -projwin {extent_latlon[1]} {extent_latlon[4]} {extent_latlon[3]} {extent_latlon[2]} -projwin_srs EPSG:4326 \\
                /vsicurl/{url_bio} {bio_file}"), ignore.stdout = TRUE, ignore.stderr = TRUE)
      progress_bar <- progress_bar + 1
      setTxtProgressBar(pb, progress_bar)
    }
    close(pb)
  }

  for (model in c("GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL")){
    for(var in c("tasmin", "tasmax", "tas", "pr", "bio"))
    {
      files.tif <- list.files(file.path(destination, "data_raw", "future_chelsa", paste('climat', phase, model, 'ssp', ssp, sep = '_'), "temp"), pattern = paste0(var, "[0-9]"), full.names = TRUE)
      for(i in 1:length(files.tif))
      {
        sourcefile <- files.tif[i]
        destfile <- gsub(".tif", "_res.tif", files.tif[i])
        system(glue("gdalwarp -overwrite -s_srs {proj.s} -t_srs {proj.t} \\
        -r bilinear -tr {resolution} {resolution} -te {extent} -ot Int16 -of GTiff -srcnodata 0 -dstnodata {nodat} \\
        {sourcefile} {destfile}"), ignore.stdout = TRUE, ignore.stderr = TRUE)
        if (var %in% c("tasmin", "tasmax", "tas") | (var == "bio" & i <= 11))
        {
          # stock °C as integer to reduce size
          # °C * 10 to keep information
          change_scale <- round(read_stars(destfile) * 10)
          write_stars(obj = change_scale, options = c("COMPRESS=LZW","PREDICTOR=2"), NA_value = nodat,
                      type = "Int16", dsn = destfile)
          rm(change_scale)
        }
      }
      files.tif <- list.files(file.path(destination, "data_raw", "future_chelsa", paste('climat', phase, model, 'ssp', ssp, sep = '_'), "temp"), pattern = paste0(var, "[0-9]"), full.names = TRUE)
      files.tif <- files.tif[grep("[[:digit:]]_res", files.tif)] # remove original file but not delete it
      r <- read_stars(sort(files.tif), along = "band")
      r <- stats::setNames(r, paste0(var, 1:length(files.tif)))
      write_stars(obj = r, options = c("COMPRESS=LZW","PREDICTOR=2"),
                  type = "Int16", dsn = file.path(destination, "data_raw","future_chelsa", paste('climat', phase, model, 'ssp', ssp, sep = '_'), paste0(var,"_res.tif")))
    }

    # Stack Tasmin, Tasmax, Tas, Pr & bio
    files.tif <- file.path(destination, "data_raw", "future_chelsa", paste('climat', phase, model, 'ssp', ssp, sep = '_'), paste0(c("tasmin","tasmax","tas","pr", "bio"), "_res.tif"))
    r <- c(read_stars(files.tif[1]), read_stars(files.tif[2]), read_stars(files.tif[3]), read_stars(files.tif[4]),
           read_stars(files.tif[5]), along = "band")
    write_stars(obj = r, options = c("COMPRESS=LZW","PREDICTOR=2"), NA_value = nodat,
                type = "Int16", dsn = file.path(destination, "data_raw","future_chelsa", paste('climat', phase, model, 'ssp', ssp, sep = '_'), "clim_res.tif"))
    rm(r)

    ## PET with Thornthwaite formula

    tas <- read_stars(file.path(destination, "data_raw", "future_chelsa", paste('climat', phase, model, 'ssp', ssp, sep = '_'), "tas_res.tif"))
    # keep only latitude coordinates
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
    rm(PET_Thornthwaite, tas_matrix, alpha, I, L, lat)
    pet_stars <- stats::setNames(pet_stars, paste0("pet_thornthwaite", 1:12))
    write_stars(pet_stars, dsn = file.path(destination, "data_raw", "future_chelsa", paste('climat', phase, model, 'ssp', ssp, sep = '_'), "pet_thornthwaite_res.tif"),
                options = c("COMPRESS=LZW","PREDICTOR=2"), type = "Int16")

    ## CWD with Thornthwaite PET

    pr <- read_stars(file.path(destination, "data_raw", "future_chelsa", paste('climat', phase, model, 'ssp', ssp, sep = '_'), "pr_res.tif"))
    cwd_thornthwaite <- merge(pet_stars) - pr
    rm(pet_stars)
    cwd_thornthwaite[[1]] <- pmax(cwd_thornthwaite[[1]], 0)
    cwd_thornthwaite <- split(cwd_thornthwaite)
    names(cwd_thornthwaite) <- "cwd_thornthwaite"
    cwd_annual <- split(tas)[1,,]
    cwd_annual[[1]] <- rowSums(merge(cwd_thornthwaite)[[1]], dims = 2)
    names(cwd_annual) <- "cwd_thornthwaite"

    write_stars(cwd_annual, dsn = file.path(destination, "data_raw", "future_chelsa", paste('climat', phase, model, 'ssp', ssp, sep = '_'), "cwd_thornthwaite_res.tif"),
                options = c("COMPRESS=LZW","PREDICTOR=2"), type = "Int16")
    rm(cwd_annual)

    ## NDM with Thornthwaite

    ndm_stars <- split(tas)[1,,]
    ndm_stars[[1]] <- rowSums(merge(cwd_thornthwaite)[[1]] > 0, dims = 2)
    rm(cwd_thornthwaite)
    names(ndm_stars) <- "ndm_thornthwaite"
    write_stars(ndm_stars , dsn = file.path(destination, "data_raw", "future_chelsa", paste('climat', phase, model, 'ssp', ssp, sep = '_'), "ndm_thornthwaite_res.tif"),
                options = c("COMPRESS=LZW","PREDICTOR=2"), type = "Int16")
    rm(ndm_stars)


    clim_file <- file.path(destination, "data_raw", "future_chelsa", paste("climat", phase, model, "ssp", ssp, sep = "_"), "clim_res.tif")
    pet_t_file <- file.path(destination, "data_raw", "future_chelsa", paste("climat", phase, model, "ssp", ssp, sep = "_"), "pet_thornthwaite_res.tif")
    cwd_t_file <- file.path(destination, "data_raw", "future_chelsa", paste("climat", phase, model, "ssp", ssp, sep = "_"), "cwd_thornthwaite_res.tif")
    ndm_t_file <- file.path(destination, "data_raw", "future_chelsa", paste("climat", phase, model, "ssp", ssp, sep = "_"), "ndm_thornthwaite_res.tif")
    destfile <- file.path(destination, "data_raw", "future_chelsa", paste("climat", phase, model, "ssp", ssp, sep = "_"), "future_chelsa_no_name.tif")
    system(glue('gdal_merge.py -o {destfile} -of GTiff -ot Int16 -co "COMPRESS=LZW" \\
                -co "PREDICTOR=2" -separate -a_nodata {nodat} \\
                {clim_file} {pet_t_file} {cwd_t_file} {ndm_t_file}'), ignore.stdout = TRUE, ignore.stderr = TRUE)

    future <- rast(file.path(destination, "data_raw", "future_chelsa", paste("climat", phase, model, "ssp", ssp, sep = "_"),  "future_chelsa_no_name.tif"))
    names(future) <- c(names(rast(file.path(destination, "data_raw", "future_chelsa", paste("climat", phase, model, "ssp", ssp, sep = "_"), "clim_res.tif"))), paste0("pet_thornthwaite_", 1:12), "cwd_thornthwaite", "ndm_thornthwaite")
    writeRaster(future, filename = file.path(destination, "data_raw", "future_chelsa", paste("climat", phase, model, "ssp", ssp, sep = "_"), paste0(paste("climat", phase, model, "ssp", ssp, sep = "_"),".tif")),
                overwrite = TRUE, datatype = "INT2S", gdal = c("COMPRESS=LZW","PREDICTOR=2"), progress = 0)
    rm(future)
    unlink(file.path(destination, "data_raw", "future_chelsa", paste("climat", phase, model, "ssp", ssp, sep = "_"), "temp"), recursive = TRUE)
    unlink(list.files(file.path(destination, "data_raw", "future_chelsa", paste("climat", phase, model, "ssp", ssp, sep = "_")), pattern = "res", full.names = TRUE))
    unlink(file.path(destination, "data_raw", "future_chelsa", paste("climat", phase, model, "ssp", ssp, sep = "_"), "future_chelsa_no_name.tif"))
  }
  ## Mean of the five models

  model <- c("GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL")
  model_files <- file.path(destination, "data_raw", "future_chelsa", paste("climat", phase, model, "ssp", ssp, sep = "_"), paste0(paste("climat", phase, model, "ssp", ssp, sep = "_"),".tif"))
  Average_model <- rast(model_files[1]) + rast(model_files[2]) + rast(model_files[3]) + rast(model_files[4]) + rast(model_files[5])
  Average_model <- round(Average_model / 5)
  writeRaster(Average_model, filename = file.path(destination, "data_raw", "future_chelsa", paste("climat", phase, "average", "ssp", ssp, sep = "_"), paste0(paste("climat", phase, "average", "ssp", ssp, sep = "_"), ".tif")),
             datatype = "INT2S", gdal = c("COMPRESS=LZW","PREDICTOR=2"), progress = 0)
}
