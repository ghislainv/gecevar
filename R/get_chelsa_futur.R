get_chelsa_futur <- function(extent, EPSG, destination, resolution = 1000, phase = "2071-2100", ssp = 585){
  #' Create several multilayer Tiff files with 81 variables from chelsa-climate.org with futur climat variables.
  #'
  #' @description
  #' Gives predictions on futur values for the choosen phase. Prediction are from multiple models (GFDL-ESM4, IPSL-CM6A-LR, MPI-ESM1-2-HR, MRI-ESM2-0, UKESM1-0-LL).
  #' Creates folder for each model and a folder with mean of this five models.
  #' Monthly variables are average temperatures, min temperatures, max temperatures, precipitation, potential evapotranspiration with Thornthwaite formula.
  #' Others variables are climatic water deficit with Thornthwaite, number of dry month with Thornthwaite and 19 bio variables (more information in chelsa documentation).
  #'
  #' @param extent character. First output of `transform_shp_coutry_extend`
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
  #' @import utils
  #' @import rgdal
  #' @import glue
  #' @import geosphere
  #' @import terra
  #' @import sp
  #' @export



  dir.create(path = destination, recursive = TRUE, showWarnings = FALSE)
  nodat <- -9999
  proj.s <- "EPSG:4326"
  proj.t <- paste0("EPSG:", EPSG)
  dir.create(paste(destination, "data_raw", sep = "/"), showWarnings = FALSE)
  dir.create(paste(destination, "data_raw","chelsa_v2_1", sep = "/"), showWarnings = FALSE) ## folder for climatic data
  dir.create(paste(destination, "data_raw","chelsa_v2_1", paste("climat", phase, "GFDL-ESM4", "ssp", ssp, sep = "_"), "temp", sep = "/"), showWarnings = FALSE, recursive = TRUE)
  dir.create(paste(destination, "data_raw","chelsa_v2_1", paste("climat", phase, "IPSL-CM6A-LR", "ssp", ssp, sep = "_"), "temp", sep = "/"), showWarnings = FALSE, recursive = TRUE)
  dir.create(paste(destination, "data_raw","chelsa_v2_1", paste("climat", phase, "MPI-ESM1-2-HR", "ssp", ssp, sep = "_"), "temp", sep = "/"), showWarnings = FALSE, recursive = TRUE)
  dir.create(paste(destination, "data_raw","chelsa_v2_1", paste("climat", phase, "MRI-ESM2-0", "ssp", ssp, sep = "_"), "temp", sep = "/"), showWarnings = FALSE, recursive = TRUE)
  dir.create(paste(destination, "data_raw","chelsa_v2_1", paste("climat", phase, "UKESM1-0-LL", "ssp", ssp, sep = "_"), "temp", sep = "/"), showWarnings = FALSE, recursive = TRUE)
  dir.create(paste(destination, "data_raw","chelsa_v2_1", paste("climat", phase, "average", "ssp", ssp, sep = "_"),  sep = "/"), showWarnings = FALSE, recursive = TRUE)
  progress_bar <- 0
  nb_var_download <- (12 * 5 + 19) * 5
  print("Download in progress")
  pb = txtProgressBar(min = 0, max = nb_var_download, initial = 0)
  for (model in c("GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL")){
    for(m in str_pad(1:12, width = 2, pad = "0")){

      ## Monthly minimum temperature (°C).
      system(glue("gdal_translate -projwin {extent_latlon[1]} {extent_latlon[4]} {extent_latlon[3]} {extent_latlon[2]} -projwin_srs EPSG:4326 \\
                /vsicurl/{paste0('https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/', phase, '/', model, '/ssp', ssp, '/tasmin/CHELSA_', tolower(model), '_r1i1p1f1_w5e5_ssp', ssp, '_tasmin_', m, '_', str_replace(phase, pattern = '-', '_'), '_norm.tif')} \\
                {paste(destination, 'data_raw', 'chelsa_v2_1', paste('climat', phase, model, 'ssp', ssp, sep = '_'), 'temp', paste0('tasmin', m, '.tif'), sep = '/')}"), ignore.stdout = TRUE, ignore.stderr = TRUE)
      progress_bar <- progress_bar + 1
      setTxtProgressBar(pb, progress_bar)

      ## Monthly maximum temperature (°C).
      system(glue("gdal_translate -projwin {extent_latlon[1]} {extent_latlon[4]} {extent_latlon[3]} {extent_latlon[2]} -projwin_srs EPSG:4326 \\
                /vsicurl/{paste0('https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/', phase, '/', model, '/ssp', ssp, '/tasmax/CHELSA_', tolower(model), '_r1i1p1f1_w5e5_ssp', ssp, '_tasmax_', m, '_', str_replace(phase, pattern = '-', '_'), '_norm.tif')} \\
                {paste(destination, 'data_raw', 'chelsa_v2_1', paste('climat', phase, model, 'ssp', ssp, sep = '_'), 'temp', paste0('tasmax', m, '.tif'), sep = '/')}"), ignore.stdout = TRUE, ignore.stderr = TRUE)
      progress_bar <- progress_bar + 1
      setTxtProgressBar(pb, progress_bar)

      ## Monthly average temperature (°C).
      system(glue("gdal_translate -projwin {extent_latlon[1]} {extent_latlon[4]} {extent_latlon[3]} {extent_latlon[2]} -projwin_srs EPSG:4326 \\
                /vsicurl/{paste0('https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/', phase, '/', model, '/ssp', ssp, '/tas/CHELSA_', tolower(model), '_r1i1p1f1_w5e5_ssp', ssp, '_tas_', m, '_', str_replace(phase, pattern = '-', '_'), '_norm.tif')} \\
                {paste(destination, 'data_raw', 'chelsa_v2_1', paste('climat', phase, model, 'ssp', ssp, sep = '_'), 'temp', paste0('tas', m, '.tif'), sep = '/')}"), ignore.stdout = TRUE, ignore.stderr = TRUE)
      progress_bar <- progress_bar + 1
      setTxtProgressBar(pb, progress_bar)

      ## Monthly precipitation (mm ~ kg/m2).
      system(glue("gdal_translate -projwin {extent_latlon[1]} {extent_latlon[4]} {extent_latlon[3]} {extent_latlon[2]} -projwin_srs EPSG:4326 \\
                /vsicurl/{paste0('https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/', phase, '/', model, '/ssp', ssp, '/pr/CHELSA_', tolower(model), '_r1i1p1f1_w5e5_ssp', ssp, '_pr_', m, '_', str_replace(phase, pattern = '-', '_'), '_norm.tif')} \\
                {paste(destination, 'data_raw', 'chelsa_v2_1', paste('climat', phase, model, 'ssp', ssp, sep = '_'), 'temp', paste0('pr', m, '.tif'), sep = '/')}"), ignore.stdout = TRUE, ignore.stderr = TRUE)
      progress_bar <- progress_bar + 1
      setTxtProgressBar(pb, progress_bar)
    }

    for(i in 1:19){

      # 19 Bioclimatic variables
      # See https://chelsa-climate.org/wp-admin/download-page/CHELSA_tech_specification_V2.pdf for details
      system(glue("gdal_translate -projwin {extent_latlon[1]} {extent_latlon[4]} {extent_latlon[3]} {extent_latlon[2]} -projwin_srs EPSG:4326 \\
                /vsicurl/{paste0('https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/', phase, '/', model, '/ssp', ssp, '/bio/CHELSA_bio', i, '_', phase, '_', tolower(model), '_ssp', ssp, '_V.2.1.tif')} \\
                {paste(destination, 'data_raw', 'chelsa_v2_1', paste('climat', phase, model, 'ssp', ssp, sep = '_'), 'temp', paste0('bio', i, '.tif'), sep = '/')}"), ignore.stdout = TRUE, ignore.stderr = TRUE)
      progress_bar <- progress_bar + 1
      setTxtProgressBar(pb, progress_bar)
    }
    close(pb)
  }

  for (model in c("GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL")){
    for(var in c("tasmin", "tasmax", "tas", "pr", "bio"))
    {
      files.tif <- list.files(paste(destination, "data_raw", "chelsa_v2_1", paste('climat', phase, model, 'ssp', ssp, sep = '_'), "temp", sep = "/"), pattern = paste0(var, "[0-9]"), full.names = TRUE)
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
      files.tif <- list.files(paste(destination, "data_raw", "chelsa_v2_1", paste('climat', phase, model, 'ssp', ssp, sep = '_'), "temp", sep = "/"), pattern = paste0(var, "[0-9]"), full.names = TRUE)
      files.tif <- files.tif[grep("[[:digit:]]_res", files.tif)] # remove original file but not delete it
      r <- read_stars(sort(files.tif), along = "band")
      r <- split(r)
      names(r) <- c(paste0(var, 1:length(names(r))))
      r <- merge(r)
      write_stars(obj = r, options = c("COMPRESS=LZW","PREDICTOR=2"),
                  type = "Int16", dsn = paste(destination, "data_raw","chelsa_v2_1", paste('climat', phase, model, 'ssp', ssp, sep = '_'), paste0(var,"_res.tif"), sep = "/"))
    }

    # Stack Tasmin, Tasmax, Tas, Pr & bio
    files.tif <- paste(destination, "data_raw", "chelsa_v2_1", paste('climat', phase, model, 'ssp', ssp, sep = '_'), paste0(c("tasmin","tasmax","tas","pr", "bio"), "_res.tif"), sep = "/")
    r <- c(read_stars(files.tif[1]), read_stars(files.tif[2]), read_stars(files.tif[3]), read_stars(files.tif[4]),
           read_stars(files.tif[5]), along = "band")
    write_stars(obj = r, options = c("COMPRESS=LZW","PREDICTOR=2"), NA_value = nodat,
                type = "Int16", dsn = paste(destination, "data_raw","chelsa_v2_1", paste('climat', phase, model, 'ssp', ssp, sep = '_'), "clim_res.tif", sep = "/"))
    rm(r)

    ## PET with Thornthwaite formula

    tas <- read_stars(paste(destination, "data_raw", "chelsa_v2_1", paste('climat', phase, model, 'ssp', ssp, sep = '_'), "tas_res.tif", sep = "/"))
    # keep only latitude coordinates
    lat <- sp::coordinates(spTransform(as_Spatial(st_as_sf(tas)), CRS("+proj=longlat +datum=WGS84")))
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
      L <- cbind(L, daylength(lat[, 2], doy = mid_month_day[i]))
    }
    PET_Thornthwaite <- 16 * (L / 12) * (10 * tas_matrix / I)^alpha
    pet_stars <- tas
    for (i in 1:12){
      pet_stars[[1]][,, i][!is.na(pet_stars[[1]][,, i])] <- PET_Thornthwaite[, i] * (month_length[i] / 30)
    }
    rm(PET_Thornthwaite, tas_matrix)
    pet_stars <- split(pet_stars)
    names(pet_stars) <- paste0("pet_thornthwaite", 1:12)
    write_stars(merge(pet_stars) , dsn = paste(destination, "data_raw", "chelsa_v2_1", paste('climat', phase, model, 'ssp', ssp, sep = '_'), "pet_thornthwaite_res.tif", sep = "/"),
                options = c("COMPRESS=LZW","PREDICTOR=2"))

    ## CWD with Thornthwaite PET

    pr <- read_stars(paste(destination, "data_raw", "chelsa_v2_1", paste('climat', phase, model, 'ssp', ssp, sep = '_'), "pr_res.tif", sep = "/"))
    cwd_thornthwaite <- merge(pet_stars) - pr
    rm(pet_stars)
    cwd_thornthwaite[[1]] <- pmax(cwd_thornthwaite[[1]], 0)
    cwd_thornthwaite <- split(cwd_thornthwaite)
    names(cwd_thornthwaite) <- "cwd_thornthwaite"
    cwd_annual <- split(tas)[1,,]
    cwd_annual[[1]] <- rowSums(merge(cwd_thornthwaite)[[1]], dims = 2)
    names(cwd_annual) <- "cwd_thornthwaite"

    write_stars(cwd_annual, dsn = paste(destination, "data_raw", "chelsa_v2_1", paste('climat', phase, model, 'ssp', ssp, sep = '_'), "cwd_thornthwaite_res.tif", sep = "/"),
                options = c("COMPRESS=LZW","PREDICTOR=2"))
    rm(cwd_annual)

    ## NDM with Thornthwaite

    ndm_stars <- split(tas)[1,,]
    ndm_stars[[1]] <- rowSums(merge(cwd_thornthwaite)[[1]] > 0, dims = 2)
    rm(cwd_thornthwaite)
    names(ndm_stars) <- "ndm_thornthwaite"
    write_stars(ndm_stars , dsn = paste(destination, "data_raw", "chelsa_v2_1", paste('climat', phase, model, 'ssp', ssp, sep = '_'), "ndm_thornthwaite_res.tif", sep = "/"),
                options = c("COMPRESS=LZW","PREDICTOR=2"))
    rm(ndm_stars)

    system(glue('gdal_merge.py -o {paste(destination, "data_raw", "chelsa_v2_1", paste("climat", phase, model, "ssp", ssp, sep = "_"), "futur_chelsa.tif", sep = "/")} -of GTiff -ot Int16 -co "COMPRESS=LZW" \\
            -co "PREDICTOR=2" -separate -a_nodata {nodat} {paste(destination, "data_raw", "chelsa_v2_1", paste("climat", phase, model, "ssp", ssp, sep = "_"), "clim_res.tif", sep = "/")} \\
            {paste(destination, "data_raw", "chelsa_v2_1", paste("climat", phase, model, "ssp", ssp, sep = "_"), "pet_thornthwaite_res.tif", sep = "/")} {paste(destination, "data_raw", "chelsa_v2_1", paste("climat", phase, model, "ssp", ssp, sep = "_"), "cwd_thornthwaite_res.tif", sep = "/")} \\
            {paste(destination, "data_raw", "chelsa_v2_1", paste("climat", phase, model, "ssp", ssp, sep = "_"), "ndm_thornthwaite_res.tif", sep = "/")}'), ignore.stdout = TRUE, ignore.stderr = TRUE)

    futur <- split(read_stars(paste(destination, "data_raw", "chelsa_v2_1", paste("climat", phase, model, "ssp", ssp, sep = "_"),  "futur_chelsa.tif", sep = "/")))
    names(futur) <- c(names(split(read_stars(paste(destination, "data_raw", "chelsa_v2_1", paste("climat", phase, model, "ssp", ssp, sep = "_"), "clim_res.tif", sep = "/")))), paste0("pet_thornthwaite_", 1:12), "cwd_thornthwaite", "ndm_thornthwaite")
    write_stars(obj = merge(futur), options = c("COMPRESS=LZW","PREDICTOR=2"), type = "Int16",
                NA_value = nodat, dsn = paste(destination, "data_raw", "chelsa_v2_1", paste("climat", phase, model, "ssp", ssp, sep = "_"), paste0(paste("climat", phase, model, "ssp", ssp, sep = "_"),".tif"), sep = "/"))
    rm(futur)
    unlink(paste(destination, "data_raw", "chelsa_v2_1", paste("climat", phase, model, "ssp", ssp, sep = "_"), "temp", sep = "/"), recursive = TRUE)
    unlink(list.files(paste(destination, "data_raw", "chelsa_v2_1", paste("climat", phase, model, "ssp", ssp, sep = "_"), sep = "/"), pattern = "res", full.names = TRUE))
    unlink(paste(destination, "data_raw", "chelsa_v2_1", paste("climat", phase, model, "ssp", ssp, sep = "_"), "futur_chelsa.tif", sep = "/"))
  }
  ## Mean of the five models
  Average_model <- read_stars(paste(destination, "data_raw", "chelsa_v2_1", paste("climat", phase, "GFDL-ESM4", "ssp", ssp, sep = "_"), paste0(paste("climat", phase, "GFDL-ESM4", "ssp", ssp, sep = "_"),".tif"), sep = "/"))
  for (model in c("IPSL-CM6A-LR", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL")){
    Average_model <- Average_model + read_stars(paste(destination, "data_raw", "chelsa_v2_1", paste("climat", phase, model, "ssp", ssp, sep = "_"), paste0(paste("climat", phase, model, "ssp", ssp, sep = "_"),".tif"), sep = "/"))
  }
  Average_model <- Average_model / 5
  write_stars(obj = Average_model, dsn = paste(destination, "data_raw", "chelsa_v2_1", paste("climat", phase, "average", "ssp", ssp, sep = "_"), paste0(paste("climat", phase, "average", "ssp", ssp, sep = "_"), ".tif"), sep = "/"),
              options = c("COMPRESS=LZW","PREDICTOR=2"))
}
