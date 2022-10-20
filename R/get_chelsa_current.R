get_chelsa_current <- function(extent, EPSG, destination, resolution = 1000, rm_download = FALSE){
  #' Create multilayer Tiff file with 107 variables from chelsa-climate.org
  #'
  #' @description
  #' Gives the average values on the data recovered between 1981 and 2010.
  #' Monthly variables are average temperatures, min temperatures, max temperatures, precipitation, potential evapotranspiration (with Penman formula and with Thornthwaite formula),
  #' and total cloud cover.
  #' Others variables are climatic water deficit (with Penman and Thornthwaite), number of dry month (with Penman and Thornthwaite) and 19 bio variables (more information in chelsa documentation).
  #'
  #' @param extent character. First output of `transform_shp_coutry_extend`
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
  #' | Climatic Water Deficit (Penman)       | kg.m^{-1}            |
  #' | Climatic Water Deficit (Thornthwaite) | kg.m^{-1}            |
  #' | Number of dry month (Penman)          | month                |
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
  #' @import geosphere
  #' @import terra
  #' @import sp
  #' @importFrom glue glue
  #' @export

  dir.create(path = destination, recursive = TRUE, showWarnings = FALSE)
  nodat <- -9999
  proj.s <- "EPSG:4326"
  proj.t <- paste0("EPSG:", EPSG)
  dir.create(paste(destination, "data_raw", sep = "/"), showWarnings = FALSE)
  dir.create(paste(destination, "data_raw","chelsa_v2_1", sep = "/"), showWarnings = FALSE) ## folder for climatic data
  dir.create(paste(destination, "data_raw","chelsa_v2_1","temp", sep = "/"), showWarnings = FALSE) ## Temporary folder
  progress_bar <- 0
  nb_var_download <- 12 * 6
  print("Download in progress")
  pb = txtProgressBar(min = 0, max = nb_var_download, initial = 0)
  for(m in str_pad(1:12, width = 2, pad = "0")){
    ## Monthly minimum temperature (°C).
    system(glue("gdal_translate -projwin {extent_latlon[1]} {extent_latlon[4]} {extent_latlon[3]} {extent_latlon[2]} -projwin_srs EPSG:4326 \\
                /vsicurl/{paste0('https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/tasmin/CHELSA_tasmin_',m,'_1981-2010_V.2.1.tif')} \\
                {paste(destination, 'data_raw', 'chelsa_v2_1', 'temp', paste0('tasmin', m, '.tif'), sep = '/')}"), ignore.stdout = TRUE, ignore.stderr = TRUE)
    progress_bar <- progress_bar + 1
    setTxtProgressBar(pb, progress_bar)
    ## Monthly maximum temperature (°C).
    system(glue("gdal_translate -projwin {extent_latlon[1]} {extent_latlon[4]} {extent_latlon[3]} {extent_latlon[2]} -projwin_srs EPSG:4326 \\
                /vsicurl/{paste0('https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/tasmax/CHELSA_tasmax_', m, '_1981-2010_V.2.1.tif')} \\
                {paste(destination, 'data_raw', 'chelsa_v2_1', 'temp', paste0('tasmax', m, '.tif'), sep = '/')}"), ignore.stdout = TRUE, ignore.stderr = TRUE)
    progress_bar <- progress_bar + 1
    setTxtProgressBar(pb, progress_bar)
    ## Monthly average temperature (°C).
    system(glue("gdal_translate -projwin {extent_latlon[1]} {extent_latlon[4]} {extent_latlon[3]} {extent_latlon[2]} -projwin_srs EPSG:4326 \\
                /vsicurl/{paste0('https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/tas/CHELSA_tas_', m, '_1981-2010_V.2.1.tif')} \\
                {paste(destination, 'data_raw', 'chelsa_v2_1', 'temp', paste0('tas', m, '.tif'), sep = '/')}"), ignore.stdout = TRUE, ignore.stderr = TRUE)
    progress_bar <- progress_bar + 1
    setTxtProgressBar(pb, progress_bar)
    ## Monthly precipitation (mm ~ kg/m2).
    system(glue("gdal_translate -projwin {extent_latlon[1]} {extent_latlon[4]} {extent_latlon[3]} {extent_latlon[2]} -projwin_srs EPSG:4326 \\
                /vsicurl/{paste0('https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/pr/CHELSA_pr_', m, '_1981-2010_V.2.1.tif')} \\
                {paste(destination, 'data_raw', 'chelsa_v2_1', 'temp', paste0('pr', m, '.tif'), sep = '/')}"), ignore.stdout = TRUE, ignore.stderr = TRUE)
    progress_bar <- progress_bar + 1
    setTxtProgressBar(pb, progress_bar)
    ## Monthly cloud area fraction
    system(glue("gdal_translate -projwin {extent_latlon[1]} {extent_latlon[4]} {extent_latlon[3]} {extent_latlon[2]} -projwin_srs EPSG:4326 \\
                /vsicurl/{paste0('https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/clt/CHELSA_clt_', m, '_1981-2010_V.2.1.tif')} \\
                {paste(destination, 'data_raw', 'chelsa_v2_1', 'temp', paste0('clt', m, '.tif'), sep = '/')}"), ignore.stdout = TRUE, ignore.stderr = TRUE)
    progress_bar <- progress_bar + 1
    setTxtProgressBar(pb, progress_bar)
    ## Monthly Pet_ penman
    # https://www.fao.org/3/x0490e/x0490e06.htm
    system(glue("gdal_translate -projwin {extent_latlon[1]} {extent_latlon[4]} {extent_latlon[3]} {extent_latlon[2]} -projwin_srs EPSG:4326 \\
                /vsicurl/{paste0('https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/pet/CHELSA_pet_penman_', m, '_1981-2010_V.2.1.tif')} \\
                {paste(destination, 'data_raw', 'chelsa_v2_1', 'temp', paste0('pet_penman', m, '.tif'), sep = '/')}"), ignore.stdout = TRUE, ignore.stderr = TRUE)
    progress_bar <- progress_bar + 1
    setTxtProgressBar(pb, progress_bar)
  }
  close(pb)
  for(i in 1:19){
    # 19 Bioclimatic variables
    # See https://chelsa-climate.org/wp-admin/download-page/CHELSA_tech_specification_V2.pdf for details
    system(glue("gdal_translate -projwin {extent_latlon[1]} {extent_latlon[4]} {extent_latlon[3]} {extent_latlon[2]} -projwin_srs EPSG:4326 \\
                /vsicurl/{paste0('https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/bio/CHELSA_bio', i, '_1981-2010_V.2.1.tif')} \\
                {paste(destination, 'data_raw', 'chelsa_v2_1', 'temp', paste0('bio', str_pad(i, width = 2, pad = '0'), '.tif'), sep = '/')}"), ignore.stdout = TRUE, ignore.stderr = TRUE)
  }

  for(var in c("tasmin", "tasmax", "tas", "pr", "bio", "clt", "pet_penman"))
  {
    files.tif <- list.files(paste(destination, "data_raw", "chelsa_v2_1", "temp", sep = "/"), pattern = paste0(var, "[0-9]"), full.names = TRUE)
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
    files.tif <- list.files(paste(destination, "data_raw", "chelsa_v2_1", "temp", sep = "/"), pattern = paste0(var, "[0-9]"), full.names = TRUE)
    files.tif <- files.tif[grep("res", files.tif)]
    r <- read_stars(sort(files.tif), along = "band")
    r <- split(r)
    names(r) <- c(paste0(var, 1:length(names(r))))
    r <- merge(r)
    write_stars(obj = r, options = c("COMPRESS=LZW","PREDICTOR=2"),
                type = "Int16", dsn = paste(destination, "data_raw","chelsa_v2_1", paste0(var,"_res.tif"), sep = "/"))
  }

  # Stack Tasmin, Tasmax, Tas, Pr, Tcc, Pet Penman & bio
  files.tif <- paste(destination, "data_raw", "chelsa_v2_1", paste0(c("tasmin","tasmax","tas","pr", "clt", "pet_penman", "bio"), "_res.tif"), sep = "/")
  r <- c(read_stars(files.tif[1]), read_stars(files.tif[2]), read_stars(files.tif[3]), read_stars(files.tif[4]),
         read_stars(files.tif[5]), read_stars(files.tif[6]), read_stars(files.tif[7]), along = "band")
  write_stars(obj = r, options = c("COMPRESS=LZW","PREDICTOR=2"), NA_value = nodat,
              type = "Int16", dsn = paste(destination, "data_raw","chelsa_v2_1", "clim_res.tif", sep = "/"))
  # file.remove(files.tif)
  rm(r)

  ## CWD: climatic water deficit
  ## NDM: number of dry months
  pr_file <- paste(destination, "data_raw", "chelsa_v2_1","pr_res.tif", sep = "/")
  pet_penman_file <- paste(destination, "data_raw", "chelsa_v2_1","pet_penman_res.tif", sep = "/")

  for (i in 1:12)
  {
    # CWD = PET_PENMAN - Pr
    # CWD is a positive values for a lack of water
    system(glue('gdal_calc.py -A {pet_penman_file} --A_band={i} -B {pr_file} --B_band={i} --quiet --type=Int16 \\
               --creation-option="COMPRESS=LZW" --creation-option="PREDICTOR=2"  --calc="A-B" --NoDataValue={nodat} \\
               --outfile={paste(destination, "data_raw", "chelsa_v2_1", paste0("cwd", i, "_res.tif"), sep = "/")} --overwrite'), ignore.stdout = TRUE, ignore.stderr = TRUE)
    # Number of Dry Month ie sum(CWD > 0)
    cwd_file <- paste(destination, "data_raw", "chelsa_v2_1", paste0("cwd", i, "_res.tif"), sep = "/")
    ndm_file <- paste(destination, "data_raw", "chelsa_v2_1", "temp", paste0("ndm", i, "_res.tif"), sep = "/")
    system(glue('gdal_calc.py -A {cwd_file} --A_band={1} --quiet --type=Int16 \\
              --creation-option="COMPRESS=LZW" --creation-option="PREDICTOR=2" \\
              --outfile={ndm_file} --calc="A>0" --overwrite --NoDataValue={nodat}'), ignore.stdout = TRUE, ignore.stderr = TRUE)
  }

  ndm_files <- list.files(paste(destination, "data_raw", "chelsa_v2_1", "temp", sep = "/"), pattern = "ndm", full.names = TRUE)
  system(glue('gdal_calc.py -A {ndm_files[1]} -B {ndm_files[2]} -C {ndm_files[3]} -D {ndm_files[4]}  -E {ndm_files[5]} \\
            -F {ndm_files[6]} -G {ndm_files[7]} -H {ndm_files[8]} -I {ndm_files[9]} -J {ndm_files[10]} -K {ndm_files[11]} \\
            -L {ndm_files[12]} --quiet --type=Int16 --creation-option="COMPRESS=LZW" --creation-option="PREDICTOR=2" \\
            --outfile={paste(destination, "data_raw", "chelsa_v2_1", "ndm_res.tif", sep = "/")} --NoDataValue={nodat} \\
            --calc="A+B+C+D+E+F+G+H+I+J+K+L" --overwrite'), ignore.stdout = TRUE, ignore.stderr = TRUE)

  cwd_files <- list.files(paste(destination, "data_raw", "chelsa_v2_1", sep = "/"), pattern = "cwd", full.names = TRUE)
  system(glue('gdal_calc.py -A {cwd_files[1]} -B {cwd_files[2]} -C {cwd_files[3]} -D {cwd_files[4]} -E {cwd_files[5]} \\
            -F {cwd_files[6]} -G {cwd_files[7]} -H {cwd_files[8]} -I {cwd_files[9]} -J {cwd_files[10]} -K {cwd_files[11]} \\
            -L {cwd_files[12]} --quiet --type=Int16 --creation-option="COMPRESS=LZW" --creation-option="PREDICTOR=2" \\
            --outfile={paste(destination, "data_raw", "chelsa_v2_1", "cwd_res.tif", sep = "/")} --NoDataValue={nodat} \\
            --calc="numpy.maximum(A,0)+numpy.maximum(B,0)+numpy.maximum(C,0)+numpy.maximum(D,0)+numpy.maximum(E,0)+numpy.maximum(F,0) \\
            +numpy.maximum(G,0)+numpy.maximum(H,0)+numpy.maximum(I,0)+numpy.maximum(J,0)+numpy.maximum(K,0)+numpy.maximum(L,0)" --overwrite'), ignore.stdout = TRUE, ignore.stderr = TRUE)

  ## PET with Thornthwaite formula

  tas <- read_stars(paste(destination, "data_raw", "chelsa_v2_1", "tas_res.tif", sep = "/"))
  # keep only latitude coordinates
  lat <- sp::coordinates(spTransform(as_Spatial(st_as_sf(tas)), CRS("+proj=longlat +datum=WGS84")))
  tas_matrix <- NULL
  for (month in 1:12) {
    tas_matrix <- cbind(tas_matrix, c(tas[[1]][,, month] / 10))
  }
  # Sys.time()
  # pet_thornthwaite <- t(thornthwaite(Tave = t(tas_matrix / 10), t(lat)))
  # Sys.time()
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
  write_stars(merge(pet_stars) , dsn = paste(destination, "data_raw", "chelsa_v2_1", "pet_thornthwaite_res.tif", sep = "/"),
              options = c("COMPRESS=LZW","PREDICTOR=2"))

  ## CWD with Thornthwaite PET

  pr <- read_stars(paste(destination, "data_raw", "chelsa_v2_1", "pr_res.tif", sep = "/"))
  cwd_thornthwaite <- merge(pet_stars) - pr
  rm(pet_stars)
  cwd_thornthwaite[[1]] <- pmax(cwd_thornthwaite[[1]], 0)
  cwd_thornthwaite <- split(cwd_thornthwaite)
  names(cwd_thornthwaite) <- "cwd_thornthwaite"
  cwd_annual <- split(tas)[1,,]
  cwd_annual[[1]] <- rowSums(merge(cwd_thornthwaite)[[1]], dims = 2)
  names(cwd_annual) <- "cwd_thornthwaite"

  write_stars(cwd_annual, dsn = paste(destination, "data_raw", "chelsa_v2_1", "cwd_thornthwaite_res.tif", sep = "/"),
              options = c("COMPRESS=LZW","PREDICTOR=2"))
  rm(cwd_annual)

  ## NDM with Thornthwaite

  ndm_stars <- split(tas)[1,,]
  ndm_stars[[1]] <- rowSums(merge(cwd_thornthwaite)[[1]] > 0, dims = 2)
  rm(cwd_thornthwaite)
  names(ndm_stars) <- "ndm_thornthwaite"
  write_stars(ndm_stars , dsn = paste(destination, "data_raw", "chelsa_v2_1", "ndm_thornthwaite_res.tif", sep = "/"),
              options = c("COMPRESS=LZW","PREDICTOR=2"))
  rm(ndm_stars)

  system(glue('gdal_merge.py -o {paste(destination, "data_raw", "current_chelsa.tif", sep = "/")} -of GTiff -ot Int16 -co "COMPRESS=LZW" \\
            -co "PREDICTOR=2" -separate -a_nodata {nodat} {paste(destination, "data_raw", "chelsa_v2_1", "clim_res.tif", sep = "/")} \\
            {paste(destination, "data_raw", "chelsa_v2_1", "cwd_res.tif", sep = "/")} {paste(destination, "data_raw", "chelsa_v2_1", "ndm_res.tif", sep = "/")} \\
            {paste(destination, "data_raw", "chelsa_v2_1", "pet_thornthwaite_res.tif", sep = "/")} {paste(destination, "data_raw", "chelsa_v2_1", "cwd_thornthwaite_res.tif", sep = "/")} \\
            {paste(destination, "data_raw", "chelsa_v2_1", "ndm_thornthwaite_res.tif", sep = "/")}'), ignore.stdout = TRUE, ignore.stderr = TRUE)
  current <- split(read_stars(paste(destination, "data_raw",  "current_chelsa.tif", sep = "/")))
  names(current) <- c(names(split(read_stars(paste(destination, "data_raw", "chelsa_v2_1", "clim_res.tif", sep = "/")))), "cwd_penman", "ndm_penman", paste0("pet_thornthwaite_", 1:12), "cwd_thornthwaite", "ndm_thornthwaite")
  write_stars(obj = merge(current), options = c("COMPRESS=LZW","PREDICTOR=2"), type = "Int16",
              NA_value = nodat, dsn = paste(destination, "data_raw", "current_chelsa.tif", sep = "/"))
  rm(current)

  if (rm_download){
    unlink(paste(destination, "data_raw", "chelsa_v2_1", sep = "/"), recursive = TRUE)
  }
  return(paste(destination, "data_raw", "current_chelsa.tif", sep = "/"))
}





