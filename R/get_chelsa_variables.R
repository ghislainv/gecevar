get_chelsa_variables <- function(extent, EPSG, destination, resolution = 1000, rm_download = FALSE){
  #' Create multilayer Tiff file with 107 variables from chelsa-climate.org
  #'
  #' @description
  #' Gives the average values on the data recovered between 1981 and 2010.
  #' Monthly variables are average temperatures, min temperatures, max temperatures, precipitation, potential evapotranspiration (with Penman formula and with Thornthwaite formula),
  #' and total cloud cover.
  #' Others variables are climatic water deficit (with Penman and Thornthwaite), number of dry month (with Penman and Thornthwaite) and 19 bio variables (more information in chelsa documentations).
  #'
  #' @param extent character. First output of `transform_shp_coutry_extend`
  #' @param EPSG int. to consider for this country/area.
  #' @param destination character. absolute path where to download files like `here()` output.
  #' @param resolution int. in meters, recommended resolution are 250m, 500m, 1km, 2km or 5km, default is 1km. See more in details.
  #' @param rm_download boolean. If TRUE remove download files and folders. Keep only environ.tif in `data_raw` folder, default is FALSE.
  #' @return character. absolute path to climate_chelsa.tif.
  #' @details `resolution` need to be carefully choosen because if Tiff file is too big, R can crash.
  #'
  #' @import stars
  #' @import stringr
  #' @import utils
  #' @import rgdal
  #' @import glue
  #' @import SPEI
  #' @import terra
  #' @import sp
  #' @export

  dir.create(path = destination, recursive = TRUE, showWarnings = FALSE)
  destination <- paste0(destination, "/")
  nodat <- -9999
  proj.s <- "EPSG:4326"
  proj.t <- paste0("EPSG:", EPSG)
  dir.create(paste(destination, "data_raw", sep = "/"), showWarnings = FALSE)
  dir.create(paste(destination, "data_raw","chelsa_v2_1", sep = "/"), showWarnings = FALSE) ## folder for climatic data
  dir.create(paste(destination, "data_raw","chelsa_v2_1","temp", sep = "/"), showWarnings = FALSE) ## Temporary folder
  progress_bar <- 0
  nb_var_download <- 12 * 6 + 19
  print("Download in progress")
  pb = txtProgressBar(min = 0, max = nb_var_download, initial = 0)
  for(m in str_pad(1:12, width = 2, pad = "0")){
    ## Monthly minimum temperature (°C).
    system(glue("gdal_translate -projwin {extent_latlon[1]} {extent_latlon[4]} {extent_latlon[3]} {extent_latlon[2]} -projwin_srs EPSG:4326 \\
                /vsicurl/{paste0('https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/tasmin/CHELSA_tasmin_',m,'_1981-2010_V.2.1.tif')} \\
                {paste(destination, 'data_raw', 'chelsa_v2_1', 'temp', paste0('tasmin_', m, '.tif'), sep = '/')}"), ignore.stdout = TRUE, ignore.stderr = TRUE)
    progress_bar <- progress_bar + 1
    setTxtProgressBar(pb, progress_bar)
    ## Monthly maximum temperature (°C).
    system(glue("gdal_translate -projwin {extent_latlon[1]} {extent_latlon[4]} {extent_latlon[3]} {extent_latlon[2]} -projwin_srs EPSG:4326 \\
                /vsicurl/{paste0('https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/tasmax/CHELSA_tasmax_', m, '_1981-2010_V.2.1.tif')} \\
                {paste(destination, 'data_raw', 'chelsa_v2_1', 'temp', paste0('tasmax_', m, '.tif'), sep = '/')}"), ignore.stdout = TRUE, ignore.stderr = TRUE)
    progress_bar <- progress_bar + 1
    setTxtProgressBar(pb, progress_bar)
    ## Monthly average temperature (°C).
    system(glue("gdal_translate -projwin {extent_latlon[1]} {extent_latlon[4]} {extent_latlon[3]} {extent_latlon[2]} -projwin_srs EPSG:4326 \\
                /vsicurl/{paste0('https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/tas/CHELSA_tas_', m, '_1981-2010_V.2.1.tif')} \\
                {paste(destination, 'data_raw', 'chelsa_v2_1', 'temp', paste0('tas_', m, '.tif'), sep = '/')}"), ignore.stdout = TRUE, ignore.stderr = TRUE)
    progress_bar <- progress_bar + 1
    setTxtProgressBar(pb, progress_bar)
    ## Monthly precipitation (mm ~ kg/m2).
    system(glue("gdal_translate -projwin {extent_latlon[1]} {extent_latlon[4]} {extent_latlon[3]} {extent_latlon[2]} -projwin_srs EPSG:4326 \\
                /vsicurl/{paste0('https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/pr/CHELSA_pr_', m, '_1981-2010_V.2.1.tif')} \\
                {paste(destination, 'data_raw', 'chelsa_v2_1', 'temp', paste0('pr_', m, '.tif'), sep = '/')}"), ignore.stdout = TRUE, ignore.stderr = TRUE)
    progress_bar <- progress_bar + 1
    setTxtProgressBar(pb, progress_bar)
    ## Monthly cloud area fraction
    system(glue("gdal_translate -projwin {extent_latlon[1]} {extent_latlon[4]} {extent_latlon[3]} {extent_latlon[2]} -projwin_srs EPSG:4326 \\
                /vsicurl/{paste0('https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/clt/CHELSA_clt_', m, '_1981-2010_V.2.1.tif')} \\
                {paste(destination, 'data_raw', 'chelsa_v2_1', 'temp', paste0('clt_', m, '.tif'), sep = '/')}"), ignore.stdout = TRUE, ignore.stderr = TRUE)
    progress_bar <- progress_bar + 1
    setTxtProgressBar(pb, progress_bar)
    ## Monthly Pet_ penman
    # https://www.fao.org/3/x0490e/x0490e06.htm
    system(glue("gdal_translate -projwin {extent_latlon[1]} {extent_latlon[4]} {extent_latlon[3]} {extent_latlon[2]} -projwin_srs EPSG:4326 \\
                /vsicurl/{paste0('https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/pet/CHELSA_pet_penman_', m, '_1981-2010_V.2.1.tif')} \\
                {paste(destination, 'data_raw', 'chelsa_v2_1', 'temp', paste0('pet_penman_', m, '.tif'), sep = '/')}"), ignore.stdout = TRUE, ignore.stderr = TRUE)
    progress_bar <- progress_bar + 1
    setTxtProgressBar(pb, progress_bar)
  }
  for(i in 1:19){
    # 19 Bioclimatic variables
    # See https://chelsa-climate.org/wp-admin/download-page/CHELSA_tech_specification_V2.pdf for details
    system(glue("gdal_translate -projwin {extent_latlon[1]} {extent_latlon[4]} {extent_latlon[3]} {extent_latlon[2]} -projwin_srs EPSG:4326 \\
                /vsicurl/{paste0('https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/bio/CHELSA_bio', i, '_1981-2010_V.2.1.tif')} \\
                {paste(destination, 'data_raw', 'chelsa_v2_1', 'temp', paste0('bio', i, '.tif'), sep = '/')}"), ignore.stdout = TRUE, ignore.stderr = TRUE)
    progress_bar <- progress_bar + 1
    setTxtProgressBar(pb, progress_bar)
  }
  close(pb)

  for(var in c("tasmin", "tasmax", "tas_", "pr", "bio", "clt", "pet_penman"))
  {
    files.tif <- list.files(paste(destination, "data_raw", "chelsa_v2_1", "temp", sep = "/"), pattern = var, full.names = TRUE)
    for(i in 1:length(files.tif))
    {
      sourcefile <- files.tif[i]
      destfile <- gsub(".tif", "_res.tif", files.tif[i])
      system(glue("gdalwarp -overwrite -s_srs {proj.s} -t_srs {proj.t} \\
        -r bilinear -tr {resolution} {resolution} -te {extent} -ot Int16 -of GTiff -srcnodata 0 -dstnodata {nodat} \\
        {sourcefile} {destfile}"), ignore.stdout = TRUE, ignore.stderr = TRUE)
      if (var %in% c("tasmin", "tasmax", "tas_") | (var == "bio" & i <= 11))
      {
        # stock °C as integer to reduce size
        # °C * 10 to keep information
        change_scale <- round(read_stars(destfile) * 10)
        write_stars(obj = change_scale, options = c("COMPRESS=LZW","PREDICTOR=2"), NA_value = nodat,
                    type = "Int16", dsn = destfile)
      }
      # file.remove(sourcefile)
    }
    files.tif <- list.files(paste(destination, "data_raw", "chelsa_v2_1", "temp", sep = "/"), pattern = var, full.names = TRUE)
    files.tif <- files.tif[grep("[[:digit:]]_res", files.tif)] # remove original file but not delete it
    r <- read_stars(sort(files.tif), along = "band")
    r <- split(r)
    names(r) <- c(paste0(var, 1:length(names(r))))
    r <- merge(r)
    write_stars(obj = r, options = c("COMPRESS=LZW","PREDICTOR=2"),
                type = "Int16", dsn = paste(destination, "data_raw","chelsa_v2_1", paste0(var,"_res.tif"), sep = "/"))
    # file.remove(files.tif)
  }

  # Stack Tasmin, Tasmax, Tas, Pr, Tcc, Pet Penman & bio
  files.tif <- paste(destination, "data_raw", "chelsa_v2_1", paste0(c("tasmin","tasmax","tas_","pr", "clt", "pet_penman", "bio"), "_res.tif"), sep = "/")
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

  tas <- read_stars(paste(destination, "data_raw", "chelsa_v2_1", "tas__res.tif", sep = "/"))
  # keep only latitude coordinates
  lat <- coordinates(spTransform(as_Spatial(st_as_sf(tas)), CRS("+proj=longlat +datum=WGS84")))[,2]
  tas_matrix <- NULL
  for (month in 1:12) {
    tas_matrix <- cbind(tas_matrix, c(tas[[1]][,, month]))
  }
  Sys.time()
  pet_thornthwaite <- t(thornthwaite(Tave = t(tas_matrix / 10), t(lat)))
  Sys.time()
  pet_stars <- tas
  for (i in 1:12){
    pet_stars[[1]][,, i][!is.na(pet_stars[[1]][,, i])] <- pet_thornthwaite[, i]
  }
  pet_stars <- split(pet_stars)
  names(pet_stars) <- paste0("pet_thornthwaite_", 1:12)
  write_stars(merge(pet_stars) , dsn = paste(destination, "data_raw", "chelsa_v2_1", "pet_thornthwaite_res.tif", sep = "/"),
              options = c("COMPRESS=LZW","PREDICTOR=2"))

  ## CWD with Thornthwaite PET

  pr <- read_stars(paste(destination, "data_raw", "chelsa_v2_1", "pr_res.tif", sep = "/"))
  cwd_thornthwaite <- merge(pet_stars) - pr
  cwd_thornthwaite[[1]] <- pmax(cwd_thornthwaite[[1]], 0)
  cwd_thornthwaite <- split(cwd_thornthwaite)
  names(cwd_thornthwaite) <- "cwd_thornthwaite"
  cwd_annual <- split(tas)[1,,]
  cwd_annual[[1]] <- rowSums(merge(cwd_thornthwaite)[[1]], dims = 2)
  names(cwd_annual) <- "cwd_thornthwaite"
  write_stars(cwd_annual, dsn = paste(destination, "data_raw", "chelsa_v2_1", "cwd_thornthwaite_res.tif", sep = "/"),
              options = c("COMPRESS=LZW","PREDICTOR=2"))

  ## NDM with Thornthwaite

  ndm_stars <- split(tas)[1,,]
  ndm_stars[[1]] <- rowSums(merge(cwd_thornthwaite)[[1]] > 0, dims = 2)
  names(ndm_stars) <- "ndm_thornthwaite"
  write_stars(ndm_stars , dsn = paste(destination, "data_raw", "chelsa_v2_1", "ndm_thornthwaite_res.tif", sep = "/"),
              options = c("COMPRESS=LZW","PREDICTOR=2"))

  system(glue('gdal_merge.py -o {paste(destination, "data_raw", "current_chelsa.tif", sep = "/")} -of GTiff -ot Int16 -co "COMPRESS=LZW" \\
            -co "PREDICTOR=2" -separate -a_nodata {nodat} {paste(destination, "data_raw", "chelsa_v2_1", "clim_res.tif", sep = "/")} \\
            {paste(destination, "data_raw", "chelsa_v2_1", "cwd_res.tif", sep = "/")} {paste(destination, "data_raw", "chelsa_v2_1", "ndm_res.tif", sep = "/")} \\
            {paste(destination, "data_raw", "chelsa_v2_1", "pet_thornthwaite_res.tif", sep = "/")} {paste(destination, "data_raw", "chelsa_v2_1", "cwd_thornthwaite_res.tif", sep = "/")} \\
            {paste(destination, "data_raw", "chelsa_v2_1", "ndm_thornthwaite_res.tif", sep = "/")}'), ignore.stdout = TRUE, ignore.stderr = TRUE)
  current <- split(read_stars(paste(destination, "data_raw",  "current_chelsa.tif", sep = "/")))
  names(current) <- c(names(split(read_stars(paste(destination, "data_raw", "chelsa_v2_1", "clim_res.tif", sep = "/")))), "cwd_penman", "ndm_penman", paste0("pet_thornthwaite_", 1:12), "cwd_thornthwaite", "ndm_thornthwaite")
  write_stars(obj = merge(current), options = c("COMPRESS=LZW","PREDICTOR=2"), type = "Int16",
              NA_value = nodat, dsn = paste(destination, "data_raw", "current_chelsa.tif", sep = "/"))



  if (rm_download){
    unlink(paste(destination, "data_raw", "chelsa_v2_1", sep = "/"), recursive = TRUE)
  }
  return(paste(destination, "data_raw", "current_chelsa.tif", sep = "/"))
}





