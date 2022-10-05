get_chelsa_variables <- function(extent, EPSG, destination){
  #' Create multilayer Tiff file with 93 variables from chelsa-climate.org
  #' Monthly variables are average temperatures, min temperatures, max temperatures, precipitation, potential evapotranspiration,
  #' and total cloud cover.
  #' Others variables are climatic water  deficit, number of dry month and 19 bio variables (more information in chelsa documentations).
  #'
  #' @param extent character. output of `transform_shp_coutry_extend`
  #' @param EPSG int. to consider for this country/area.
  #' @param destination character. absolute path where to download files like `here()` output.
  #' @return character. absolute path to climate_chelsa.tif.

  #'
  #' @import here
  #' @import stars
  #' @import stringr
  #' @import utils
  #' @import rgdal
  #' @import glue
  #' @import qpdf

  dir.create(path = destination, recursive = TRUE, showWarnings = FALSE)
  destination <- paste0(destination, "/")
  nodat <- -9999
  proj.s <- "EPSG:4326"
  proj.t <- paste0("EPSG:", EPSG)
  dir.create(paste0(destination, "data_raw","chelsa_v2_1", sep = "/"), showWarnings = FALSE) ## folder for climatic data
  dir.create(paste0(destination, "data_raw","chelsa_v2_1","temp", sep = "/"), showWarnings = FALSE) ## Temporary folder

  for(m in str_pad(1:12, width = 2, pad = "0")){
    ## Monthly minimum temperature (°C).
    download.file(paste0('https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/tasmin/CHELSA_tasmin_',m,'_1981-2010_V.2.1.tif'),
                  destfile = paste0(destination, "data_raw", "chelsa_v2_1", "temp", paste0("tasmin_",m,".tif"), sep = "/"), method = 'wget')
    ## Monthly maximum temperature (°C).
    download.file(paste0('https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/tasmax/CHELSA_tasmax_',m,'_1981-2010_V.2.1.tif'),
                  destfile = paste0(destination, "data_raw","chelsa_v2_1", "temp", paste0("tasmax_",m,".tif"), sep = "/"), method = 'wget')
    ## Monthly average temperature (°C).
    download.file(paste0('https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/tas/CHELSA_tas_',m,'_1981-2010_V.2.1.tif'),
                  destfile = paste0(destination, "data_raw", "chelsa_v2_1", "temp", paste0("tas_",m,".tif"), sep = "/"), method = 'wget')
    ## Monthly precipitation (mm ~ kg/m2).
    download.file(paste0('https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/pr/CHELSA_pr_',m,'_1981-2010_V.2.1.tif'),
                  destfile = paste0(destination, "data_raw", "chelsa_v2_1", "temp", paste0("pr_",m,".tif"), sep = "/"), method = 'wget')
    ## Monthly cloud cover
    download.file(paste0('https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/tcc/CHELSA_tcc_',m,'_1981-2010_V.2.1.tif'),
                  destfile = paste0(destination, "data_raw", "chelsa_v2_1", "temp", paste0("tcc_mean_",m,".tif"), sep = "/"), method = 'wget')
    ## Monthly Pet_ penman
    # https://www.fao.org/3/x0490e/x0490e06.htm
    download.file(paste0('https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/pet/CHELSA_pet_penman_',m,'_1981-2010_V.2.1.tif'),
                  destfile = paste0(destination, "data_raw", "chelsa_v2_1", "temp", paste0("pet_penman",m,".tif"), sep = "/"), method = 'wget')
  }
  for(i in 1:19){
    # 19 Bioclimatic variables
    # See https://chelsa-climate.org/wp-admin/download-page/CHELSA_tech_specification_V2.pdf for details
    download.file(paste0('https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/bio/CHELSA_bio',i,'_1981-2010_V.2.1.tif'),
                  destfile = paste0(destination, "data_raw", "chelsa_v2_1", "temp", paste0("bio", str_pad(i, 2, pad = "0"), ".tif"), sep = "/"), method = 'wget')
  }

  for(var in c("tasmin", "tasmax", "tas_", "pr", "bio", "tcc", "pet_penman"))
  {
    files.tif <- list.files(paste0(destination, "data_raw", "chelsa_v2_1", "temp", sep = "/"), pattern = var, full.names = TRUE)
    for(i in 1:length(files.tif))
    {
      sourcefile <- files.tif[i]
      destfile <- gsub(".tif", "_1km.tif", files.tif[i])
      system(glue("gdalwarp -overwrite -s_srs {proj.s} -t_srs {proj.t} \\
        -r bilinear -tr 1000 1000 -te {extent} -ot Int16 -of GTiff -srcnodata 0 -dstnodata {nodat} \\
        {sourcefile} {destfile}"))
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
    files.tif <- list.files(paste0(destination, "data_raw", "chelsa_v2_1", "temp", sep = "/"), pattern = var, full.names = TRUE)
    files.tif <- files.tif[grep("[[:digit:]]_1km", files.tif)] # remove original file but not delete it
    r <- read_stars(sort(files.tif), along = "band", NA_value = nodat)
    r <- split(r)
    names(r) <- c(paste0(var, 1:length(names(r))))
    r <- merge(r)
    write_stars(obj = r, options = c("COMPRESS=LZW","PREDICTOR=2"), NA_value = nodat,
                type = "Int16", dsn = paste0(destination, "data_raw","chelsa_v2_1", paste0(var,"_1km.tif"), sep = "/"))
    # file.remove(files.tif)
  }
  ###
  # file.remove(list.files(paste0(destination, "data_raw", "chelsa_v2_1", "temp", sep = "/"), pattern = "_1km", full.names = TRUE))
  ###

  # Stack Tasmin, Tasmax, Tas, Pr, Tcc, Pet Penman & bio
  files.tif <- paste0(destination, "data_raw", "chelsa_v2_1", paste0(c("tasmin","tasmax","tas_","pr", "tcc", "pet_penman", "bio"),"_1km.tif", sep = "/"))
  r <- c(read_stars(files.tif[1]), read_stars(files.tif[2]), read_stars(files.tif[3]), read_stars(files.tif[4]),
         read_stars(files.tif[5]), read_stars(files.tif[6]), read_stars(files.tif[7]), along = "band")
  write_stars(obj = r, options = c("COMPRESS=LZW","PREDICTOR=2"), NA_value = nodat,
              type = "Int16", dsn = paste0(destination, "data_raw","chelsa_v2_1", "clim_1km.tif", sep = "/"))
  # file.remove(files.tif)
  rm(r)

  ## CWD: climatic water deficit
  ## NDM: number of dry months
  pr_file <- paste0(destination, "data_raw", "chelsa_v2_1","pr_1km.tif", sep = "/")
  pet_penman_file <- paste0(destination, "data_raw", "chelsa_v2_1","pet_penman_1km.tif", sep = "/")

  for (i in 1:12)
  {
    # CWD = PET_PENMAN - Pr
    # CWD is a positive values for a lack of water
    system(glue('gdal_calc.py -A {pet_penman_file} --A_band={i} -B {pr_file} --B_band={i} --quiet --type=Int16 \\
               --creation-option="COMPRESS=LZW" --creation-option="PREDICTOR=2"  --calc="A-B" --NoDataValue={nodat} \\
               --outfile={paste0(destination, "data_raw", "chelsa_v2_1", paste0("cwd", i, "_1km.tif"), sep = "/")} --overwrite'))
    # Number of Dry Month ie sum(CWD > 0)
    cwd_file <- paste0(destination, "data_raw", "chelsa_v2_1", paste0("cwd", i, "_1km.tif"), sep = "/")
    ndm_file <- paste0(destination, "data_raw", "chelsa_v2_1", "temp", paste0("ndm", i, "_1km.tif"), sep = "/")
    system(glue('gdal_calc.py -A {cwd_file} --A_band={1} --quiet --type=Int16 \\
              --creation-option="COMPRESS=LZW" --creation-option="PREDICTOR=2" \\
              --outfile={ndm_file} --calc="A>0" --overwrite --NoDataValue={nodat}'))
  }

  ndm_files <- list.files(paste0(destination, "data_raw", "chelsa_v2_1", "temp", sep = "/"), pattern = "ndm", full.names = TRUE)
  system(glue('gdal_calc.py -A {ndm_files[1]} -B {ndm_files[2]} -C {ndm_files[3]} -D {ndm_files[4]}  -E {ndm_files[5]} \\
            -F {ndm_files[6]} -G {ndm_files[7]} -H {ndm_files[8]} -I {ndm_files[9]} -J {ndm_files[10]} -K {ndm_files[11]} \\
            -L {ndm_files[12]} --quiet --type=Int16 --creation-option="COMPRESS=LZW" --creation-option="PREDICTOR=2" \\
            --outfile={paste0(destination, "data_raw", "chelsa_v2_1", "ndm_1km.tif", sep = "/")} --NoDataValue={nodat} \\
            --calc="A+B+C+D+E+F+G+H+I+J+K+L" --overwrite'))

  cwd_files <- list.files(paste0(destination, "data_raw", "chelsa_v2_1", sep = "/"), pattern = "cwd", full.names = TRUE)
  system(glue('gdal_calc.py -A {cwd_files[1]} -B {cwd_files[2]} -C {cwd_files[3]} -D {cwd_files[4]} -E {cwd_files[5]} \\
            -F {cwd_files[6]} -G {cwd_files[7]} -H {cwd_files[8]} -I {cwd_files[9]} -J {cwd_files[10]} -K {cwd_files[11]} \\
            -L {cwd_files[12]} --quiet --type=Int16 --creation-option="COMPRESS=LZW" --creation-option="PREDICTOR=2" \\
            --outfile={paste0(destination, "data_raw", "chelsa_v2_1", "cwd_1km.tif", sep = "/")} --NoDataValue={nodat} \\
            --calc="numpy.maximum(A,0)+numpy.maximum(B,0)+numpy.maximum(C,0)+numpy.maximum(D,0)+numpy.maximum(E,0)+numpy.maximum(F,0) \\
            +numpy.maximum(G,0)+numpy.maximum(H,0)+numpy.maximum(I,0)+numpy.maximum(J,0)+numpy.maximum(K,0)+numpy.maximum(L,0)" --overwrite'))

  system(glue('gdal_merge.py -o {paste0(destination, "output", "current_chelsa.tif", sep = "/")} -of GTiff -ot Int16 -co "COMPRESS=LZW" \\
            -co "PREDICTOR=2" -separate -a_nodata {nodat} {paste0(destination, "data_raw", "chelsa_v2_1", "clim_1km.tif", sep = "/")} \\
            {paste0(destination, "data_raw", "chelsa_v2_1", "cwd_1km.tif", sep = "/")} {paste0(destination, "data_raw", "chelsa_v2_1", "ndm_1km.tif", sep = "/")}'))

  names(current) <- c(names(split(read_stars(paste0(destination, "data_raw", "chelsa_v2_1", "clim_1km.tif", sep = "/")))), "cwd", "ndm")
  write_stars(obj = merge(current), options = c("COMPRESS=LZW","PREDICTOR=2"), type = "Int16",
              NA_value = nodat, dsn = paste0(destination, "output", "current_chelsa.tif", sep = "/"))

  return(paste0(destination, "output", "current_chelsa.tif", sep = "/"))
}





