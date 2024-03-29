% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gdal_utils_translate.R
\name{gdal_utils_translate}
\alias{gdal_utils_translate}
\title{Download data with gdal_translate.}
\usage{
gdal_utils_translate(
  ifile,
  ofile,
  ullr_extent,
  proj_s = "EPSG:4326",
  overwrite = TRUE,
  opts = NULL
)
}
\arguments{
\item{ifile}{character. Input file}

\item{ofile}{character. Output file}

\item{ullr_extent}{vector. Extent c(ulx, uly, lrx, lry) which
corresponds to c(xmin, ymax, xmax, ymin).}

\item{proj_s}{character. EPSG code of the input file. Default to
"EPSG:4326".}

\item{opts}{vector. Additional GDAL options.}

\item{overwite}{boolean. If FALSE, do not overwrite raster if it
exists. Default to TRUE.}
}
\description{
Use gdal_translate to download raster data from a Cloud Optimized
Geotiffs.
}
\keyword{internal}
