## https://www.urbandemographics.org/post/figures-map-layers-r/
## https://stefanjuenger.github.io/gesis-workshop-geospatial-techniques-R/slides/2_4_Advanced_Maps_II/2_4_Advanced_Maps_II.html#8
## https://gis.stackexchange.com/questions/328313/creating-isometric-stack-of-raster-bands-using-r

## With library rgl
library(rgl)
library(orientlib)

## View
view <- structure(c(0.707637012004852, -0.409657686948776, 0.575699925422668,
0, 0.706575989723206, 0.409529566764832, -0.577092587947845,
0, 0.000644235638901591, 0.815147757530212, 0.579252660274506,
0, 0, 0, 0, 1), dim = c(4L, 4L))

## 3D plot
open3d()
rgl.pop("lights")
light3d(theta=0, phi=45, specular="black")
view3d(userMatrix=view, fov=0)
#view3d(theta=10, phi=-35, fov=0)
nobs <- 6*6
set.seed(12345)
m <- matrix(runif(nobs), 6, 6)
col1 <- terrain.colors(20)[cut(m, 20)]
col2 <- heat.colors(20)[cut(m, 20)]
col3 <- topo.colors(20)[cut(m, 20)]
surface3d(1:6, 1:6, rep(0, nobs), color=col3, back = "lines", smooth=FALSE)
surface3d(1:6, 1:6, rep(2.5, nobs), color=col2, back = "lines", smooth=FALSE)
surface3d(1:6, 1:6, rep(5, nobs), color=col1, back = "lines", smooth=FALSE)

## Save plot
rgl.snapshot(file=here("logo", "background.png"), fmt="png")

## End of file
