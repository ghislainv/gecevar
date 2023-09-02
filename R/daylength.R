#' Length of daylight
#' 
#' Compute duration of day light for a given latitude and Julian Day.
#' 
#' It considers sunrise and sunset as the time when the center of the
#' sun pass above or below the horizon, it does not take into account
#' limb, summer time, atmospheric refraction or twilight. Code comes
#' from the archived `insol` R package on CRAN written by Javier
#' G. Corripio <jgc@meteoexploration.com>.
#' 
#' @param lat Latitude in degrees and decimal fraction.
#' @param long Longitude in degrees and decimal fraction.
#' @param jd Julian Day.
#' @param tmz Timezone, west of Greenwich is negative.
#' @return \item{sunrise }{Time of sunrise.} \item{sunset }{Time of sunset.}
#' \item{daylen }{Duration of daylight in hours and decimal fraction.} It
#' returns NA for sunrise and sunset during the polar night.
#' @note You may like to double check at:
#' \url{https://www.esrl.noaa.gov/gmd/grad/solcalc/azel.html}
#' @author Javier G. Corripio <jgc@meteoexploration.com>
#' @seealso \code{\link{declination}}, \code{\link{eqtime}}
#' @references Corripio, J. G.: 2003, Vectorial algebra algorithms for
#' calculating terrain parameters from DEMs and the position of the sun for
#' solar radiation modelling in mountainous terrain, \emph{International
#' Journal of Geographical Information Science} 17(1), 1-23.
#' @examples
#'
#' # Daylength is 12 hours on March, 20th at the Equator. 
#' daylength(0, 6.86933, 79, +1)
#' 
#' # Daylength for the whole year in Chamonix (assuming UTC+1)
#' jd <- c(1:365)
#' plot(daylength(45.92375, 6.86933, jd, 1)[, 3], type="l",
#'      xlab='Day of the year', ylab='day length [h]', ylim=c(0,24))
#' @export
#' 
daylength <- function (lat, long=0, jd, tmz=0) {
  if (nargs() < 4) {
    cat("USAGE: daylength(latitude, longitude, jd, timezone) \n values in degrees, julian days, hours \n"); return()
  }
  EqTime <- eqtime(jd)
  delta <- declination(jd)
  tanlatdel <- -tan(radians(lat)) * tan(radians(delta))
  tanlatdel[tanlatdel > 1] <- 1
  omega <- acos(tanlatdel)
  daylen <- (2*omega)/(2*pi/24)
  stndmeridian <- tmz*15
  deltaLatTime <- long-stndmeridian
  deltaLatTime <- deltaLatTime * 24/360 
  sunrise <- 12*(1-omega/pi)-deltaLatTime-EqTime/60 
  sunset <- 12*(1+omega/pi)-deltaLatTime-EqTime/60
  sunrise[omega == 0] <- NA
  sunset[omega == 0] <- NA
  return(round(cbind(sunrise,sunset,daylen),2))
}

#' Degrees to radians
#' 
#' Accessory function to transform degrees into radians.
#' 
#' 
#' @param degree Angle in degrees and decimal fraction.
#' @return Angle in radians.
#' @seealso \code{\link{degrees}}
#' @examples
#'
#' \dontrun{
#' radians(seq(0,360,90))
#' }
#'
#' @keywords internal 
#' 
radians <- function (degree) {
    radian <- degree * (pi/180.0) 
    return(radian) 
}

#' Radians to degrees
#' 
#' Accessory function to transform radians into degrees.
#' 
#' 
#' @param radian Angle in radians and decimal fraction.
#' @return Angle in degrees.
#' @seealso \code{\link{radians}}
#' @examples
#'
#' \dontrun{
#' degrees(seq(0,2*pi,pi/2))
#' }
#'
#' @keywords internal 
#' 
degrees <- function (radian) {
    degree <- radian * (180.0/pi) 
    return(degree) 
}

#' Declination
#' 
#' Computes the declination of the Sun for a given Julian Day.
#' 
#' 
#' @param jd Julian Day.
#' @return Declination in degrees and decimal fraction.
#' @author Javier G. Corripio <jgc@meteoexploration.com>
#' @references
#' \url{https://www.esrl.noaa.gov/gmd/grad/solcalc/calcdetails.html}
#' 
#' Meeus, J. 1999.  \emph{Astronomical Algorithms}. Willmann-Bell, Richmond,
#' Virginia, USA.
#' 
#' Reda, I. and Andreas, A. 2003. \emph{Solar Position Algorithm for Solar
#' Radiation Applications}. 55 pp.; NREL Report No. TP-560-34302, Revised
#' January 2008.  \url{https://www.nrel.gov/docs/fy08osti/34302.pdf}
#' 
#' @examples
#'
#' \dontrun{
#' declination(120)
#' 
#' ## Plot daily changes in declination
#' jdays=c(1:365)
#' plot(declination(jdays), xlab="days of the year",
#'      ylab="declination", type="l")
#' }
#' 
#' @keywords internal 
#' 
declination <- function (jd) {
    if (nargs() < 1) {
        cat("USAGE: declination(jd) \n jd = Julian day \n")
        return()
    }
    # Julian Centuries (Meeus, Astronomical Algorithms 1999. (24.1))
    T <- (jd - 2451545)/36525.0
    # mean obliquity of the ecliptic (21.2)
    epsilon <- (23+26/60.0+21.448/3600.0) - (46.8150/3600.0)*T - 
    			(0.00059/3600.0)*T^2 + (0.001813/3600.0)*T^3
    # geometric mean longitude of the sun (24.2)
    L0 <- 280.46645 + 36000.76983*T + 0.0003032*T^2
    # L0 <- (L0 - 360 * (L0%/%360))%%360
    # mean anomaly of the Sun (24.3)
    M <- 357.52910 + 35999.05030*T - 0.0001559*T^2 - 0.00000048*T^3
    # eccentricity of the Earth's orbit (24.4)
    e <- 0.016708617 - 0.000042037*T - 0.0000001236*T^2
    # Sun's equation of center
    C <- (1.914600 - 0.004817*T - 0.000014*T^2)*sin(radians(M)) + 
        (0.019993 - 0.000101*T)*sin(2*radians(M)) +
        0.000290*sin(3*radians(M))
    # Sun's true longitude
    Theta <- L0 + C
    # Sun's true anomaly
    v <- M + C
    # Sun's Radius Vector (24.5)
    # R <- (1.000001018*(1-e^2))/(1 + e*cos(radians(v)))
    #  Longitude of the ascending node of the moon
    Omega <- 125.04452 - 1934.136261*T +0.0020708*T^2 +(T^3)/450000
    # Apparent longitude of the sun
    lambda <- Theta - 0.00569 - 0.00478*sin(radians(Omega))
    # Sun's declination (24.7)
    delta <- asin(sin(radians(epsilon)) * sin(radians(lambda)))
    return(degrees(delta))
}


#' Equation of time
#' 
#' Computes the equation of time for a given Julian Day.
#' 
#' 
#' @param jd Julian Day.
#' @return Equation of time in minutes.
#' @author Javier G. Corripio <jgc@meteoexploration.com>
#' @references
#' \url{https://www.esrl.noaa.gov/gmd/grad/solcalc/calcdetails.html}
#' 
#' Meeus, J. 1999.  \emph{Astronomical Algorithms}. Willmann-Bell, Richmond,
#' Virginia, USA.
#' 
#' Reda, I. and Andreas, A. 2003. \emph{Solar Position Algorithm for Solar
#' Radiation Applications}. 55 pp.; NREL Report No. TP-560-34302, Revised
#' January 2008. \url{https://www.nrel.gov/docs/fy08osti/34302.pdf}
#' 
#' @examples
#' \dontrun{
#' # plot the equation of time at daily intervals
#' jd = c(1:365)
#' plot(eqtime(jd), type="l")
#' abline(h=0,col=8)
#' 
#' # Analema
#' plot(eqtime(jd), declination(jd))
#' 
#' # Analema from Greenwich Observatory
#' latGwch = 51.4791
#' x = 180+eqtime(jd)*15/60
#' y = 90-latGwch+declination(jd)
#' plot(x,y,ylim=c(0,90),xlab=expression(paste('Azimuth (',degree,')')),
#' 	ylab=expression(paste('Elevation (',degree,')')))
#' 
#' ## Add the solstices and equinoxes (nearest day, see Meeus ch. 26 for more precision)
#' decl = declination(jd)
#' wintersolstice = which(decl==min(decl))
#' summersolstice = which(decl==max(decl))
#' ## spring equinox: when declination becomes zero in the first part of the year
#' springeqx = uniroot(declination,jd[c(1,180)])$root
#' autumeqx = uniroot(declination,jd[c(180,360)])$root
#' nodeseqx = c(springeqx,summersolstice,autumeqx,wintersolstice)
#' points(x[nodeseqx],y[nodeseqx],pch=19,col=3)
#' abline(h=c(90-latGwch,90-latGwch+max(decl),
#'        90-latGwch+min(decl)),col=8)
#' }
#' 
#' @keywords internal 
#' 
eqtime <- function(jd) {
  if (nargs() < 1 ) {cat("USAGE: eqtime(jd)\n"); return()}
  jdc = (jd - 2451545.0)/36525.0
  sec = 21.448 - jdc*(46.8150 + jdc*(0.00059 - jdc*(0.001813)))
  e0 = 23.0 + (26.0 + (sec/60.0))/60.0 
  ecc = 0.016708634 - jdc * (0.000042037 + 0.0000001267 * jdc)
  oblcorr = e0 + 0.00256 * cos(radians(125.04 - 1934.136 * jdc)) 
  y = (tan(radians(oblcorr)/2))^2
  l0 = 280.46646 + jdc * (36000.76983 + jdc*(0.0003032))
  l0 = (l0-360*(l0%/%360))%%360
  rl0 = radians(l0)
  gmas = 357.52911 + jdc * (35999.05029 - 0.0001537 * jdc)
  gmas = radians(gmas)
  EqTime = y*sin(2*rl0)-2.0*ecc*sin(gmas)+4.0*ecc*y*sin(gmas)*cos(2*rl0)-
    0.5*y^2*sin(4*rl0)-1.25*ecc^2*sin(2*gmas)
  return(degrees(EqTime)*4)
}

# End
