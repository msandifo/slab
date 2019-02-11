
#' azdip2xyz
#'
#' @param az
#' @param dip
#' @param orth logical default is FALSE, specifie if dip
#' @param str logical default is FALSE, specify if strike tren is provide instead of azimuth, assuems right hand rule (aziumth is striike +90 modulo 360)
#'
#' @return xyz
#' @export
#'
#' @examples azdip(az=20, dip=60, orth=F, str=F)
#'
#'
azdip2xyz <- function(az=20, dip=60, orth=F, str=F,  ver=F){

  if ( !is.vector(az) | !is.numeric(az))  stop("az is not a numeric vector")
  if ( !is.vector(dip) | !is.numeric(dip))  stop("dip is not a numeric vector")

  if (str) az = (az+90)%%360
  if (orth ) {
    if (dip>0) dip = dip-90 else dip=dip+90
     az = (az+180)%%360
  }

  if (ver) message("dips +ve up")
  zh.ratio <- tan(dip*pi/180)
  yx.ratio <- tan(az*pi/180)
  x <- yx.ratio
  y <-1
  z <- zh.ratio* sqrt(y^2+x^2)

  if (az>90 & az<=270) y = -y
  if (az>180 & az<=360) x = -x
#  if (dip<0) z =  -z

 round(c(x,y,z)/max(abs(c(x,y,z))) ,3)
}


#'  the angle in dgrees between two lines
#'
#' @param l1 line 1 as c(x,y,z) or c(az, dip)
#' @param l2 line2 as c(x,y,z) or c(az, dip)
#' @param ver verbose
#' @param sig significnace in round resauklt (default=1)
#'
#' @return numeric
#' @export
#'
#' @examples
#' l1 <- c(330,20)
#' l2 <- c(220, 49)
#' lines2angle(l1,l2)
#'
lines2angle <-function(l1=azdip2xyz(),
                       l2=azdip2xyz(dip= -30, ver=FALSE), ver=FALSE, sig=1
                       ){
  if (length(l1) ==2) l1 <- azdip2xyz(l1[1],l1[2])
  if (length(l2) ==2) l2 <- azdip2xyz(l2[1],l2[2])

  if (ver) {print(l1); print(l2)}
  costheta <- sum(l1*l2)/(sqrt(sum(l1^2))*sqrt(sum(l2^2)))

   round(acos(costheta )* 180/pi,sig)


}

