#' mfocpoint
#'
#' @param az1
#' @param dip1
#' @param col
#' @param pch
#' @param lab
#' @param up
#' @param plot
#' @param tsize
#' @param tcol
#' @param adj
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' improved version of focpoint  *RFOC - jonahton lees- for more graphical control
#'
mfocpoint <-function (az1, dip1, col = 2, pch = 5, lab = "", up = FALSE,
          plot = TRUE, tsize=2.5, tcol="black", adj=c(1.1,0),...)
{
  if (missing(col)) {
    col = rep(1, length(az1))
  }
  if (missing(pch)) {
    pch = rep(5, length(az1))
  }
  if (  missing(plot)) {
    plot = TRUE
  }
  if (length(col) < length(az1)) {
    col = rep(col, length(az1))
  }
  if (length(pch) < length(az1)) {
    pch = rep(pch, length(az1))
  }
  if (missing(up)) {
    up = FALSE
  }
  dip1 = 90 - dip1
  if (  up==TRUE) {
    az1 = az1 + 180
  }
  DEG2RAD = pi/180
  dflag = dip1 > 90
  dip1[dflag] = dip1[dflag] - 90
  az1[dflag] = az1[dflag] - 180
  pch[dflag] = 15
  col[dflag] = 3
  tdip = dip1
  trot = DEG2RAD * az1
  xi = DEG2RAD * tdip
  tq = sqrt(2) * sin(xi/2)
  pltx = tq * sin(trot)
  plty = tq * cos(trot)
#  pltz =   sin( DEG2RAD *( tdip))
  if (  plot==TRUE) {
    points(pltx, plty, pch = pch, col = col, ...)
    if (!missing(lab)) {
      text(pltx, plty, labels = lab,  cex=tsize, adj=adj , col=tcol)
    }
  }
  invisible(list(x = pltx, y = plty  ))
}

pmax(cmt1.ppp[p2]$marks$np2dip[cmt1.ppp[p2]$marks$distance > 120],
     cmt1.ppp[p2]$marks$np1dip[cmt1.ppp[p2]$marks$distance > 120]) %>% mean()

max(cmt1.ppp[p2]$marks$np2dip[cmt1.ppp[p2]$marks$distance > 120],
    cmt1.ppp[p2]$marks$np1dip[cmt1.ppp[p2]$marks$distance > 120]) %>% mean()
cmt1.ppp[p2]$marks$pa[cmt1.ppp[p2]$marks$distance > 120]

par(mai=c(0.2,0,.2,0))
library(MASS)
library(RFOC)
CC = PLTcirc(PLOT=FALSE, add=FALSE,  ndiv=360,  angs=c(-pi, pi))
#focpoint(cmt1.ppp[p2]$marks$pa[cmt1.ppp[p2]$marks$distance > 150],
#                cmt1.ppp[p2]$marks$pp[cmt1.ppp[p2]$marks$distance > 150], col='red',  pch=3, lab="", UP=FALSE)
PZZ <-mfocpoint(cmt1.ppp[p2]$marks$pa[cmt1.ppp[p2]$marks$distance > 150],
               cmt1.ppp[p2]$marks$pp[cmt1.ppp[p2]$marks$distance > 150], col='black',  pch=2, lab="", up=FALSE )
TZZ <-mfocpoint(cmt1.ppp[p2]$marks$ta[cmt1.ppp[p2]$marks$distance > 150],
               cmt1.ppp[p2]$marks$tp[cmt1.ppp[p2]$marks$distance > 150], col='black',  pch=2, lab="", up=FALSE )


KT = kde2d(TZZ$x, TZZ$y, n=180, lims=c(-1, 1, -1, 1) )
KP = kde2d(PZZ$x, PZZ$y, n=180, lims=c(-1, 1, -1, 1))
np1 <-mfocpoint((cmt1.ppp[p2]$marks$np1strike[cmt1.ppp[p2]$marks$distance > 150] +90) %% 360,
               cmt1.ppp[p2]$marks$np1dip[cmt1.ppp[p2]$marks$distance > 150], col='black',  pch=2, lab="", up=FALSE )
np2 <-mfocpoint((cmt1.ppp[p2]$marks$np2strike[cmt1.ppp[p2]$marks$distance > 150]+90) %% 360,
               cmt1.ppp[p2]$marks$np2dip[cmt1.ppp[p2]$marks$distance > 150], col='black',  pch=2, lab="", up=FALSE )

NP1 = kde2d(np1$x, np1$y, n=180, lims=c(-1, 1, -1, 1) )
NP2 = kde2d(np2$x, np2$y, n=180, lims=c(-1, 1, -1, 1))
NP <-  kde2d(c(np1$x, np2$x), c(np1$y,np2$y), n=180, lims=c(-1, 1, -1, 1))



### nodal planes

cmt2<- cmt1.ppp[p2]$marks %>% subset(distance > 150)

# np1 <-focpoint(cmt2$np1strike[cmt2$np1strike[]],
#                cmt1.ppp[p2]$marks$np1dip[cmt1.ppp[p2]$marks$distance > 150], col='black',  pch=2, lab="", UP=FALSE )
# np2 <-focpoint(cmt1.ppp[p2]$marks$np2strike[cmt1.ppp[p2]$marks$distance > 150],
#                cmt1.ppp[p2]$marks$np2dip[cmt1.ppp[p2]$marks$distance > 150], col='black',  pch=2, lab="", UP=FALSE )
#

#image(NP$x, NP$y, NP$z, add=F, col=heat.colors(100))

#contour(NP$x, NP$y, NP$z, add=T, col="grey20",  nlevels=6)
# image(KT$x, KT$y, KT$z, add=F, col=c(rep('#ffffff',22),terrain.colors(190) %>% rev))
# contour(KT$x, KT$y, KT$z, add=T, col="grey80", lwd=1, nlevels=8)
# points(TZZ$x, TZZ$y, cex=3, pch=2)
# points(PZZ$x, PZZ$y, cex=3, pch=4, col="black")
# contour(NP2$x, NP2$y, NP2$z, add=T, col="red", lwd=1, nlevels=4)
#   contour(NP1$x, NP1$y, NP1$z, add=T, col="yellow4", lwd=1, nlevels=4)
# net(add=1, col="grey10", border="white", lwd = .24)
# GEOmap::antipolygon(CC$x,CC$y,col="white")
# pcirc(gcol = "grey50", border = "black",   ndiv = 360)
#

np2m <- c(
(cmt1.ppp[p2]$marks$np2strike[cmt1.ppp[p2]$marks$distance > 150 ]) %>% as.double() %>% median() +90,
cmt1.ppp[p2]$marks$np2dip[cmt1.ppp[p2]$marks$distance > 150 ] %>% as.double() %>%median(),
cmt1.ppp[p2]$marks$np2dip[cmt1.ppp[p2]$marks$distance > 150 ] %>% as.double() %>%sd())


np1m <-c(
  (cmt1.ppp[p2]$marks$np1strike[cmt1.ppp[p2]$marks$distance > 150 ] %>% median() +90)%%360,
  (cmt1.ppp[p2]$marks$np1dip[cmt1.ppp[p2]$marks$distance > 150 ]) %>% median(),
(cmt1.ppp[p2]$marks$np1dip[cmt1.ppp[p2]$marks$distance > 150 ]) %>% sd())

np2m.r <- c(min(cmt1.ppp[p2]$marks$np2dip[cmt1.ppp[p2]$marks$distance > 150 ] ), max(cmt1.ppp[p2]$marks$np2dip[cmt1.ppp[p2]$marks$distance > 150 ]  ))
np1m.r <- c(min(cmt1.ppp[p2]$marks$np1dip[cmt1.ppp[p2]$marks$distance > 150 ] ), max(cmt1.ppp[p2]$marks$np1dip[cmt1.ppp[p2]$marks$distance > 150 ]  ))



#mfocpoint(np1m[1], np1m[2],  pch=24, cex=3., bg="yellow", lab =paste0(" ", np1m[2],"°=>", np1m[1],"°"), tsize=0)
mfocpoint(np1m[1]-30, np1m[2]-10,  pch=24, cex=0., bg="yellow", lab =paste0("M(np1)=",np1m[2],"\n(", np1m.r[1] ,"-",np1m.r[2] ,")"), tcol="yellow4", tsize=1.75, adj=c(.5,.5))
#mfocpoint(np2m[1], np2m[2],  pch=24, cex=3., bg="yellow", lab =paste0(" ", np2m[2],"°=>", np2m[1],"°"), tsize=1.7)

mfocpoint(np2m[1]-70, np2m[2]-3,  pch=24, cex=0., bg="yellow", lab =  paste0("M(np2)=",np2m[2],"\n(", np2m.r[1] ,"-",np2m.r[2] ,")"), tcol="red2", tsize=1.75, adj=c(.5,.5))

mfocpoint(110-90, 42, pch=22, cex=3., bg="yellow", lab =paste0(" puebla"), tsize=1.7, adj=c(-.1,1))

#recordedplot <- recordPlot()

pmax(cmt1.ppp[p2]$marks$np2dip[cmt1.ppp[p2]$marks$distance > 150],
     cmt1.ppp[p2]$marks$np1dip[cmt1.ppp[p2]$marks$distance > 150]) %>% mean()

pmax(cmt1.ppp[p2]$marks$np2dip[cmt1.ppp[p2]$marks$distance > 150],
    cmt1.ppp[p2]$marks$np1dip[cmt1.ppp[p2]$marks$distance > 150]) %>% mean()
cmt1.ppp[p2]$marks$pa[cmt1.ppp[p2]$marks$distance > 150]

#
# rbind(cmt1.ppp[p2]$marks %>% subset(distance > 150) %>% dplyr::mutate(np="np1"),
# cmt1.ppp[p2]$marks %>% subset(distance > 150) %>% dplyr::mutate(np="np2")) -> np
#

#ggplot(cmt1.ppp[p2]$marks %>% subset(distance > 150) , aes(np1dip, m, size=depth))+geom_point()

