library(tidyverse)

library(raster)
library(spatstat)
library(maptools)
library(slab)
library(ggalt)
library(rgl)
library(fields)
library(sf)
source('~/Dropbox/msandifo/documents/programming/r/packages/slab/R/funcs.R')

########

stereo_grab <-function(p.dat, p=T, n =4, S=F ){

  TZZ <-mfocpoint(p.dat$ta,
                  p.dat$tp, col='black',  pch=2, lab="", up=FALSE, plot=F )
  PZZ <-mfocpoint(p.dat$pa,
                  p.dat$pp, col='black',  pch=2, lab="", up=FALSE, plot=F )

  KT = kde2d(TZZ$x, TZZ$y, n=180, h=.35,lims=c(-1, 1, -1, 1) )
  KP = kde2d(PZZ$x, PZZ$y, n=180, h=.35,lims=c(-1, 1, -1, 1) )
  CC = PLTcirc(PLOT=FALSE, add=FALSE,  ndiv=360,  angs=c(-pi, pi))

  image(KT$x, KT$y, KT$z, add=F, col=c(rep('#ffffff',22),terrain.colors(190) %>% rev))
  contour(KT$x, KT$y, KT$z, add=T, col="grey30", lwd=.4, nlevels=n)
  if (p) contour(KP$x, KP$y, KP$z, add=T, col="red3", lwd=.4, nlevels=n )
  GEOmap::antipolygon(CC$x,CC$y,col="white")
  mnet(add=1, col="grey80", border="black", lwd = .2)
  mpcirc(gcol = "grey50", border = "black",   ndiv = 360 )
  text(0,1.1, "N", xpd=NA, cex=.6)
 if (S) { text(0,-1.1, "S", xpd=NA, cex=.6)
  text( -1.1,0, "W", xpd=NA, cex=.6)
  text( 1.1,0, "E", xpd=NA, cex=.6)
 }
  grid.echo()
  grid.grab()

}

#######

mpcirc <- function (gcol = "black", border = "black", ndiv = 36, lwd=.5,  cross=F)
{
  if (missing(gcol)) {
    gcol = "black"
  }
  if (missing(border)) {
    border = "black"
  }
  if (missing(ndiv)) {
    ndiv = 36
  }
  phi = seq(0, 2 * pi, by = 2 * pi/ndiv)
  x = cos(phi)
  y = sin(phi)
  lines(x, y, col = border, lwd=lwd)
if (cross) {  lines(c(-1, 1), c(0, 0), col = gcol)
  lines(c(0, 0), c(-1, 1), col = gcol)
}
}

########
mnet <- function (add = FALSE, col = gray(0.7), border = "black", lwd = 1,
          LIM = c(-1, -1, +1, +1), ndiv=360, cross=F)
{
  if (missing(add)) {
    add = FALSE
  }
  if (missing(col)) {
    col = gray(0.7)
  }
  if (missing(lwd)) {
    lwd = .4
  }
  if (missing(border)) {
    border = "black"
  }
  if (missing(LIM)) {
    LIM = c(-1, -1, +1, +1)
  }
  if (add == FALSE) {
    plot(c(LIM[1], LIM[3]), c(LIM[2], LIM[4]), type = "n",
         xlab = "", ylab = "", asp = 1, axes = FALSE, ann = FALSE)
  }
  mpcirc(gcol = col, border = border, ndiv = ndiv, lwd=lwd, cross=cross)
  lam = pi * seq(from = 0, to = 180, by = 5)/180
  lam0 = pi/2
  for (j in seq(from = -80, to = 80, by = 10)) {
    phi = j * pi/180
    R = sqrt(2)/2
    kp = sqrt(2/(1 + cos(phi) * cos(lam - lam0)))
    x = R * kp * cos(phi) * sin(lam - lam0)
    y = R * kp * sin(phi)
    lines(x, y, col = col, lwd = lwd)
  }
  phi = seq(from = -90, to = 90, by = 5) * pi/180
  for (j in seq(from = 10, to = 170, by = 10)) {
    lam = j * pi/180
    R = sqrt(2)/2
    kp = sqrt(2/(1 + cos(phi) * cos(lam - lam0)))
    x = R * kp * cos(phi) * sin(lam - lam0)
    y = R * kp * sin(phi)
    lines(x, y, col = col, lwd = lwd)
  }
  segments(c(-0.02, 0), c(0, -0.02), c(0.02, 0), c(0, 0.02),
           col = "black")
}

#########

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

#########


#' slab_profie
#' extracts
#' @param l  line
#' @param slab  slab model
#' @param rp  reference point
#'
#' @return dataframe.
#' @export
#'
#' @examples
#'
#'
#'
slab_profile <- function(l, slab=mex, rp=ref, p=p, tail=T) {
  deps <-raster::extract(slab$slab$depth, l %>% as.SpatialLines.psp(), df=F, cellnumbers=T, method="bilinear") %>% as.data.frame()
  xys <-xyFromCell(slab$slab$depth, deps$cell)
  depths <- cbind(xys, deps)

 if (tail) i <- length( depths[ ,1]) else i <-1

  depths$distance <- geosphere::distGeo(  depths[ i,1:2], depths[,1:2])/1000
  depths.ppp<- as.ppp(depths[, 1:2],   W=p)
  depths.ppp$marks <- depths
  names(depths.ppp$marks) <- c("long", "lat", "cell", "depth", "distance")
  return(depths.ppp)
}


########
plot_profile<- function(poly, vo=T, t=T, p=F,slab=T, scale=c(.5,6), offset=T ){

  # offset==T sets the x axis to slab
if (offset)  mint <- min((poly$depths$m$marks %>% subset(!is.na(depth)) %>% tail())$distance) else mint=0

  plot1 <- ggplot()+
    geom_point(data= poly$ehb[poly$p]$marks %>%  subset(m>4),
               aes(distance-mint, -depth , size=m)    , fill="white", show.legend = F, alpha=1, col="grey30") +
    geom_point(data=poly$cmt[poly$p]$marks %>% subset( m>4    & scalarmoment < 13   ),
               aes(distance-mint, -depth, colour=regime, size=m),shape=1, stroke=1., show.legend = F) +
    scale_radius(range=scale)
  #+ scale_color_manual(values=c("red",  "green4", "blue", "green2","grey50"))
  if (vo) plot1 <- plot1+
      geom_point(data=poly$vo[poly$p]$marks  ,aes(distance, 10), shape=24, size=2, fill="yellow", col="red3")


  if (t)   plot1 <-  plot1 +
      geom_segment(data=poly$ta%>% subset(  profile=="p0"),
                   aes(x=x-mint,xend=xend-mint,y= -y, yend= -yend, col=regime), size=.5)
      # geom_segment(data=ta %>% subset(depth.anom> 0 | depth>40),  aes(x=x,xend=xend,y= -y, yend= -yend, col=regime))+
     if (p)     plot1 <- plot1 +
      geom_segment(data=poly$pa%>% subset( profile=="p0"),
                   aes(x=x-mint,xend=xend-mint,y= -y, yend= -yend, col=regime), size=.5)
      # geom_segment(data=ta %>% subset(depth.anom> 0 | depth>40),  aes(x=x,xend=xend,y= -y, yend= -yend, col=regime))+
    if (slab ==T){
    plot1 <- plot1 + geom_line(data=poly$depths$m$marks[  seq(1,length(poly$depths$m$marks$distance), 5),] %>% subset(!is.na(depth)),
                               aes(distance-mint, depth), col="grey60", linetype=1, size=.2)+
      geom_line(data=poly$depths$r$marks[  seq(1,length(poly$depths$r$marks$distance), 5),] %>% subset(!is.na(depth)),
                aes(distance-mint, depth), col="grey50", linetype=2, size=.2)+
      geom_line(data=poly$depths$l$marks[  seq(1,length(poly$depths$l$marks$distance), 5),] %>% subset(!is.na(depth)),
                aes(distance-mint, depth), col="grey50", linetype=5, size=.2)

 #   mint <- min((poly$depths$m$marks %>% subset(!is.na(depth)) %>% tail())$distance)
    # plot1 <- plot1 + geom_vline(xintercept= mint,
    #                              col="grey60", linetype=1, size=.4)
    }
  return(plot1+   coord_fixed(ratio = 1)+
           theme(legend.position=c(.95,.45), legend.title = element_blank())+labs(x=NULL)
  )
}

#########

print_profile <- function(poly){
   mint <-min((poly$depths$m$marks %>% subset(!is.na(depth)) %>% tail())$distance)

  print(mint)
  poly$depths$m$marks$distance <- poly$depths$m$marks$distance-mint

                poly$depths$m$marks[, c(1,2,4,5)] %>% subset(!is.na(depth)) %>% arrange(distance)

}

####


project_poly <- function(slab,
                         poly.list= list( x1 = -77.35, # -78
                                          y1 = -5,
                                          wid = 1., #1.2
                                          ang = 0.5,
                                          len =8),
                         do_plot=F,
                         vo.exists=T,
                         seg=3,
                         scale=15, tail=T
){

  p2 <- get_poly(long=poly.list$x1,
                 lat= poly.list$y1 , angle= poly.list$ang, width=poly.list$wid, length=poly.list$len )
print(p2)
  p2n <-  p2 %>% complement.owin(ras_owin(slab$slab$topo) )

  ref = data.frame(long= poly.list$x1, lat= poly.list$y1)
  coordinates(ref) = ~long+lat




  l3a <-  get_line(long=  poly.list$x1, lat= poly.list$y1 , angle= poly.list$ang, width=poly.list$wid,  length=poly.list$len, n=1, psp=T)
  l3 <-   get_line(long=  poly.list$x1, lat= poly.list$y1 , angle= poly.list$ang, width=0,              length=poly.list$len, n=1, psp=T)
  l3b <-  get_line(long=  poly.list$x1, lat= poly.list$y1 , angle= poly.list$ang, width=-poly.list$wid, length=poly.list$len, n=1, psp=T)

  l2 = get_pline(p2, segment=seg)  #%>% plot( add=T, lwd=3)

  if (do_plot) {
    plates %>% plot()
    plot(p2, add=T)
    plot(l2, add=T, col="red")
    plot(l3b, add=T, col="green")
    point2 <-  ref %>% as.ppp()#crossing.psp(l2, bird.plates.psp[p2]) #get crosing point of
    plot(point2, add=T)
  }
  #win <- owin_poly(p2)


  depths.ppp<- slab_profile(l3, slab,ref,  p=p2, tail=tail)
  depthsa.ppp<- slab_profile(l3a, slab,ref, p=p2, tail=tail)
  depthsb.ppp<- slab_profile(l3b, slab,ref, p=p2, tail=tail)


  #print(  poly_dims(p2))

  project_ppp1( slab$cmt.ppp, p2, plates=l2) -> cmt1.ppp

  project_ppp1(slab$ehb.ppp, p2, plates=l2) -> ehb1.ppp

 if(vo.exists){ project_ppp1( slab$vo.ppp, p2, plates=l2) -> vo1.ppp
   vo1.ppp$marks$depth <-0
 } else vo1.ppp<- NULL


  profile.bearing  <-cmt1.ppp$marks$bearing%%180 %>% median(na.rm =T)
  profile.bearing  <-cmt1.ppp$marks$bearing  %>% median(na.rm =T)
  profile.bearing  <-  (poly.list$ang )%%360
  vo.exists <-F

  pa<-project_axes1(cmt1.ppp, scale=scale, axis=
                      "P", bearing=profile.bearing)
  ta<-project_axes1(cmt1.ppp, scale=scale, axis=
                      "T", bearing=profile.bearing)
  ta  <- cbind(cmt1.ppp$marks, ta )
  pa  <- cbind(cmt1.ppp$marks, pa )



  return(list(p=p2,
              pn = p2n,
              cmt =cmt1.ppp,
              ehb=ehb1.ppp,
              vo=vo1.ppp,
              ta=ta,
              pa=pa,
              depths=list(l=depthsa.ppp, m=depths.ppp, r=depthsb.ppp)
  )
  )
}

########

fig.dir="/Users/msandifo/Dropbox/msandifo/documents/programming/r/packages/slab/examples/ecuador/figures/"
source('~/Dropbox/msandifo/documents/programming/r/2018/misc/ggteeth.R')
barb.scale= 1.5
barb.n=16
boundaries <-  c( 79,80)

########
#mex <-assemble_slab(slab.name="sam", extend.lims = c(-2, -11, 38, 0),  simplify=F)
mex <-assemble_slab(slab.name="sam", extend.lims = c(-8, -6, 25, 0),  simplify=F)
plates= mex$plates.ppp [mex$plates.ppp$marks %in% c("80", "79", "78") ]
coasts.df <- mex$coasts%>% fortify()
proj.laea <- "+proj=laea +lat_0=0 +lon_0=-78"
mex$extent <- extent(mex$slab)
mex.sig <- read.delim("/Users/msandifo/Dropbox/data/global/quakes/ngdc/signif.txt.tsv") %>%
  subset(LONGITUDE > mex$extent[1] & LONGITUDE < mex$extent[2] & LATITUDE > mex$extent[3] & LATITUDE<mex$extent[4])
mex.sig$names <- " "

mex.sig$deaths=0
mex.sig$deaths[!is.na(mex.sig$TOTAL_DEATHS)]="1"
mex.sig$deaths[is.na(mex.sig$TOTAL_DEATHS)]="0"
mex.sig$ll = mex.sig$LONGITUDE/3 +2*mex.sig$LATITUDE

 bird.plates.fn <- "/Volumes/data/data/global/polygons/tectonicplates-master/PB2002_boundaries.shp"
  bird.plates = readShapeLines(bird.plates.fn) %>% as("sf")%>% st_crop(extent(mex$slab)) %>% as( "Spatial") %>% fortify()
 bird.plates.psp =readShapeLines(bird.plates.fn)  %>% as("sf")%>% st_crop(extent(mex$slab)) %>% as( "Spatial")  %>% as.psp()
 message("Bird plate boudary ids : " , bird.plates$id %>%  unique  %>% str_c( collapse = ", "))

 ggthrust( (bird.plates %>% subset(id  %in% boundaries ))[,1:2],  h =3, N=barb.n, REV = T, endtol=0.01, scale=barb.scale  )->teeth.s

mex$cmt.ppp$marks<- mex$cmt.ppp$marks %>% mutate(date = as.POSIXct(date), m = pmax(mb,ms)) #%>% distinct(eventname, .keep_all=T)
mex$cmt.ppp <- mex$cmt.ppp %>% subset( !duplicated(eventname))
mex$ehb.ppp$marks$m <- pmax(mex$ehb.ppp$marks$mw, mex$ehb.ppp$marks$ms, mex$ehb.ppp$marks$mb)

####


poly.list1 <- list( x1 = -77.35, y1 = -5, wid = 2, ang= 0.5,len=8)
poly1 <- project_poly(mex,  poly.list1, do_plot=T,seg=3, scale=30)
plot_profile(poly1, t=T, vo=T)

poly.list2 = list(x1 = -82.55, y1 = -4,wid = 3., ang= 89.99, len=8)
poly2 <- project_poly(mex,  poly.list2, do_plot=T, seg=3, vo=F, scale=30, tail=F)
plot_profile(poly2, t=T, vo=F, off=0)
poly.list3 = list(x1 = -80., y1 = 3.05, wid = 3., ang= 120, len=8)
poly3 <- project_poly(mex,  poly.list3, do_plot=T, seg=2, vo=T, scale=30, tail=F)
plot_profile(poly3, t=T, vo=T, off=0)

poly.list4 = list(x1 = -79.5+.3, y1 = 1+.3,wid = 2., ang= 145, len=6)
poly4 <- project_poly(mex,  poly.list4, do_plot=T, seg=2, vo=T, scale=30, tail=F)
plot_profile(poly4, t=T, vo=T, off=0)

poly.list5 = list(x1 = -80.+.3, y1 = -3.5+.3,wid = 2., ang= 50, len=6)
poly5 <- project_poly(mex,  poly.list5, do_plot=T, seg=3, vo=T, scale=30, tail=T)
plot_profile(poly5, t=T, vo=T)


poly.list6 = list(x1 = -82.55, y1 = -1.,wid = 3., ang= 89, len=8)
poly6 <- project_poly(mex,  poly.list6, do_plot=T, seg=3, vo=T, scale=30, tail=T)
plot_profile(poly6, t=T, vo=T)


poly.list7 = list(x1 = -82.5, y1 = -9.,wid = 5., ang= 67, len=12)
poly7 <- project_poly(mex,  poly.list7, do_plot=T, seg=3, vo=F, scale=30, tail=T)
plot_profile(poly7, t=T, vo=F, slab=T)






######
#ploting
###

library(ggalt)
library(ggmap)

## note need dev verson on ggplot install_github("hadley/ggplot2")
(p.sig.mex<- ggplot( ) +
      ggalt::geom_cartogram( data=coasts.df, aes(  map_id=id  ), map=coasts.df, colour="blue4" ,alpha=.12, size=.3, fill="brown") +
      ggalt::coord_proj(proj.laea, xlim=extent(mex$slab)[1:2],ylim=extent(mex$slab)[3:4]) +
    # geom_polygon(data=mex$borders%>% fortify(), aes(long, lat, group=group), fill="white",show.legend = F, alpha=.4, col="grey90", size=.2)+
    geom_point(data=mex$vo.ppp$marks, aes(long, lat), shape =24,  size=3, col="grey30", fill="Yellow") +
    geom_path(data=bird.plates %>% subset(id %in% boundaries)  , aes(x=long, y=lat, group=id), colour="darkgreen", lwd=.5)+
     geom_path(data=bird.plates   , aes(x=long, y=lat, group=id), colour="darkgreen", lwd=.5)+
 #    geom_path(data= mc.rad, aes(x=long, y=lat, linetype= factor(km)), colour="red2", lwd=.5,  show.legend = F)+
    geom_polygon(data=teeth.s, aes(x,y, group=id), color="darkgreen", fill="darkgreen")+
  #  geom_point(data=mex.sig %>% subset(YEAR<=2016)  , aes(x=LONGITUDE, y=LATITUDE,  size= log10(DEATHS)),  stroke=.5, show.legend = F, shape=5) +
  #  geom_point(data=mex.sig %>% subset(YEAR>2016)  , aes(x=LONGITUDE, y=LATITUDE,  size= log10(DEATHS)),  stroke=1., colour="red2",show.legend = F, shape=5) +
    scale_radius(range=c(1,12)) +
    scale_linetype_manual(values=c("dashed", "longdash"))+
    labs(x=NULL, y=NULL)  +  scale_y_continuous(labels = degreesN)+scale_x_continuous(labels = degreesE)+
    theme(legend.position = "None")
)



(p.join <-  p.sig.mex +

      geom_point(data=poly1$cmt[poly1$pn]$marks , # %>% subset(profile !="p0") ,
                 aes(x=long, y=lat,   shape=regime), col="brown",
                stroke=1, size=1.5, show.legend=F, alpha=.5) +
    geom_point(data=poly1$cmt[poly1$p]$marks  %>% subset(profile =="p0") ,
               aes(x=long, y=lat,   shape=regime), col="orange",
               stroke=1, size=1.5, show.legend=T, alpha=1) +
    geom_point(data=mex$ehb.ppp$marks   ,
               aes(x=long, y=lat ),
               stroke=0, size=1, col="green4",show.legend=F) +
  #  geom_point(data=mex.sig %>% subset(YEAR<=2016)  , aes(x=LONGITUDE, y=LATITUDE,  size= log10(DEATHS)),  stroke=.75, show.legend = F, shape=5) +
 #   geom_point(data=mex.sig %>% subset(YEAR>2016)  , aes(x=LONGITUDE, y=LATITUDE,  size= log10(DEATHS)),  stroke=1.25, colour="red2",show.legend = F, shape=5) +
    geom_point(data=mex$vo.ppp$marks, aes(x=long, lat), shape =24,  size=3, col="grey30", fill="Yellow",show.legend=F)+
     scale_linetype_manual(values=c("dashed", "longdash"))+
    # geom_path(data= owin_poly(poly1$p), aes(x=x, y=y), linetype=1, size=.25,color="blue3")+
    # geom_path(data= owin_poly(poly2$p), aes(x=x, y=y), linetype=1, size=.25,color="blue3")+
    geom_path(data= owin_poly(poly7$p), aes(x=x, y=y), linetype=1, size=.25,color="blue3")+
   # geom_path(data= owin_poly(poly4$p), aes(x=x, y=y), linetype=1, size=.25,color="red2")+
   #  geom_path(data= owin_poly(poly5$p), aes(x=x, y=y), linetype=1, size=.25,color="red2")+
  #  annotate("text", x=-100,y=16, label="b ", size=4, colour="blue3")+      labs(x=NULL, y=NULL)+
    scale_y_continuous(labels = degreesN)+scale_x_continuous(labels = degreesE)+
    guides(linetype="none", size="none")+theme(legend.position=c(.15,.25), legend.title = element_blank())
)

ggsave(paste0(fig.dir,"/peru.figure.1.pdf"), plot=p.join, width=10, height=10)

fig.dir
(pr1<-plot_profile(poly7, t=T, vo=F) +
    ylim(c(-250,10))+xlim(c(-50,930))#+ylim(c(-300,10))+xlim(c(0,630))+theme(legend.position = c(.1,.3))  + annotate("text", x=c(0,630), y=c(10,10), label=c("NW", "SE"), size=6)+ labs(subtitle="a)"),
)
ggsave(paste0(fig.dir,"/peru.pro.1.pdf"), plot=pr1, width=10, height=4)


(pr2<- plot_profile(poly7, t=T, vo=F) +ylim(c(-160,10))+xlim(c(-50,650))+ theme(legend.position=c(.05, .4))
  #+ylim(c(-300,10))+xlim(c(0,630))+theme(legend.position = c(.1,.3))  + annotate("text", x=c(0,630), y=c(10,10), label=c("NW", "SE"), size=6)+ labs(subtitle="a)"),
)
ggsave(paste0(fig.dir,"/peru.pro.2.pdf"), plot=pr2, width=10, height=4)


(pr0<-plot_profile(poly7, t=T, vo=F) + theme(legend.position=c(.1, .4)))
ggsave(paste0(fig.dir,"/peru.pro.0.pdf"), plot=pr0, width=10, height=6)


write.csv(print_profile(poly7), file=paste0(fig.dir,"/peru.profile.m.csv"))

profile.p2 <-gridExtra::grid.arrange(#p.join,
                        plot_profile(poly4, t=T, vo=T, off=0) +ylim(c(-300,10))+xlim(c(0,630))+theme(legend.position = c(.1,.3))  + annotate("text", x=c(0,630), y=c(10,10), label=c("NW", "SE"), size=6)+ labs(subtitle="a)"),
                        plot_profile(poly5, t=T, vo=T)+ylim(c(-300,10))+xlim(c(0,630))+theme(legend.position = "None") + annotate("text", x=c(0,630), y=c(10,10), label=c("SW", "NE"), size=6)+ labs(subtitle="b)"),

                        ncol=1)

profile.p1 <-gridExtra::grid.arrange(#p.join,
  plot_profile(poly3, t=T, vo=T,off=0) +ylim(c(-300,10))+xlim(c(0,800)) + annotate("text", x=c(0,800), y=c(10,10), label=c("NW", "SE"), size=6)+ labs(subtitle="a)"),
  plot_profile(poly2, t=T, vo=F,  off=0)+ylim(c(-300,10))+xlim(c(0,800))+theme(legend.position = "None") + annotate("text", x=c(0,800), y=c(10,10), label=c("W", "E"), size=6)+ labs(subtitle="b)"),
  plot_profile(poly1, t=T, vo=T)+ylim(c(-300,10))+xlim(c(0,800))+theme(legend.position = "None") + annotate("text", x=c(0,800), y=c(10,10), label=c("S", "N"), size=6)+ labs(subtitle="c)"),

  ncol=1)
profile.p1a <-gridExtra::grid.arrange(#p.join,
 # plot_profile(poly3, t=T, vo=T) +ylim(c(-300,10))+xlim(c(0,800)) + annotate("text", x=c(0,800), y=c(10,10), label=c("NW", "SE"), size=6)+ labs(subtitle="a)"),
  plot_profile(poly2, t=T, vo=F, off=0)+ylim(c(-300,10))+xlim(c(0,800))+theme(legend.position = "None") + annotate("text", x=c(0,800), y=c(10,10), label=c("W", "E"), size=6)+ labs(subtitle="b)"),
  plot_profile(poly1, t=T, vo=T)+ylim(c(-300,10))+xlim(c(0,800))+theme(legend.position = "None") + annotate("text", x=c(0,800), y=c(10,10), label=c("S", "N"), size=6)+ labs(subtitle="c)"),

  ncol=1)


ggsave(paste0(fig.dir,"profiles.pdf"),   plot=profile.p1,width=11, height=14)
ggsave(paste0(fig.dir,"profiles1.pdf"),   plot=profile.p1a,width=11, height=9)
ggsave(paste0(fig.dir,"profiles2.pdf"),   plot=profile.p2,width=11, height=9)

ggsave(paste0(fig.dir,"map.pdf"),   plot=p.join,width=8, height=8)



library(MASS)
library(RFOC)
library(gridGraphics)
p.dat <- poly1$ta %>% subset(depth>70 & depth<280 &lat>= -2.5 & lat < -1 & long >-79)
p.dat1 <- poly2$ta %>% subset(depth>70 & depth<280 &lat< -2.5  & long > -80 )
p.dat2<- poly2$ta %>% subset(depth<70 & long < -80   )
p.dat3<- poly3$ta %>% subset(depth<70 & long < -78   )

p.dat6<- poly6$ta %>% subset(depth<70 & long < -78   )


mapgrob <- stereo_grab(p.dat)
mapgrob1 <- stereo_grab(p.dat1)
mapgrob2 <- stereo_grab(p.dat2)
mapgrob3 <- stereo_grab(p.dat3)
mapgrob6 <- stereo_grab(p.dat6)


pdf(file =paste0(fig.dir,"map1.pdf"), width=10, height=11)

p.join
wid=.14
pushViewport( viewport(x = .725, y = .575, height = wid, width = wid, clip="on") )
grid.draw(mapgrob )
upViewport(n = 1)
pushViewport(viewport(x = .835, y = .315, height = wid, width = wid, clip="on") )
grid.draw(mapgrob1 )
upViewport(n = 1)
pushViewport(viewport(x = .125, y = .2, height = wid, width = wid, clip="on") )
grid.draw(mapgrob2 )

upViewport(n = 1)
pushViewport(viewport(x = .155, y = .55, height = wid, width = wid, clip="on") )
grid.draw(mapgrob6 )


upViewport(n = 1)
pushViewport(viewport(x = .255, y = .825, height = wid, width = wid, clip="on") )
grid.draw(mapgrob2 )

dev.off()



#####
proj.laea = CRS(paste0("+proj=laea +lat_0=", round(min(raster::extent(mex$slab$depth)[3:4]),1), " +lon_0=", round(mean(raster::extent(mex$slab$depth)[1:2]), 1)))
mex$slab$depth %>% raster::projectRaster( crs= proj.laea)  %>% aggregate( fact=5) -> mex.dep.laea
lex <- extent(mex.dep.laea)
extent(mex.dep.laea) <-c(lex[1], lex[2], lex[3], lex[4])/1000
sumaco <- c( -77.625833,-0.538056)
sumaco.laea <- convertPts(sumaco[1],sumaco[2], from="+init=epsg:4326" , to=proj.laea)/1000

ehb.slab <- mex$ehb.ppp$marks %>%
  subset(depth.anom >= -5    | (depth.anom >= -20  & depth > 70 )) %>%
  dplyr::arrange(depth.anom)
#ehb1.slab <- mex$ehb.ppp$marks %>% subset(depth.anom >=0)
vo.slab <- mex$vo.ppp$marks

#ehb1 <-convertPts(ehb1.slab$long, ehb1.slab$lat,  from="+init=epsg:4326" , to=proj.laea)/1000
vo <-convertPts(vo.slab$long, vo.slab$lat,  from="+init=epsg:4326" , to=proj.laea)/1000
bird <- convertPts(bird.plates$long, bird.plates$lat,  from="+init=epsg:4326" , to=proj.laea)/1000
bird$z <- -5
bird$id <- bird.plates$id
# image(mex.dep.laea)
# points(sumaco.laea$x, sumaco.laea$y)
#ehb.slab$depth.anom %>% range() %>% diff()



#set colouring
ehb.slab$depth.anom.r <-round(ehb.slab$depth.anom)
ehb.slab$depth.anom.r[ehb.slab$depth.anom.r> 30 ]<- 30


cols<-c(colorRampPalette(c( "red",  "white" ))(abs(min(ehb.slab$depth.anom.r))+1),
colorRampPalette(c(     "white" , "blue"))(abs(max(ehb.slab$depth.anom.r))+2))

ehb.slab$cols <- cols[c(ehb.slab$depth.anom.r+abs(min(ehb.slab$depth.anom.r))+1)]
tear.zone=list()

tear.zone$x=c(-78.8, -78, -77,-74.5)
tear.zone$y=c(-1.75, -.6, -.2, .1)-.3

tear.zone$x=c(-78.3, -78, -77,-74.5)
tear.zone$y=c(-1.5, -.6, -.2, .1)-.3

tear.zone$x=c(-78.3, -78, -77,-76)
tear.zone$y=c(-1.5, -.6, -.2, -1)-.3

tear.zone.laea =(GEOmap::getsplineG(tear.zone$x, tear.zone$y, kdiv=20) %>%  convertPts(to=proj.laea))/1000

mex.dep.laea1 <- mex.dep.laea
na.cells <-extract(mex.dep.laea, tear.zone.laea[20:40,] , cellnumbers=T, buffer=35)
mex.dep.laea1[(do.call("rbind",na.cells))[,1]] <- NA
na.cells <-extract(mex.dep.laea, tear.zone.laea[10:50,], cellnumbers=T, buffer=25)
mex.dep.laea1[(do.call("rbind",na.cells))[,1]] <- NA
na.cells1 <-extract(mex.dep.laea, tear.zone.laea , cellnumbers=T, buffer=15)
mex.dep.laea1[(do.call("rbind",na.cells1))[,1]] <- NA


tear.zone$x=c(-78.3, -78, -77,-76.5, -76.35)
tear.zone$y=c(-1.5, -.6, -.2, -.5, -.9)-.3

tear.zone.laea =(GEOmap::getsplineG(tear.zone$x, tear.zone$y, kdiv=20) %>%  convertPts(to=proj.laea))/1000

mex.dep.laea1 <- mex.dep.laea
na.cells <-extract(mex.dep.laea, tear.zone.laea[20:30,] , cellnumbers=T, buffer=30)
mex.dep.laea1[(do.call("rbind",na.cells))[,1]] <- NA
na.cells <-extract(mex.dep.laea, tear.zone.laea[10:40,], cellnumbers=T, buffer=20)
mex.dep.laea1[(do.call("rbind",na.cells))[,1]] <- NA
na.cells1 <-extract(mex.dep.laea, tear.zone.laea , cellnumbers=T, buffer=10)
mex.dep.laea1[(do.call("rbind",na.cells1))[,1]] <- NA


# tear.poly =cbind(c(-78.8, -78, -77,-74.5, -77, -78, -77.8),
#                 c(-1.75, -.6, -.2, .1, -2,-3, -1.75))
# tear.poly.laea =(GEOmap::getsplineG(tear.poly[,1], tear.poly[,2], kdiv=20) %>%  convertPts(to=proj.laea))/1000
#
# tear.sp<-SpatialPolygons(list(Polygons(list(Polygon(tear.poly.laea)),1)))
#
# warp.cells <- cellFromPolygon(mex.dep.laea, tear.sp)
# mex.dep.laea1[warp.cells[[1]]] <- mex.dep.laea1[warp.cells[[1]]] *
#   (1+( yFromCell(mex.dep.laea1,warp.cells[[1]])-580
#        )/380*1.05)
#
v <- ehb.slab$depth.anom
v[ehb.slab$depth>300]=0
v[ehb.slab$depth<70]=0
#### Thin plate spline model
ehb <-convertPts(ehb.slab$long, ehb.slab$lat,  from="+init=epsg:4326" , to=proj.laea)/1000
  # ehb1 = rbind(ehb,convertPts(c(-76, -76, -76, -75.5, -75.7, -75.5, -75, -75, -75) ,
  #                            c( -1, -.7, -.3, -.5,  -.2, .1, 0.1,.3,.5)+.3, from="+init=epsg:4326" , to=proj.laea)/1000)
  # v= c(v, 140, 160, 0, 120, 150, 0, 90,100,0)

V.zone= tear.zone
V.zone$y=tear.zone$y+.1
V.zone$z=V.zone$y*0

ehb1 = rbind(ehb,convertPts(c(-77, -77, -77, -77, 77, -75.5, -75.5, -75.5, -75, -75, -75, -77.625833, V.zone$x,V.zone$x, V.zone$x,V.zone$x) ,
                              c( -1, -.7, -.3,-.2, -.5, 0,  -.2, .1, 0.1,.3,.5, -0.538056, V.zone$y, V.zone$y-.1,V.zone$y-.2, V.zone$y-.3) , from="+init=epsg:4326" , to=proj.laea)/1000)
  v= c(v, 70, 90, 120,0,  0, 0, 0, 0, 0,0,0,0, V.zone$z-20,V.zone$z-30, V.zone$z+150, V.zone$z+120)

inds <- which(ehb.slab$m >= 5)

  tps <- Tps(ehb1[inds,], v[inds], method="REML")
# p <- mex.dep.laea
# plot(r)
p <- interpolate(mex.dep.laea, tps)
p <- mask(p, mex.dep.laea)

mex1 <- mex.dep.laea
mex1[p<7]<-NA
mex1[is.na(mex.dep.laea1)]<-NA

mex2 <- mex.dep.laea
mex2[p > -7 ]<-NA
mex2[is.na(mex.dep.laea1)]<-NA


qm <- quadmesh::quadmesh(mex.dep.laea)
qm2 <- quadmesh::quadmesh(mex1)
qm2a <- quadmesh::quadmesh(mex2)
qm1 <- quadmesh::quadmesh(mex.dep.laea1-p)

# rgl.clear()
# shade3d(qm)
# rglwidget()
par3d("mouseMode" = c("trackball", "zoom" , "pull" , "polar"))
rgl.clear()
rgl.spheres(sumaco.laea[1], sumaco.laea[2], 0, r = 15, color = "orange3")
rgl.lines(c(sumaco.laea[1],sumaco.laea[1]), c(sumaco.laea[2],sumaco.laea[2]), c(-200,0), col="orange3", lwd=2)
for (i in 1:length(vo$x)){
  rgl.lines(c(vo$x[i],vo$x[i]), c(vo$y[i],vo$y[i]), c(-100,0), col="yellow", lwd=1, alpha=.5)
}
rgl.spheres(vo$x, vo$y, 0, r = 8, color = "yellow")

wire3d(qm2, col = "grey70",  lit = FALSE, alpha=.6)
wire3d(qm2a, col = "yellow2",  lit = FALSE, alpha=.6)
  wire3d(qm1, col = "#D95F02",  lit = FALSE, alpha=.1)
  shade3d(qm1, col = "#D95F02", alpha=.5 )
  bg3d("grey30")
  rgl.spheres(ehb$x, ehb$y, -ehb.slab$depth, r = pmax(ehb.slab$m-3.5,.5)*4, color =  ehb.slab$cols) #colorRampPalette(c( "red",  "white", "blue"))(length(ehb$x)))
#rgl.spheres(ehb1$x, ehb1$y, -ehb1.slab$depth, r = (ehb1.slab$m-3.5)*4, color = "red4")
for (i in unique(bird$id)){
  b <- bird %>% subset(id ==i)
  lines3d(b$x, b$y, b$z,   color = "green4",lwd=2)
}


do.call("rbind", purrr::map2(cmt.slab$ta,-cmt.slab$tp,azdip2xyz)) ->
  cmt.off

cmt.slab <- mex$cmt.ppp$marks %>%
  subset(depth.anom >= -5    | (depth.anom >= -20  & depth > 70 ))

#cmt.slab <- mex$cmt.ppp$marks[c("tp", "ta", "lat", "long", "depth", "m")]

cmt.laea =convertPts(cmt.slab$long, cmt.slab$lat, from="+init=epsg:4326" , to=proj.laea)/1000

scale=20

xs= data.frame(cmt.laea$x+cmt.off[,1]*scale, cmt.laea$x-cmt.off[,1]*scale) %>% as.matrix( ) %>% t() %>% as.vector()
ys= data.frame(cmt.laea$y+cmt.off[,2]*scale, cmt.laea$y-cmt.off[,2]*scale) %>% as.matrix( ) %>% t() %>% as.vector()
zs= data.frame(-cmt.slab$depth+cmt.off[,3]*scale, -cmt.slab$depth-cmt.off[,3]*scale) %>% as.matrix( ) %>% t() %>% as.vector()
rgl.lines(xs  ,
          ys,
          zs ,
          color = c("red4", "yellow"),lwd=2)


# for (i in 1:length(cmt.laea[,1])){
#
#   rgl.lines(c(cmt.laea$x[i]+cmt.off[i,1]*scale,cmt.laea$x[i]-cmt.off[i,1]*scale ),
#           c(cmt.laea$y[i]+cmt.off[i,2]*scale,cmt.laea$y[i]-cmt.off[i,2]*scale ),
#           c(-cmt.slab$depth[i]+cmt.off[i,3]*scale,-cmt.slab$depth[i]-cmt.off[i,3]*scale ),
#           color = "red4",lwd=2)
# }
#
# rgl.lines(matrix(rep(cmt.laea$x,2), ncol=2, byrow=F)+matrix(c(cmt.off[,1], -cmt.off[,1]), ncol=2),
# matrix(rep(cmt.laea$y,2), ncol=2, byrow=F)+matrix(c(cmt.off[,2], -cmt.off[,2]), ncol=2),
# matrix(rep(-cmt.slab$depth,2), ncol=2, byrow=F)+matrix(c(cmt.off[,3], -cmt.off[,3]), ncol=2),
# color = "red4",lwd=2)



# na.df <-data.frame( NA,NA)
# xs <- matrix(rep(cmt.laea$x,2), ncol=2, byrow=F)+matrix(c(cmt.off[,1], -cmt.off[,1]), ncol=2)*scale %>% as.data.frame()
# xs <- do.call(rbind,  apply(xs, 1, function(x) {rbind(x, na.df)}))
# ys <- matrix(rep(cmt.laea$y,2), ncol=2, byrow=F)+matrix(c(cmt.off[,2], -cmt.off[,2]), ncol=2)*scale %>% as.data.frame()
# ys <- do.call(rbind,  apply(ys, 1, function(x) {rbind(x, na.df)}))
# zs <- matrix(rep(-cmt.slab$depth,2), ncol=2, byrow=F)+matrix(c(cmt.off[,3], -cmt.off[,3]), ncol=2)*scale %>% as.data.frame()
# zs <- do.call(rbind,  apply(zs, 1, function(x) {rbind(x, na.df)}))
#
# rep(cmt.laea$x, each=2) + c
# cmt.laea$x +c(cmt.off[,1], -cmt.off[,1])
# rgl.lines(xs %>% as.matrix(),
#           ys%>% as.matrix(),zs %>% as.matrix(),
#           color = "red4",lwd=2)



# share <- rglShared(id)
#     # This puts the selector below the widget.
#      rglwidget(shared = share, width = 300, height = 300) %>% rglMouse()
#     # This puts the selector above the widget.
#    rglMouse() %>% rglwidget(shared = share, width = 300, height = 300, controllers = .)


#observer3d(-40,40, 2000, auto=F)

# TZZ <-mfocpoint(p.dat$ta,
#                 p.dat$tp, col='black',  pch=2, lab="", up=FALSE )
#
# p.dat.m <-c(median(p.dat$ta[p.dat$ta<90 ]),
#   median(p.dat$tp[p.dat$ta<90 ]))
#
# p.dat1.m <-c(median(p.dat1$ta[p.dat1$ta>180 & p.dat1$ta< 270 ]),
# median(p.dat1$tp[p.dat1$ta>180 & p.dat1$ta< 270 ]))
# p.dat2.m <-c(median(p.dat2$ta[p.dat2$ta>180 & p.dat2$ta< 270 ]),
#              median(p.dat2$tp[p.dat2$ta>180 & p.dat2$ta< 270 ]))
#
# TZZ1 <-mfocpoint(p.dat1$ta,
#                 p.dat1$tp, col='black',  pch=2, lab="", up=FALSE )
# TZZ2 <-mfocpoint(p.dat2$ta,
#                  p.dat2$tp, col='black',  pch=2, lab="", up=FALSE )
#
# KT = kde2d(TZZ$x, TZZ$y, n=180, h=.35,lims=c(-1, 1, -1, 1) )
# KT1 = kde2d(TZZ1$x, TZZ1$y, n=180, h=.35, lims=c(-1, 1, -1, 1) )
# KT2 = kde2d(TZZ2$x, TZZ2$y, n=180, h=.35, lims=c(-1, 1, -1, 1) )
# CC = PLTcirc(PLOT=FALSE, add=FALSE,  ndiv=360,  angs=c(-pi, pi))
#
# image(KT$x, KT$y, KT$z, add=F, col=c(rep('#ffffff',22),terrain.colors(190) %>% rev))
# contour(KT$x, KT$y, KT$z, add=T, col="grey30", lwd=.4, nlevels=6)
# #contour(KT1$x, KT1$y, KT1$z, add=T,   lwd=1, nlevels=6, col="red3")
# #points(TZZ$x, TZZ$y, cex=.5, pch=2)
# #points(TZZ1$x, TZZ1$y, cex=1, pch=1, col="red")
# GEOmap::antipolygon(CC$x,CC$y,col="white")
# mnet(add=1, col="grey80", border="black", lwd = .2)
# mpcirc(gcol = "grey50", border = "black",   ndiv = 360 )
# #mfocpoint(p.dat.m[1], p.dat.m[2], pch=24, cex=2., bg="yellow")
# #mfocpoint(p.dat1.m[1], p.dat1.m[2], pch=21, cex=2., bg="yellow")
# text(0,1.07, "N", xpd=NA, cex=.6)
# text(0,-1.07, "S", xpd=NA, cex=.6)
# text( -1.07,0, "W", xpd=NA, cex=.6)
# text( 1.07,0, "E", xpd=NA, cex=.6)
# grid.echo()
# grid.grab() -> mapgrob
#
# image(KT1$x, KT1$y, KT1$z, add=F, col=c(rep('#ffffff',22),terrain.colors(190) %>% rev))
# contour(KT1$x, KT1$y, KT1$z, add=T,   lwd=.4, nlevels=6, col="black")
# #points(TZZ1$x, TZZ1$y, cex=.5, pch=2)
# GEOmap::antipolygon(CC$x,CC$y,col="white")
# mnet(add=1, col="grey80", border="black", lwd = .2)
# mpcirc(gcol = "grey50", border = "black",   ndiv = 360)
# #mfocpoint(p.dat1.m[1], p.dat1.m[2], pch=21, cex=2., bg="yellow")
# text(0,1.07, "N", xpd=NA, cex=.6)
# text(0,-1.07, "S", xpd=NA, cex=.6)
# text( -1.07,0, "W", xpd=NA, cex=.6)
# text( 1.07,0, "E", xpd=NA, cex=.6)
# grid.echo()
# grid.grab() -> mapgrob1
#
# image(KT2$x, KT2$y, KT2$z, add=F, col=c(rep('#ffffff',22),terrain.colors(190) %>% rev))
# contour(KT2$x, KT2$y, KT2$z, add=T,   lwd=.4, nlevels=6, col="black")
# #points(TZZ1$x, TZZ1$y, cex=.5, pch=2)
# GEOmap::antipolygon(CC$x,CC$y,col="white")
# mnet(add=1, col="grey80", border="black", lwd = .2)
# mpcirc(gcol = "grey50", border = "black",   ndiv = 360)
# #mfocpoint(p.dat1.m[1], p.dat1.m[2], pch=21, cex=2., bg="yellow")
# text(0,1.07, "N", xpd=NA, cex=.6)
# text(0,-1.07, "S", xpd=NA, cex=.6)
# text( -1.07,0, "W", xpd=NA, cex=.6)
# text( 1.07,0, "E", xpd=NA, cex=.6)
# grid.echo()
# grid.grab() -> mapgrob2
#

#ggsave(paste0(fig.dir,"map1.pdf"),    width=8, height=8)

# mapgrob
#
# m.ind<-which(KT$z == max(KT$z), arr.ind = TRUE)
# ms <-c(KT$x[m.ind[1]], KT$y[m.ind[2]], median(TZZ$z))
# to.spherical(ms[1], ms[2],-ms[3])
#

 #
# (p.2b <-ggplot()+
#     hrbrthemes::theme_ipsum(axis = TRUE, ticks = F,grid=c("XY"),axis_text_size=14, axis_title_size=18)+
#     geom_path(data=depths.ppp$marks[  seq(1,length(depths.ppp$marks$distance), 5),] %>% subset(!is.na(depth)), aes(distance, depth), col="grey60", linetype=1, size=.2)+
#     geom_path(data=depthsa.ppp$marks[  seq(1,length(depthsa.ppp$marks$distance), 5),] %>% subset(!is.na(depth)), aes(distance, depth), col="grey60", linetype=5, size=.2)+
#     geom_path(data=depthsb.ppp$marks[  seq(1,length(depthsb.ppp$marks$distance), 5),] %>% subset(!is.na(depth)), aes(distance, depth), col="grey60", linetype=5, size=.2)+
#     geom_point(data=ehb1.ppp$marks %>% subset(profile== "p0"),
#                aes( distance, -depth, size=m), show.legend = T, stroke=.25, alpha=.5, shape=1, fill="white") +
#     geom_point(data=cmt1.ppp$marks %>% subset(  profile== "p0"),
#                aes (distance, -depth, colour=regime, size=m), shape=1, stroke=.25, show.legend = F) +
#     geom_point(data=vo1.ppp$marks %>% subset(profile=="p0" ),
#                aes( distance, -depth), shape=24, size=2, fill="yellow", col="red3")+
#     scale_radius(range=c(.5,4)) #+ scale_color_manual(values=c("red",  "green4", "blue", "green2","grey50"))
# )
#
# (p.2c <- p.2b +
#     geom_segment(data=ta%>% subset(   profile=="p0" ), aes(x=x,xend=xend,y= -y, yend= -yend, col=regime), size=.55)+
#        theme_update(panel.grid.minor =element_blank(), panel.grid.major =element_line(colour = "grey85", size=.065, linetype=1))+
#     theme(legend.position=c(.95,.65), legend.title = element_blank()) +
#     labs(x=NULL, y="depth - kms")
#
# )
#
# ( p.1<-gridExtra::grid.arrange( p.join,
#                                 p.2c +ylim(c(-270,10)) ,
#                                 ncol=1, heights=c(2.,1 )  )
# )
#
#
#
# plot.combo1.a <-cowplot::plot_grid(p.join,p.2c+theme(legend.position = "bottom")
#                                      + coord_fixed(ratio=2.5),
#                                    labels = c("a", "b"), nrow = 2, align = "v",
#                                    rel_heights=c(2,1.35), label_fontface="plain", label_size=18)
#
#
# ggsave(paste0(fig.dir,"fig1-EC.pdf"),   plot=plot.combo1.a,width=10, height=10)
#
#
# #widthsmm <- 6.0
#
#
# cmt1.ppp$marks$m[ !is.na(cut(cmt1.ppp$marks$m, c( mm,10)))]
#
#
# (p.kernel.cmt.1<-plot_kernel(cmt1.ppp$marks %>% subset(profile =="p0")
#                              , min=5, cuts= c( mm,10), points=T, bw="SJ", kernel="cosine",
#                            leg=c(.7,.95), mscale=1.5,  ylim=c(0,0.005), xlim=NULL )+
#
#   guides(fill="none")+theme(legend.direction = "horizontal")+
#     geom_hline(yintercept= (mm-5)/ (   (1000/4.5)+4), linetype=1, size=.2)
#
# )
#
#
# plot.combo1.b <-cowplot::plot_grid(p.join+
#      theme( legend.background=element_blank()),
#    p.2c +
#      scale_x_continuous(expand = c(0,0) )+
#      theme( legend.position="bottom")+ #    theme(legend.direction = "vertical" , legend.position=c(.1,.22), legend.background=element_blank(), legend.box = 'horizontal')+
#      annotate("segment", x=0, xend=10,  y= -50, yend= -50, linetype=1, size=.2, alpha=.5)+
#      annotate("segment", x=0, xend=10,  y= -75, yend= -75, linetype=1, size=.2, alpha=.5)+
#      scale_y_continuous(expand = c(0,0) , lim=c(-230,10)),
#    p.kernel.cmt.1+
#      theme_update(panel.grid.minor =element_blank(), panel.grid.major =element_line(colour = "grey85", size=.065, linetype=1))+
#                     guides(fill="none")+
#    theme( legend.position="bottom")+
#    labs(x=NULL, y="kernel density")+
#      scale_x_continuous(expand = c(0,0) )+
#      scale_fill_manual(values ="grey80"), labels = c("a", "b", "c"), nrow = 3, align = "v", rel_heights=c(1.,.5,.5), label_fontface="plain", label_size=18)
#
# plot.combo1.b
# ggsave(paste0(fig.dir,"fig1-ECa.pdf"),   plot=plot.combo1.b,width=12, height=16)
#
#




# deps <-raster::extract(mex$slab$depth, l3 %>% as.SpatialLines.psp(), df=F, cellnumbers=T, method="bilinear") %>% as.data.frame()
# xys <-xyFromCell(mex$slab$depth, deps$cell)
# depths <- cbind(xys, deps)
#
# depths$distance <- geosphere::distGeo(  ref, depths[,1:2])/1000
# depths.ppp<- as.ppp(depths[, 1:2],   W=p2)
# depths.ppp$marks <- depths
# names(depths.ppp$marks) <- c("long", "lat", "cell", "depth", "distance")
# #ndis <- -depths.ppp[p2a]$marks$distance
# #depths.ppp[p2a]$marks$distance <-   -(depths.ppp[p2a]$marks$distance)
#
#
# depsa <-raster::extract(mex$slab$depth, l3a %>% as.SpatialLines.psp(), df=F, cellnumbers=T, method="bilinear") %>% as.data.frame()
# xys <-xyFromCell(mex$slab$depth, depsa$cell)
# depthsa <- cbind(xys, depsa)
#
# depthsa$distance <- geosphere::distGeo(  ref, depthsa[,1:2])/1000
# depthsa.ppp<- as.ppp(depthsa[, 1:2],   W=p2)
# depthsa.ppp$marks <- depthsa
# names(depthsa.ppp$marks) <- c("long", "lat", "cell", "depth", "distance")
#
#
# depsb <-raster::extract(mex$slab$depth, l3b %>% as.SpatialLines.psp(), df=F, cellnumbers=T, method="bilinear") %>% as.data.frame()
# xys <-xyFromCell(mex$slab$depth, depsb$cell)
# depthsb <- cbind(xys, depsb)
#
# depthsb$distance <- geosphere::distGeo(  ref, depthsb[,1:2])/1000
# depthsb.ppp<- as.ppp(depthsb[, 1:2],   W=p2)
# depthsb.ppp$marks <- depthsb
# names(depthsb.ppp$marks) <- c("long", "lat", "cell", "depth", "distance")


