#-------------

#-------------
library(tidyverse)
library(raster)
library(spatstat)
library(RColorBrewer)
library(sf)
library(sp)
# source('~/Dropbox/msandifo/documents/programming/r/2017/water/geofabric/raster_functions.R')
# source('~/Dropbox/msandifo/documents/programming/r/2017/ecuador/slab_functions.R')

library(slab)
# library(raster)
# library(magrittr)
# library(stringr)
  library(spatstat)
# library(readr)
library(maptools)
library(isc)
#source('~/Dropbox/msandifo/documents/programming/r/2018/mexico/r/prepare coast.R')
source('~/Dropbox/msandifo/documents/programming/r/2018/geophys/mexico/r/prepare coast.R')
library(ggalt)
library(ggthemes)
#source('~/Dropbox/msandifo/documents/programming/r/2018/misc/ggteeth.R')


 source('~/Dropbox/msandifo/documents/programming/r/2018/geophys/PTprojections/ecuador2.R')
#-------------

#-------------
setwd("/Users/msandifo/Dropbox/msandifo/documents/programming/r/2018/geophys/PTprojections")
slab.name="sam"
shade=T
if (shade) name="./figures/ecuador/ecuador_shade3" else name="./figures/ecuador/ecuador3"
extend.lims =c(-11,-0,4,7)
`%ni%` = Negate(`%in%`)
sam1 <- assemble_slab(slab.name=slab.name, fact=2 , extend.lims =extend.lims)
sam <- proj_slab(sam1)

#plot_slab(sam)

ainds <- which( abs(sam$ehb.ppp$marks$depth.anom)>50)
sam$ehb.ppp$marks$depth.anom[ainds]   <- 50* sign(sam$ehb.ppp$marks$depth.anom[ainds])

lims<-(sam$slab %>% extent())[1:4]
#  coast.sp <-  get_coast( (sam$slab %>% extent())[1:4], sp=T, detail="h") %>%fortify( region="id")
coast.sf <-sam$coastlines %>% st_as_sf()  #region="id")
# rivers.sp <- get_rivers( (sam$slab %>% extent())[1:4], sp=F, detail="h")
# #borders.sp <- get_borders( (sam$slab %>% extent())[1:4], sp=F, detail="h")
# #borders.sp<- get_borders_TM() %>% crop(extent(sam$slab))  %>%fortify(  )
#
plot(coast.sf)
#  borders<-get_borders_TM( ) #%>% crop(extent(sam$slab))
#
# get_borders<- function(countries) { border.select <- borders %>% subset(NAME %in% countries)
#    get_i_borders <- function(country) borders %>% subset(NAME %in% countries) %>% fortify %>% mutate(country=country)
#    purrr::map_df(countries, get_i_borders)  %>% rbind  %>% arrange(country,id, piece)
#   }
#
# borders.sp<-get_borders(countries)
#borders.sp<-purrr::map_dfr(countries, get_borders) #%>% mutate(country=countries) %>% rbind  %>% arrange(country,id, piece)
#borders.peru.sp<-%>% subset(NAME=="Peru")%>%fortify(   )


# bird.plates.fn <- "/Volumes/data/data/global/polygons/tectonicplates-master/PB2002_boundaries.shp"
# bird.plates = readShapeLines(bird.plates.fn) %>%  #crop(extent(sam$slab)) %>%
#   fortify() %>% subset(long >lims[1] &long <lims[2] & lat>lims[3] & lat<lims[4])
# message("Bird plate boudary ids : " , bird.plates$id %>%  unique  %>% str_c( collapse = ", "))
# (sam$ehb.ppp %>% subset( depth>0)) %>% as.data.frame() -> ehb.df

lims <- raster::extent(sam$slab)

sam$wsm.ppp %>% subset( lat >lims[3] & lat<lims[4] & long>lims[1] & long<lims[2]  & depth< 70 )   -> ism

ismp<-getlines(ism,0, scale=.35)
regimes<- c('NF','NS','SS','TF','TS','U')
for (reg in regimes){
  ismp1<- subset(ismp, regime ==reg)
  if (reg == 'NF'| reg =='NS') col<- "Red3"
  if (reg == 'SS') col<- "DarkGreen"
  if (reg == 'TF'| reg =='TS') col<- "Blue3"
  if (reg == 'U') col<- "Black"
  #  lines(ismp1$x, ismp1$y, col=col, lwd=5)
  #		points(wsm$LON, wsm$LAT, pch=16,cex=.3, col="WHITE")
}

source('~/Dropbox/msandifo/documents/programming/r/2018/misc/ggteeth.R')

subset(id  %in% boundaries )
sam$plates %>% subset(ID %in% boundaries)
ggthrust( sam1$plates %>% subset(Type=="subduction") %>% fortify()  %>% subset(lat<5),
          h =5, N=barb.n, REV = T, endtol=0.01, scale=barb.scale  )   ->teeth.s
# ggthrust( (sam$plates.ppp%>% as.data.frame() %>% subset(marks  %in% boundaries))[, 1:2],
#           h =5, N=barb.n, REV = T, endtol=0.01, scale=barb.scale  )   ->teeth.s
plot(sam1$plates.ppp)
 teeth.s[,1:2]<-teeth.s %>% convertPts(to=p)

 ggthrust( sam1$plates %>% subset(Type=="subduction") %>% fortify()  %>% subset(lat<5),
           h =5, N=barb.n, REV = T, endtol=0.01, scale=barb.scale/2  )   ->teeth2.s
 teeth2.s[,1:2]<-teeth2.s %>% convertPts(to=p)


# lims1 <- extent(sam1$slab)[1:4]
# inc = c(5,5)
# longs <- seq(ceiling(lims1[1]/inc[1])*inc[1],
#   floor(lims1[2]/inc[1])*inc[1], inc[1])
#
#   lats <- seq( ceiling(lims1[3]/inc[2])*inc[2],
#   floor(lims1[4]/inc[2])*inc[2], inc[2]
#   )
#
#
#
#   graticule_longs(m$longs=5)
#   get_proj(sam1)
 #   p <- get_proj(sam)
    p <- get_proj(sam1)
    plot(sam$slab$topo)
  #  gridlines(sam1$slab) %>% spTransform(p) %>%plot(add=T)
    g<-graticule(sam1)
    gridlines(sam1$slab, easts=g$longs, norths=g$lats) %>% spTransform(p) %>%plot(add=T)

    gridlines(sam1$slab, easts=g$longs, norths=g$lats) %>% spTransform(p) %>% as( "SpatialLinesDataFrame") %>%fortify() -> g.df

#  graticule(sam1)  %>%  graticule_longs() -> gl
#  graticule(sam1)  %>%  graticule_lats(sp=T)   %>% as("SpatialLinesDataFrame")  -> g1
#  graticule(sam1)  %>%  graticule_lats(sp=T) ->g1
#  g1 %>%  spTransform( p)  #%>% crop(sam)
# g1
# plot(sam$slab$topo)
plot(g1, add=T)#
# if (!is.na(lat0)) proj.laea <-
#   paste0("+proj=laea +lat_0=0 +lon_0=", lims[1]+diff(lims[1:2])/2) else
#     proj.laea <- paste0("+proj=laea +lat_0=", lims[3]+diff(lims[3:4])/2," +lon_0=", lims[1]+diff(lims[1:2])/2)
#
# lims1<- sam$slab$shade%>%raster::projectRaster( crs=proj.laea ) %>%
#   projectRaster(crs=projection(sam$slab$shade)) %>% extent()
# bbox.poly = data.frame(x=bbox(sam$slab$shade)[1,c(1,2,2,1,1)] , y=bbox(sam$slab$shade)[2,c(1,1,2,2,1)]   )
#-------------
# fudge alignment
#-------------
lims2 <-lims1-((lims1-lims)*ras.alignment)[1:4]  #fudging realignment....

#l1<-borders.sp$piece %>% unique %>% length
# fill.colours <- rep(colours,  each= c(3,3,3))
# fill.colours<- colours
lims<-(sam$slab %>% extent())[1:4]

my.stretch= c(.35,.9)


ptemp.cont <-  ggplot(ismp ) +
  geom_sf(data=coast.sf)+
  coord_sf()

ptemp.cont<- ptemp.cont +  # ggmap::inset_raster(sam$slab$shade %>% as.raster(col= grey.colors(200), maxpixels=511*451), xmin= e.ex@xmin, xmax=e.ex@xmax, ymin= e.ex@ymin, ymax=e.ex@ymax )+
  ggmap::inset_raster(sam$slab$shade   %>%   stretch_ras(my.stretch) %>%      # raster::projectRaster( crs=proj.laea  ) %>%
                        as.raster(col= grey.colors(200),                                                                                                                                                                                                                   maxpixels=sam$slab$shade@ncols*sam$slab$shade@nrows),
                      xmin=lims[1], xmax=lims[2], ymin= lims[3], ymax=lims[4] ) +
    geom_sf(data=coast.sf) + geom_path(data=g.df, aes(long,lat, group=group), size=.2, col="lightblue", alpha=.85, linetype=1)

ptemp.cont1 <- ptemp.cont +  #coord_proj(proj.laea, xlim= lims[1:2] , ylim= lims[3:4])+
  # scale_fill_manual(values= tail(fill.colours, -1))+
   geom_path(data=sam$borders , aes(x= long , y=lat,    group=group ), lwd=0. , col="black",   alpha=.5, show.legend = F )+
  #geom_polygon(data=sam$borders , aes(x= long , y=lat,    group=group ), lwd=0. , col="black",   alpha=.5, show.legend = F )+
  #geom_polygon(data=borders.peru.sp , aes(x= long , y=lat, group= piece,group=id), lwd=0 , col="white", fill="purple", alpha=.1)+
  geom_path(data=sam$rivers %>% fortify() , aes(x= long , y=lat, group=group ), lwd=.1, col="grey60" )+
   geom_path(data=sam$plates %>% subset(Type=="subduction") %>% fortify()  , aes(x=long, y=lat, group=id),
             colour="darkgreen", lwd=.5)+
  geom_path(data=sam$plates %>% subset(is.na(Type)  ) %>% fortify()  , aes(x=long, y=lat, group=id),
            colour="darkgreen", lwd=.5, linetype=2)+
 # geom_path(data=bird.plates  %>% subset(id %ni% boundaries ), aes(x=long, y=lat, group=id), colour="darkgreen", linetype=2, lwd=.5)+
  geom_point(data=sam$vo %>% as.data.frame(),aes(mx,my), colour="black", shape=24, fill="yellow", size=2,lwd=.1) +
  theme_bw()+
  theme(legend.position = c(.9,.75),  legend.background = element_rect(fill="transparent"))+
  #     geom_point(data=sam$vo %>% as.data.frame()%>% subset(NAME!="Sumaco"),aes(x,y), colour="black", shape=24, fill="yellow", size=2,lwd=.2, alpha=.3) +
  #geom_polygon(data=bbox.poly, aes(x,y),fill=NA, col="white", lwd=2 )+
  labs(x=NULL, y=NULL)+
  geom_path(data=rasterToContour(sam$slab$depth, levels= contour.levels) %>% fortify(), aes(long,lat , group=group),linetype=2, size=.2, show.legend = FALSE)
contour.labels %>% convertPts(to=p) ->contour.labels1
if (!is.na(contour.labels)) ptemp.cont1 <- ptemp.cont1 +   annotate("text", x=contour.labels1$x, y=contour.labels1$y, label=str_c(abs(contour.levels)), size=contour.label.size, col="grey30")

if (!is.na(volcanoes)) ptemp.cont1 <- ptemp.cont1 + geom_point(data=sam$vo.ppp %>% as.data.frame() %>% subset(vo.name %in% volcanoes), aes(x,y), colour="black", shape=24, fill="red", size=3,lwd=.2 )



fac=c(1,.5)

ptemp.cont2<-ptemp.cont1 + coord_sf(xlim = c(-.75e6,.75e6)*1.8, ylim = c(-2.6e6, .5e6)*1.5)+
  geom_polygon(data=teeth.s, aes(x,y, group=id), color="darkgreen", fill="darkgreen")
#ptemp.cont
ggsave(paste0("~/Desktop/", "sam1_pcont.pdf"), ptemp.cont2, width=plot.dim[1]/fac[1], height=plot.dim[2]/fac[2])

ptemp.cont3 <-ptemp.cont1 + coord_sf(xlim = c(-.85e6,.8e6)*1., ylim = c(-1.1e6, .5e6)*1.5)+
  geom_polygon(data=teeth2.s, aes(x,y, group=id), color="darkgreen", fill="darkgreen")
#ptemp.cont
ggsave(paste0("~/Desktop/", "sam1_pcont3.pdf"), ptemp.cont3, width=plot.dim[1]/fac[1], height=plot.dim[2]/fac[2])


#-------------------
dept=70

p.size = 2.
ehb.df <-sam$ehb %>% as.data.frame()

p.depth <- ptemp.cont2 +
 # geom_polygon(data=bbox.poly, aes(x,y),fill=NA, col="white", lwd=2 )+
  #  scale_color_continuous( colorspace::diverge_hsv(n=50))+
  scale_colour_gradientn(colours=depth.cols)+#colorspace::diverge_hsv(n=50))+
  geom_point(data=ehb.df  %>% subset(depth > dept & depth<340 ), aes(mx, my ), size=p.size, shape=1, colour="black",lwd=.15)+
  geom_point(data=ehb.df  %>% subset(depth>=340), aes(mx, my ), size=p.size/1.5, shape=0, colour="black",lwd=.15)+
  geom_point(data=ehb.df  %>% subset(depth<dept  ), aes(mx, my, colour=depth),size=.3,   colour="grey40",lwd=.15)+
  geom_point(data=ehb.df  %>% subset(depth>dept & depth<340), aes(mx, my, colour=depth), size=p.size/1.1, shape=19) +
  geom_point(data=sam$vo.ppp$marks  %>% subset(name!="Sumaco"),aes(long,lat), colour="black", shape=24, fill="yellow", size=2,lwd=.2, alpha=.6) +
  theme(legend.title = element_blank(), legend.position = leg.pos, legend.direction = leg.orient, legend.background = element_rect(fill="white"))+
  labs(title="a) ehb depth") +
  theme(text=element_text(family="Arial", size=14))

p.depth

depth.anon.trim <- -50
if(!is.na( depth.anon.trim)) ehb.df$depth.anom <-  pmax(ehb.df$depth.anom, depth.anon.trim )

p.depth.anom <- ptemp.cont2 +
 # geom_polygon(data=bbox.poly, aes(x,y),fill=NA, col="white", lwd=2 )+
  #  scale_color_continuous( colorspace::diverge_hsv(n=50))+
  scale_colour_gradientn(colours=depth.anom.cols)+#colorspace::diverge_hsv(n=50))+
  geom_point(data=ehb.df  %>% subset(depth>dept &  depth<340 & !is.na(depth.anom)), aes(mx, my, colour=depth.anom), size=p.size, shape=1, colour="black",lwd=.15)+
  geom_point(data=ehb.df  %>% subset( depth>=350), aes(mx, my, colour=depth.anom), size=p.size/1.5, shape=0, colour="black",lwd=.2)+
  geom_point(data=ehb.df  %>% subset( depth<dept  ), aes(mx, my, colour=depth.anom),size=.3,   colour="grey40",lwd=.15)+
  geom_point(data=ehb.df  %>% subset( depth>dept &  depth<340 & !is.na(depth.anom) ), aes(mx, my, colour=depth.anom ), size=p.size/1.1, shape=19)+
  geom_point(data=sam$vo.ppp$marks  %>% subset(name!="Sumaco"),aes(long,lat), colour="black", shape=24, fill="yellow", size=2,lwd=.2, alpha=.6) +
  theme(legend.title = element_blank(),   legend.position = leg.pos, legend.direction = leg.orient, legend.background = element_rect(fill="white"))+
  labs(title="b) ehb depth - slab1.0 depth") +
  theme(text=element_text(family="Arial", size=14))


p.depths <-gridExtra::grid.arrange(p.depth, p.depth.anom, ncol=ncol)


ggsave(paste0("~/Desktop/", "_depths.png"), p.depths, width=plot.dim[1], height=plot.dim[2])

#BlRe.colours(  10,5,F, drop=2) %>% rev() -> dpeth.cols

p.size = 3.

p.depth3 <- ptemp.cont3 +
  # geom_polygon(data=bbox.poly, aes(x,y),fill=NA, col="white", lwd=2 )+
  #  scale_color_continuous( colorspace::diverge_hsv(n=50))+
  scale_colour_gradientn(colours=depth.cols)+#colorspace::diverge_hsv(n=50))+
  geom_point(data=ehb.df  %>% subset(depth > dept & depth<340 ), aes(mx, my ), size=p.size, shape=1, colour="black",lwd=.15)+
  geom_point(data=ehb.df  %>% subset(depth>=340), aes(mx, my ), size=p.size/1.5, shape=0, colour="black",lwd=.15)+
  geom_point(data=ehb.df  %>% subset(depth<dept  ), aes(mx, my, colour=depth),size=.3,   colour="grey40",lwd=.15)+
  geom_point(data=ehb.df  %>% subset(depth>dept & depth<340), aes(mx, my, colour=depth), size=p.size/1.1, shape=19) +
  geom_point(data=sam$vo.ppp$marks  %>% subset(name!="Sumaco"),aes(long,lat), colour="black", shape=24, fill="yellow", size=2,lwd=.2, alpha=.6) +
  theme(legend.title = element_blank(), legend.position = leg.pos, legend.direction = leg.orient, legend.background = element_rect(fill="white"))+
  labs(title="a) ehb depth") +
  theme(text=element_text(family="Arial", size=14))



depth.anon.trim <- -50
if(!is.na( depth.anon.trim)) ehb.df$depth.anom <-  pmax(ehb.df$depth.anom, depth.anon.trim )

p.depth.anom3 <- ptemp.cont3 +
  # geom_polygon(data=bbox.poly, aes(x,y),fill=NA, col="white", lwd=2 )+
  #  scale_color_continuous( colorspace::diverge_hsv(n=50))+
  scale_colour_gradientn(colours=depth.anom.cols)+#colorspace::diverge_hsv(n=50))+
  geom_point(data=ehb.df  %>% subset(depth>dept &  depth<340 & !is.na(depth.anom)), aes(mx, my, colour=depth.anom), size=p.size, shape=1, colour="black",lwd=.15)+
  geom_point(data=ehb.df  %>% subset( depth>=350), aes(mx, my, colour=depth.anom), size=p.size/1.5, shape=0, colour="black",lwd=.2)+
  geom_point(data=ehb.df  %>% subset( depth<dept  ), aes(mx, my, colour=depth.anom),size=.3,   colour="grey40",lwd=.15)+
  geom_point(data=ehb.df  %>% subset( depth>dept &  depth<340 & !is.na(depth.anom) ), aes(mx, my, colour=depth.anom ), size=p.size/1.1, shape=19)+
  geom_point(data=sam$vo.ppp$marks  %>% subset(name!="Sumaco"),aes(long,lat), colour="black", shape=24, fill="yellow", size=2,lwd=.2, alpha=.6) +
  theme(legend.title = element_blank(),   legend.position = leg.pos, legend.direction = leg.orient, legend.background = element_rect(fill="white"))+
  labs(title="b) ehb depth - slab1.0 depth")+
  theme(text=element_text(family="Arial", size=14))
#extrafont::loadfonts()

p.depths3 <-gridExtra::grid.arrange(p.depth3, p.depth.anom3, ncol=ncol)


ggsave(paste0("~/Desktop/", "_depths3.png"), p.depths3, width=plot.dim[1], height=plot.dim[2])




#--------------------
# scaled length arrows
#--------------------
sam$cmt.ppp$marks$azi <-sam$cmt.ppp$marks$ta
sam$cmt.ppp$marks$dip  <-sam$cmt.ppp$marks$tp

brewer.pal(30,"Spectral")[c(1,2,4, 9:11)]-> dpeth.cols

ismp  <-subset(sam$cmt.ppp, lat>lims[3] & lat<lims[4] & long>lims[1] & long<lims[2] & depth >=dept   ) %>% getlines(  scale=50000,  segment=T, norm=T)
p.T.gt <- ptemp.cont3 +
 # geom_polygon(data=bbox.poly, aes(x,y),fill=NA, col="white", lwd=2 )+
   geom_point(data=ismp, aes(x=x,y=y,  colour=plunge, shape=regime),size=1.3, lwd=.5 , show_guide=F ) +
  geom_segment(data=ismp, aes(x=x,xend=xend ,y=y,yend=yend), colour="white", lwd=.75 , lineend="round" ,arrow = arrow(length = unit(0.01, "npc")))+
  geom_segment(data=ismp, aes(x=x,xend=xend ,y=y,yend=yend, colour=plunge), lwd=.5 , lineend="round" ,arrow = arrow(length = unit(0.01, "npc")))+
  scale_colour_gradientn(colours=plunge.cols, limits=c(0,90)) +
  labs(title = paste("a) CMT T-axes, depths >",depth, "kms"))+
  theme(legend.title = element_blank(), legend.position = leg.pos, legend.direction=leg.orient, legend.background = element_rect(fill="white")) +
  theme(text=element_text(family="Arial", size=14))


ismp.lt40 <-subset(sam$cmt.ppp, lat>lims[3] & lat<lims[4] & long > lims[1] & long < lims[2] & depth<dept   ) %>% getlines(  scale=50000,  segment=T, norm=T)

p.T.lt <- ptemp.cont3 +
#  geom_polygon(data=bbox.poly, aes(x,y),fill=NA, col="white", lwd=2 )+
  geom_point(data=ismp.lt40, aes(x=x,y=y,  colour=plunge, shape=regime),size=1.3, lwd=.5 , show_guide=F ) +
  geom_segment(data=ismp.lt40, aes(x=x,xend=xend ,y=y,yend=yend),colour="white", lwd=.75 , lineend="round" ,arrow = arrow(length = unit(0.01, "npc")))+
  geom_segment(data=ismp.lt40, aes(x=x,xend=xend ,y=y,yend=yend, colour=plunge), lwd=.5 , lineend="round" ,arrow = arrow(length = unit(0.01, "npc")))+
  scale_colour_gradientn(colours=plunge.cols, limits=c(0,90)) +
  labs(title = paste("b) CMT T-axes, depths <",depth, "kms"))+
  theme(legend.title = element_blank(), legend.position = leg.pos, legend.direction=leg.orient, legend.background = element_rect(fill="white"))+
  theme(text=element_text(family="Arial", size=14))



ismt  <-subset(sam$cmt.ppp, lat>lims[3] &lat<lims[4] & long >lims[1] & long <lims[2]     ) %>% getlines(  scale=50000,  segment=T, norm=T)
p.T <- ptemp.cont3 +
  #geom_polygon(data=bbox.poly, aes(x,y),fill=NA, col="white", lwd=2 )+
  geom_point(data=ismt, aes(x=x,y=y,  colour=plunge, shape=regime),size=2, lwd=.5 , show_guide=F ) +
  geom_segment(data=ismt, aes(x=x,xend=xend ,y=y,yend=yend), colour="white", lwd=.75 , lineend="round" ,arrow = arrow(length = unit(0.01, "npc")))+
  geom_segment(data=ismt, aes(x=x,xend=xend ,y=y,yend=yend, colour=plunge), lwd=.5 , lineend="round" ,arrow = arrow(length = unit(0.01, "npc")))+
  scale_colour_gradientn(colours=plunge.cols, limits=c(0,90)) +
  labs(title = "a) CMT T-axes")+
  theme(legend.title = element_blank(), legend.position = leg.pos, legend.direction=leg.orient, legend.background = element_rect(fill="white"))+
  theme(text=element_text(family="Arial", size=14))



p.T.depths <-gridExtra::grid.arrange(p.T.gt, p.T.lt, ncol=ncol)
ggsave(paste0("~/Desktop/", "_cmt_T_depth_norm.png"), p.T.depths, width=plot.dim[1], height=plot.dim[2])


p.an.T.depths <-gridExtra::grid.arrange(p.depth.anom3, p.T.gt, ncol=ncol)
ggsave(paste0("~/Desktop/", "anom_cmt_T_depth_norm.png"), p.an.T.depths, width=plot.dim[1], height=plot.dim[2])

#--------------------
# P-axes- normalised
#--------------------

# set AZI, DIP

sam$cmt.ppp$marks$azi <- sam$cmt.ppp$marks$ta
sam$cmt.ppp$marks$dip  <-sam$cmt.ppp$marks$pp


ismp  <-subset(sam$cmt.ppp, lat>lims[3] & lat<lims[4] & long>lims[1] & long<lims[2] & depth >=dept   ) %>% getlines(  scale=50000,  segment=T, norm=T)
p.P.gt <- ptemp.cont3 +
 # geom_polygon(data=bbox.poly, aes(x,y),fill=NA, col="white", lwd=2 )+
  geom_point(data=ismp, aes(x=x,y=y,  colour=plunge, shape=regime),size=2, lwd=.5 , show_guide=F ) +
  geom_segment(data=ismp, aes(x=x,xend=xend ,y=y,yend=yend), colour="white", lwd=.75 , lineend="round" ,arrow = arrow(length = unit(0.01, "npc")))+
  geom_segment(data=ismp, aes(x=x,xend=xend ,y=y,yend=yend, colour=plunge), lwd=.5 , lineend="round" ,arrow = arrow(length = unit(0.01, "npc")))+
  scale_colour_gradientn(colours=plunge.cols, limits=c(0,90)) +
  labs(title = paste("a) CMT P-axes, depths >",depth, "kms"))+
  theme(legend.title = element_blank(), legend.position = leg.pos, legend.direction=leg.orient, legend.background = element_rect(fill="white"))+
  theme(text=element_text(family="Arial", size=14))




ismp.lt40 <-subset(sam$cmt.ppp, lat>lims[3] & lat<lims[4] & long>lims[1] & long<lims[2] & depth< dept    ) %>% getlines(  scale=50000,  segment=T, norm=T)

p.P.lt <- ptemp.cont3 +
#  geom_polygon(data=bbox.poly, aes(x,y),fill=NA, col="white", lwd=2 )+
  geom_point(data=ismp.lt40, aes(x=x,y=y,  colour=plunge, shape=regime),size=2, lwd=.5 , show_guide=F ) +
  geom_segment(data=ismp.lt40, aes(x=x,xend=xend ,y=y,yend=yend),colour="white", lwd=.75 , lineend="round" ,arrow = arrow(length = unit(0.01, "npc")))+
  geom_segment(data=ismp.lt40, aes(x=x,xend=xend ,y=y,yend=yend, colour=plunge), lwd=.5 , lineend="round" ,arrow = arrow(length = unit(0.01, "npc")))+
  scale_colour_gradientn(colours=plunge.cols, limits=c(0,90)) +
  labs(title = paste("b) CMT P-axes, depths <",depth, "kms"))+
  theme(legend.title = element_blank(), legend.position = leg.pos, legend.direction=leg.orient, legend.background = element_rect(fill="white"))+
  theme(text=element_text(family="Arial", size=14))





ismp  <-subset(sam$cmt.ppp, lat>lims[3] & lat<lims[4] & long>lims[1] & long<lims[2]    ) %>% getlines(  scale=50000,  segment=T, norm=T)
p.P <- ptemp.cont3 +
 # geom_polygon(data=bbox.poly, aes(x,y),fill=NA, col="white", lwd=2 )+
  geom_point(data=ismp, aes(x=x,y=y,  colour=plunge, shape=regime),size=2, lwd=.5 , show_guide=F ) +
  geom_segment(data=ismp, aes(x=x,xend=xend ,y=y,yend=yend), colour="white", lwd=.75 , lineend="round" ,arrow = arrow(length = unit(0.01, "npc")))+
  geom_segment(data=ismp, aes(x=x,xend=xend ,y=y,yend=yend, colour=plunge), lwd=.5 , lineend="round" ,arrow = arrow(length = unit(0.01, "npc")))+
  scale_colour_gradientn(colours=plunge.cols, limits=c(0,90)) +
  labs(title = "a) CMT P-axes")+
  theme(legend.title = element_blank(), legend.position = leg.pos, legend.direction=leg.orient, legend.background = element_rect(fill="white"))+
  theme(text=element_text(family="Arial", size=14))





p.T.depths <-gridExtra::grid.arrange(p.P.gt, p.P.lt, ncol=ncol)
ggsave(paste0("~/Desktop/","_cmt_P_depth_norm.png"), p.T.depths, width=plot.dim[1], height=plot.dim[2])


p.TP <-gridExtra::grid.arrange(p.T,p.P+ labs(title="b) CMT P-axes"), ncol=ncol)
ggsave(paste0("~/Desktop/","_cmt_TP_norm.png"), p.TP, width=plot.dim[1], height=plot.dim[2])



#ggsave("mex_cmt_depths_Paxes_col_dip_norm.png", pax.n, width=plot.dim[1], height=plot.dim[2])

#sam$cmt


plates= get_sf("~/Dropbox/data/global/polygons/tectonicplates-master/PB2002_boundaries.shp" , sam1$slab, sp=F)  %>% add_column(., ID=as.numeric(row.names(.)), .before = "LAYER")

plates.sp <- as(plates,"Spatial")

plates.sp %>% maptools::as.psp.SpatialLines() ->plates1.psp

s.win <-owin(xrange=extent(sam1$slab)[1:2], yrange=extent(sam1$slab)[3:4])

plates.sp %>% as.psp( window=s.win, marks= ) ->plates.psp
