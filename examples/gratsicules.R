#-------------

#-------------
library(tidyverse)
library(raster)
library(spatstat)
library(RColorBrewer)

library(sp)
# source('~/Dropbox/msandifo/documents/programming/r/2017/water/geofabric/raster_functions.R')
# source('~/Dropbox/msandifo/documents/programming/r/2017/ecuador/slab_functions.R')

library(slab)
# library(raster)
# library(magrittr)
# library(stringr)
# library(spatstat)
# library(readr)
library(maptools)
library(isc)
#source('~/Dropbox/msandifo/documents/programming/r/2018/mexico/r/prepare coast.R')
source('~/Dropbox/msandifo/documents/programming/r/2018/geophys/mexico/r/prepare coast.R')
library(ggalt)
library(ggthemes)
#source('~/Dropbox/msandifo/documents/programming/r/2018/misc/ggteeth.R')

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
coast.sp <-sam$coasts %>% fortify( region="id")
# rivers.sp <- get_rivers( (sam$slab %>% extent())[1:4], sp=F, detail="h")
# #borders.sp <- get_borders( (sam$slab %>% extent())[1:4], sp=F, detail="h")
# #borders.sp<- get_borders_TM() %>% crop(extent(sam$slab))  %>%fortify(  )
#
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
ggthrust( (sam$plates.ppp%>% as.data.frame())[, 1:2],  h =5, N=barb.n, REV = T, endtol=0.01, scale=barb.scale  )->teeth.s

lims1 <- extent(sam1$slab)[1:4]
inc = c(5,5)
longs <- seq(ceiling(lims1[1]/inc[1])*inc[1],
  floor(lims1[2]/inc[1])*inc[1], inc[1])

  lats <- seq( ceiling(lims1[3]/inc[2])*inc[2],
  floor(lims1[4]/inc[2])*inc[2], inc[2]
  )

  graticule <- function(slab, inc=c(5,5)) {
    lims <- extent(slab$slab)[1:4]
    longs <- seq(ceiling(lims[1]/inc[1])*inc[1],
                 floor(lims[2]/inc[1])*inc[1], inc[1])

    lats <- seq( ceiling(lims[3]/inc[2])*inc[2],
                 floor(lims[4]/inc[2])*inc[2], inc[2]
    )
    list(longs=longs, lats=lats)
  }

  lsign<- function(label,  labs = c("°W","  ", "°E")){
    vec.sign = round(label/abs(label))
    vec.sign[is.na(vec.sign)] =0
    str_c(label, labs[vec.sign+2])

  }

  graticule_longs <- function(vec, inc=.1, proj4string= CRS("+proj=laea +lat_0=0 +lon_0=384.7 +ellps=WGS84")){
    labs = c("°W","  ", "°E")

      build_df <- function(i)  {  data.frame(x= i,  y=seq(-90,90, inc), id=i)
    }
    map(vec$longs, build_df) %>% do.call("rbind", .) %>% mutate(label=lsign(id, labs=labs))
  }


  graticule_lats <- function(vec, inc=.1, sp=F, proj4string= CRS("+proj=laea +lat_0=0 +lon_0=384.7 +ellps=WGS84")){
    labs = c("°S","  ", "°N")
    build_df <- function(i)  data.frame(x=seq(-180,180,inc),  y=i,  ID=i)
    build_ls <- function(i)  Lines(Line(cbind(seq(-180,180,inc),  rep(i ,360/inc+1))),   ID=lsign(i, labs))
   if (!sp) grat <- map(vec$lats, build_df) %>% do.call("rbind", .) %>% mutate(label=lsign(ID, labs=labs)) else
   map(vec$lats, build_ls)  %>% SpatialLines(proj4string=proj4string )

  }

  graticule_longs(m$longs=5)
  get_proj(sam1)
  p <- get_proj(sam)
 graticule(sam1)  %>%  graticule_longs() -> gl
 graticule(sam1)  %>%  graticule_lats(sp=T)      %>% spTransform( p) %>% crop(sam)
plot(sam)
#
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
ptemp.cont <-  ggplot(ismp ) +
  geom_map( data=coast.sp, aes(map_id=id ), map=coast.sp , fill=fill.colours[1]  )+
  scale_x_continuous( expand=c(0,0))   +
  scale_y_continuous( expand=c(0,0) )+
  coord_fixed()
# scale_colour_manual(values = c("red3",  "orange","green4","blue4", "black", "brown" ))+

lims2 <- lims
lims<-(sam$slab %>% extent())[1:4]

my.stretch= c(.35,.9)
if (shade) ptemp.cont <- ptemp.cont +  # ggmap::inset_raster(sam$slab$shade %>% as.raster(col= grey.colors(200), maxpixels=511*451), xmin= e.ex@xmin, xmax=e.ex@xmax, ymin= e.ex@ymin, ymax=e.ex@ymax )+
  ggmap::inset_raster(sam$slab$shade   %>%   stretch_ras(my.stretch) %>%
                        # raster::projectRaster( crs=proj.laea  ) %>%
                        as.raster(col= grey.colors(200),                                                                                                                                                                                                                   maxpixels=sam$slab$shade@ncols*sam$slab$shade@nrows),
                      xmin=lims[1], xmax=lims[2], ymin= lims[3], ymax=lims[4] ) +
  geom_polygon(data=coast.sp , aes(x= long, y=lat, group=group),
               lwd=.2, col="grey60",fill=NA, alpha=.3) else
                 ptemp.cont <- ptemp.cont + geom_polygon(data=coast.sp ,
                                                         aes(x= long, y=lat, group=group), lwd=.2, col="grey60",fill="grey90", alpha=.3)

ptemp.cont <- ptemp.cont +  #coord_proj(proj.laea, xlim= lims[1:2] , ylim= lims[3:4])+
  # scale_fill_manual(values= tail(fill.colours, -1))+
  # geom_polygon(data=borders.sp , aes(x= long , y=lat,    group=group, fill=id), lwd=0. , col="black",   alpha=.5, show.legend = F )+
  #geom_polygon(data=borders.peru.sp , aes(x= long , y=lat, group= piece,group=id), lwd=0 , col="white", fill="purple", alpha=.1)+
 # geom_path(data=rivers.sp , aes(x= X-360, y=Y, group=PID), lwd=.1, col="grey60" )+
  geom_path(data=coast.sp  , aes(x= long, y=lat, group=group), lwd=.2, col="blue4")+
 # geom_path(data=bird.plates %>% subset(id %in% boundaries)  , aes(x=long, y=lat, group=id), colour="darkgreen", lwd=.5)+
  geom_polygon(data=teeth.s, aes(x,y, group=id), color="darkgreen", fill="darkgreen")+
 # geom_path(data=bird.plates  %>% subset(id %ni% boundaries ), aes(x=long, y=lat, group=id), colour="darkgreen", linetype=2, lwd=.5)+
  geom_point(data=sam$vo %>% as.data.frame(),aes(mx,my), colour="black", shape=24, fill="yellow", size=2,lwd=.1) +
  theme_bw()+
  theme(legend.position = c(.9,.75),  legend.background = element_rect(fill="transparent"))+
  #     geom_point(data=sam$vo %>% as.data.frame()%>% subset(NAME!="Sumaco"),aes(x,y), colour="black", shape=24, fill="yellow", size=2,lwd=.2, alpha=.3) +
  #geom_polygon(data=bbox.poly, aes(x,y),fill=NA, col="white", lwd=2 )+
  labs(x=NULL, y=NULL)+
  geom_path(data=rasterToContour(sam$slab$depth, levels= contour.levels) %>% fortify(), aes(long,lat , group=group),linetype=2, size=.2, show.legend = FALSE)

if (!is.na(contour.labels)) ptemp.cont <- ptemp.cont +   annotate("text", x=contour.labels$x, y=contour.labels$y, label=str_c(abs(contour.levels)), size=contour.label.size, col="grey30")

if (!is.na(volcanoes)) ptemp.cont <- ptemp.cont + geom_point(data=sam$vo.ppp %>% as.data.frame() %>% subset(vo.name %in% volcanoes), aes(x,y), colour="black", shape=24, fill="red", size=3,lwd=.2 )

ptemp.cont

ggsave(paste0(name, "_pcont.pdf"), ptemp.cont, width=plot.dim[1]/2., height=plot.dim[2])

#-------------------
dept=70

p.depth <- ptemp.cont +
  geom_polygon(data=bbox.poly, aes(x,y),fill=NA, col="white", lwd=2 )+
  #  scale_color_continuous( colorspace::diverge_hsv(n=50))+
  scale_colour_gradientn(colours=depth.cols)+#colorspace::diverge_hsv(n=50))+
  geom_point(data=ehb.df  %>% subset(depth > dept & depth<340), aes(long, lat ), size=3.25, shape=1, colour="black",lwd=.15)+
  geom_point(data=ehb.df  %>% subset(depth>=340), aes(long, lat ), size=1.5, shape=0, colour="black",lwd=.15)+
  geom_point(data=ehb.df  %>% subset(depth<dept  ), aes(long, lat, colour=depth),size=.3,   colour="grey40",lwd=.15)+
  geom_point(data=ehb.df  %>% subset(depth>dept & depth<340), aes(long, lat, colour=depth), size=3., shape=19) +
  geom_point(data=sam$vo.ppp$marks  %>% subset(name!="Sumaco"),aes(long,lat), colour="black", shape=24, fill="yellow", size=2,lwd=.2, alpha=.6) +
  theme(legend.title = element_blank(), legend.position = leg.pos, legend.direction = leg.orient, legend.background = element_rect(fill="white"))+
  labs(title="a) ehb depth")+
  coord_fixed()

p.depth
if(!is.na( depth.anon.trim)) ehb.df$depth.anom <-  pmax(ehb.df$depth.anom, depth.anon.trim )

p.depth.anom<- ptemp.cont +
  geom_polygon(data=bbox.poly, aes(x,y),fill=NA, col="white", lwd=2 )+
  #  scale_color_continuous( colorspace::diverge_hsv(n=50))+
  scale_colour_gradientn(colours=depth.anom.cols)+#colorspace::diverge_hsv(n=50))+
  geom_point(data=ehb.df  %>% subset(depth>dept &  depth<340), aes(x, y, colour=depth.anom), size=3.25, shape=1, colour="black",lwd=.15)+
  geom_point(data=ehb.df  %>% subset( depth>=350), aes(x, y, colour=depth.anom), size=1.5, shape=0, colour="black",lwd=.2)+
  geom_point(data=ehb.df  %>% subset( depth<dept  ), aes(x, y, colour=depth.anom),size=.3,   colour="grey40",lwd=.15)+
  geom_point(data=ehb.df  %>% subset( depth>dept &  depth<340  ), aes(x, y, colour=depth.anom ), size=3., shape=19)+
  geom_point(data=sam$vo.ppp$marks  %>% subset(name!="Sumaco"),aes(long,lat), colour="black", shape=24, fill="yellow", size=2,lwd=.2, alpha=.6) +
  theme(legend.title = element_blank(),   legend.position = leg.pos, legend.direction = leg.orient, legend.background = element_rect(fill="white"))+
  labs(title="b) ehb depth - slab1.0 depth")+
  coord_fixed()



p.depths <-gridExtra::grid.arrange(p.depth, p.depth.anom, ncol=ncol)


ggsave(paste0(name, "_depths.png"), p.depths, width=plot.dim[1], height=plot.dim[2])

#BlRe.colours(  10,5,F, drop=2) %>% rev() -> dpeth.cols



#--------------------
# scaled length arrows
#--------------------
sam$cmt.ppp$marks$azi <-sam$cmt.ppp$marks$ta
sam$cmt.ppp$marks$dip  <-sam$cmt.ppp$marks$tp

brewer.pal(30,"Spectral")[c(1,2,4, 9:11)]-> dpeth.cols

ismp  <-subset(sam$cmt.ppp, lat>lims[3] & lat<lims[4] & long>lims[1] & long<lims[2] & depth >=dept   ) %>% getlines(  scale=.5,  segment=T, norm=T)
p.T.gt <- ptemp.cont +
  geom_polygon(data=bbox.poly, aes(x,y),fill=NA, col="white", lwd=2 )+
  geom_point(data=ismp, aes(x=x,y=y,  colour=plunge, shape=regime),size=2, lwd=.5 , show_guide=F ) +
  geom_segment(data=ismp, aes(x=x,xend=xend ,y=y,yend=yend), colour="white", lwd=.75 , lineend="round" ,arrow = arrow(length = unit(0.01, "npc")))+
  geom_segment(data=ismp, aes(x=x,xend=xend ,y=y,yend=yend, colour=plunge), lwd=.5 , lineend="round" ,arrow = arrow(length = unit(0.01, "npc")))+
  scale_colour_gradientn(colours=plunge.cols, limits=c(0,90)) +
  labs(title = paste("a) CMT T-axes, depths >",depth, "kms"))+
  theme(legend.title = element_blank(), legend.position = leg.pos, legend.direction=leg.orient, legend.background = element_rect(fill="white"))+
  coord_fixed()


ismp.lt40 <-subset(sam$cmt.ppp, lat>lims[3] & lat<lims[4] & long > lims[1] & long < lims[2] & depth<dept   ) %>% getlines(  scale=.5,  segment=T, norm=T)

p.T.lt <- ptemp.cont +
  geom_polygon(data=bbox.poly, aes(x,y),fill=NA, col="white", lwd=2 )+
  geom_point(data=ismp.lt40, aes(x=x,y=y,  colour=plunge, shape=regime),size=2, lwd=.5 , show_guide=F ) +
  geom_segment(data=ismp.lt40, aes(x=x,xend=xend ,y=y,yend=yend),colour="white", lwd=.75 , lineend="round" ,arrow = arrow(length = unit(0.01, "npc")))+
  geom_segment(data=ismp.lt40, aes(x=x,xend=xend ,y=y,yend=yend, colour=plunge), lwd=.5 , lineend="round" ,arrow = arrow(length = unit(0.01, "npc")))+
  scale_colour_gradientn(colours=plunge.cols, limits=c(0,90)) +
  labs(title = paste("b) CMT T-axes, depths <",depth, "kms"))+
  theme(legend.title = element_blank(), legend.position = leg.pos, legend.direction=leg.orient, legend.background = element_rect(fill="white"))+
  coord_fixed()



ismt  <-subset(sam$cmt.ppp, lat>lims[3] &lat<lims[4] & long >lims[1] & long <lims[2]     ) %>% getlines(  scale=.5,  segment=T, norm=T)
p.T <- ptemp.cont +
  geom_polygon(data=bbox.poly, aes(x,y),fill=NA, col="white", lwd=2 )+
  geom_point(data=ismt, aes(x=x,y=y,  colour=plunge, shape=regime),size=2, lwd=.5 , show_guide=F ) +
  geom_segment(data=ismt, aes(x=x,xend=xend ,y=y,yend=yend), colour="white", lwd=.75 , lineend="round" ,arrow = arrow(length = unit(0.01, "npc")))+
  geom_segment(data=ismt, aes(x=x,xend=xend ,y=y,yend=yend, colour=plunge), lwd=.5 , lineend="round" ,arrow = arrow(length = unit(0.01, "npc")))+
  scale_colour_gradientn(colours=plunge.cols, limits=c(0,90)) +
  labs(title = "a) CMT T-axes")+
  theme(legend.title = element_blank(), legend.position = leg.pos, legend.direction=leg.orient, legend.background = element_rect(fill="white"))+
  coord_fixed()


p.T.depths <-gridExtra::grid.arrange(p.T.gt, p.T.lt, ncol=ncol)
ggsave(paste0(name, "_cmt_T_depth_norm.png"), p.T.depths, width=plot.dim[1], height=plot.dim[2])

#--------------------
# P-axes- normalised
#--------------------

# set AZI, DIP

sam$cmt.ppp$marks$azi <- sam$cmt.ppp$marks$ta
sam$cmt.ppp$marks$dip  <-sam$cmt.ppp$marks$pp


ismp  <-subset(sam$cmt.ppp, lat>lims[3] & lat<lims[4] & long>lims[1] & long<lims[2] & depth >=dept   ) %>% getlines(  scale=.5,  segment=T, norm=T)
p.P.gt <- ptemp.cont +
  geom_polygon(data=bbox.poly, aes(x,y),fill=NA, col="white", lwd=2 )+
  geom_point(data=ismp, aes(x=x,y=y,  colour=plunge, shape=regime),size=2, lwd=.5 , show_guide=F ) +
  geom_segment(data=ismp, aes(x=x,xend=xend ,y=y,yend=yend), colour="white", lwd=.75 , lineend="round" ,arrow = arrow(length = unit(0.01, "npc")))+
  geom_segment(data=ismp, aes(x=x,xend=xend ,y=y,yend=yend, colour=plunge), lwd=.5 , lineend="round" ,arrow = arrow(length = unit(0.01, "npc")))+
  scale_colour_gradientn(colours=plunge.cols, limits=c(0,90)) +
  labs(title = paste("a) CMT P-axes, depths >",depth, "kms"))+
  theme(legend.title = element_blank(), legend.position = leg.pos, legend.direction=leg.orient, legend.background = element_rect(fill="white"))+
  coord_fixed()



ismp.lt40 <-subset(sam$cmt.ppp, lat>lims[3] & lat<lims[4] & long>lims[1] & long<lims[2] & depth< dept    ) %>% getlines(  scale=.5,  segment=T, norm=T)

p.P.lt <- ptemp.cont +
  geom_polygon(data=bbox.poly, aes(x,y),fill=NA, col="white", lwd=2 )+
  geom_point(data=ismp.lt40, aes(x=x,y=y,  colour=plunge, shape=regime),size=2, lwd=.5 , show_guide=F ) +
  geom_segment(data=ismp.lt40, aes(x=x,xend=xend ,y=y,yend=yend),colour="white", lwd=.75 , lineend="round" ,arrow = arrow(length = unit(0.01, "npc")))+
  geom_segment(data=ismp.lt40, aes(x=x,xend=xend ,y=y,yend=yend, colour=plunge), lwd=.5 , lineend="round" ,arrow = arrow(length = unit(0.01, "npc")))+
  scale_colour_gradientn(colours=plunge.cols, limits=c(0,90)) +
  labs(title = paste("b) CMT P-axes, depths <",depth, "kms"))+
  theme(legend.title = element_blank(), legend.position = leg.pos, legend.direction=leg.orient, legend.background = element_rect(fill="white"))+
  coord_fixed()




ismp  <-subset(sam$cmt.ppp, lat>lims[3] & lat<lims[4] & long>lims[1] & long<lims[2]    ) %>% getlines(  scale=.5,  segment=T, norm=T)
p.P <- ptemp.cont +
  geom_polygon(data=bbox.poly, aes(x,y),fill=NA, col="white", lwd=2 )+
  geom_point(data=ismp, aes(x=x,y=y,  colour=plunge, shape=regime),size=2, lwd=.5 , show_guide=F ) +
  geom_segment(data=ismp, aes(x=x,xend=xend ,y=y,yend=yend), colour="white", lwd=.75 , lineend="round" ,arrow = arrow(length = unit(0.01, "npc")))+
  geom_segment(data=ismp, aes(x=x,xend=xend ,y=y,yend=yend, colour=plunge), lwd=.5 , lineend="round" ,arrow = arrow(length = unit(0.01, "npc")))+
  scale_colour_gradientn(colours=plunge.cols, limits=c(0,90)) +
  labs(title = "a) CMT P-axes")+
  theme(legend.title = element_blank(), legend.position = leg.pos, legend.direction=leg.orient, legend.background = element_rect(fill="white"))+
  coord_fixed()




p.T.depths <-gridExtra::grid.arrange(p.P.gt, p.P.lt, ncol=ncol)
ggsave(paste0(name,"_cmt_P_depth_norm.png"), p.T.depths, width=plot.dim[1], height=plot.dim[2])


p.TP <-gridExtra::grid.arrange(p.T,p.P+ labs(title="b) CMT P-axes"), ncol=ncol)
ggsave(paste0(name,"_cmt_TP_norm.png"), p.TP, width=plot.dim[1], height=plot.dim[2])



#ggsave("mex_cmt_depths_Paxes_col_dip_norm.png", pax.n, width=plot.dim[1], height=plot.dim[2])

#sam$cmt


