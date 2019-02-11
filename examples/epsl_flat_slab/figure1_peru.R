library(tidyverse)
library(raster)
library(spatstat)
library(maptools)
library(slab)
library(sf)
source('~/Dropbox/msandifo/documents/programming/r/packages/slab/R/misc.R')
source('~/Dropbox/msandifo/documents/programming/r/packages/slab/R/funcs.R')
#########



fig.dir="/Users/msandifo/Dropbox/msandifo/documents/programming/r/packages/slab/examples/epsl_flat_slab/figures/"
# mex <-assemble_slab(slab.name="mex", extend.lims = c(-1, -7, 4, 1),  simplify=F)
# mex <-assemble_slab(slab.name="mex", extend.lims = c(-11, -5, 4, 12),  simplify=F)
peru <-assemble_slab(slab.name="sam", extend.lims = c(-8, -6, 25, 0),  simplify=F)
plates= peru$plates.ppp[peru$plates.ppp$marks %in% c("57", "54", "123") ]
coasts.df <- peru$coasts %>% fortify()
dplyr::select(peru$vo.ppp$marks, starts_with("lon"), starts_with("lat") )
proj.laea <- "+proj=laea +lat_0=0 +lon_0=-97"

peru$extent <- extent(peru$slab)
peru.sig <- read.delim("/Users/msandifo/Dropbox/data/global/quakes/ngdc/signif.txt.tsv") %>%
  subset(LONGITUDE > peru$extent[1] & LONGITUDE < peru$extent[2] & LATITUDE > peru$extent[3] & LATITUDE<peru$extent[4])
peru.sig$names <- " "
peru.sig$names[str_detect(peru.sig$LOCATION_NAME, "MICHOACAN")] <- "MICHOACAN, "
peru.sig$names[str_detect(peru.sig$LOCATION_NAME, "PUEBLA") & peru.sig$YEAR==2017] <- "PUEBLA, "
peru.sig$names[str_detect(peru.sig$LOCATION_NAME, "CHIAPAS")] <- "CHIAPAS, "

peru.sig$deaths=0
peru.sig$deaths[!is.na(peru.sig$TOTAL_DEATHS)]="1"
peru.sig$deaths[is.na(peru.sig$TOTAL_DEATHS)]="0"


peru.sig$ll = peru.sig$LONGITUDE/3 +2*peru.sig$LATITUDE

source('~/Dropbox/msandifo/documents/programming/r/2018/misc/ggteeth.R')
barb.scale= 1.5
barb.n=16
boundaries <-  c("57" ,"239" ) #, 238, 239)
bird.plates.fn <- "/Volumes/data/data/global/polygons/tectonicplates-master/PB2002_boundaries.shp"
bird.plates = readShapeLines(bird.plates.fn) %>% crop(extent(peru$slab)) %>% fortify()
bird.plates.psp =readShapeLines(bird.plates.fn) %>% crop(extent(peru$slab)) %>% as.psp()
message("Bird plate boudary ids : " , bird.plates$id %>%  unique  %>% str_c( collapse = ", "))
ggthrust( (bird.plates %>% subset(id  %in% boundaries ))[,1:2],  h =5, N=barb.n, REV = T, endtol=0.01, scale=barb.scale  )->teeth.s


#cmt.ppp <-cmt.unpruned.ppp <- project_ppp( peru$cmt.ppp, plates, profile="p0")

#cmt.ppp <-cmt.unpruned.ppp$marks<-peru$cmt.ppp

# length(cmt.ppp$marks$lat)
# anyDuplicated(cmt.ppp[p2], rule="deldir")
peru$cmt.ppp$marks<- peru$cmt.ppp$marks %>% mutate(date = as.POSIXct(date), m = pmax(mb,ms)) #%>% distinct(eventname, .keep_all=T)

peru$cmt.ppp <- peru$cmt.ppp %>% subset( !duplicated(eventname))


peru$ehb.ppp$marks$m <- pmax(peru$ehb.ppp$marks$mw, peru$ehb.ppp$marks$ms, peru$ehb.ppp$marks$mb)
# win=list(x=c(-97.3, -100.4, -98.7, -95.9 , -97.3 ) %>% rev() ,
#         y=c(14.67,  15.8,   19.9,  18.85 ,  14.67) %>% rev())

# win=list(x=c(-97.3, -100.9, -99.2, -95.9 , -97.3 ) %>% rev() ,
#          y=c(14.67,  16,   20.1,  18.85 ,  14.67) %>% rev())
# p2n <- pwin( peru$cmt.ppp,
#              win=get_poly(long=mean(-97.3, -100.4), lat=mean(14.67,  15.8), angle=16, width=3.5, length=5 ,owin=F),
#             n=T)
p2=get_poly(long=mean(c(-97.3, -100.3)), lat=mean(c(14.67,  15.8)), angle=16.7, width=3.2, length=5 )

poly_dims(p2)
l2 =
  get_line(long=mean(c(-97.3, -100.3)), lat=mean(c(14.67,  15.8)), angle=16.7, length=5,  n=1, psp=T)


point2 <- crossing.psp(l2, bird.plates.psp[p2]) #get crosing point of

p2n <-  p2 %>% complement.owin(ras_owin(peru$slab$topo) )
win <- owin_poly(p2)
# p2 <- pwin( peru$cmt.ppp, win=win)
# p2n <- pwin( peru$cmt.ppp, win=win, n=T)
p2a<-split_owin(bird.plates.psp[p2], p2)
poly_dims(p2a)
#purrr::map_dfr( c(34:35, c(3:5)  ), join_isc) %>%  isc_ppp() -> isc.ppp

download=F
if (download ==TRUE)   isc.ppp <- get_isc_srn_rect(year=1960, min.mag=2.5) %>%
  get_this_isc(csv=T) %>%
  isc_ppp()

isc.ppp <- "/Volumes/data/data/global/quakes/isc/tmp/1960_mm2.5mtMSrect-105:-88:12.5:20.csv" %>%
  get_this_isc(csv=T) %>%
  isc_ppp()

project_ppp1(isc.ppp[p2], p2) -> isc1.ppp
isc1.ppp[p2a]$marks$distance <- -abs(  isc1.ppp[p2a]$marks$distance)
isc1.ppp
# plot(isc1.ppp[p2]$marks$distance, -isc1.ppp[p2]$marks$depth )
#
#
# plot(ras_owin(peru$slab$topo))
# points(isc1.ppp[p2] %>% subset( date>=lubridate::ymd("2013-01-01") & m>5), col="red" )


#purrr::map(get_ieb_files(), get_ieb) %>% bind_rows() -> ieb3.df



deps <-raster::extract(peru$slab$depth, l2 %>% as.SpatialLines.psp(), df=F, cellnumbers=T, method="bilinear") %>% as.data.frame()
xys <-xyFromCell(peru$slab$depth, deps$cell)

depths <- cbind(xys, deps)

depths$distance <- geosphere::distGeo( point2 %>% as.SpatialPoints.ppp(), depths[,1:2])/1000
depths.ppp<- as.ppp(depths[, 1:2],   W=p2)
depths.ppp$marks <- depths
names(depths.ppp$marks) <- c("long", "lat", "cell", "depth", "distance")
#ndis <- -depths.ppp[p2a]$marks$distance



###note this change
# depths.ppp[p2a]$marks$distance  <-    (-depths.ppp[p2a]$marks$distance)

### to this???
setmarks( depths.ppp[p2a], -depths.ppp[p2a]$marks$distance)

project_ppp1( peru$cmt.ppp, p2) -> cmt1.ppp

# cmt1.ppp[p2]$marks$pa[cmt1.ppp[p2]$marks$distance > 120]
#
#
# cmt1.ppp[p2]$marks$tp[cmt1.ppp[p2]$marks$distance > 120]
# cmt1.ppp[p2]$marks$t[cmt1.ppp[p2]$marks$distance > 120]

cmt1.ppp[p2a]$marks$distance <- -abs( cmt1.ppp[p2a]$marks$distance)
#cmt1.ppp<-cmt1.ppp[duplicated( cmt1.ppp$marks$eventname)]
project_ppp1( peru$ehb.ppp, p2) -> ehb1.ppp
ehb1.ppp[p2a]$marks$distance <- -abs( ehb1.ppp[p2a]$marks$distance)
project_ppp1( peru$vo.ppp, p2) -> vo1.ppp


# plot(p2n,add=F )
# plot(cmt1.ppp, which.marks="distance",pch=5, add=T, col='red')
# plot(ehb1.ppp, which.marks="distance",pch=5, add=T, col='green')
# plot(cmt1.ppp[p2n], which.marks="m", add=T )
# plot(bird.plates.psp[p2] , add=T)
# plot(bird.plates.psp[p2n] , add=T, col="orange")
# #plot(cmt1.ppp.proj$Xproj[ p2], which.marks="m", add=T, col="darkgreen")
# points(ehb1.ppp[p2n], which.marks=NA, add=T, col="blue", pch=2, size=.6)
# points(vo1.ppp[p2n], which.marks=NA, add=T, col="yellow", pch=24, size=.6)
# points(vo1.ppp[p2], which.marks=NA, add=T, col="grey20", pch=24, size=.6)

# ggplot(ehb1.ppp[p2]$marks, aes(distance, -depth, size=m))+geom_point(shape=25)+
#   geom_point(data=cmt1.ppp[p2]$marks, shape=24, col="orange")
# lines(bird.plates, col="darkgreen")
#

cmt1.ppp[p2]$marks$platesii <- split_ppp(cmt1.ppp[p2]$marks)
cmt1.ppp[p2]$marks$distance[cmt1.ppp[p2]$marks$plates=="0"] <- -cmt1.ppp[p2]$marks$distance

# plot(p2)
# plot(cmt1.ppp[p2][cmt1.ppp[p2]$marks$plates=="0"], which.marks="distance", add=T, size=.1)
# plot(bird.plates.psp[p2] , add=T)
# plot(cmt1.ppp[cmt1.ppp[p2]$marks$distance>=0], which.marks="distance", add=T,col="red")
#
# plot(cmt1.ppp[p2]$marks$distance,cmt1.ppp[p2]$marks$bearing)

#lines(teeth.s, col="darkgreen")


#
#
#
# ehb.ppp <- project_ppp( peru$ehb.ppp, plates, profile="p0")
# ehb.ppp$marks$profile <-"p0"
# ehb.ppp[ p2]$marks$profile  <-"p2"
#
# vo.ppp <-  project_ppp( peru$vo.ppp, plates, profile="p0" )
# vo.ppp$marks$profile <-"p0"
# vo.ppp[ p2]$marks$profile  <-"p2"
#
#
# cmt.ppp$marks$pro.bearing <- 0
#
#
# cmt.ppp[p2]$marks$pro.bearing <-
#   bearing(cmt.ppp[p2]$marks[c("pro.long","pro.lat")],
#           cmt.ppp[p2]$marks[ c("long","lat")] ) %>%
#   median()
#
#
# cmt.ppp[p2]$marks$pro.lat
# #cmt.ppp$marks %>% group_by(profile) %>% summarise(bearing= pro.bearing[1])
#
# pro.bearings<-cmt.ppp$marks %>% group_by(profile) %>% dplyr::summarise(pro.bearing=median(pro.bearing))
#
#
# c(97.7, 16.8, (newLonLat(97.7, 16.8,pro.bearings$pro.bearing[2], 500 ))[1:2])
#

(p.sig.mex<-  ggplot( ) +
    ggalt::geom_cartogram( data=coasts.df, aes(map_id=id ), map=coasts.df , fill="grey90" ) +

    ggalt::coord_proj(proj.laea, xlim=extent(peru$slab)[1:2],ylim=extent(peru$slab)[3:4]) +
 #   geom_polygon(data=peru$borders%>% fortify(), aes(long, lat, group=group), fill="white",show.legend = F, alpha=.4, col="grey90", size=.2)+
    # geom_polygon(data=peru$borders %>% #subset(NAME =="Mexico") %>%
    #                fortify() , aes(long, lat, group=group),  fill="grey80", show.legend = F, alpha=.4, col="grey90", size=.2)+
    geom_point(data=peru$vo.ppp$marks, aes(long, lat), shape =24,  size=3, col="grey30", fill="Yellow") +
    geom_path(data=bird.plates %>% subset(id %in% boundaries)  , aes(x=long, y=lat, group=id), colour="darkgreen", lwd=.5)+
    geom_path(data=bird.plates   , aes(x=long, y=lat, group=id), colour="darkgreen", lwd=.5)+
 #   geom_path(data= mc.rad, aes(x=long, y=lat, linetype= factor(km)), colour="red2", lwd=.5,  show.legend = F)+
    geom_polygon(data=teeth.s, aes(x,y, group=id), color="darkgreen", fill="darkgreen")+
    #geom_path(data=plates%>% fortify(), aes(long, lat ), show.legend = F, alpha=.4)+

    # geom_path(data=coasts.df   ,
    #              aes(x=long-360, y=lat , group=group ), size=0,  alpha=0, col="white", fill="white" , show.legend = F) +
    # geom_point(data=peru.sig %>% subset(YEAR<=2016)  , aes(x=LONGITUDE, y=LATITUDE,  size= log10(DEATHS)),  stroke=.5, show.legend = F, shape=5) +
    # geom_point(data=peru.sig %>% subset(YEAR>2016)  , aes(x=LONGITUDE, y=LATITUDE,  size= log10(DEATHS)),  stroke=1., colour="red2",show.legend = F, shape=5) +
    # geom_point(data=peru.city   , aes(x=long, y=lat),  size=5, stroke=.5, show.legend = F, shape=24, fill="yellow") +
    scale_radius(range=c(1,12)) +
    scale_linetype_manual(values=c("dashed", "longdash"))+


    labs(x=NULL, y=NULL)  +  scale_y_continuous(labels = degreesN)+scale_x_continuous(labels = degreesE)+
    annotate("text", x=-101, y= 14.5, label="Cocos Plate", size=4.5)+
    annotate("text", x=-106.5, y= 14.5, label="Pacific\nPlate", size=4.5)+
    annotate("text", x=-92, y= 18.2, label="North American\nPlate", size=4.5)+
    # annotate("text", x=-88.15, y= 21., label="NGDC singnificant\nearthquake database", size=3.5, hjust=1)+
    annotate("text", x=-96, y= 14.85, label="MAT", size=4.5, col=  "darkgreen") #+
   # annotate("text", x=peru.city$long, y= peru.city$lat, label="MC", size=4, col="darkred")
    # annotate("text", x=-94.75,y=19.45, label="MFS", size=4)+
    # ggalt::geom_encircle(data=peru.sig %>%subset(LONGITUDE > -99 &LONGITUDE< -93 & LATITUDE <20 & LATITUDE>17.5 ), aes(LONGITUDE,LATITUDE),
    #                      s_shape=2., expand=0.04, linetype=2)+
    # ggrepel::geom_text_repel(data=peru.sig  %>% subset(DEATHS >=90  & LONGITUDE < -93 & ll < 4),aes(x=LONGITUDE, y=LATITUDE, label=paste0(names,"f=", DEATHS,"\n",month.abb[MONTH]," ",YEAR )),
    #                          size=3.,
    #                          point.padding=.05 ,
    #                          box.padding= .1,
    #                          min.segment.length=.5,
    #                          segment.size=.32, alpha=.6, col="blue4",
    #                          nudge_y= -6, nudge_x= 0,
    #                          force=1, max.iter=2000 )+
    # ggrepel::geom_text_repel(data=peru.sig  %>% subset(DEATHS >=90  & LONGITUDE < -93 & ll >= 4),aes(x=LONGITUDE, y=LATITUDE, label=paste0(names,"f=", DEATHS,"\n",month.abb[MONTH]," ",YEAR )),
    #                          size=3.,
    #                          point.padding=.05 ,
    #                          box.padding= .1,
    #                          min.segment.length=.5,
    #                          segment.size=.32, alpha=.6, col="blue4",
    #                          nudge_y=  3.2, nudge_x=  .5,
    #                          force=5, max.iter=2000 )



)


dev.off()
library(ggalt)
(p.join <-  p.sig.mex +

    geom_point(data=cmt1.ppp[p2n]$marks , # %>% subset(profile !="p0") ,
               aes(x=long, y=lat,   shape=regime), col="brown",
               stroke=1, size=1.5, show.legend=F, alpha=.5) +
    geom_point(data=cmt1.ppp[p2]$marks  %>% subset(profile =="p0") ,
               aes(x=long, y=lat,   shape=regime), col="orange",
               stroke=1, size=1.5, show.legend=T, alpha=1) +
    geom_point(data=ehb1.ppp$marks   ,
               aes(x=long, y=lat ),
               stroke=0, size=1, col="green4",show.legend=F) +
  #  geom_point(data=peru.sig %>% subset(YEAR<=2016)  , aes(x=LONGITUDE, y=LATITUDE,  size= log10(DEATHS)),  stroke=.75, show.legend = F, shape=5) +
  #  geom_point(data=peru.sig %>% subset(YEAR>2016)  , aes(x=LONGITUDE, y=LATITUDE,  size= log10(DEATHS)),  stroke=1.25, colour="red2",show.legend = F, shape=5) +

    # geom_point(data=cmt.ppp$marks %>% subset(profile == "p0") ,
    #            aes(lon, lat, size=depth.anom,   shape=profile),
    #            col="grey60",stroke=1, size=2, show.legend = F)+
    #  geom_point(data=peru.city   , aes(x=long, y=lat),  size=5, stroke=.5, show.legend = F, shape=24, fill="orange") +
    # geom_point(data=peru.city_0  %>% subset(profile !="pa" & (m>=6. )) %>%  unique(),
    #            aes(x=long, y=lat,  size=scalarmoment),size=5, shape=1, colour="white", stroke=1.5,show.legend = F)+
    # geom_point(data=peru.city_0 %>% subset(profile !="pa" & (m>=6.  ))%>%  unique(),
    #            aes(x=long, y=lat, size=scalarmoment),size=5, colour="black",shape=1,stroke=.75, show.legend = F)+
    geom_point(data=peru$vo.ppp$marks, aes(x=long, lat), shape =24,  size=3, col="grey30", fill="Yellow",show.legend=F)+
    geom_path(data= mc.rad, aes(x=long, y=lat, linetype= factor(km)), colour="red2", lwd=.25,  show.legend = F) +
    scale_linetype_manual(values=c("dashed", "longdash")) +
    # ggrepel::geom_label_repel(data=peru.city_0 %>% subset(profile !="pa" & (m>=6.  ) & ll < -15) %>%  unique() ,
    #                          aes(x=long, y=lat, label=paste0((date %>% as.Date) %>% as.character(),"\n",regime,", ",m, ", z=",depth)%>% str_replace_all("-","/")),
    #                          size=2.5, segment.size=.2,nudge_y=-2,nudge_x=-1, point.padding=1,label.padding=0.15, label.size= 0.1,label.alpha=.5) +
    #
    # ggrepel::geom_label_repel(data=peru.city_0 %>% subset(profile !="pa" & (m>=6.  ) & ll > -15) %>%  unique() ,
    #                           aes(x=long, y=lat, label=paste0((date %>% as.Date) %>% as.character(),"\n",regime,", ",m, ", z=",depth)%>% str_replace_all("-","/")),
    #                           size=2.5, segment.size=.2,nudge_y=2,nudge_x=1, point.padding=1,label.padding=0.15, label.size= 0.1,label.alpha=.5) +

   # annotate("text", x=peru.city$long, y= peru.city$lat, label="MC", size=4, col="darkred")+
    annotate("text", x=-94.75,y=19.25, label="MFS", size=4)+
    annotate("segment",  x=-95.25,y=19.05, xend=-94.75-1.25,yend=19.45-1.05,linetype=1, size=.25)+
    #  geom_path(aes(x=c(-97.5, -99.95, -98.2,-96.3,-97.5) , y=c(15.1, 16, 19.7,19.0,15.1)), linetype=1, size=.25,color="blue3")+
    geom_path(aes(x=win$x, y=win$y), linetype=1, size=.25,color="blue3")+
    annotate("text", x=-100,y=16, label="b ", size=4, colour="blue3")+
    ggalt::geom_encircle(data=ehb1.ppp$marks %>%subset((long > -100.2 & long< -95.9 & lat <20 & lat>17.7 & depth>45) |(long > -98. & long< -96 & lat <20 & lat>17.2 & depth>45) ) , aes(long,lat),
                         s_shape=2., expand=0.07, linetype=2) +
    labs(x=NULL, y=NULL)+
    scale_y_continuous(labels = degreesN)+scale_x_continuous(labels = degreesE)+
    guides(linetype="none", size="none")+theme(legend.position=c(.95,.8), legend.title = element_blank())
)

#ggsave(paste0(fig.dir,"/mexico.figure.1.pdf"), plot=p.join, width=12, height=7)


###

####
profile.bearing  <-cmt1.ppp$marks$bearing%%180 %>% median(na.rm =T)



pa<-project_axes1(cmt1.ppp[p2], scale=10, axis=
                    "P", bearing=profile.bearing)
ta<-project_axes1(cmt1.ppp[p2], scale=10, axis=
                    "T", bearing=profile.bearing)


ta  <- cbind(cmt1.ppp[p2]$marks, ta )
pa  <- cbind(cmt1.ppp[p2]$marks, pa )

vo1.ppp$marks$depth <-0

(p.1b <-ggplot()+
    geom_point(data=ehb1.ppp[p2]$marks %>% subset(profile== "p0"),
               aes(distance, -depth , size=m)    ,show.legend = F, alpha=.5) +
    geom_point(data=cmt1.ppp[p2]$marks %>% subset(  !is.na(depth.anom)    & scalarmoment < 13  & profile== "p0"),
               aes(distance, -depth, colour=regime, size=m),shape=1, stroke=1., show.legend = F) +
    geom_point(data=vo1.ppp[p2]$marks %>% subset(profile=="p0"),aes(distance, -depth), shape=24, size=2, fill="yellow", col="red3")+
    scale_radius(range=c(.5,5)) #+ scale_color_manual(values=c("red",  "green4", "blue", "green2","grey50"))
)

(p.1c <- p.1b +
    geom_segment(data=ta%>% subset((depth.anom> 0 | depth<70) & profile=="p0"),
                 aes(x=x,xend=xend,y= -y, yend= -yend, col=regime), size=.5)+
    # geom_segment(data=ta %>% subset(depth.anom> 0 | depth>40),  aes(x=x,xend=xend,y= -y, yend= -yend, col=regime))+
    coord_fixed(ratio = 1)+
    xlim(-100,400)+ theme(legend.position=c(.95,.45), legend.title = element_blank())+labs(x=NULL)

)


(p.2b <-ggplot()+
    hrbrthemes::theme_ipsum(axis = TRUE, ticks = F,grid=c("XY"),axis_text_size=14, axis_title_size=18)+
    geom_point(data=ehb1.ppp$marks %>% subset(profile== "p0"),
               aes( distance, -depth, size=m), show.legend = T, stroke=.5, alpha=.5, shape=1, fill="white") +
    geom_point(data=cmt1.ppp$marks %>% subset(  profile== "p0"),
               aes (distance, -depth, colour=regime, size=m), shape=1, stroke=.5, show.legend = F) +
    geom_point(data=vo1.ppp$marks %>% subset(profile=="p0" ),
               aes( distance, -depth), shape=24, size=2, fill="yellow", col="red3")+
    scale_radius(range=c(.5,6)) #+ scale_color_manual(values=c("red",  "green4", "blue", "green2","grey50"))
)

# peru.slab.pro <- rbind( data.frame(x=c(0, 10, 55, 110, 150,   200,  260, 400), y= -c(0, 1,12, 38,43, 47,50,   200), profile="p1"),
#                        data.frame(x=c(0, 10, 55, 110, 150,   250,  325, 360), y= -c(5, 6,16, 38,43, 45,47,   75), profile="p2"),
#                        data.frame(x=c(0, 10, 55, 120, 170,   250,  320, 550), y= -c(0, 1,12, 35,55, 100, 130, 300), profile="p3"),
#                        data.frame(x=c(0, 10, 55, 110, 150,   200,  290, 360)*1.1, y= -c(0, 2,18, 50,80, 130, 230, 300), profile="p4"))

# (p.2c <- p.2b +
#     geom_segment(data=ta%>% subset(   profile=="p0"), aes(x=x,xend=xend,y= -y, yend= -yend, col=regime), size=.55)+
#     # geom_segment(data=ta %>% subset(depth.anom> 0 | depth>40),  aes(x=x,xend=xend,y= -y, yend= -yend, col=regime))+
#    # coord_fixed(ratio = 1.3)+
#     xlim(-10,400)+
#     theme_update(panel.grid.minor =element_blank(), panel.grid.major =element_line(colour = "grey85", size=.065, linetype=1))+
#     theme(legend.position=c(.95,.65), legend.title = element_blank()) +
#     labs(x=NULL, y="depth - kms")+
#     ggalt::geom_xspline(data=peru.slab.pro %>% subset(profile=="p2"), aes(x,y),  size=.35, linetype=5, spline_shape=1)
#
# )

offset=510
#offset=513
# offset200=580
# offset100=560

mod <-"/Users/msandifo/Dropbox/msandifo/documents/programming/r/packages/slab/examples/nature_figs/mod1/"
source(paste0(mod,"offset.r"))
mod1 <-read.csv(paste0(mod,"slabTop.csv"), col.names=c("x", "depth")) %>%
  subset(x>= -offset-100 )%>%
  dplyr::mutate(x= x+offset, depth = - depth )#((depth-50)*1 +50))
# mod100 <-read.csv(paste0(mod,"slabTop.csv", col.names=c("x", "depth")) %>%
#   subset(x>= -offset100-100)%>%
#   mutate(x= x+offset100, depth = -depth)
# mod200 <-read.csv("/Users/msandifo/Dropbox/msandifo/documents/programming/r/packages/slab/examples/slabTop200.csv", col.names=c("x", "depth")) %>%
#   subset(x>= -offset200-100)%>%
#   mutate(x= x+offset200, depth = -depth)

mid1 <-read.csv(paste0(mod,"midPlane.csv"),
                col.names=c("x", "depth")) %>%
  subset(x>= -offset -100) %>%
  mutate(x= x+offset, depth = -depth)
# mid100 <-read.csv("/Users/msandifo/Dropbox/msandifo/documents/programming/r/packages/slab/examples/midPlane100.csv",
#                 col.names=c("x", "depth")) %>%
#   subset(x>= -offset100-100)%>%
#   mutate(x= x+offset100, depth = -depth)
# mid200 <-read.csv("/Users/msandifo/Dropbox/msandifo/documents/programming/r/packages/slab/examples/midPlane200.csv",
#                  col.names=c("x", "depth")) %>%
#   subset(x>= -offset200-100)%>%
#   mutate(x= x+offset200, depth = -depth)

c600 <-read.csv(paste0(mod,"600C_contour.csv"),
                col.names=c("x", "depth")) %>%
  mutate(x= x) %>%
  #subset(x>= -offset -100)%>%
  mutate(x= x+offset, depth = -depth)
c650 <-read.csv(paste0(mod,"650C_contour.csv"),
                col.names=c("x", "depth")) %>%
  mutate(x= x) %>%
  #subset(x>= -offset -100)%>%
  mutate(x= x+offset, depth = -depth)
c700 <-read.csv(paste0(mod,"700C_contour.csv"),
                col.names=c("x", "depth")) %>%
  mutate(x= x) %>%
  #subset(x>= -offset -100)%>%
  mutate(x= x+offset, depth = -depth)


# need to run mex_kernel for p.kernel.cmt

mod.fac = 1
(p.2c <- p.2b +
    geom_segment(data=ta%>% subset(   profile=="p0" ), aes(x=x,xend=xend,y= -y, yend= -yend, col=regime), size=.55)+
    # geom_segment(data=ta %>% subset(depth.anom> 0 | depth>40),  aes(x=x,xend=xend,y= -y, yend= -yend, col=regime))+
    # coord_fixed(ratio = 1.3)+
    xlim(-100,400)+
    theme_update(panel.grid.minor =element_blank(), panel.grid.major =element_line(colour = "grey85", size=.065, linetype=1))+
    theme(legend.position=c(.95,.65), legend.title = element_blank()) +
    labs(x=NULL, y="depth - kms")+
    geom_line(data=mod1 , aes(x,depth ),  size=.35, linetype=1, col="blue3")+
    geom_path(data=depths.ppp$marks[  seq(1,length(depths.ppp$marks$distance), 10),] %>% subset(!is.na(depth)), aes(distance, depth), col="red3", linetype=1)+
    geom_line(data=mid1 , aes(x ,depth  ),  size=.35, linetype=3, col="blue3")+
    geom_line(data=mod1 , aes(x*mod.fac ,depth*mod.fac  ),  size=.35, linetype=5, col="blue3")+
    geom_path(data=c600 , aes(x , depth  ),  size=.35, linetype=5, col="orange") +#+
    geom_path(data=c650 , aes(x , depth  ),  size=.35, linetype=5, col="orange3") +#+
    geom_path(data=c700 , aes(x , depth  ),  size=.35, linetype=5, col="brown") #+
  # ggalt::geom_xspline(data=peru.slab.pro %>% subset(profile=="p2"), aes(x,y),  size=.35, linetype=5, spline_shape=1)

)


(p.2ca <- p.2b +
    geom_segment(data=ta%>% subset(   profile=="p0" ), aes(x=x,xend=xend,y= -y, yend= -yend, col=regime), size=.55)+
    # geom_segment(data=ta %>% subset(depth.anom> 0 | depth>40),  aes(x=x,xend=xend,y= -y, yend= -yend, col=regime))+
    # coord_fixed(ratio = 1.3)+
    xlim(-100,400)+
    theme_update(panel.grid.minor =element_blank(), panel.grid.major =element_line(colour = "grey85", size=.065, linetype=1))+
    theme(legend.position=c(.95,.65), legend.title = element_blank()) +
    labs(x=NULL, y="depth - kms")+
    # geom_line(data=mod1 , aes(x,depth ),  size=.35, linetype=1, col="blue3")+
    geom_line(data=mod1 , aes(x,depth ),  size=.45, linetype=1, col="blue3" )+
    #  geom_line(data=mod100 , aes(x,depth+4 ),  size=.35, linetype=5, col="blue3")+
    geom_path(data=depths.ppp$marks[  seq(1,length(depths.ppp$marks$distance), 10),] %>% subset(!is.na(depth)), aes(distance, depth), col="red3", linetype=5)+
    geom_line(data=mid1 , aes(x ,depth  ),  size=.35, linetype=3, col="blue3")+
    #  geom_line(data=mod1 , aes(x*mod.fac ,depth*mod.fac  ),  size=.35, linetype=5, col="blue3")+
    geom_path(data=c600 , aes(x , depth  ),  size=.25, linetype=5, col="orange") +#+
    # geom_path(data=c650 , aes(x , depth  ),  size=.1, linetype=5, col="orange3") +#+
    geom_path(data=c700 , aes(x , depth  ),  size=.25, linetype=5, col="brown") +#+
    ylim(c(-100,3))+ xlim(c(-100,500))#+
  # ggalt::geom_xspline(data=peru.slab.pro %>% subset(profile=="p2"), aes(x,y),  size=.35, linetype=5, spline_shape=1)

)

curvGrad <-read.csv(paste0(mod,"curvGrad.csv"), col.names=c("x", "cg")) %>%
  subset(x>= -offset -100)%>%
  mutate(x= x+offset)
# curvGrad100 <-read.csv("/Users/msandifo/Dropbox/msandifo/documents/programming/r/packages/slab/examples/curvGrad100.csv", col.names=c("x", "cg")) %>%
#   subset(x>= -offset100-200)%>%
#   mutate(x= x+offset100)
# curvGrad200 <-read.csv("/Users/msandifo/Dropbox/msandifo/documents/programming/r/packages/slab/examples/curvGrad200.csv", col.names=c("x", "cg")) %>%
#   subset(x>= -offset200-200)%>%
#   mutate(x= x+offset200)


( p.1<-gridExtra::grid.arrange( p.join,
                                p.2c +ylim(c(-100,0)) ,
                                ncol=1, heights=c(4.9,4.3 ) )
)



cowplot::plot_grid(p.join,p.2c+ylim(c(-100,0)), labels = c("a", "b"), nrow = 2, align = "v", rel_heights=c(1,.5), label_fontface="plain", label_size=18)


mm <- 6.0


cmt1.ppp$marks$m[ !is.na(cut(cmt1.ppp$marks$m, c( mm,10)))]


(p.kernel.cmt.1<-plot_kernel(cmt1.ppp$marks %>% subset(profile =="p0"  &
                                                         (depth.anom> -0 | is.na(depth.anom) | depth > 0)), min=5, cuts= c( mm,10), points=T, bw="SJ", kernel="cosine",
                             leg=c(.7,.95), mscale=3.5, xlim=c(-110,400 ), ylim=c(0,0.0155) )+
    #  scale_x_continuous(lim=c(0,350), expand=c(0,0))+
    # annotate("text", 40, .0135, label="wedge", size=4.5)+
    annotate("text", 90, .015, label="megathrust", size=4.)+
    annotate("text", 280, .015, label="MFS", size=4.)+ guides(fill="none")+theme(legend.direction = "horizontal")+
    geom_hline(yintercept= (mm-5)/ (   (1000/4.5)+4), linetype=1, size=.2)+
    geom_line(data=curvGrad, (aes(x*1, abs(cg*100))), col="blue3", size=.3, linetype=5)


)

curvGrad$cgP<- curvGrad$cg
curvGrad$cgP[curvGrad$cg<0] <-NA
curvGrad$cgM<- curvGrad$cg
curvGrad$cgM[curvGrad$cg>0] <-NA

(p.kernel.cmt.2<-plot_kernel(cmt1.ppp$marks %>% subset(profile =="p0"  & (depth.anom >= -0 | is.na(depth.anom) | depth >= 40)),
                             min=5,cuts= c( mm,10), points=T, bw="SJ", kernel="gaussian", mlimit=5, stroke=.5,size=.3,
                             leg=c(.7,.95), mscale=4.5, xlim=c(-110,400 ), ylim=c(0,0.015), regime=T, alpha=.15)+
    #  geom_density( data=ehb1.ppp$marks %>% subset(profile =="p0"  &  m>5.9 &(depth.anom >= -0 | is.na(depth.anom) | depth >= 40)), aes(distance), col="red", linetype=2)+
    #  scale_x_continuous(lim=c(0,350), expand=c(0,0))+
    # annotate("text", 40, .0135, label="wedge", size=4.5)+
    annotate("text", -60, .0145, label="outer rise", size=4.)+
    annotate("text", 9, .0145, label="trench", size=4.)+
    annotate("text", 75, .0145, label="megathrust", size=4.)+
    annotate("text", 260, .0145, label="MFS", size=4.)+
    guides(fill="none")+theme(legend.direction = "horizontal")+
    annotate("text", 165, .0145,label="seismic gap", size=4.)+ guides(fill="none")+theme(legend.direction = "horizontal")+
    #  geom_hline(yintercept= (mm-5)/ (   (1000/4.5)+4), linetype=1, size=.2)+
    #  geom_line(data=curvGrad1, (aes(x  , abs(cg*60))), col="blue3", size=.2, linetype=1)+scale_size(range=c(.5,6))+
    #geom_line(data=curvGrad1, (aes(x  , abs(cg*160))), col="blue3", size=.4, linetype=1)+
    geom_line(data=curvGrad, (aes(x  , abs(cgP*80))), col="blue3", size=.7, linetype=1)+
    geom_line(data=curvGrad, (aes(x  , abs(cgM*80))), col="red3", size=.7, linetype=1)+
    #  geom_line(data=curvGrad100, (aes(x  , abs(cg*80))), col="blue3", size=.4, linetype=5)+
    scale_size(range=c(.5,6))


)

p.kernel.cmt.3 <-p.kernel.cmt.2+
  geom_point(data= cmt1.ppp$marks %>% subset(profile =="p0"  & depth.anom < 0 & distance<120  ),
             aes(x=  distance, y = (m-5)/(1000/4.5)  ),
             size=1.5,show.legend=F, alpha=.5,shape=2, stroke=.35)+
  annotate("text",  311, .0105, label="Puebla", size=4, col="blue4")+
  geom_point(data=vo1.ppp$marks %>% subset(profile=="p0" ),aes( distance, .0145), shape=24, size=2, fill="yellow", col="red3")


p.kernel.cmt.3

plot.combo1.b <-cowplot::plot_grid(p.join+
                                     theme( legend.background=element_blank()),
                                   p.2ca +
                                     scale_x_continuous(expand = c(0,0),  limits=c(-100,400))+
                                     theme(legend.direction = "vertical" , legend.position=c(.1,.22), legend.background=element_blank(), legend.box = 'horizontal')+
                                     annotate("segment", x=0, xend=10,  y= -50, yend= -50, linetype=1, size=.2, alpha=.5)+
                                     annotate("segment", x=0, xend=10,  y= -75, yend= -75, linetype=1, size=.2, alpha=.5)+
                                     scale_y_continuous(expand = c(0,0),  limits=c(-105,8))+
                                     annotate("text",  311, -48, label="Puebla", size=4, col="blue4")+
                                     geom_line(data= data.frame(x=c(290,307),y=c(-56, -73)), #arrow=arrow(length=unit(0.30,"cm")),
                                               aes(x=x, y=y), size=1.3, col="red4", linetype=1),
                                   p.kernel.cmt.3+
                                     theme_update(panel.grid.minor =element_blank(), panel.grid.major =element_line(colour = "grey85", size=.065, linetype=1))+
                                     guides(fill="none")+
                                     theme(legend.direction = "vertical" , legend.position=c(.1,.71), legend.title = element_blank(),  legend.box = 'horizontal')+
                                     labs(x=NULL, y="kernel density")+
                                     scale_x_continuous(expand = c(0,0),  limits=c(-100,400))+
                                     scale_fill_manual(values ="grey80"), labels = c("a", "b", "c"), nrow = 3, align = "v", rel_heights=c(1,.8,.8), label_fontface="plain", label_size=18)

 plot.combo1.b
#dev.off()
#ggsave(paste0(fig.dir,"/mexico.figure.1b_.pdf"),   plot=plot.combo1.b,width=9, height=10)
ggsave(paste0(fig.dir,"fig1.pdf"),   plot=plot.combo1.b,width=9, height=10)


# source('~/Dropbox/transfers/code snip/ieb.3.R')
# library(ncdf4)
# purrr::map(get_ieb_files(), get_ieb) %>% bind_rows() -> ieb3.df
#
# head(ieb3.df)
# map_dfr( c(34:35, c(3:8)  ),join_isc) -> isc.df
# ggplot()+geom_point(data=ieb3.df, aes(lon, lat))

