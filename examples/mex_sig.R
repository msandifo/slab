fig.dir="/Users/msandifo/Dropbox/msandifo/documents/programming/r/packages/slab/examples/figures"
 mex <-assemble_slab(slab.name="mex", extend.lims = c(-1, -7, 4, 1),  simplify=F)
 mex <-assemble_slab(slab.name="mex", extend.lims = c(-11, -5, 4, 12),  simplify=F)
 mex <-assemble_slab(slab.name="mex", extend.lims = c(-2, -7, 5.5, .5),  simplify=F)

plates= mex$plates.ppp[mex$plates.ppp$marks %in% c("57", "54", "123") ]
coasts.df <- mex$coast%>% fortify()
dplyr::select(mex$vo.ppp$marks, starts_with("lon"), starts_with("lat") )
proj.laea <- "+proj=laea +lat_0=0 +lon_0=-97"
mex.city <- data.frame( long= -99.13333, lat=19.43333 )
mc.rad <-rbind( plotCircle(mex.city[[1]] ,mex.city[[2]],125),  plotCircle(mex.city[[1]] ,mex.city[[2]],250))
mc.rad <-  plotCircle(mex.city[[1]] ,mex.city[[2]],125)

mex$extent <- extent(mex$slab)
mex.sig <- read.delim("/Users/msandifo/Dropbox/data/global/quakes/ngdc/signif.txt.tsv") %>%
  subset(LONGITUDE > mex$extent[1] & LONGITUDE < mex$extent[2] & LATITUDE > mex$extent[3] & LATITUDE<mex$extent[4])


mex.sig$deaths=0
mex.sig$deaths[!is.na(mex.sig$TOTAL_DEATHS)]="1"
mex.sig$deaths[is.na(mex.sig$TOTAL_DEATHS)]="0"
names(mex.sig)

source('~/Dropbox/msandifo/documents/programming/r/2018/misc/ggteeth.R')
barb.scale= 1.5
barb.n=16
boundaries <-  c("57" ,"239" ) #, 238, 239)
bird.plates.fn <- "/Volumes/data/data/global/polygons/tectonicplates-master/PB2002_boundaries.shp"
bird.plates = readShapeLines(bird.plates.fn) %>% crop(extent(mex$slab)) %>% fortify()
message("Bird plate boudary ids : " , bird.plates$id %>%  unique  %>% str_c( collapse = ", "))
ggthrust( (bird.plates %>% subset(id  %in% boundaries ))[,1:2],  h =5, N=barb.n, REV = T, endtol=0.01, scale=barb.scale  )->teeth.s

(p.sig.mex<-ggplot( ) +
    ggalt::geom_cartogram( data=coasts.df, aes(map_id=id ), map=coasts.df  ) +

    ggalt::coord_proj(proj.laea, xlim=extent(mex$slab)[1:2],ylim=extent(mex$slab)[3:4]) +
    geom_polygon(data=mex$borders%>% fortify(), aes(long, lat, group=group), fill="white",show.legend = F, alpha=.4, col="grey90", size=.2)+
    geom_polygon(data=mex$borders%>% subset(NAME =="Mexico") %>%fortify() , aes(long, lat, group=group),  fill="grey80", show.legend = F, alpha=.4, col="grey90", size=.2)+
    geom_point(data=mex$vo.ppp$marks, aes(long, lat), shape =24,  size=3, col="grey30", fill="Yellow") +
    geom_path(data=bird.plates %>% subset(id %in% boundaries)  , aes(x=long, y=lat, group=id), colour="darkgreen", lwd=.5)+
    geom_path(data=bird.plates   , aes(x=long, y=lat, group=id), colour="darkgreen", lwd=.5)+
     geom_path(data= mc.rad, aes(x=long, y=lat, linetype= factor(km)), colour="red2", lwd=.5,  show.legend = F)+
    geom_polygon(data=teeth.s, aes(x,y, group=id), color="darkgreen", fill="darkgreen")+
    #geom_path(data=plates%>% fortify(), aes(long, lat ), show.legend = F, alpha=.4)+

    # geom_path(data=coasts.df   ,
    #              aes(x=long-360, y=lat , group=group ), size=0,  alpha=0, col="white", fill="white" , show.legend = F) +
    geom_point(data=mex.sig %>% subset(YEAR<=2016)  , aes(x=LONGITUDE, y=LATITUDE,  size= log10(DEATHS)),  stroke=.5, show.legend = F, shape=5) +
    geom_point(data=mex.sig %>% subset(YEAR>2016)  , aes(x=LONGITUDE, y=LATITUDE,  size= log10(DEATHS)),  stroke=1., colour="red2",show.legend = F, shape=5) +
   # geom_point(data=mex.city   , aes(x=long, y=lat),  size=5, stroke=.5, show.legend = F, shape=24, fill="yellow") +
    scale_radius(range=c(1,12)) +
    scale_linetype_manual(values=c("dashed", "longdash"))+


    labs(x=NULL, y=NULL)  +  scale_y_continuous(labels = degreesN)+scale_x_continuous(labels = degreesE)+
    annotate("text", x=-100, y= 13.5, label="Cocos Plate", size=6)+
    annotate("text", x=-106.5, y= 14.5, label="Pacific\nPlate", size=6)+
    annotate("text", x=-101, y= 21.2, label="North American Plate", size=6)+
    annotate("text", x=-88.15, y= 21., label="NGDC singnificant\nearthquake database", size=3.5, hjust=1)+
    annotate("text", x=-95, y= 13.75, label="MAT", size=6, col=  "darkgreen")+
  annotate("text", x=mex.city$long, y= mex.city$lat, label="MC", size=4, col="darkred")+
    # annotate("text", x=-94.75,y=19.45, label="flat slab\nquakes", size=4)+
    # ggalt::geom_encircle(data=mex.sig %>%subset(LONGITUDE > -99 &LONGITUDE< -93 & LATITUDE <20 & LATITUDE>17.5 ), aes(LONGITUDE,LATITUDE),
    #                      s_shape=2., expand=0.04, linetype=2)+
     ggrepel::geom_text_repel(data=mex.sig  %>% subset(DEATHS >=90  & LONGITUDE < -93),aes(x=LONGITUDE, y=LATITUDE, label=paste0("f=", DEATHS,"\n",month.abb[MONTH]," ",YEAR )),
                             size=4.5, point.padding=.5 ,box.padding= 4.1,    segment.size=.32, alpha=.6, col="blue4",
                             force=7, max.iter=10000 )


)

#imports p.map.a from mex1.R

p.comb<-gridExtra::grid.arrange(p.sig.mex,
                        p.map.a+theme(legend.position = c(.4,.075), legend.direction = "horizontal")+
                          annotate("text", x=-88.15, y= 21., label="CMT catalog\nISC-EHB catalag", size=3.5, hjust=1)
                          , ncol=1)
ggsave(paste0(fig.dir,"/mexican.sig.quakes.mex1.pdf"), plot=p.comb,width=12, height=11)

# all cntral amaerica

mex <-assemble_slab(slab.name="mex", extend.lims = c(-3, 4, -4,6),  simplify=F)

plates= mex$plates.ppp[mex$plates.ppp$marks %in% c("57", "54", "123") ]
coasts.df <- mex$coast%>% fortify()
dplyr::select(mex$vo.ppp$marks, starts_with("lon"), starts_with("lat") )
proj.laea <- "+proj=laea +lat_0=0 +lon_0=-97"
mex.city <- data.frame( long= -99.13333, lat=19.43333 )
mex$extent <- extent(mex$slab)
mex.sig <- read.delim("/Users/msandifo/Dropbox/data/global/quakes/ngdc/signif.txt.tsv") %>%
  subset(LONGITUDE > mex$extent[1] & LONGITUDE < mex$extent[2] & LATITUDE > mex$extent[3] & LATITUDE<mex$extent[4])


mex.sig$deaths=0
mex.sig$deaths[!is.na(mex.sig$TOTAL_DEATHS)]="1"
mex.sig$deaths[is.na(mex.sig$TOTAL_DEATHS)]="0"
names(mex.sig)

source('~/Dropbox/msandifo/documents/programming/r/2018/misc/ggteeth.R')
barb.scale= 1.5
barb.n=16
boundaries <-  c("57", "54" ) #, 238, 239)
bird.plates.fn <- "/Volumes/data/data/global/polygons/tectonicplates-master/PB2002_boundaries.shp"
bird.plates = readShapeLines(bird.plates.fn) %>% crop(extent(mex$slab)) %>% fortify()
message("Bird plate boudary ids : " , bird.plates$id %>%  unique  %>% str_c( collapse = ", "))
#ggthrust( (bird.plates %>% subset(id  %in% boundaries ))[,1:2],  h =5, N=barb.n, REV = T, endtol=0.01, scale=barb.scale  )->teeth.s

(p.sig.ca <-ggplot( ) +
    ggalt::geom_cartogram( data=coasts.df, aes(map_id=id ), map=coasts.df  ) +
    ggalt::coord_proj(proj.laea, xlim=extent(mex$slab)[1:2],ylim=extent(mex$slab)[3:4]) +
    geom_polygon(data=mex$borders%>% fortify(), aes(long, lat, fill=group), show.legend = F, alpha=.4, col="grey90", size=.2)+
    geom_path(data=bird.plates %>% subset(id %in% boundaries)  , aes(x=long, y=lat, group=id), colour="darkgreen", lwd=.5)+
    geom_path(data=bird.plates   , aes(x=long, y=lat, group=id), colour="darkgreen", lwd=.5)+
    geom_path(data= mc.rad, aes(x=long, y=lat, linetype= factor(km)), colour="red2", lwd=.25,  show.legend = F)+
    #  geom_polygon(data=teeth.s, aes(x,y, group=id), color="darkgreen", fill="darkgreen")+
    #geom_path(data=plates%>% fortify(), aes(long, lat ), show.legend = F, alpha=.4)+

    # geom_path(data=coasts.df   ,
    #              aes(x=long-360, y=lat , group=group ), size=0,  alpha=0, col="white", fill="white" , show.legend = F) +
    geom_point(data=mex.sig   , aes(x=LONGITUDE, y=LATITUDE,  size= log10(DEATHS)),  stroke=.3, show.legend = F, shape=1) +
    geom_point(data=mex.city   , aes(x=long, y=lat),  size=5, stroke=.5, show.legend = F, shape=24, fill="yellow") +
    scale_radius(range=c(1,12)) +
    scale_linetype_manual(values=c("dashed", "longdash"))+

    ggrepel::geom_text_repel(data=mex.sig  %>% subset(DEATHS >=100 ),
                             aes(x=LONGITUDE, y=LATITUDE, label=paste0("f=", DEATHS,"\n",month.abb[MONTH]," ",YEAR )),
                             size=3.5, point.padding=.5 ,box.padding=3.53,    segment.size=.2, alpha=.6, col="blue4", force=5, max.iter=50 )+
    labs(x=NULL, y=NULL)  +  scale_y_continuous(labels = degreesN)+scale_x_continuous(labels = degreesE)
)



ggsave(paste0(fig.dir,"mexican.sig.quakes.pdf"), plot=p.sig.ca,width=12, height=8)



library(maps)
library(mapdata)#For the worldHires database
library(mapproj)#For the mapproject function
plotElipse <- function(x, y, r) {#Gary's function ;-)
  angles <- seq(0,2*pi,length.out=360)
  lines(r*cos(angles)+x,r*sin(angles)+y)
}
plotCircle <- function(LonDec, LatDec, Km, plot=F) {#Corrected function
  #LatDec = latitude in decimal degrees of the center of the circle
  #LonDec = longitude in decimal degrees
  #Km = radius of the circle in kilometers
  ER <- 6371 #Mean Earth radius in kilometers. Change this to 3959 and you will have your function working in miles.
  AngDeg <- seq(1:360) #angles in degrees
  Lat1Rad <- LatDec*(pi/180)#Latitude of the center of the circle in radians
  Lon1Rad <- LonDec*(pi/180)#Longitude of the center of the circle in radians
  AngRad <- AngDeg*(pi/180)#angles in radians
  Lat2Rad <-asin(sin(Lat1Rad)*cos(Km/ER)+cos(Lat1Rad)*sin(Km/ER)*cos(AngRad)) #Latitude of each point of the circle rearding to angle in radians
  Lon2Rad <- Lon1Rad+atan2(sin(AngRad)*sin(Km/ER)*cos(Lat1Rad),cos(Km/ER)-sin(Lat1Rad)*sin(Lat2Rad))#Longitude of each point of the circle rearding to angle in radians
  Lat2Deg <- Lat2Rad*(180/pi)#Latitude of each point of the circle rearding to angle in degrees (conversion of radians to degrees deg = rad*(180/pi) )
  Lon2Deg <- Lon2Rad*(180/pi)#Longitude of each point of the circle rearding to angle in degrees (conversion of radians to degrees deg = rad*(180/pi) )
 if (plot)  polygon(Lon2Deg,Lat2Deg,lty=2)
   data.frame(long=Lon2Deg, lat=Lat2Deg, km=Km)
}
# map("worldHires", region="mexico")#draw a map of Belgium (yes i am Belgian ;-)
# mex.pt <- mapproject(mex.city[1] ,mex.city[2])#coordinates of Bruxelles
# points(mex.pt,pch=20,col='blue',cex=2)#draw a blue dot for Bruxelles
# plotCircle(-99.13333 ,19.43333, 250)#Plot a dashed circle of 50 km arround Bruxelles
# plotElipse(-4.330,50.830,0.5)#Tries to plot a plain circle of 50 km arround Bruxelles, but drawn an ellipse

mc.rad <-rbind( plotCircle(mex.city[[1]] ,mex.city[[2]],125),  plotCircle(mex.city[[1]] ,mex.city[[2]],250))
mex.city

mex$slab$distmap <-distmap( mex$plates.ppp, dimyx=512) %>% raster()  %>% resample( mex$slab)
mex$slab$distmap %>% contour()
