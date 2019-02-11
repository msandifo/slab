fig.dir="/Users/msandifo/Dropbox/msandifo/documents/programming/r/packages/slab/examples/figures"
# mex <-assemble_slab(slab.name="mex", extend.lims = c(-1, -7, 4, 1),  simplify=F)
# mex <-assemble_slab(slab.name="mex", extend.lims = c(-11, -5, 4, 12),  simplify=F)
mex <-assemble_slab(slab.name="mex", extend.lims = c(-2, -7, 5.5, .5),  simplify=T)

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
mex.sig$names <- " "
mex.sig$names[str_detect(mex.sig$LOCATION_NAME, "MICHOACAN")] <- "MICHOACAN, "
mex.sig$names[str_detect(mex.sig$LOCATION_NAME, "PUEBLA") & mex.sig$YEAR==2017] <- "PUEBLA, "
mex.sig$names[str_detect(mex.sig$LOCATION_NAME, "CHIAPAS")] <- "CHIAPAS, "

mex.sig$deaths=0
mex.sig$deaths[!is.na(mex.sig$TOTAL_DEATHS)]="1"
mex.sig$deaths[is.na(mex.sig$TOTAL_DEATHS)]="0"
names(mex.sig)

mex.sig$ll = mex.sig$LONGITUDE/3 +2*mex.sig$LATITUDE

source('~/Dropbox/msandifo/documents/programming/r/2018/misc/ggteeth.R')
barb.scale= 1.5
barb.n=16
boundaries <-  c("57" ,"239" ) #, 238, 239)
bird.plates.fn <- "/Volumes/data/data/global/polygons/tectonicplates-master/PB2002_boundaries.shp"
bird.plates = readShapeLines(bird.plates.fn) %>% crop(extent(mex$slab)) %>% fortify()
message("Bird plate boudary ids : " , bird.plates$id %>%  unique  %>% str_c( collapse = ", "))
ggthrust( (bird.plates %>% subset(id  %in% boundaries ))[,1:2],  h =5, N=barb.n, REV = T, endtol=0.01, scale=barb.scale  )->teeth.s


cmt.ppp <-cmt.unpruned.ppp <- project_ppp( mex$cmt.ppp, plates, profile="p0")

cmt.ppp$marks <- cmt.unpruned.ppp$marks %>% mutate(date = as.POSIXct(date), m = pmax(mb,ms)) %>% distinct(eventname, .keep_all=T)
tail(cmt.ppp$marks)
cmt.ppp$marks$profile <-"p0"
cmt.ppp$marks[ which(cmt.ppp$marks$pro.long <= -99.8 & cmt.ppp$marks$pro.long > -102   & cmt.ppp$marks$bearing >= 10 & cmt.ppp$marks$bearing <35 ),"profile" ]  <-"p1"
cmt.ppp$marks[ which(cmt.ppp$marks$pro.long <= -96.6 & cmt.ppp$marks$pro.long > -99.8 & cmt.ppp$marks$bearing >= 10 & cmt.ppp$marks$bearing <35),"profile" ]  <-"p2"
cmt.ppp$marks[ which(cmt.ppp$marks$pro.long <= -93.3 & cmt.ppp$marks$pro.long > -95.8   & cmt.ppp$marks$bearing >= 10 & cmt.ppp$marks$bearing <35 ),"profile" ]  <-"p3"
cmt.ppp$marks[ which(cmt.ppp$marks$pro.long <= -90.8 & cmt.ppp$marks$pro.long > -93.3  & cmt.ppp$marks$bearing >= 10 & cmt.ppp$marks$bearing <35 ),"profile" ]  <-"p4"


ehb.ppp <- project_ppp( mex$ehb.ppp, plates, profile="p0")
ehb.ppp$marks$profile <-"p0"
ehb.ppp$marks[ which(ehb.ppp$marks$pro.long <= -99.8 & ehb.ppp$marks$pro.long > -102   & ehb.ppp$marks$bearing >= 10 & ehb.ppp$marks$bearing <35 ),"profile" ]  <-"p1"
ehb.ppp$marks[ which(ehb.ppp$marks$pro.long <= -96.6 & ehb.ppp$marks$pro.long > -99.8 & ehb.ppp$marks$bearing >= 10 & ehb.ppp$marks$bearing <35),"profile" ]  <-"p2"
ehb.ppp$marks[ which(ehb.ppp$marks$pro.long <= -93.3 & ehb.ppp$marks$pro.long > -95.8   & ehb.ppp$marks$bearing >= 10 & ehb.ppp$marks$bearing <35 ),"profile" ]  <-"p3"
ehb.ppp$marks[ which(ehb.ppp$marks$pro.long <= -90.8 & ehb.ppp$marks$pro.long > -93.3   & ehb.ppp$marks$bearing >= 10 & ehb.ppp$marks$bearing <35 ),"profile" ]  <-"p4"
ehb.ppp$marks$m <- pmax(ehb.ppp$marks$mw, ehb.ppp$marks$ms, ehb.ppp$marks$mb)
vo.ppp <-  project_ppp( mex$vo.ppp, plates, profile="p0" )
vo.ppp$marks$profile <-"p0"
vo.ppp$marks[ which(vo.ppp$marks$pro.long <= -99.8 & vo.ppp$marks$pro.long > -102   ),"profile" ]  <-"p1"
vo.ppp$marks[ which(vo.ppp$marks$pro.long <= -96.6 & vo.ppp$marks$pro.long > -99.8  ),"profile" ]  <-"p2"
vo.ppp$marks[ which(vo.ppp$marks$pro.long <= -93.3 & vo.ppp$marks$pro.long > -95.8  ),"profile" ]  <-"p3"
vo.ppp$marks[ which(vo.ppp$marks$pro.long <= -90.8 & vo.ppp$marks$pro.long > -93.3   ),"profile" ]  <-"p4"


cmt.ppp$marks$pro.bearing <- 0

cmt.ppp$marks[ which(cmt.ppp$marks$profile=="p1" ),"pro.bearing" ] <-
  bearing((cmt.ppp$marks%>% subset(profile=="p1"))[, c("pro.long","pro.lat")],
          (cmt.ppp$marks%>% subset(profile=="p1"))[, c("long","lat")]) %>%
  median()

cmt.ppp$marks[ which(cmt.ppp$marks$profile=="p2" ),"pro.bearing" ] <-
  bearing((cmt.ppp$marks%>% subset(profile=="p2"))[, c("pro.long","pro.lat")],
          (cmt.ppp$marks%>% subset(profile=="p2"))[, c("long","lat")]) %>%
  median()

cmt.ppp$marks[ which(cmt.ppp$marks$profile=="p3" ),"pro.bearing" ] <-
  bearing((cmt.ppp$marks%>% subset(profile=="p3"))[, c("pro.long","pro.lat")],
          (cmt.ppp$marks%>% subset(profile=="p3"))[, c("long","lat")]) %>%
  median()


cmt.ppp$marks[ which(cmt.ppp$marks$profile=="p4" ),"pro.bearing" ] <-
  bearing((cmt.ppp$marks%>% subset(profile=="p4"))[, c("pro.long","pro.lat")],
          (cmt.ppp$marks%>% subset(profile=="p4"))[, c("long","lat")]) %>%
  median()


#cmt.ppp$marks %>% group_by(profile) %>% summarise(bearing= pro.bearing[1])

pro.bearings<-cmt.ppp$marks %>% group_by(profile) %>% dplyr::summarise(pro.bearing=median(pro.bearing))


c(97.7, 16.8, (newLonLat(97.7, 16.8,pro.bearings$pro.bearing[2], 500 ))[1:2])


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
    annotate("text", x=-101, y= 14.5, label="Cocos Plate", size=4.5)+
    annotate("text", x=-106.5, y= 14.5, label="Pacific\nPlate", size=4.5)+
    annotate("text", x=-92, y= 18.2, label="North American\nPlate", size=4.5)+
   # annotate("text", x=-88.15, y= 21., label="NGDC singnificant\nearthquake database", size=3.5, hjust=1)+
    annotate("text", x=-96, y= 14.85, label="MAT", size=4.5, col=  "darkgreen")+
    annotate("text", x=mex.city$long, y= mex.city$lat, label="MC", size=4, col="darkred")+
    # annotate("text", x=-94.75,y=19.45, label="MFS", size=4)+
    # ggalt::geom_encircle(data=mex.sig %>%subset(LONGITUDE > -99 &LONGITUDE< -93 & LATITUDE <20 & LATITUDE>17.5 ), aes(LONGITUDE,LATITUDE),
    #                      s_shape=2., expand=0.04, linetype=2)+
    ggrepel::geom_text_repel(data=mex.sig  %>% subset(DEATHS >=90  & LONGITUDE < -93 & ll < 4),aes(x=LONGITUDE, y=LATITUDE, label=paste0(names,"f=", DEATHS,"\n",month.abb[MONTH]," ",YEAR )),
                             size=3.,
                             point.padding=.05 ,
                             box.padding= .1,
                             min.segment.length=.5,
                             segment.size=.32, alpha=.6, col="blue4",
                             nudge_y= -6, nudge_x= 0,
                             force=1, max.iter=2000 )+
    ggrepel::geom_text_repel(data=mex.sig  %>% subset(DEATHS >=90  & LONGITUDE < -93 & ll >= 4),aes(x=LONGITUDE, y=LATITUDE, label=paste0(names,"f=", DEATHS,"\n",month.abb[MONTH]," ",YEAR )),
                             size=3.,
                             point.padding=.05 ,
                             box.padding= .1,
                             min.segment.length=.5,
                             segment.size=.32, alpha=.6, col="blue4",
                             nudge_y=  3.2, nudge_x=  .5,
                             force=5, max.iter=2000 )



)


dev.off()
library(ggalt)
(p.join <-  p.sig.mex +

    geom_point(data=cmt.ppp$marks  %>% subset(profile !="p2") ,
               aes(x=long, y=lat,   shape=regime), col="orange",
               stroke=1, size=1.5, show.legend=F, alpha=.5) +
    geom_point(data=cmt.ppp$marks  %>% subset(profile =="p2") ,
               aes(x=long, y=lat,   shape=regime), col="orange",
               stroke=1, size=1.5, show.legend=T, alpha=1) +
    geom_point(data=mex$ehb.ppp$marks   ,
               aes(x=long, y=lat ),
               stroke=0, size=1, col="green4",show.legend=F) +
    geom_point(data=mex.sig %>% subset(YEAR<=2016)  , aes(x=LONGITUDE, y=LATITUDE,  size= log10(DEATHS)),  stroke=.75, show.legend = F, shape=5) +
    geom_point(data=mex.sig %>% subset(YEAR>2016)  , aes(x=LONGITUDE, y=LATITUDE,  size= log10(DEATHS)),  stroke=1.25, colour="red2",show.legend = F, shape=5) +

    # geom_point(data=cmt.ppp$marks %>% subset(profile == "p0") ,
    #            aes(lon, lat, size=depth.anom,   shape=profile),
    #            col="grey60",stroke=1, size=2, show.legend = F)+
    #  geom_point(data=mex.city   , aes(x=long, y=lat),  size=5, stroke=.5, show.legend = F, shape=24, fill="orange") +
    # geom_point(data=mex.city_0  %>% subset(profile !="pa" & (m>=6. )) %>%  unique(),
    #            aes(x=long, y=lat,  size=scalarmoment),size=5, shape=1, colour="white", stroke=1.5,show.legend = F)+
    # geom_point(data=mex.city_0 %>% subset(profile !="pa" & (m>=6.  ))%>%  unique(),
    #            aes(x=long, y=lat, size=scalarmoment),size=5, colour="black",shape=1,stroke=.75, show.legend = F)+
    geom_point(data=mex$vo.ppp$marks, aes(x=long, lat), shape =24,  size=3, col="grey30", fill="Yellow",show.legend=F)+
    geom_path(data= mc.rad, aes(x=long, y=lat, linetype= factor(km)), colour="red2", lwd=.25,  show.legend = F)+
    scale_linetype_manual(values=c("dashed", "longdash"))+
    # ggrepel::geom_label_repel(data=mex.city_0 %>% subset(profile !="pa" & (m>=6.  ) & ll < -15) %>%  unique() ,
    #                          aes(x=long, y=lat, label=paste0((date %>% as.Date) %>% as.character(),"\n",regime,", ",m, ", z=",depth)%>% str_replace_all("-","/")),
    #                          size=2.5, segment.size=.2,nudge_y=-2,nudge_x=-1, point.padding=1,label.padding=0.15, label.size= 0.1,label.alpha=.5) +
    #
    # ggrepel::geom_label_repel(data=mex.city_0 %>% subset(profile !="pa" & (m>=6.  ) & ll > -15) %>%  unique() ,
    #                           aes(x=long, y=lat, label=paste0((date %>% as.Date) %>% as.character(),"\n",regime,", ",m, ", z=",depth)%>% str_replace_all("-","/")),
    #                           size=2.5, segment.size=.2,nudge_y=2,nudge_x=1, point.padding=1,label.padding=0.15, label.size= 0.1,label.alpha=.5) +

    annotate("text", x=mex.city$long, y= mex.city$lat, label="MC", size=4, col="darkred")+
    annotate("text", x=-94.75,y=19.25, label="MFS", size=4)+
    annotate("segment",  x=-95.25,y=19.05, xend=-94.75-1.25,yend=19.45-1.05,linetype=1, size=.25)+
  geom_path(aes(x=c(-97.5, -99.95, -98.2,-96.3,-97.5) , y=c(15.1, 16, 19.7,19.0,15.1)), linetype=1, size=.25,color="blue3")+
    annotate("text", x=-100,y=16, label="b ", size=4, colour="blue3")+
    ggalt::geom_encircle(data=ehb.ppp$marks %>%subset((long > -100.2 & long< -95.9 & lat <20 & lat>17.7 & depth>45) |(long > -98. & long< -96 & lat <20 & lat>17.2 & depth>45) ) , aes(long,lat),
                         s_shape=2., expand=0.07, linetype=2)+
    labs(x=NULL, y=NULL)+
    scale_y_continuous(labels = degreesN)+scale_x_continuous(labels = degreesE)+
    guides(linetype="none", size="none")+theme(legend.position=c(.95,.8), legend.title = element_blank())
)

ggsave(paste0(fig.dir,"/mexico.figure.1.pdf"), plot=p.join, width=12, height=7)


profile.bearing <-bearing( cmt.ppp$marks[c("pro.long","pro.lat")] , cmt.ppp$marks[c("long","lat")] )

  profile.bearing  <- 16

pa<-project_axes(cmt.ppp, scale=10, axis=
                   "P", bearing=profile.bearing)
ta<-project_axes(cmt.ppp, scale=10, axis=
                   "T", bearing=profile.bearing)


ta  <- cbind(cmt.ppp$marks, ta )
pa  <- cbind(cmt.ppp$marks, pa )

vo.ppp$marks$depth <-0

(p.1b <-ggplot()+
    geom_point(data=ehb.ppp$marks %>% subset(profile== "p1"),
               aes(boundary.distance, -depth, size=m)    ,show.legend = F, alpha=.5) +
    geom_point(data=cmt.ppp$marks %>% subset(  !is.na(depth.anom)    & scalarmoment < 13  & profile== "p1"),
               aes(boundary.distance, -depth, colour=regime, size=m),shape=1, stroke=1., show.legend = F) +
    geom_point(data=vo.ppp$marks %>% subset(profile=="p1"),aes(boundary.distance, -depth), shape=24, size=2, fill="yellow", col="red3")+
    scale_radius(range=c(.5,5)) #+ scale_color_manual(values=c("red",  "green4", "blue", "green2","grey50"))
)

(p.1c <- p.1b +
    geom_segment(data=ta%>% subset((depth.anom> 0 | depth<70) & profile=="p1"),
                 aes(x=x,xend=xend,y= -y, yend= -yend, col=regime), size=.5)+
    # geom_segment(data=ta %>% subset(depth.anom> 0 | depth>40),  aes(x=x,xend=xend,y= -y, yend= -yend, col=regime))+
    coord_fixed(ratio = 1)+
      xlim(-10,400)+ theme(legend.position=c(.95,.45), legend.title = element_blank())+labs(x=NULL)

)


(p.2b <-ggplot()+
    hrbrthemes::theme_ipsum(axis = TRUE, ticks = F,grid=c("XY"),axis_text_size=14, axis_title_size=18)+
    geom_point(data=ehb.ppp$marks %>% subset(profile== "p2"   ),
               aes(boundary.distance, -depth, size=m), show.legend = T, stroke=.3, alpha=.5, shape=1, fill="white") +
    geom_point(data=cmt.ppp$marks %>% subset(  profile== "p2"   ),
               aes(boundary.distance, -depth, colour=regime, size=m), shape=1, stroke=.3, show.legend = F) +
    geom_point(data=vo.ppp$marks %>% subset(profile=="p2" ),aes(boundary.distance, -depth), shape=24, size=2, fill="yellow", col="red3")+
    scale_radius(range=c(.5,6)) #+ scale_color_manual(values=c("red",  "green4", "blue", "green2","grey50"))
)

mex.slab.pro <- rbind( data.frame(x=c(0, 10, 55, 110, 150,   200,  260, 400), y= -c(0, 1,12, 38,43, 47,50,   200), profile="p1"),
                       data.frame(x=c(0, 10, 55, 110, 150,   250,  325, 360), y= -c(5, 6,16, 38,43, 45,47,   75), profile="p2"),
                       data.frame(x=c(0, 10, 55, 120, 170,   250,  320, 550), y= -c(0, 1,12, 35,55, 100, 130, 300), profile="p3"),
                       data.frame(x=c(0, 10, 55, 110, 150,   200,  290, 360)*1.1, y= -c(0, 2,18, 50,80, 130, 230, 300), profile="p4"))

(p.2c <- p.2b +
    geom_segment(data=ta%>% subset(   profile=="p2"), aes(x=x,xend=xend,y= -y, yend= -yend, col=regime), size=.55)+
    # geom_segment(data=ta %>% subset(depth.anom> 0 | depth>40),  aes(x=x,xend=xend,y= -y, yend= -yend, col=regime))+
   # coord_fixed(ratio = 1.3)+
    xlim(-10,400)+
    theme_update(panel.grid.minor =element_blank(), panel.grid.major =element_line(colour = "grey85", size=.065, linetype=1))+
    theme(legend.position=c(.95,.65), legend.title = element_blank()) +
    labs(x=NULL, y="depth - kms")+
    ggalt::geom_xspline(data=mex.slab.pro %>% subset(profile=="p2"), aes(x,y),  size=.35, linetype=5, spline_shape=1)

)

mod1 <-read.csv("/Users/msandifo/Dropbox/msandifo/documents/programming/r/packages/slab/examples/slabTop.csv", col.names=c("x", "depth")) %>%
subset(x>= -600)%>%
  mutate(x= x+600, depth = -depth)

mid1 <-read.csv("/Users/msandifo/Dropbox/msandifo/documents/programming/r/packages/slab/examples/midPlane.csv",
                col.names=c("x", "depth")) %>%
  subset(x>= -602)%>%
  mutate(x= x +600 , depth = -depth)

c650 <-read.csv("/Users/msandifo/Dropbox/msandifo/documents/programming/r/packages/slab/examples/650C_contour.csv",
                col.names=c("x", "depth")) %>%
  subset(x>= -630)%>%
  mutate(x= x +600 , depth = -depth)


# need to run mex_kernel for p.kernel.cmt

mod.fac = .85
(p.2c <- p.2b +
    geom_segment(data=ta%>% subset(   profile=="p2" ), aes(x=x,xend=xend,y= -y, yend= -yend, col=regime), size=.55)+
    # geom_segment(data=ta %>% subset(depth.anom> 0 | depth>40),  aes(x=x,xend=xend,y= -y, yend= -yend, col=regime))+
    # coord_fixed(ratio = 1.3)+
    xlim(-10,400)+
    theme_update(panel.grid.minor =element_blank(), panel.grid.major =element_line(colour = "grey85", size=.065, linetype=1))+
    theme(legend.position=c(.95,.65), legend.title = element_blank()) +
    labs(x=NULL, y="depth - kms")+
    geom_line(data=mod1 , aes(x,depth ),  size=.35, linetype=1, col="blue3")+
     geom_line(data=mid1 , aes(x ,depth  ),  size=.35, linetype=3, col="blue3")+
    geom_line(data=mod1 , aes(x*mod.fac ,depth*mod.fac  ),  size=.35, linetype=5, col="blue3")+
  geom_path(data=c650 , aes(x, depth  ),  size=.35, linetype=5, col="orange")#+
  # ggalt::geom_xspline(data=mex.slab.pro %>% subset(profile=="p2"), aes(x,y),  size=.35, linetype=5, spline_shape=1)

)


curvGrad1 <-read.csv("/Users/msandifo/Dropbox/msandifo/documents/programming/r/packages/slab/examples/curvGrad.csv", col.names=c("x", "cg")) %>%
  subset(x>= -offset-100)%>%
  mutate(x= x+offset)


( p.1<-gridExtra::grid.arrange( p.join,
                                p.2c  ,
                                ncol=1, heights=c(4.9,4.3 ) )
)



cowplot::plot_grid(p.join,p.2c, labels = c("a", "b"), nrow = 2, align = "v", rel_heights=c(1,.6), label_fontface="plain", label_size=18)


mm <- 6.0


cmt.ppp$marks$m[ !is.na(cut(cmt.ppp$marks$m, c( mm,10)))]


(p.kernel.cmt.1<-plot_kernel(cmt.ppp$marks %>% subset(profile =="p2"  & (depth.anom> -0 | is.na(depth.anom) | depth > 0)), min=5, cuts= c( mm,10), points=T, bw="SJ", kernel="cosine",
                           leg=c(.7,.95), mscale=3.5, xlim=c(0,400 ))+
    #  scale_x_continuous(lim=c(0,350), expand=c(0,0))+
    # annotate("text", 40, .0135, label="wedge", size=4.5)+
    annotate("text", 90, .015, label="megathrust", size=4.)+
    annotate("text", 280, .015, label="MFT", size=4.)+ guides(fill="none")+theme(legend.direction = "horizontal")+
    geom_hline(yintercept= (mm-4)/ (   (1000/3.5)+4), linetype=1, size=.2)+
    geom_line(data=curvGrad1, (aes(x*mod.fac, abs(cg*60))), col="blue3", size=.3, linetype=5)


)


(p.kernel.cmt.2<-plot_kernel(cmt.ppp$marks %>% subset(profile =="p2"  & (depth.anom >= 0 | is.na(depth.anom) | depth >= 40)),
                             min=4.95,cuts= c( mm,10), points=T, bw="SJ", kernel="cosine", mlimit=5,
                             leg=c(.7,.95), mscale=4.5, xlim=c(0,400 ), ylim=c(0,0.014))+
    #  scale_x_continuous(lim=c(0,350), expand=c(0,0))+
    # annotate("text", 40, .0135, label="wedge", size=4.5)+
    annotate("text", 90, .013, label="megathrust", size=4.)+
    annotate("text", 280, .013, label="MFT", size=4.)+ guides(fill="none")+theme(legend.direction = "horizontal")+
    annotate("text", 182, .013,label="seismic gap", size=4.)+ guides(fill="none")+theme(legend.direction = "horizontal")+
    geom_hline(yintercept= (mm-5)/ (   (1000/4.5)+4), linetype=1, size=.2)+
    geom_line(data=curvGrad1, (aes(x, abs(cg*60))), col="blue3", size=.3, linetype=5)


)

p.kernel.cmt.3 <-p.kernel.cmt.2+
  geom_point(data= cmt.ppp$marks %>%
               subset(profile =="p2"  & depth.anom < 0 & boundary.distance<120  ),
                                                          aes(x= boundary.distance, y = (m-5)/(1000/4.5)  ,size=depth), show.legend=F, shape=2, stroke=.1)+

  annotate("text",  311, .0098, label="PUEBLA", size=3, col="blue4")

p.kernel.cmt.3

plot.combo1.b <-cowplot::plot_grid(p.join+
                                     theme( legend.background=element_blank()),
                                   p.2c +
                                     scale_x_continuous(expand = c(0,0),  limits=c(-100,400))+
                                     theme(legend.direction = "vertical" , legend.position=c(.94,.5), legend.background=element_blank())+
                                     annotate("segment", x=0, xend=10,  y= -50, yend= -50, linetype=1, size=.2, alpha=.5)+
                                     annotate("segment", x=0, xend=10,  y= -75, yend= -75, linetype=1, size=.2, alpha=.5)+

                                     scale_y_continuous(expand = c(0,0),  limits=c(-105,3))+
                                     annotate("text",  311, -48, label="PUEBLA", size=3, col="blue4")
                                   ,
                                   p.kernel.cmt.3+
                                     theme_update(panel.grid.minor =element_blank(), panel.grid.major =element_line(colour = "grey85", size=.065, linetype=1))+
                                                    guides(fill="none")+
                                     theme(legend.direction = "vertical" , legend.position=c(.94,.65), legend.title = element_blank())+
                                     labs(x=NULL, y="kernel density")+
                                     scale_x_continuous(expand = c(0,0),  limits=c(-100,400))+
                                     scale_fill_manual(values ="grey80")

                                   , labels = c("a", "b", "c"), nrow = 3, align = "v", rel_heights=c(1,.8,.8), label_fontface="plain", label_size=18)

plot.combo1.b
dev.off()
ggsave(paste0(fig.dir,"/mexico.figure.1b.pdf"),   plot=plot.combo1.b,width=9, height=10)
#
#
# source('~/Dropbox/transfers/code snip/ieb.3.R')
# library(ncdf4)
# purrr::map(get_ieb_files(), get_ieb) %>% bind_rows() -> ieb3.df
#
# head(ieb3.df)
# map_dfr( c(34:35, c(3:8)  ),join_isc) -> isc.df
# ggplot()+geom_point(data=ieb3.df, aes(lon, lat))

