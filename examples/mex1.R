#----
extrafont::loadfonts()
library(slab)
#_-----

fig.dir<-"/Users/msandifo/Dropbox/msandifo/documents/programming/r/packages/slab/examples/figures/"

#mex <-assemble_slab(slab.name="mex", extend.lims = c(-1, -7, 4, 1),  simplify=F)

mex <-assemble_slab(slab.name="cam", extend.lims = c(-2, -7, 5.5, .5),  simplify=F, slab2=T)


#plates= mex$plates[mex$plates$ID %in% c("57", "54", "123") ] %>% fortify()
plates= mex$plates[mex$plates$data$ID %in% c("57", "54", "123") ] %>% fortify()
coasts.df <- mex$coasts%>% fortify()


dplyr::select(mex$vo.ppp$marks, starts_with("lon"), starts_with("lat") )
proj.laea <- "+proj=laea +lat_0=0 +lon_0=-97"


#note the use of 'bearing' gives contrains on the selced field being in NNE sector (<45) in thsi case
cmt.ppp <-cmt.unpruned.ppp <- project_ppp( mex$cmt.ppp, plates, profile="p0")
cmt.ppp$marks <- cmt.unpruned.ppp$marks %>% mutate(date = as.POSIXct(date)) %>% distinct(eventname, .keep_all=T)

cmt.ppp$marks$profile <-"p0"
cmt.ppp$marks[ which(cmt.ppp$marks$pro.long <= -99.6 & cmt.ppp$marks$pro.long > -102   & cmt.ppp$marks$bearing >= -10 & cmt.ppp$marks$bearing <45 ),"profile" ]  <-"p1"
cmt.ppp$marks[ which(cmt.ppp$marks$pro.long <= -96.4 & cmt.ppp$marks$pro.long > -99.6 & cmt.ppp$marks$bearing >= -10 & cmt.ppp$marks$bearing <45),"profile" ]  <-"p2"
cmt.ppp$marks[ which(cmt.ppp$marks$pro.long <= -93.3 & cmt.ppp$marks$pro.long > -95.8   & cmt.ppp$marks$bearing >= -10 & cmt.ppp$marks$bearing <45 ),"profile" ]  <-"p3"
cmt.ppp$marks[ which(cmt.ppp$marks$pro.long <= -90.8 & cmt.ppp$marks$pro.long > -93.3  & cmt.ppp$marks$bearing >= -10 & cmt.ppp$marks$bearing <45 ),"profile" ]  <-"p4"

ehb.ppp <- project_ppp( mex$ehb.ppp, plates, profile="p0")
ehb.ppp$marks$profile <-"p0"
ehb.ppp$marks[ which(ehb.ppp$marks$pro.long <= -99.6 & ehb.ppp$marks$pro.long > -102   & ehb.ppp$marks$bearing >= -10 & ehb.ppp$marks$bearing <45 ),"profile" ]  <-"p1"
ehb.ppp$marks[ which(ehb.ppp$marks$pro.long <= -96.4 & ehb.ppp$marks$pro.long > -99.6 & ehb.ppp$marks$bearing >= -10 & ehb.ppp$marks$bearing <45),"profile" ]  <-"p2"
ehb.ppp$marks[ which(ehb.ppp$marks$pro.long <= -93.3 & ehb.ppp$marks$pro.long > -95.8   & ehb.ppp$marks$bearing >= -10 & ehb.ppp$marks$bearing <45 ),"profile" ]  <-"p3"
ehb.ppp$marks[ which(ehb.ppp$marks$pro.long <= -90.8 & ehb.ppp$marks$pro.long > -93.3   & ehb.ppp$marks$bearing >= -10 & ehb.ppp$marks$bearing <45 ),"profile" ]  <-"p4"

vo.ppp <-  project_ppp( mex$vo.ppp, plates, orfile="p0" )
vo.ppp$marks$profile <-"p0"
vo.ppp$marks[ which(vo.ppp$marks$pro.long <= -99.6 & vo.ppp$marks$pro.long > -102   ),"profile" ]  <-"p1"
vo.ppp$marks[ which(vo.ppp$marks$pro.long <= -96.4 & vo.ppp$marks$pro.long > -99.6  ),"profile" ]  <-"p2"
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





#pa<-project_axes(cmt.slab.ppp, scale=5, bearing=profile.bearing)


scale=10

cmt.axes <- rbind(project_axes(cmt.ppp, scale=scale, axis = "P", bearing=cmt.ppp$marks$pro.bearing, profile=cmt.ppp$marks$profile) %>% cbind(cmt.ppp$marks[, 1:61]),
                  project_axes(cmt.ppp, scale=scale, axis = "T", bearing=cmt.ppp$marks$pro.bearing, profile=cmt.ppp$marks$profile) %>% cbind(cmt.ppp$marks[, 1:61]),
                  project_axes(cmt.ppp, scale=scale, axis = "B", bearing=cmt.ppp$marks$pro.bearing, profile=cmt.ppp$marks$profile) %>% cbind(cmt.ppp$marks[, 1:61])
)



mex.slab.pro <- rbind( data.frame(x=c(0, 10, 55, 110, 150,   200,  260, 400), y= -c(0, 1,12, 38,43, 47,50,   200), profile="p1"),
                       data.frame(x=c(0, 10, 55, 110, 150,   250,  325, 470), y= -c(0, 1,12, 38,43, 47,50,   200), profile="p2"),
                      data.frame(x=c(0, 10, 55, 120, 170,   250,  320, 550), y= -c(0, 1,12, 35,55, 100, 130, 300), profile="p3"),
                      data.frame(x=c(0, 10, 55, 110, 150,   200,  290, 360)*1.1, y= -c(0, 2,18, 50,80, 130, 230, 300), profile="p4"))


(cmt.axes[which(cmt.axes$date %>% as.Date()>=  lubridate::ymd("1985-09-17") &
                  cmt.axes$date %>% as.Date() <=  lubridate::ymd("1985-09-19") &
                  cmt.axes$ms>5),] %>%as.data.frame())[1,][c("long","lat","boundary.distance","depth", "profile", "regime","ms","mb")] ->
  mex.city.1985

(cmt.axes[which(cmt.axes$date %>% as.Date()>=  lubridate::ymd("2017-09-15") &
                  cmt.axes$date %>% as.Date() <=  lubridate::ymd("2017-09-19") &
                  cmt.axes$ms>5 |cmt.axes$mb>5 ),] %>%as.data.frame())[1,][c("long","lat","boundary.distance","depth", "profile", "regime","ms","mb")] ->
  mex.city.2017

mex.city_0<-rbind(mex.city.1985,mex.city.2017)

mex.events <- (read_tsv("/Users/msandifo/Dropbox/msandifo/documents/programming/r/2018/geophys/mexico/data/mexico_wiki_quakes.txt",skip = 0, col_names=F))[,1:7]
names(mex.events) <- c("date", "location", "magnitude", "mercali","deaths","injuries", "notes")
mex.events$magnitude %>% str_sub(1,3)     %>% as.numeric -> m
mex.events$magnitude %>% str_sub(4, 7)    %>% str_trim() -> mex.events$measure
mex.events$magnitude <-m

oaxaca.2018 <- data.frame( date= lubridate::ymd_hms("2018-02-16 23:39:42", tz= "UTC"), lat= 16.646, lon=   -97.653 ,depth= 24.7)

#mex.city_0<-cmt.ppp[which((cmt.axes$date %>% as.Date()) %in%  mex.events$date)]
mex.city_0<-cmt.ppp$marks %>% subset((date %>% as.Date()) %in%  mex.events$date)
mex.city_0$m <- pmax(mex.city_0$ms, mex.city_0$mb)

mex.city_0$date <-mex.city_0$date %>% as.Date()
mex.city_all <- mex.city_0

mex.city_0 <-mex.city_all   %>% dplyr::arrange( desc(m,date)) %>% distinct(date,.keep_all = TRUE) %>% mutate(ll=lat+long/3)

mex.city <- data.frame(long= -99.13333,lat=19.43333 )
#mex.city.laea <- convertPts(mex.city[1],mex.city[2], from="+init=epsg:4326" , to=proj.laea)

mc.rad <-rbind( plotCircle(mex.city[[1]] ,mex.city[[2]],125),  plotCircle(mex.city[[1]] ,mex.city[[2]],250))
mc.rad <-  plotCircle(mex.city[[1]] ,mex.city[[2]],125)

library(ggalt)
(p.map<- ggplot( ) +
    ggalt::geom_cartogram( data=coasts.df, aes(map_id=id ), map=coasts.df  ) +
    ggalt::coord_proj(proj.laea, xlim=extent(mex$slab)[1:2],ylim=extent(mex$slab)[3:4]) +
    geom_polygon(data=coasts.df   ,
                 aes(x=long-360, y=lat , group=group ), size=0,  alpha=.25, col="white", fill="grey60" , show.legend = F) +
 #   geom_path(data=bird.plates   , aes(x=long, y=lat, group=id), colour="darkgreen", lwd=.5)+
 #    geom_polygon(data=teeth.s, aes(x,y, group=id), color="darkgreen", fill="darkgreen")+

    geom_path(data=mex$plates %>% fortify()  ,
              aes(x=long, y=lat ,group=id ),colour="darkgreen", lwd=.5, show.legend = F) +
    geom_point(data=cmt.ppp$marks   ,
               aes(x=long, y=lat,   shape=regime, col=profile),
                stroke=1, size=2) +
    geom_point(data=ehb.ppp$marks   ,
               aes(x=long, y=lat ),
               stroke=0, size=1, col="black") +
    # geom_point(data=cmt.ppp$marks %>% subset(profile == "p0") ,
    #            aes(lon, lat, size=depth.anom,   shape=profile),
    #            col="grey60",stroke=1, size=2, show.legend = F)+
  #  geom_point(data=mex.city   , aes(x=long, y=lat),  size=5, stroke=.5, show.legend = F, shape=24, fill="orange") +
    # geom_point(data=mex.city_0  %>% subset(profile !="pa" & (m>=6. )) %>%  unique(),
    #            aes(x=long, y=lat,  size=scalarmoment),size=5, shape=1, colour="white", stroke=1.5,show.legend = F)+
    # geom_point(data=mex.city_0 %>% subset(profile !="pa" & (m>=6.  ))%>%  unique(),
    #            aes(x=long, y=lat, size=scalarmoment),size=5, colour="black",shape=1,stroke=.75, show.legend = F)+
    geom_point(data=mex$vo.ppp$marks, aes(x=long, lat), shape =24,  size=3, col="grey30", fill="Yellow")+
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
    annotate("text", x=-94.75,y=19.45, label="flat slab\nquakes", size=4)+
    ggalt::geom_encircle(data=ehb.ppp$marks %>%subset(long > -100.6 & long< -96 & lat <20 & lat>17.7 & depth>45 ) , aes(long,lat),
                         s_shape=2., expand=0.07, linetype=2)+
    labs(x=NULL, y=NULL)+
    scale_y_continuous(labels = degreesN)+scale_x_continuous(labels = degreesE)+
    guides(colour="none")
)


(p.map.a <- p.map + geom_segment(data=cmt.ppp$marks %>% subset(profile !="p0" & bearing >= -10 ),
                        aes(x=long, y=lat, xend= pro.long, yend=pro.lat, col=profile), size=.15,  alpha=.4)+
    geom_text(data=data.frame(x=c(-101.5,-98.5,-95,-92.5), y=c(16,14.95,13.8, 12.8), profile=c("p1","p2","p3", "p4")),
              aes(x,y, label=profile, col=profile),size=5)+
   # hrbrthemes::theme_ipsum(axis = F, ticks = F)+
    theme(panel.grid.minor=element_blank(),
          panel.grid.major=element_line( linetype="FF", colour = "grey50", size=.05),
          legend.position=c(.95,.85) )

)

ggsave(paste0(fig.dir,"/mexican.slab.map.pdf"), p.map.a ,width=14, height=9)


#cmt.ppp$marks$pro.lon <= -93 &cmt.ppp$marks$pro.lon > -96.8#ggsave(paste0(fig.dir,"/mexican.slab.a.pdf"), width=10, height=6)



#geosphere::distGeo(cmt.slab.ppp %>% as.SpatialPoints.ppp(), Xproj %>% as.SpatialPoints.ppp() )


( p.profile <- ggplot()+
  geom_point(data=cmt.axes  %>% subset( profile != "p0"  & depth.anom >= 0  & scalarmoment < 15   ),
             aes(boundary.distance, -depth, colour=regime, size=scalarmoment),shape=1, stroke=.3, show.legend = F) +
     geom_point( data=cmt.axes %>% subset( profile != "p0"   & depth.anom < 0  & scalarmoment < 15   ),
              aes(boundary.distance, -depth, colour=regime, size=scalarmoment),
              stroke=.3, show.legend = F) +
    geom_point( data=ehb.ppp$marks %>% subset( profile != "p0"   ),
                aes(boundary.distance, -depth ),
                size=.2, show.legend = F, col="grey40") +

    scale_radius(range=c(0,2))  +
    geom_hline(yintercept= -100, size=.35, linetype=2, col="blue3")+
   facet_wrap(~profile, ncol=2, scales="free_x")#+ scale_color_manual(values=c("red",  "green4", "blue", "green2","grey50"))
)



(p.profile.T <- p.profile +
     ggalt::geom_xspline(data=mex.slab.pro, aes(x,y),  size=.35, linetype=5, spline_shape=1)+
    #   geom_point(data=mex.slab.pro, aes(x,y))+
    geom_segment(data=cmt.axes %>% subset(profile != "p0"   & axis== "T" & profile !="pa" & ( depth.anom> 0 | depth>0)),
                 aes(x=x,xend=xend,y= -y, yend= -yend, col=regime), size=.25)+
    # geom_segment(data=ta %>% subset(depth.anom> 0 | depth>40),  aes(x=x,xend=xend,y= -y, yend= -yend, col=regime))+
    geom_point(data=vo.ppp$marks %>% subset(profile != "p0" & profile != "pa"), aes(boundary.distance, 1), shape =24,  size=3, col="grey30", fill="Yellow")+
    coord_fixed(ratio = 1, xlim=c(0, 400), ylim=c(-200,0))+
    labs(title="T-axes")+
    geom_point(data=mex.city_0  %>% subset(profile !="p0" & (m>=6. )) %>%  unique(),  aes(boundary.distance, -depth,  size=scalarmoment),size=5, shape=1, colour="white", stroke=1.1,show.legend = F)+
    geom_point(data=mex.city_0   %>% subset(profile !="p0" & (m>=6.  ))%>%  unique(),  aes(boundary.distance, -depth, size=scalarmoment),size=5, colour="black",shape=1,stroke=.5, show.legend = F)+
    ggrepel::geom_text_repel(data=mex.city_0  %>% subset(profile !="p0" & (m>=6.  ) & depth <55) %>%  unique()  ,  aes(boundary.distance, -depth, label=paste0((date %>% as.Date) %>% as.character(),"\n",m)),
                             size=2, nudge_y=20,point.padding=1,label.padding=0.15, label.size=0)+
    ggrepel::geom_text_repel(data=mex.city_0  %>% subset(profile !="p0" & (m>=6.  ) & depth >=55) %>%  unique()  ,  aes(boundary.distance, -depth, label=paste0((date %>% as.Date) %>% as.character(),"\n",m)),
                             size=2, nudge_y= -20,point.padding=1,label.padding=0.15, label.size=0)+
    hrbrthemes::theme_ipsum(axis = TRUE, ticks = F,grid=c("XY"))+
    theme(panel.grid.minor=element_blank(),panel.grid.major=element_line(size=.1, linetype=1) ))



dev.off();
ggsave(paste0(fig.dir,"mexican.slabs.dist.T.pdf"), plot=p.profile.T,width=12, height=8)

(p.profile.P <- p.profile +
    ggalt::geom_xspline(data=mex.slab.pro, aes(x,y),  size=.35, linetype=5, spline_shape=1)+
    #   geom_point(data=mex.slab.pro, aes(x,y))+
    geom_segment(data=cmt.axes %>% subset( profile != "p0" & axis== "P" & profile !="pa" &( depth.anom> 0 | depth>0)), aes(x=x,xend=xend,y= -y, yend= -yend, col=regime), size=.25)+
    # geom_segment(data=ta %>% subset(depth.anom> 0 | depth>40),  aes(x=x,xend=xend,y= -y, yend= -yend, col=regime))+
    geom_point(data=vo.ppp$marks %>% subset(profile != "p0"), aes(boundary.distance, 1), shape =24,  size=3, col="grey30", fill="Yellow")+
    coord_fixed(ratio = 1, xlim=c(0, 400), ylim=c(-200,0))+
    labs(title="P-axes")+
    geom_point(data=mex.city_0 %>% subset(profile !="p0" & (m>=6. )) %>%  unique(),  aes(boundary.distance, -depth,  size=scalarmoment),size=5, shape=1, colour="white", stroke=1.1,show.legend = F)+
    geom_point(data=mex.city_0 %>% subset(profile !="p0" & (m>=6.  ))%>%  unique(),  aes(boundary.distance, -depth, size=scalarmoment),size=5, colour="black",shape=1,stroke=.5, show.legend = F)+
    ggrepel::geom_text_repel(data=mex.city_0 %>% subset(profile !="p0" & (m>=6.  ) & depth <55) %>%  unique()  ,  aes(boundary.distance, -depth, label=paste0((date %>% as.Date) %>% as.character(),"\n",m)),
                             size=2, nudge_y=20,point.padding=1,label.padding=0.15, label.size=0)+
    ggrepel::geom_text_repel(data=mex.city_0 %>% subset(profile !="p0" & (m>=6.  ) & depth >=55) %>%  unique()  ,  aes(boundary.distance, -depth, label=paste0((date %>% as.Date) %>% as.character(),"\n",m)),
                             size=2, nudge_y= -20,point.padding=1,label.padding=0.15, label.size=0)+
    hrbrthemes::theme_ipsum(axis = TRUE, ticks = F,grid=c("XY"))+
    theme(panel.grid.minor=element_blank(),panel.grid.major=element_line(size=.1, linetype=1) )

)
dev.off();
ggsave(paste0(fig.dir,"mexican.slabs.dist.P.pdf"), plot=p.profile.P,width=12, height=8)


(p.profile.B <- p.profile +
  ggalt::geom_xspline(data=mex.slab.pro, aes(x,y),  size=.35, linetype=5, spline_shape=1)+
  #   geom_point(data=mex.slab.pro, aes(x,y))+
  geom_segment(data=cmt.axes %>% subset( profile != "p0" & axis== "B" & profile !="pa" &( depth.anom> 0 | depth>0)), aes(x=x,xend=xend,y= -y, yend= -yend, col=regime), size=.25)+
  # geom_segment(data=ta %>% subset(depth.anom> 0 | depth>40),  aes(x=x,xend=xend,y= -y, yend= -yend, col=regime))+
  geom_point(data=vo.ppp$marks %>% subset(profile != "p0"), aes(boundary.distance, 1), shape =24,  size=3, col="grey30", fill="Yellow")+
  coord_fixed(ratio = 1, xlim=c(0, 400), ylim=c(-200,0))+
  labs(title="B-axes")+
    geom_point(data=mex.city_0  %>% subset(profile !="p0" & (m>=6. )) %>%  unique(),  aes(boundary.distance, -depth,  size=scalarmoment),size=5, shape=1, colour="white", stroke=1.1,show.legend = F)+
    geom_point(data=mex.city_0  %>% subset(profile !="p0" & (m>=6.  ))%>%  unique(),  aes(boundary.distance, -depth, size=scalarmoment),size=5, colour="black",shape=1,stroke=.5, show.legend = F)+
    ggrepel::geom_text_repel(data=mex.city_0  %>% subset(profile !="p0" & (m>=6.  ) & depth <55) %>%  unique()  ,  aes(boundary.distance, -depth, label=paste0((date %>% as.Date) %>% as.character(),"\n",m)),
                             size=2, nudge_y=20,point.padding=1,label.padding=0.15, label.size=0)+
    ggrepel::geom_text_repel(data=mex.city_0   %>% subset(profile !="p0" & (m>=6.  ) & depth >=55) %>%  unique()  ,  aes(boundary.distance, -depth, label=paste0((date %>% as.Date) %>% as.character(),"\n",m)),
                             size=2, nudge_y= -20,point.padding=1,label.padding=0.15, label.size=0)+
    hrbrthemes::theme_ipsum(axis = TRUE, ticks = F,grid=c("XY"))+
  theme(panel.grid.minor=element_blank(),panel.grid.major=element_line(size=.1, linetype=1) )

)
dev.off();
ggsave(paste0(fig.dir,"mexican.slabs.dist.B.pdf"), plot=p.profile.B,width=12, height=8)

ehb.ppp$marks$m <- pmax(ehb.ppp$marks$ms,ehb.ppp$marks$mw,ehb.ppp$marks$mb)

(p.profiles.ISCEHB <-ggplot(ehb.ppp$marks %>% subset(boundary.distance > 0 & boundary.distance < 460 & m >=4 & profile !="p0" & (depth.anom >= -2 | ((depth.anom> -25 | is.na(depth.anom )) & depth>=49)) ), aes(boundary.distance, -depth)) +
  geom_point(aes(size=m), shape=1)+
  geom_point(data=ehb.ppp$marks %>% subset(boundary.distance > 0 & boundary.distance < 460 & m >=4 & profile !="p0" &   (depth.anom < -2 | depth<49 )), aes(size=m), shape=1,alpha=.2)+
  geom_hline(yintercept = c(-53, -70), size=.1, linetype=2)+ scale_radius(range=c(.5,7))+
    coord_fixed(ratio = 1.5, xlim=c(0, 320), ylim=c(-130,0))+
  facet_wrap(~profile,ncol=2)+
  geom_point(data=vo.ppp$marks %>% subset(profile != "p0"), aes(boundary.distance, 1), shape =24,  size=3, col="grey30", fill="Yellow")+
  hrbrthemes::theme_ipsum(axis = TRUE, ticks = F,grid=c("XY"))+
    ggrepel::geom_text_repel(data=mex.city_0  %>% subset(profile !="p0" & (m>=6.  ) & depth <55) %>%  unique()  ,
                             aes(boundary.distance, -depth, label=paste0((date %>% as.Date) %>% as.character(),"\n",regime,", M=",m)),
                             size=2.5, nudge_y=20,point.padding=0,label.padding=0.15, label.size=0)+
    ggrepel::geom_text_repel(data=mex.city_0  %>% subset(profile !="p0" & (m>=6.  ) & depth >=55) %>%  unique()  ,
                             aes(boundary.distance, -depth, label=paste0((date %>% as.Date) %>% as.character(),"\n",regime,", M=",m)),
                             size=2.5, nudge_y= -20,point.padding=0,label.padding=0.15, label.size=0)+
  theme(panel.grid.minor=element_blank(),panel.grid.major=element_line(size=.1, linetype=1) )+
    labs(title="ISC-EHB")
)
dev.off();
ggsave(paste0(fig.dir,"mexican.slabs.ISCEHB.profiles.pdf"), plot=p.profiles.ISCEHB,width=12, height=8)

#----

#need to use unpruned data set here...]

( p.profile.cat <- ggplot()+
    geom_point(data=cmt.axes  %>% subset( profile == "p2"  & depth.anom >= 0  & scalarmoment < 15   ),
               aes(boundary.distance, -depth, colour=regime, size=scalarmoment),shape=1, stroke=.3, show.legend = F) +
    geom_point( data=cmt.axes %>% subset( profile == "p2"   & depth.anom < 0  & scalarmoment < 15   ),
                aes(boundary.distance, -depth, colour=regime, size=scalarmoment),
                stroke=.3, show.legend = F) +
    geom_point( data=ehb.ppp$marks %>% subset( profile == "p2"   ),
                aes(boundary.distance, -depth ),
                size=.2, show.legend = F, col="grey40") +

    scale_radius(range=c(0,2))  +
    geom_hline(yintercept= -100, size=.35, linetype=2, col="blue3")+
    facet_wrap(~referencecatalog, ncol=2, scales="free_x")#+ scale_color_manual(values=c("red",  "green4", "blue", "green2","grey50"))
)


(p.profile.cat.T <- p.profile.cat +
   # ggalt::geom_xspline(data=mex.slab.pro, aes(x,y),  size=.35, linetype=5, spline_shape=1)+
    #   geom_point(data=mex.slab.pro, aes(x,y))+
    geom_segment(data=cmt.axes %>% subset(profile == "p2"   & axis== "T" & profile !="pa" & ( depth.anom> 0 | depth>0)),
                 aes(x=x,xend=xend,y= -y, yend= -yend, col=regime), size=.25)+
    # geom_segment(data=ta %>% subset(depth.anom> 0 | depth>40),  aes(x=x,xend=xend,y= -y, yend= -yend, col=regime))+
    geom_point(data=vo.ppp$marks %>% subset(profile == "p2" & profile != "pa"), aes(boundary.distance, 1), shape =24,  size=3, col="grey30", fill="Yellow")+
    coord_fixed(ratio = 1, xlim=c(0, 400), ylim=c(-200,0))+
    labs(title="T-axes")+
    geom_point(data=mex.city_all$marks %>% subset(profile =="p2" & (m>=6. )) %>%  unique(),  aes(boundary.distance, -depth,  size=scalarmoment),size=5, shape=1, colour="white", stroke=1.1,show.legend = F)+
    geom_point(data=mex.city_all$marks %>% subset(profile =="p2" & (m>=6.  ))%>%  unique(),  aes(boundary.distance, -depth, size=scalarmoment),size=5, colour="black",shape=1,stroke=.5, show.legend = F)+
    ggrepel::geom_text_repel(data=mex.city_all$marks %>% subset(profile =="p2" & (m>=6.  ) & depth <55) %>%  unique()  ,
                             aes(boundary.distance, -depth, label=paste0((date %>% as.Date) %>% as.character(),"\n",m, ", ", eventname)),
                             size=2, nudge_y=20,point.padding=1,label.padding=0.15, label.size=0)+
    ggrepel::geom_text_repel(data=mex.city_all$marks %>% subset(profile =="p2" & (m>=6.  ) & depth >=55) %>%  unique()  ,
                             aes(boundary.distance, -depth, label=paste0((date %>% as.Date) %>% as.character(),"\n",m, ",", eventname)),
                             size=2, nudge_y= -20,point.padding=1,label.padding=0.15, label.size=0)+
    hrbrthemes::theme_ipsum(axis = TRUE, ticks = F,grid=c("XY"))+
    theme(panel.grid.minor=element_blank(),panel.grid.major=element_line(size=.1, linetype=1) ))


# (p.1c <- p.1b +
#     ggalt::geom_xspline(data=mex.slab.pro, aes(x,y),  size=.35, linetype=5, spline_shape=1)+
#     #   geom_point(data=mex.slab.pro, aes(x,y))+
#     geom_segment(data=pa%>% subset(depth.anom> 0 | depth>40), aes(x=x,xend=xend,y= -y, yend= -yend, col=regime), size=.25)+
#     # geom_segment(data=ta %>% subset(depth.anom> 0 | depth>40),  aes(x=x,xend=xend,y= -y, yend= -yend, col=regime))+
#     geom_point(data=vo.ppp$marks, aes(boundary.distance, 1), shape =24,  size=3, col="grey30", fill="Yellow")+
#     coord_fixed(ratio = 1, xlim=c(0, 340))
#
# )
#
# (p.1d <- p.1b +
#     #   geom_segment(data=pa%>% subset(depth.anom> 0 | depth>40), aes(x=x,xend=xend,y= -y, yend= -yend, col=regime), size=.2)+
#     ggalt::geom_xspline(data=mex.slab.pro, aes(x,y),  size=.35, linetype=5, spline_shape=1)+
#     #
#     geom_segment(data=ta %>% subset(depth.anom> 0 | depth>40),
#                  aes(x=x,xend=xend,y= -y, yend= -yend, col=regime), size=.25)+
#     geom_point(data=vo.ppp$marks, aes(boundary.distance, 1), shape =24,  size=3, col="grey30", fill="Yellow")+
#     coord_fixed(ratio = 1, xlim=c(0, 340))
#
# )
#
#
# ta  <- cbind(cmt.slab.ppp$marks, ta )
#
#
# (pro.2 <-ggplot()+
#     geom_point(data=profile.2 %>% subset(  !is.na(depth.anom) & depth.anom >= 0  & scalarmoment < 13   ),
#                aes(boundary.distance, -depth, colour=regime, size=scalarmoment),shape=1, stroke=1., show.legend = F) +
#     geom_point( data=profile.2 %>% subset(  !is.na(depth.anom) & depth.anom < 0  & scalarmoment < 13  ),
#                 aes(boundary.distance, -depth, colour=regime, size=scalarmoment),
#                 stroke=.3, show.legend = F) +
#     scale_radius(range=c(0,5)) #+ scale_color_manual(values=c("red",  "green4", "blue", "green2","grey50"))
# )
#
#
#
#
#
#
# # ggsave("~/Desktop/mexican.slab.png", width=10, height=6)
#
# # p.1b <-ggplot(data=cmt.slab.ppp$marks %>% subset( regime %in% c("NF","TF", "SS", "NS") & depth.anom> -0 & depth.anom < 70 ))+
# #   geom_point( aes(boundary.distance, -depth, colour=regime, size=scalarmoment))
#
# p.1<-gridExtra::grid.arrange(p.1a+labs(y=" \n ")+theme(legend.position= "None"),
#                              p.1c+theme(legend.position= "None")+labs(x=NULL),
#                              p.1d+theme(legend.position= "bottom")+
#                                guides(
#                                  size= guide_legend(show = FALSE)
#                                )
#                              , ncol=1, heights=c(4.9,2.8,3.5))
#
# # ggsave("~/Desktop/mexican.slab.dist.png", p.1,width=10, height=6)
# ggsave(paste0(fig.dir,"mexican.slab.dist.pdf"), p.1,width=10, height=13)
#
#
#
# #-----
#
# ##########
# #cmt.slab.ppp <- mex$cmt.ppp %>% subset( !is.na(depth.anom) & lon > -99.75 & lon < -96.5)
#
# #proj.cmt <-project2segment(cmt.slab.ppp  , mex$plates.ppp[mex$plates.ppp$marks %in% c("57", "54", "123")])
# plot(mex$plates.ppp , lwd=.5, col="darkgreen")
#
# plot(mex$plates.ppp[mex$plates.ppp$marks %in% c("57", "54", "123")], lwd=3, col="darkgreen", add=T)
# plot(mex$cmt.ppp, add=TRUE, col="red", pch=16, size=.5)
# #inds <-which(proj.cmt$mapXY !=1  )
# #v <- project2segment(X,Y)
# # Xproj <- proj.cmt$Xproj
# # cmt.slab.ppp$marks$pro.lon <- Xproj$x
# # cmt.slab.ppp$marks$pro.lat <- Xproj$y
# #cmt.slab.ppp$marks$boundary.distance <-geosphere::distGeo(cmt.slab.ppp %>% as.SpatialPoints.ppp(), Xproj %>% as.SpatialPoints.ppp() )/1000
# plot(mex$vo, add=TRUE, bg="red", pch=18, size=2)
#
# plot(Xproj, add=TRUE, pch=16, size=.3)
# #arrows(cmt.slab.ppp$x[inds], cmt.slab.ppp$y[inds], Xproj$x[inds], Xproj$y[inds], angle=10, length=0.05, lwd=.2, col="red")
# arrows(cmt.slab.ppp$x , cmt.slab.ppp$y , Xproj$x , Xproj$y , angle=10, length=0.1, lwd=.3, col="red")
#
#
#
# ###
# mex$ehb.ppp$marks$lon <-   mex$ehb.ppp$x
# mex$ehb.ppp$marks$lat <-   mex$ehb.ppp$y
# ehb.slab.ppp <- mex$ehb.ppp %>% subset(!is.na(depth.anom) & lon < -95.5)
# ehb.slab.ppp
# project2segment(ehb.slab.ppp  , mex$plates.ppp[mex$plates.ppp$marks %in% c("57", "54", "123")])  ->proj.ehb
# plot(mex$plates.ppp , lwd=.5, col="darkgreen")
#
# plot(mex$plates.ppp[mex$plates.ppp$marks %in% c("57", "54", "123")], lwd=3, col="darkgreen", add=T)
# plot(mex$ehb.ppp, add=TRUE, col="red", pch=16, size=.5)
# #inds <-which(proj.ehb$mapXY !=1  )
# v <- project2segment(X,Y)
# Xproj <- proj.ehb$Xproj
# ehb.slab.ppp$marks$pro.lon <- Xproj$x
# ehb.slab.ppp$marks$pro.lat <- Xproj$y
# ehb.slab.ppp$marks$boundary.distance <-geosphere::distGeo(ehb.slab.ppp %>% as.SpatialPoints.ppp(), Xproj %>% as.SpatialPoints.ppp() )/1000
# plot(mex$vo, add=TRUE, bg="red", pch=18, size=2)
#
# plot(Xproj, add=TRUE, pch=16, size=.3)
# #arrows(ehb.slab.ppp$x[inds], ehb.slab.ppp$y[inds], Xproj$x[inds], Xproj$y[inds], angle=10, length=0.05, lwd=.2, col="red")
# arrows(ehb.slab.ppp$x , ehb.slab.ppp$y , Xproj$x , Xproj$y , angle=10, length=0.1, lwd=.3, col="red")
#
# #geosphere::distGeo(ehb.slab.ppp %>% as.SpatialPoints.ppp(), Xproj %>% as.SpatialPoints.ppp() )
#
# ehb.slab.ppp$marks$m <- pmax(  ehb.slab.ppp$marks$mb,   ehb.slab.ppp$marks$ms,   ehb.slab.ppp$marks$mw)
# ggplot(data=ehb.slab.ppp$marks %>% subset(  m>3 &depth.anom> -0 & depth.anom < 70 ))+
#   geom_point( aes(boundary.distance, -depth, size=m), col="red3", alpha=.5) + scale_radius(range=c(0,10))
#
#
# +scale_size_manual(range=c(4,8))
# ehb.slab.ppp <- mex$ehb.ppp %>% subset(!is.na(depth.anom) & lon < -95.5)
# project2segment(ehb.slab.ppp  , mex$plates.ppp[mex$plates.ppp$marks %in% c("57", "54", "123")])  ->proj.ehb
# plot(mex$plates.ppp , lwd=.5, col="darkgreen")
# plot(mex$plates.ppp[mex$plates.ppp$marks %in% c("57", "54", "123")], lwd=3, col="darkgreen", add=T)
# plot(mex$ehb.ppp, add=TRUE, col="red", pch=16, size=.5)
# #inds <-which(proj.ehb$mapXY !=1  )
# v <- project2segment(X,Y)
# Xproj <- proj.ehb$Xproj
# ehb.slab.ppp$marks$pro.lon <- Xproj$x
# ehb.slab.ppp$marks$pro.lat <- Xproj$y
# ehb.slab.ppp$marks$boundary.distance <-geosphere::distGeo(ehb.slab.ppp %>% as.SpatialPoints.ppp(), Xproj %>% as.SpatialPoints.ppp() )/1000
# plot(mex$vo, add=TRUE, bg="red", pch=18, size=2)
#
# plot(Xproj, add=TRUE, pch=16, size=.3)
# #arrows(ehb.slab.ppp$x[inds], ehb.slab.ppp$y[inds], Xproj$x[inds], Xproj$y[inds], angle=10, length=0.05, lwd=.2, col="red")
# arrows(ehb.slab.ppp$x , ehb.slab.ppp$y , Xproj$x , Xproj$y , angle=10, length=0.1, lwd=.3, col="red")
#
# #geosphere::distGeo(ehb.slab.ppp %>% as.SpatialPoints.ppp(), Xproj %>% as.SpatialPoints.ppp() )
#
# map2_df(cmt.slab.ppp$marks[c("lon","lat")]  %>% as.matrix(),
#         cmt.slab.ppp$marks[c("pro.lon","pro.lat")]  %>% as.matrix(), gzAzimuth  ) %>% bind_rows()
# ff
