fig.dir<-"/Users/msandifo/Dropbox/msandifo/documents/programming/r/packages/slab/examples/figures/"

mex <-assemble_slab(slab.name="cam", extend.lims = c(-1, -7, 4, 1),  simplify=F, slab2=T)


#plates= mex$plates.ppp[mex$plates.ppp$marks %in% c("57", "54", "123") ]
plates= mex$plates[mex$plates$data$ID %in% c("57", "54", "123") ] %>% fortify()


project_ppp <- function(my.ppp, plates
                         ) {

  proj.cmt <-project2segment(my.ppp,  plates %>% as.ppp( c(0,1,0,1)))
#v <- project2segment(X,Y)?
Xproj <- proj.cmt$Xproj
my.ppp$marks$pro.lon <- proj.cmt$Xproj$x
my.ppp$marks$pro.lat <- proj.cmt$Xproj$y
my.ppp$marks$boundary.distance <-geosphere::distGeo(my.ppp %>% as.SpatialPoints.ppp(), Xproj %>% as.SpatialPoints.ppp() )/1000
my.ppp$marks$bearing <- bearing(my.ppp$marks[c("pro.lon","pro.lat")], my.ppp$marks[c("long","lat")]  )
my.ppp
}# from bounasary to point

cmt.slab.ppp <- project_ppp( mex$cmt.ppp %>%
                               subset( (!is.na(depth.anom) & long > -99.75 & long < -96.5) |
                                         (is.na(depth.anom) & long < -96.5 & long > -99.75 & lat >17.5 & lat<20)), plates
                             )

vo.ppp <-  project_ppp( mex$vo.ppp %>%
                          subset(  long > -99.75 & long < -96.5), plates )

# cmt.vz.ppp <-  mex$cmt.ppp %>% subset(!is.na(depth.anom) & lon < -96.5 & lon> -99.75 & lat >17.5 & lat<20)
# proj.cmt <-project2segment(cmt.vz.ppp  , mex$plates.ppp[mex$plates.ppp$marks %in% c("57", "54", "123")])
# #v <- project2segment(X,Y)
# Xproj <- proj.cmt$Xproj
# cmt.vz.ppp$marks$pro.lon <- proj.cmt$Xproj$x
# cmt.vz.ppp$marks$pro.lat <- proj.cmt$Xproj$y
# cmt.vz.ppp$marks$boundary.distance <-geosphere::distGeo(cmt.vz.ppp%>% as.SpatialPoints.ppp(), Xproj %>% as.SpatialPoints.ppp() )/1000
# cmt.vz.ppp$marks$bearing <- bearing(cmt.vz.ppp$marks[c("pro.lon","pro.lat")], cmt.vz.ppp$marks[c("lon","lat")]  )


profile.bearing <-bearing( cmt.slab.ppp$marks[c("pro.lon","pro.lat")] , cmt.slab.ppp$marks[c("long","lat")] )  %>% median()



project_axes <- function(df, axes="T", scale=10, bearing=18){

  if (axes=="T"){ azi= df$marks$t; plunge=df$marks$tp;
  if (is.na(bearing)) bearing= df$marks$bearing
  }
  if (axes=="P"){ azi= df$marks$p; plunge=df$marks$pp;  if (is.na(bearing)) bearing= df$marks$bearing }
  if (axes=="B"){ azi= df$marks$b; plunge=df$marks$np;  if (is.na(bearing)) bearing= df$marks$bearing }

  pro.bearing <- (azi - bearing)
  pro.bearing[pro.bearing>180] <-  pro.bearing[pro.bearing>180]-360
  ends.xy <- scale*cos((pro.bearing)* pi/180)
  ends.xy <- scale* cos(plunge* pi/180)
  ends.z <- scale* sin(plunge* pi/180)

  pro.sign <- 0 * pro.bearing +1
  pro.sign[abs(pro.bearing) < 90] <- -1
  #print(data.frame(azi, pro.bearing, azi-pro.bearing, pro.sign))

 # if (abs(pro.bearing) > 90) ends.z <- -ends.z
  data.frame(x=df$marks$boundary.distance+ends.xy,
             xend= df$marks$boundary.distance-ends.xy,
             y= df$marks$depth-(ends.z*pro.sign),
             yend= df$marks$depth+(ends.z*pro.sign),
             regime=df$marks$regime,
             pro.bearing=pro.bearing )
}

pa<-project_axes(cmt.slab.ppp, scale=5, bearing=profile.bearing)

proj.laea <- "+proj=laea +lat_0=0 +lon_0=-93"



coasts.df <- mex$coast%>% fortify()
mex$vo$marks$long <- mex$vo$x
mex$vo$marks$lat <-mex$vo$y


(p1<- ggplot( ) +
  ggalt::geom_cartogram( data=coasts.df, aes(map_id=id ), map=coasts.df  ) +
  ggalt::coord_proj(proj.laea, xlim=extent(mex$slab)[1:2],ylim=extent(mex$slab)[3:4])+
  geom_polygon(data=coasts.df   ,
            aes(long-360,lat , group=group ), size=0,   col="white", fill="grey90" , show.legend = F) +
geom_point(data=mex$vo.ppp$marks, aes(long, lat), shape =24,  size=3, col="grey30", fill="Yellow") +
  geom_path(data=mex$plates %>% fortify()  ,
            aes(long,lat ,group=id ), size=.4, show.legend = F)+
  geom_point(data=mex$cmt.ppp$marks %>% subset(   depth.anom >= -15    ),
           aes(long, lat, size=depth.anom, col=regime),
           shape=1 , stroke=1)+
  geom_point(data=mex$cmt.ppp$marks %>% subset(  is.na(depth.anom | depth.anom < -15)    ),
             aes(long, lat,  col=regime), size=2,
             alpha=1,  stroke=0,
             show_guide=F)  +
    theme(legend.position="bottom")+
  labs(x=NULL, y=NULL)
)



 # geom_point(data=mex$cmt.ppp$marks %>% subset(regime=="NF" & depth.anom> -10 & depth.anom<60), col="red3", shape=1)

ggsave(paste0(fig.dir,"/mexican.slab.pdf"), width=10, height=6)


##########
#cmt.slab.ppp <- mex$cmt.ppp %>% subset( !is.na(depth.anom) & lon > -99.75 & lon < -96.5)

#proj.cmt <-project2segment(cmt.slab.ppp  , mex$plates.ppp[mex$plates.ppp$marks %in% c("57", "54", "123")])
plot(mex$plates.ppp , lwd=.5, col="darkgreen")

plot(mex$plates.ppp[mex$plates.ppp$marks %in% c("57", "54", "123")], lwd=3, col="darkgreen", add=T)
plot(mex$cmt.ppp, add=TRUE, col="red", pch=16, size=.5)
#inds <-which(proj.cmt$mapXY !=1  )
#v <- project2segment(X,Y)
# Xproj <- proj.cmt$Xproj
# cmt.slab.ppp$marks$pro.lon <- Xproj$x
# cmt.slab.ppp$marks$pro.lat <- Xproj$y
#cmt.slab.ppp$marks$boundary.distance <-geosphere::distGeo(cmt.slab.ppp %>% as.SpatialPoints.ppp(), Xproj %>% as.SpatialPoints.ppp() )/1000
plot(mex$vo, add=TRUE, bg="red", pch=18, size=2)

plot(Xproj, add=TRUE, pch=16, size=.3)
#arrows(cmt.slab.ppp$x[inds], cmt.slab.ppp$y[inds], Xproj$x[inds], Xproj$y[inds], angle=10, length=0.05, lwd=.2, col="red")
arrows(cmt.slab.ppp$x , cmt.slab.ppp$y , Xproj$x , Xproj$y , angle=10, length=0.1, lwd=.3, col="red")



(  p.1a<-p1+ geom_segment(data=cmt.slab.ppp$marks %>%subset( pro.lon > -104), aes(x=long, y=lat, xend= pro.lon, yend=pro.lat), size=.2, colour="red", alpha=.3))


#geosphere::distGeo(cmt.slab.ppp %>% as.SpatialPoints.ppp(), Xproj %>% as.SpatialPoints.ppp() )

(p.1b <-ggplot()+
  geom_point(data=cmt.slab.ppp$marks %>% subset(  !is.na(depth.anom) & depth.anom >= 0  & scalarmoment < 13  & pro.lon > -104),
             aes(boundary.distance, -depth, colour=regime, size=scalarmoment),shape=1, stroke=1., show.legend = F) +
  geom_point( data=cmt.slab.ppp$marks %>% subset(  !is.na(depth.anom) & depth.anom < 0  & scalarmoment < 13 & pro.lon > -104 ),
              aes(boundary.distance, -depth, colour=regime, size=scalarmoment),
                stroke=.3, show.legend = F) +
  scale_radius(range=c(0,5)) #+ scale_color_manual(values=c("red",  "green4", "blue", "green2","grey50"))
)

pa<-project_axes(cmt.slab.ppp, scale=7, axis=
                   "P", bearing=profile.bearing)
ta<-project_axes(cmt.slab.ppp, scale=7, axis=
                   "T", bearing=profile.bearing)


ta  <- cbind(cmt.slab.ppp$marks, ta )
pa  <- cbind(cmt.slab.ppp$marks, pa )

(p.1c <- p.1b +
  geom_segment(data=pa%>% subset(depth.anom> 0 | depth>40), aes(x=x,xend=xend,y= -y, yend= -yend, col=regime), size=.25)+
 # geom_segment(data=ta %>% subset(depth.anom> 0 | depth>40),  aes(x=x,xend=xend,y= -y, yend= -yend, col=regime))+
    coord_fixed(ratio = 1)

)

(p.1d <- p.1b +
 #   geom_segment(data=pa%>% subset(depth.anom> 0 | depth>40), aes(x=x,xend=xend,y= -y, yend= -yend, col=regime), size=.2)+
    geom_segment(data=ta %>% subset(depth.anom> 0 | depth>40),
                 aes(x=x,xend=xend,y= -y, yend= -yend, col=regime), size=.25)+
    coord_fixed(ratio = 1)

)



 # ggsave("~/Desktop/mexican.slab.png", width=10, height=6)

  # p.1b <-ggplot(data=cmt.slab.ppp$marks %>% subset( regime %in% c("NF","TF", "SS", "NS") & depth.anom> -0 & depth.anom < 70 ))+
  #   geom_point( aes(boundary.distance, -depth, colour=regime, size=scalarmoment))

  ( p.1<-gridExtra::grid.arrange(p.1a+labs(y=" \n ")+theme(legend.position= "None"),
                                p.1c+theme(legend.position= "None")+labs(x=NULL),
                                p.1d+theme(legend.position= "bottom")+
                                  guides(
                                    size= guide_legend(show = FALSE)
                                  )
                                , ncol=1, heights=c(4.9,2.8,3.325))
)
  # ggsave("~/Desktop/mexican.slab.dist.png", p.1,width=10, height=6)
  ggsave(paste0(fig.dir,"mexican.slab.dist.pdf"), p.1,width=10, height=13)


###
  mex$ehb.ppp$marks$lon <-   mex$ehb.ppp$x
  mex$ehb.ppp$marks$lat <-   mex$ehb.ppp$y
  ehb.slab.ppp <- mex$ehb.ppp %>% subset(!is.na(depth.anom) & lon < -95.5)
  ehb.slab.ppp
  project2segment(ehb.slab.ppp  , mex$plates.ppp[mex$plates.ppp$marks %in% c("57", "54", "123")])  ->proj.ehb
  plot(mex$plates.ppp , lwd=.5, col="darkgreen")

  plot(mex$plates.ppp[mex$plates.ppp$marks %in% c("57", "54", "123")], lwd=3, col="darkgreen", add=T)
  plot(mex$ehb.ppp, add=TRUE, col="red", pch=16, size=.5)
  #inds <-which(proj.ehb$mapXY !=1  )
  v <- project2segment(X,Y)
  Xproj <- proj.ehb$Xproj
  ehb.slab.ppp$marks$pro.lon <- Xproj$x
  ehb.slab.ppp$marks$pro.lat <- Xproj$y
  ehb.slab.ppp$marks$boundary.distance <-geosphere::distGeo(ehb.slab.ppp %>% as.SpatialPoints.ppp(), Xproj %>% as.SpatialPoints.ppp() )/1000
  plot(mex$vo, add=TRUE, bg="red", pch=18, size=2)

  plot(Xproj, add=TRUE, pch=16, size=.3)
  #arrows(ehb.slab.ppp$x[inds], ehb.slab.ppp$y[inds], Xproj$x[inds], Xproj$y[inds], angle=10, length=0.05, lwd=.2, col="red")
  arrows(ehb.slab.ppp$x , ehb.slab.ppp$y , Xproj$x , Xproj$y , angle=10, length=0.1, lwd=.3, col="red")

  #geosphere::distGeo(ehb.slab.ppp %>% as.SpatialPoints.ppp(), Xproj %>% as.SpatialPoints.ppp() )

  ehb.slab.ppp$marks$m <- pmax(  ehb.slab.ppp$marks$mb,   ehb.slab.ppp$marks$ms,   ehb.slab.ppp$marks$mw)
  ggplot(data=ehb.slab.ppp$marks %>% subset(  m>3 &depth.anom> -0 & depth.anom < 70 ))+
    geom_point( aes(boundary.distance, -depth, size=m), col="red3", alpha=.5) + scale_radius(range=c(0,10))


  +scale_size_manual(range=c(4,8))
  ehb.slab.ppp <- mex$ehb.ppp %>% subset(!is.na(depth.anom) & lon < -95.5)
  project2segment(ehb.slab.ppp  , mex$plates.ppp[mex$plates.ppp$marks %in% c("57", "54", "123")])  ->proj.ehb
  plot(mex$plates.ppp , lwd=.5, col="darkgreen")
  plot(mex$plates.ppp[mex$plates.ppp$marks %in% c("57", "54", "123")], lwd=3, col="darkgreen", add=T)
  plot(mex$ehb.ppp, add=TRUE, col="red", pch=16, size=.5)
  #inds <-which(proj.ehb$mapXY !=1  )
  v <- project2segment(X,Y)
  Xproj <- proj.ehb$Xproj
  ehb.slab.ppp$marks$pro.lon <- Xproj$x
  ehb.slab.ppp$marks$pro.lat <- Xproj$y
  ehb.slab.ppp$marks$boundary.distance <-geosphere::distGeo(ehb.slab.ppp %>% as.SpatialPoints.ppp(), Xproj %>% as.SpatialPoints.ppp() )/1000
  plot(mex$vo, add=TRUE, bg="red", pch=18, size=2)

  plot(Xproj, add=TRUE, pch=16, size=.3)
  #arrows(ehb.slab.ppp$x[inds], ehb.slab.ppp$y[inds], Xproj$x[inds], Xproj$y[inds], angle=10, length=0.05, lwd=.2, col="red")
  arrows(ehb.slab.ppp$x , ehb.slab.ppp$y , Xproj$x , Xproj$y , angle=10, length=0.1, lwd=.3, col="red")

  #geosphere::distGeo(ehb.slab.ppp %>% as.SpatialPoints.ppp(), Xproj %>% as.SpatialPoints.ppp() )

  map2_df(cmt.slab.ppp$marks[c("lon","lat")]  %>% as.matrix(),
          cmt.slab.ppp$marks[c("pro.lon","pro.lat")]  %>% as.matrix(), gzAzimuth  ) %>% bind_rows()

