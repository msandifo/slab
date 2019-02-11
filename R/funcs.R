#pwin  not really needed

#' Title
#'
#' @param ppp
#' @param win
#' @param n
#'
#' @return
#' @export
#'
#' @examples
pwin <- function(ppp,  win=list(x=c(-97.5, -99.95, -98.3,-96.3   ) %>% rev() ,   y=c(14.7, 15.6, 19.7,19.0 ) %>% rev()),
                 n=F){

  p2 <- owin(poly=win) # owin(poly=list(x=c(-97.5, -99.95, -98.3,-96.3   ) %>% rev() , y=c(14.7, 15.6, 19.7,19.0 ) %>% rev()))
  full.win <-list(x=c(as.owin(ppp)$xrange , as.owin(ppp)$xrange%>% rev ),
                  y= c(as.owin(ppp)$yrange[1] ,as.owin(ppp)$yrange[1], as.owin(ppp)$yrange[2] ,as.owin(ppp)$yrange[2]))

  p2n <-  owin(poly=list(full.win, list(x=win$x %>% rev   , y=win$y %>% rev)))

  if(n==T) return(p2n) else return(p2)
}

project_ppp1 <- function(...){
  project_ppp(...)
}

#' Title
#'
#' @param my.ppp
#' @param win
#' @param plates
#' @param profile
#' @param add
#' @param test
#'
#' @return
#' @export
#'
#' @examples
project_ppp<- function(my.ppp,
                        win=pwin(my.ppp),
                        plates=bird.plates.psp,
                        profile="p0",
                        add=F, test=F){

 if (test) { my.ppp=isc.ppp[p2]
  win=p2
  plates=bird.plates.psp
  profile="p0"
  add=F
 }
  #cmt.my.ppp <- cmt.my.ppp %>% subset(dplyr::distinct(cmt.my.ppp$marks$eventname, .keep_all=T))
  if (!add){
    my.ppp$marks$profile <-NA
    my.ppp$marks$bearing <-NA
    my.ppp$marks$pro.lat <-NA
    my.ppp$marks$pro.long <-NA
    my.ppp$marks$distance <-NA
  }

  my.ppp[win]$marks$profile <- profile
  my.ppp.proj <-project2segment(my.ppp[ win], plates[win])
  my.ppp[win]$marks$pro.long <-my.ppp.proj$Xproj$x
  my.ppp[win]$marks$pro.lat <- my.ppp.proj$Xproj$y

  my.ppp[win]$marks$bearing <- geosphere::bearing( my.ppp[win]$marks[c("pro.long", "pro.lat")] %>% as.matrix, my.ppp[win]$marks[c("long", "lat")] %>% as.matrix)
  my.ppp[win]$marks$distance <-geosphere::distGeo(my.ppp[win] %>% as.SpatialPoints.ppp(),  my.ppp.proj$Xproj %>% as.SpatialPoints.ppp() )/1000

  my.ppp[win]$marks$plates <- split_ppp(my.ppp[win]$marks)

  my.ppp[win]$marks$distance[my.ppp[win]$marks$plates=="0"] <-  (-my.ppp[win]$marks$distance)

  #print(mean)
  #  plot(my.ppp[win]$marks$distance, my.ppp[win]$marks$bearing)

   return(my.ppp)
}

#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
split_ppp<- function(df= cmt1.ppp[p2]$marks){

  med.bearing<-median(df$bearing %% 180)
  splits <- med.bearing + c(-90,90)
  df$plate <- "1"
  df$plate[df$bearing<splits[1] | df$bearing>splits[2]] <-"0"
  df$plate
}

project_axes1 <- function(...){
  project_axes(...)
}

#' Title
#'
#' @param df
#' @param axis
#' @param scale
#' @param bearing
#' @param profile
#' @param test
#'
#' @return
#' @export
#'
#' @examples
project_axes <- function(df, axis="T", scale=10, bearing=18, profile="1", test=F){
  if(test){df<-pro.ppp
  axis<- "T"
  bearing=18
  scale=10
  profile="1"
  }
  if (axis=="T"){ azi= df$marks$ta; plunge=df$marks$tp;
  if (is.na(bearing)) bearing= df$marks$bearing
  }
  if (axis=="P"){ azi= df$marks$pa; plunge=df$marks$pp;  if (is.na(bearing)) bearing= df$marks$bearing }
  if (axis=="B"){ azi= df$marks$ba; plunge=df$marks$bp;  if (is.na(bearing)) bearing= df$marks$bearing }

  pro.bearing <- (azi - bearing)
  pro.bearing[pro.bearing>180] <-  pro.bearing[pro.bearing>180]-360
  ends.xy.plan <-  cos((pro.bearing)* pi/180)
  ends.xy <- -scale* cos(plunge* pi/180) *  ends.xy.plan
  ends.z <- scale* sin(plunge* pi/180)


  # pro.sign <- 0 * pro.bearing +1
  # pro.sign[abs(pro.bearing) < 90] <- -1
  #print(data.frame(azi, pro.bearing, azi-pro.bearing, pro.sign))

  # if (abs(pro.bearing) > 90) ends.z <- -ends.z
  data.frame(x=df$marks$distance+ ends.xy  ,
             xend= df$marks$distance-ends.xy ,
             y= df$marks$depth-(ends.z ),
             yend= df$marks$depth+(ends.z ),
             regime=df$marks$regime,
             pro.bearing=pro.bearing,
             profile=profile,
             axis= axis)
}



#  p2a<-split_owin(bird.plates.psp[p2], p2)
# p2b<-split_owin(bird.plates.psp[p2], inds=c(2,1), reverse=T, p2)
# plot(p2); plot(p2a, add=T);plot(p2b, add=T, border="red")

#' Title
#'
#' @param my.psp
#' @param my.owin
#' @param inds
#' @param reverse
#'
#' @return
#' @export
#'
#' @examples
split_owin <- function(my.psp,
                       my.owin,
                       inds=c(3,4),
                       reverse=F){

 my.owin.seg <- cbind(my.owin$bdry[[1]]$x, my.owin$bdry[[1]]$y)[inds[1]:inds[2],]

  xys <-rbind(
    rbind(my.psp$ends[, 1:2] %>% as.matrix,
          tail(my.psp$ends,1)[, 3:4] %>% as.matrix),
    my.owin.seg )

  if (!reverse) my.poly<- list(x=xys[,1] , y=xys[,2]  ) else my.poly<- list(x=xys[,1] %>% rev() , y=xys[,2] %>% rev() )
  #print(my.poly)
  new.owin <- owin(poly=my.poly)
  new.owin
}

# left right to split owin halfs

#' Title
#'
#' @param long
#' @param lat
#' @param length
#' @param width
#' @param angle
#' @param owin
#' @param clockwise
#' @param left
#' @param right
#' @param top
#' @param bottom
#'
#' @return
#' @export
#'
#' @examples
get_poly <- function(long=-100, lat=15, length=5, width=2, angle=15, owin=T,
                     clockwise=F, left=1, right=1, top=1, bottom=1 #for holes i
                     ){
  adtor <- angle*pi/180

  long1<- long+ length*sin(adtor)
  lat1 <- lat+ length*cos(adtor)
longs = c( long  - right*bottom*width/2*cos(adtor), long + left*bottom*width/2*cos(adtor), long1 + left*top*width/2*cos(adtor), long1-right*top*width/2*cos(adtor))
lats = c( lat + right*bottom*width/2*sin(adtor), lat - left*bottom*width/2*sin(adtor),  lat1  -left*top*width/2*sin(adtor),  lat1 +right*top*width/2*sin(adtor))

if (clockwise) {longs=rev(longs); lats=rev(lats)}
if (!owin)
  return(list(x=longs, y=lats)) else return(owin(poly=list(x=longs, y=lats)))
}

#' Title
#'
#' @param p
#' @param segment
#' @param n
#' @param psp
#' @param win
#'
#' @return
#' @export
#'
#' @examples
get_pline <-function(p, segment=4, n=1, psp=T, win=NA){

  x<- p$bdry[[1]]$x
  y<- p$bdry[[1]]$y
   segment = segment%%length(x)

  if (segment== 0) {ind=1; segment= length(x)} else ind = segment + 1

  if (segment > length(x)) segment = segment%%length(x)

  longs= x[c(segment, ind)]
  lats=y[c(segment, ind)]

  x = seq(longs[1], longs[2],  (diff(longs))/n)
  y= seq(lats[1], lats[2],  (diff(lats))/n)
  if(is.na(win)) win = owin(xrange=c(min(longs), max(longs)),
                            yrange=c(min(lats), max(lats)))

  if (!psp)  return(list(x= x, y= y )) else
    return( as.psp(data.frame(x0=head(x,-1),y0=head(y,-1), x1=tail(x,-1),y1= tail(y,-1)), window=win))

}
#' Title
#'
#' @param long
#' @param lat
#' @param length
#' @param width
#' @param angle
#' @param psp
#' @param win
#' @param n
#'
#' @return
#' @export
#'
#' @examples
get_line <- function(long=-100, lat=15, length=5,  width=0,  angle=15, psp=T, win=NA, n=101 ){


  adtor <- angle*pi/180

   long1<- long+ length*sin(adtor)
   lat1 <- lat+ length*cos(adtor)

  longs = c( long  +   width/2*cos(adtor),  long1  +   width/2*cos(adtor) )
  lats = c(  lat -    width/2*sin(adtor),  lat1  -  width/2*sin(adtor) )


  # if(is.na(win)) win = owin(xrange=c(min(c(long,long1)), max(c(long,long1))),
  #                                    yrange=c(min(c(lat,lat1)), max(c(lat,lat1))))

  if(is.na(win)) win = owin(xrange=c(min(longs), max(longs)),
                            yrange=c(min(lats), max(lats)))



  # x = seq(long, long1, abs(diff(c(long, long1)))/n)
  # y= seq(lat, lat1, abs(diff(c(lat, lat1)))/n)
  x = seq(longs[1], longs[2],  (diff(longs))/n)
  y= seq(lats[1], lats[2],  (diff(lats))/n)


   if (!psp)  return(list(x= x, y= y )) else
     return( as.psp(data.frame(x0=head(x,-1),y0=head(y,-1), x1=tail(x,-1),y1= tail(y,-1)), window=win))
}



#' Title
#'
#' @param ras
#' @param dmin
#'
#' @return
#' @export
#'
#' @examples
ras_owin <- function(ras, dmin=.01){

  ras %>% as.im() %>% as.owin() %>% simplify.owin(dmin=dmin)
}


#' Title
#'
#' @param ow
#' @param close
#' @param df
#'
#' @return
#' @export
#'
#' @examples
owin_poly <- function(ow, close=T,df=T){
  pol <-ow$bdry[[1]]
  if (close) {pol$x <- c(pol$x, pol$x[1]);pol$y <- c(pol$y, pol$y[1]) }
  if(df) pol <- as.data.frame(pol)
  return(pol)
}
  #  <- #get_poly() %>% plot()


#' Title
#'
#' @param longlat
#' @param slab
#' @param plates
#' @param win
#' @param ll
#'
#' @return
#' @export
#'
#' @examples
get_slab_details<- function(longlat=c(-94.11, 14.85), slab, plates=NA, win=NA, ll=F){

#  longlat=cmt.e; slab=mex$slab; plates=bird.plates.psp; win=NA
  if (is.vector(longlat) ) longlat <- data.frame(long=longlat[1], lat=longlat[2])

  if (is.data.frame(longlat) ) names(longlat) <- c("long", "lat")

  if (is.na(win)) win<-owin( xrange= c(min(longlat$long)-1, max(longlat$long)+1),yrange=c(min(longlat$lat)-1, max(longlat$lat)+1 ))


  cells =cellFromXY(slab,longlat%>% as.matrix())

  slab.details <-data.frame(
                             slab.depth=extract(slab$depth, cells),
        slab.dip=extract(slab$dip, cells),
        slab.strike= extract(slab$strike, cells))
  if (is.psp(plates)) {
    p.longlat <-project2segment( as.ppp(longlat, win), plates)

    p.longlat.m <- as.data.frame( p.longlat$Xproj)  %>% as.matrix( )
   # p.longlat <- c(my.ppp.proj$Xproj$x, my.ppp.proj$Xproj$y)

   slab.details$bound.bearing <- geosphere::bearing(     p.longlat.m , longlat  )


   slab.details$bound.distance <-geosphere::distGeo( longlat  ,  p.longlat.m  )/1000

   if(ll){ long=  slab.details$longlat$long
   slab.details$lat=longlat$lat}

  }
  return(slab.details)
}


#' Title
#'
#' @param p
#'
#' @return
#' @export
#'
#' @examples
poly_dims<-function(p){

  bdry <-data.frame(long= p$bdry[[1]]$x, lat= p$bdry[[1]]$y)
  lx <- length(bdry$long)
  distances <- geosphere::distGeo( c( bdry$long[1],  bdry$lat[1]),  c(bdry$long[2], bdry$lat[2]))
  for (i in 2:(lx-1))  distances <- c(distances,  geosphere::distGeo( c( bdry$long[i],  bdry$lat[i]),  c(bdry$long[i+1], bdry$lat[i+1])) )

  distances <- c(distances,  geosphere::distGeo( c( bdry$long[lx],  bdry$lat[lx]),  c(bdry$long[1], bdry$lat[1])))

  return(list(bdry=bdry, distances=distances/1000))
}

