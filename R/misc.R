#' Title
#'
#' @param rad
#'
#' @return
#' @export
#'
#' @examples
rad <- function(rad) {(rad * 180) / (pi)}

#' Title
#'
#' @param deg
#'
#' @return
#' @export
#'
#' @examples
deg<- function(deg) {(deg * pi) / (180)}



#' Title
#'
#' @param plates
#'
#' @return
#' @export
#'
#' @examples
get_plate_boundary_ids <- function(plates) {
  purrr::map(1:length(plates@lines), function(i) plates@lines[[i]]@ID) %>% unlist()

}


#'
#' #' Title
#' #'
#' #' @param df
#' #' @param axis
#' #' @param scale
#' #' @param bearing
#' #' @param profile
#' #' @param test
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' project_axes <- function(df, axis="T", scale=10, bearing=18, profile="1", test=F){
#'   if(test){df<-pro.ppp
#'   axis<- "T"
#'   bearing=18
#'   scale=10
#'   profile="1"
#'   }
#'   if (axis=="T"){ azi= df$marks$ta; plunge=df$marks$tp;
#'   if (is.na(bearing)) bearing= df$marks$bearing
#'   }
#'   if (axis=="P"){ azi= df$marks$pa; plunge=df$marks$pp;  if (is.na(bearing)) bearing= df$marks$bearing }
#'   if (axis=="B"){ azi= df$marks$ba; plunge=df$marks$bp;  if (is.na(bearing)) bearing= df$marks$bearing }
#'
#'   pro.bearing <- (azi - bearing)
#'   pro.bearing[pro.bearing>180] <-  pro.bearing[pro.bearing>180]-360
#'   ends.xy.plan <-  cos((pro.bearing)* pi/180)
#'   ends.xy <- -scale* cos(plunge* pi/180) *  ends.xy.plan
#'   ends.z <- scale* sin(plunge* pi/180)
#'
#'   # pro.sign <- 0 * pro.bearing +1
#'   # pro.sign[abs(pro.bearing) < 90] <- -1
#'   #print(data.frame(azi, pro.bearing, azi-pro.bearing, pro.sign))
#'
#'   # if (abs(pro.bearing) > 90) ends.z <- -ends.z
#'   data.frame(x=df$marks$boundary.distance+ ends.xy  ,
#'              xend= df$marks$boundary.distance-ends.xy ,
#'              y= df$marks$depth-(ends.z ),
#'              yend= df$marks$depth+(ends.z ),
#'              regime=df$marks$regime,
#'              pro.bearing=pro.bearing,
#'              profile=profile,
#'              axis= axis)
#' }

#' project_ppp1 <- function() project_ppp(...)
#' #' Title
#' #'
#' #' @param my.ppp
#' #' @param plates
#' #' @param profile
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' project_ppp <- function(my.ppp, plates, profile=NULL) {
#'
#'   proj.cmt <-project2segment(my.ppp,  plates)
#'   #v <- project2segment(X,Y)
#'   Xproj <- proj.cmt$Xproj
#'   my.ppp$marks$pro.long <- proj.cmt$Xproj$x
#'   my.ppp$marks$pro.lat <- proj.cmt$Xproj$y
#'   my.ppp$marks$boundary.distance <-geosphere::distGeo(my.ppp %>% as.SpatialPoints.ppp(), Xproj %>% as.SpatialPoints.ppp() )/1000
#'   my.ppp$marks$bearing <- bearing(my.ppp$marks[c("pro.long","pro.lat")],
#'                                   dplyr::select(my.ppp$marks, starts_with("lon"), starts_with("lat") )
#'   )
#'   if (!is.null(profile) ) my.ppp$marks$profile <- profile
#'   return(my.ppp)
#' }# from bo
#'
#'
#' #' vectroized version of gzAzimuth
#' #'
#' #' @param from
#' #' @param to
#' #' @param type
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' bearing <-function (from, to, type = "snyder_sphere")
#' {
#'   deg2rad <- function(x) x * pi/180
#'   rad2deg <- function(x) x * 180/pi
#'
#'   lon <- -deg2rad(from[, 1])
#'   lat <- deg2rad(from[, 2])
#'   lon0 <- -deg2rad(to[,1])
#'   lat0 <- deg2rad(to[,2])
#'
#'
#'   dflon = lon - lon0
#'   if (type == "abdali")
#'     res <- base::atan2(sin(dflon), ((cos(lat) * tan(lat0)) - (sin(lat) *
#'                                                                 cos(dflon))))
#'   else if (type == "snyder_sphere")
#'     res <- base::atan2((cos(lat0) * sin(dflon)), (cos(lat) * sin(lat0)) -
#'                          (sin(lat) * cos(lat0) * cos(dflon)))
#'   else stop("type unkown")
#'   is.na(res) <- lon == lon0 & lat == lat0
#'   rad2deg(res)
#' }
#'
#'

#' Title
#'
#' @param pp
#' @param full.extent
#' @param crs
#' @param remove.marks
#'
#' @return
#' @export
#'
#' @examples
ppp2spdf <-function(pp,  full.extent, crs="+init=epsg:4326", remove.marks=T){
  pp <-
    crop(pp %>% maptools::as.SpatialPointsDataFrame.ppp(),
         full.extent)
  crs(pp) <- "+init=epsg:4326"
  if(remove.marks) names(pp) <- str_replace(names(pp) , "marks.","")
  names(pp) <- str_to_lower(names(pp))
  return(pp)
}

#' Title
#'
#' @param sp
#'
#' @return
#' @export
#'
#' @examples
spp2spl<- function(sp ){

  if ( (sp %>% class())[1] =="SpatialPolygons")    as(sp, "SpatialLines") else
    as(sp, "SpatialLines")
}



wsm_lines<- function(wsm,   scale=1.5, psp=F,Pax=FALSE, Tax=FALSE){
  #returns stress trajectories
  #scale=1
  message(wsm$marks %>% str(max=1))

  if (Pax==TRUE) {wsm$marks$ilength = (cos(rad(wsm$marks$pp))*scale) #exagerates
  Tax =FALSE } else if (Tax==TRUE) wsm$marks$ilength = (cos(rad(wsm$marks$tp))*scale) else
    wsm$marks$ilength=scale
  sinazi<- sin(wsm$marks$azi*pi/180)*wsm$marks$ilength
  cosazi<- cos(wsm$marks$azi*pi/180)*wsm$marks$ilength
  x<- matrix(ncol=length(wsm$y),nrow=3)
  y<- matrix(ncol=length(wsm$y),nrow=3)
  z<- matrix(ncol=length(wsm$y),nrow=3)
  x[1,]<-wsm$x+ sinazi
  x[2,]<-wsm$x- sinazi
  y[1,]<-wsm$y+ cosazi
  y[2,]<-wsm$y- cosazi
  z[1,]<-0
  z[2,]<-0
  if (Pax==TRUE)  {
    z[1,wsm$marks$p<180]<-1
    z[2,wsm$marks$p>=180]<-1
  }
  if (Tax==TRUE)  {
    z[1,wsm$marks$t<180]<-1
    z[2,wsm$marks$t>=180]<-1
  }


  # cols<- vector("character",length(wsm$REGIME))
  # cols[wsm$REGIME=="NF"] <-"Red"
  # cols[wsm$REGIME=="NS"] <-"Red"
  # cols[wsm$REGIME=="SS"] <-"Green"
  # cols[wsm$REGIME=="TF"] <-"Blue"
  # cols[wsm$REGIME=="TS"] <-"Blue"
  # cols[wsm$REGIME=="U"] <-"Black"
  # cols<- rep(cols, each=3)
  if (psp==T){
    xrange<- c(min(x[2,])-wsm$marks$ilength, max(x[1,])+wsm$marks$ilength)
    yrange<- c(min(y[2,])-wsm$marks$ilength, max(y[1,])+wsm$marks$ilength)
    print(xrange)
    print(yrange)
    return(psp(x[1,], y[1,], x[2,], y[2,], owin(xrange,yrange), marks =wsm$marks$regime))
  } else {
    return(data.frame(x=as.vector(x), y=as.vector(y),z=as.vector(z), regime=rep(wsm$marks$regime, each=3)))
  }
}





#' Title
#'
#' @param x
#' @param N
#'
#' @return
#' @export
#'
#' @examples
degrees <- function (x , N=T)
{
  if (N==T) {dir.p ="N" ; dir.n="S"} else {dir.p ="E" ; dir.n="W"}
  dir= rep(dir.p, length(x))
  dir[x<0] <-dir.n

  if (length(x) == 0)
    return(character())
  paste0(abs(x), "°", dir)
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
degreesN <- function(x){
  degrees(x, N=T)}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
degreesE <- function(x){
  degrees(x, N=F)
}


#' Title
#'
#' @param x
#' @param y
#' @param r
#'
#' @return
#' @export
#'
#' @examples
plotElipse <- function(x, y, r) {#Gary's function ;-)
  angles <- seq(0,2*pi,length.out=360)
  lines(r*cos(angles)+x,r*sin(angles)+y)
}
#' Title
#'
#' @param LonDec
#' @param LatDec
#' @param Km
#' @param plot
#'
#' @return
#' @export
#'
#' @examples
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




#' @title Directional bearing between two geographic locations
#' @description \code{newLonLat} calculates a new lon/lat position given an sarting lon/lat
#' position, a bearing and a distance to the new lon/lat position.
#' Either the lon and lat arguments or the bearing and distance arguments can be a vector,
#' whereby a vector of new locations will be calculated.
#'
#' @param lon Longitude 1 (in decimal degrees)
#' @param lat Latitude 1 (in decimal degrees)
#' @param bearing Longitude 2 (in decimal degrees)
#' @param distance Distance to new lon/lat position (km)
#'
#' @return List of new lon/lat locations
#'
#' @examples
#' # Single new lon/lat position calculation
#' newLonLat(0,0,45,1000)
#'
#' # Vector of new lon/lat positions and plot
#' startPos <- list(lon=0,lat=0)
#' endPos <- newLonLat(startPos$lon, startPos$lat, seq(0,360,20), 1000)
#' plot(1, t="n", xlim=range(endPos$lon), ylim=range(endPos$lat), xlab="lon", ylab="lat")
#' segments(startPos$lon, startPos$lat, endPos$lon, endPos$lat, col=rainbow(length(endPos$lon)))
#' points(startPos$lon, startPos$lat)
#' points(endPos$lon, endPos$lat)
#'
#' @keywords geographic
#'
#' @export
#'
newLonLat <-
  function (lon, lat, bearing, distance)
  {
    rad <- pi/180
    a1 <- lat * rad
    a2 <- lon * rad
    tc <- bearing * rad
    d <- distance/6378.145
    nlat <- asin(sin(a1) * cos(d) + cos(a1) * sin(d) * cos(tc))
    dlon <- atan2(sin(tc) * sin(d) * cos(a1), cos(d) - sin(a1) *
                    sin(nlat))
    nlon <- ((a2 + dlon + pi)%%(2 * pi)) - pi
    npts <- list(lon=nlon/rad, lat=nlat/rad)
    return(npts)
  }



#' Title
#'
#' @param regions
#'
#' @return
#' @export
#'
#' @examples
assemble_isc<-function(regions=c(3:8)){

  map_dfr( c(34:35, regions  ),join_isc) -> isc.df
  return(isc.df[,c( 7,11, 5,6)]  %>% subset(!is.na(DEPTH)) %>% mutate(DEPTH = -DEPTH*1000))
}



get_isc_srn<-function(year=year(Sys.Date()),
                      srn=8,
                      agcy="Any",
                      min.mag=3.5,
                      out.dir="/Volumes/data/data/global/quakes/isc/srn/"){

  srn.q.string <- paste0("http://www.isc.ac.uk/cgi-bin/web-db-v4?out_format=CATCSV&request=COMPREHENSIVE&searchshape=FE&srn=",srn,"&start_year=",year,"&start_month=01&start_day=01&start_time=00:00:00&end_year=",year+1,"&end_month=01&end_day=01&end_time=00:00:00&min_mag=",min.mag,"&req_mag_agcy=",agcy,"&req_mag_type=Any")

  print(srn.q.string )
  out.file =paste0(out.dir,srn,"/",year,"_mm",min.mag,".csv")
  download.file(srn.q.string, out.file, method="wget")
}

#' Title
#'
#' @param dir
#' @param srn
#'
#' @return
#' @export
#'
#' @examples
get_isc_files <- function(dir="/Volumes/data/data/global/quakes/isc/srn/", srn=srn){

  list.files(paste0(dir, srn), full.names=TRUE, pattern =".csv")
}


#' Title
#'
#' @param isc.file
#' @param csv
#'
#' @return
#' @export
#'
#' @examples
get_this_isc<- function(isc.file = get_isc_files()[1], csv=F ){
  #read_csv(isc.file, skip=26) %>% head(-4)
  #data.table::fread(isc.file, sep=",",skip=26, header=T) %>% head(-4)
  #LaF::laf_open_csv(isc.file, sep=",",skip=26,   ncol=8) %>% head(-4)
  print(isc.file)
 if (!csv)  {
   read.fwf(isc.file,skip=28,widths=96 ) %>% head(-5) -> tmp.df
   names(tmp.df) <- str_to_lower("  EVENTID,AUTHOR   ,DATE      ,TIME       ,LAT     ,LONG      ,DEPTH,DEPFIX,AUTHOR1   ,TYPE  ,MAG ")
   }
  else{
   read_csv(isc.file,skip=29,
            col_types="ccccdddcccd") %>% head(-5) -> tmp.df

  names(tmp.df) <- str_to_lower(   c("EVENTID","AUTHOR","DATE","TIME","LAT","LONG","DEPTH","DEPFIX","AUTHOR1","TYPE","M" ))
 paste(tmp.df$date,tmp.df$time)   %>% lubridate::ymd_hms() ->   tmp.df$date
  tmp.df <- tmp.df[, c(1:3, 5:11)]}
 return( tmp.df )
}




#' Title
#'
#' @param srn
#' @param save
#'
#' @return
#' @export
#'
#' @examples
get_isc <-function (srn=8, save=T){
    #get_isc_files()

  # get_isc(get_isc_files()[47]) -> tmp.df
  #  str(tmp.df)
  map(get_isc_files(srn=srn), get_this_isc) %>% bind_rows()   -> isc.df

  isc.df %>% dplyr::filter(V1 != "  EVENTID,AUTHOR   ,DATE      ,TIME       ,LAT     ,LON      ,DEPTH,DEPFIX,AUTHOR   ,TYPE  ,MAG " |
V1 != "     eventid,author   ,date      ,time       ,lat     ,long      ,depth,depfix,author1   ,type  ,mag "  ) -> isc1.df
  con <- file("/Volumes/data/data/global/quakes/isc/srn/tmp/tmp.csv", "w")
  write_lines(str_to_lower("  EVENTID,AUTHOR   ,DATE      ,TIME       ,LAT     ,LONG      ,DEPTH,DEPFIX,AUTHOR1   ,TYPE  ,M "),con)
  for (i in 1:length(isc1.df$V1)) write_lines(isc1.df$V1[i], con)
  close(con)
  read_csv(file="/Volumes/data/data/global/quakes/isc/srn/tmp/tmp.csv") ->isc.df
#  names(isc.df) <- c("eventid","author","date","time","lat","long","depth","author1","source","type", "m")
  if (save) save(isc.df,file=paste0("/Volumes/data/data/global/quakes/isc/srn/rdata/ISC_", srn, ".rData"))
  return(isc.df)
}

#' Title
#'
#' @param srn
#' @param ppp
#' @param win
#'
#' @return
#' @export
#'
#' @examples
join_isc <- function(srn=8:9, ppp=F, win=owin()){
  isc.df <-bind_rows(get_isc( srn[1], save=F),get_isc( srn[2], save=F))
  isc.df$date <- lubridate::ymd_hms(str_c(isc.df$date, " ",isc.df$time ))
   return(isc.df[, c(6,5,3,1,2,7,9,10,11)])
}


#' Title
#'
#' @param isc.df
#' @param win
#'
#' @return
#' @export
#'
#' @examples
isc_ppp <- function(isc.df, win=NA){
  if (is.na(win)) win <-as.owin(c(min(isc.df$long), max(isc.df$long), min(isc.df$lat), max(isc.df$lat)))
 isc.ppp <-as.ppp(isc.df[,c("long", "lat")], W=win)
  isc.ppp$marks =  isc.df
  return(isc.ppp)

}



#' Title
#'
#' @param rect
#' @param min.mag
#' @param mag.type
#' @param year
#' @param agcy
#' @param out.dir
#'
#' @return
#' @export
#'
#' @examples
get_isc_srn_rect <- function(rect=c(-105, -88,12.5,20 ), min.mag=5,
                             mag.type="MS",
                             year=2014,  agcy="Any", out.dir="/Volumes/data/data/global/quakes/isc/tmp/"){

  if (class(rect)=="RasterStack" | class(rect)=="raster" | class(rect) =="RasterBrick") rect <- extent(rect)[1:4]
  print(rect)
  out.file =paste0(out.dir,year,"_mm",min.mag,"mt", mag.type,"rect", paste(rect, collapse=":"),".csv")
  print(  out.file )

  srn.q.string <- paste0("http://www.isc.ac.uk/cgi-bin/web-db-v4?out_format=CATCSV&request=COMPREHENSIVE&searchshape=RECT&bot_lat=",rect[3],"&top_lat=",rect[4],"&left_lon=",rect[1],"&right_lon=",rect[2],"&start_year=",year,"&start_month=01&start_day=01&start_time=00:00:00&end_year=",lubridate::year(Sys.Date())+1,"&end_month=01&end_day=01&end_time=00:00:00&min_mag=",min.mag,"&max_mag=9.8&req_mag_agcy=",agcy,"&req_mag_type=",mag.type)

  #  srn.q.string <- "http://www.isc.ac.uk/cgi-bin/web-db-v4?request=COMPREHENSIVE&out_format=ISF&searchshape=RECT&bot_lat=12.5&top_lat=20&left_lon=-105&right_lon=-90&ctr_lat=&ctr_lon=&radius=&max_dist_units=deg&srn=5&grn=&start_year=2017&start_month=9&start_day=19&start_time=00%3A00%3A00&end_year=2017&end_month=9&end_day=20&end_time=00%3A00%3A00&min_dep=&max_dep=&null_dep=on&min_mag=&max_mag=&req_mag_type=Any&req_mag_agcy=Any"

  print(srn.q.string )
  download.file(srn.q.string, out.file, method="wget")
  return(out.file)
}

