

#' Title
#'
#' @param my.raster
#' @param size
#' @param asRaster
#' @param useRGDAL
#'
#' @return
#' @export
#'
#' @examples
flow_directions <- function(my.raster, size=24000, asRaster=T, useRGDAL=T){

  flow.dir <- raster::sampleRegular(my.raster, size=size, asRaster=asRaster, useRGDAL=useRGDAL)
  flow.dir.x<-as.matrix(flow.dir ) %>%t() %>% pracma::fliplr()
  flow.dir.x[flow.dir.x<0] <- NA
  flow.dir.u1 <- rbind(NA,- diff(flow.dir.x)  )

  flow.dir.v1 <- cbind(NA,-t(diff(t(flow.dir.x))))  # (volcano[, -1] - volcano[ ,-ncol(volcano)] )/dy

  flow.dir.u <- flow.dir.u1/sqrt(flow.dir.u1^2+flow.dir.v1^2)
  flow.dir.v<- flow.dir.v1/sqrt(flow.dir.u1^2+flow.dir.v1^2)
  flow.dir.v[flow.dir.u1==0 & flow.dir.v1==0]  <-NaN
  flow.dir.u[flow.dir.u1==0 & flow.dir.v1==0] <-NaN

  flow.dir.lons <-seq(flow.dir@extent@xmin, flow.dir@extent@xmax, (flow.dir@extent@xmax-flow.dir@extent@xmin)/(flow.dir@ncols-1))
  flow.dir.lats <-seq(flow.dir@extent@ymin, flow.dir@extent@ymax, (flow.dir@extent@ymax-flow.dir@extent@ymin)/(flow.dir@nrows-1))
  flow.dir.lons<- flow.dir.lons-diff(flow.dir.lons[1:2])/2
  flow.dir.lats<- flow.dir.lats-diff(flow.dir.lats[1:2])/2
  return(list(u=flow.dir.u, v=flow.dir.v, lons=flow.dir.lons, lats=flow.dir.lats))

}

#' Title
#'
#' @param cad.sp.1
#' @param res
#' @param init.space
#' @param method
#'
#' @return
#' @export
#'
#' @examples
contours2ras<- function(cad.sp.1, res= c(8, 16,32), init.space=.5, method=""){
  facs <- res/c(1/init.space , diff(res))
  r <- raster(resolution=init.space,  xmn=bbox(cad.sp.1)[1,1], xmx=bbox(cad.sp.1)[1,2],
              ymn=bbox(cad.sp.1)[2,1], ymx=bbox(cad.sp.1)[2,2])
  res.raster<-rasterize(cad.sp.1 , r, cad.sp.1@data$z, fun=mean, background=NA)
  for (i in 1:length(res)) {
    r.d <- raster( resolution=1/res[i],  xmn=bbox(cad.sp.1)[1,1], xmx=bbox(cad.sp.1)[1,2],
                   ymn=bbox(cad.sp.1)[2,1], ymx=bbox(cad.sp.1)[2,2])
    d.raster<-rasterize(cad.sp.1, r.d, cad.sp.1@data$z, fun =mean, background=NA)
    d.m.raster <-disaggregate(res.raster, facs[i], method=method)
    res.raster<-merge(d.raster,d.m.raster)
    print("...")
  }
  names(res.raster)<-"z"
  res.raster
}

#' Title
#'
#' @param cad.sp.1
#' @param res
#' @param init.space
#' @param method
#' @param asis
#'
#' @return
#' @export
#'
#' @examples
pnts2ras<- function(cad.sp.1, res= c(8, 16,32,64), init.space=c(.5, .5), method="",asis=FALSE){
  if  (is.data.frame(cad.sp.1))  cad.sp.1<-
    SpatialPointsDataFrame(SpatialPoints(
      cbind(cad.sp.1[,1], cad.sp.1[,2]),  proj4string = CRS("+init=epsg:4326")),  cad.sp.1 )

  if (length(init.space)==1) init.space<- c(init.space,init.space)
  facs <- res/c(1/init.space[1] , diff(res))

  print(facs)
  r <- raster(resolution=init.space,  xmn=bbox(cad.sp.1)[1,1], xmx=bbox(cad.sp.1)[1,2],
              ymn=bbox(cad.sp.1)[2,1], ymx=bbox(cad.sp.1)[2,2])
  res.raster<-rasterize(cad.sp.1 , r, cad.sp.1@data$z, fun=mean, background=NA)
 if (asis==FALSE) {
   for (i in 1:length(res)) {
    r.d <- raster( resolution=1/res[i],  xmn=bbox(cad.sp.1)[1,1], xmx=bbox(cad.sp.1)[1,2],
                   ymn=bbox(cad.sp.1)[2,1], ymx=bbox(cad.sp.1)[2,2])
    d.raster<-rasterize(cad.sp.1, r.d, cad.sp.1@data$z, fun =mean, background=NA)
    d.m.raster <-disaggregate(res.raster, facs[i], method=method)
    res.raster<-merge(d.raster,d.m.raster)
    print("...")
   }
 }
  names(res.raster)<-"z"
  res.raster
}


#
# r <- raster(resolution=1/2,  xmn=bbox(cad.sp)[1,1], xmx=bbox(cad.sp)[1,2], ymn=bbox(cad.sp)[2,1], ymx=bbox(cad.sp)[2,2])
# m.raster<-rasterize(cad.sp , r, fun =mean)
# r9 <- raster( resolution=1/8,  xmn=bbox(cad.sp)[1,1], xmx=bbox(cad.sp)[1,2], ymn=bbox(cad.sp)[2,1], ymx=bbox(cad.sp)[2,2])
# m9.raster<-rasterize(cad.sp , r9, fun =mean, background=NA)
# d.m.raster <-disaggregate(m.raster,4, method="")
# res.raster<-merge(d.m.raster, m9.raster, fun=last)
#
# r18 <- raster( resolution=1/16,  xmn=bbox(cad.sp)[1,1], xmx=bbox(cad.sp)[1,2], ymn=bbox(cad.sp)[2,1], ymx=bbox(cad.sp)[2,2])
# m18.raster<-rasterize(cad.sp , r18, fun =mean, background=NA)
# d.m.raster <-disaggregate(res.raster,2, method="")
#
# res.raster<-merge(d.m.raster, m18.raster, fun=last)


#' Title
#'
#' @param cadOw
#'
#' @return
#' @export
#'
#' @examples
contour2sp<- function(cadOw){
  cadOw.coords <- coordinates(cadOw)
  #https://stat.ethz.ch/pipermail/r-sig-geo/2010-January/007480.html

  ## z extracts a contour list that maps to coord list.
  z<- rep(cadOw@data$ContValue[1],  length(cadOw.coords[[1]][[1]][,2]))
  for (i in 2:  length(cadOw.coords))  z<- c(z, rep(cadOw@data$ContValue[i], length( (do.call("rbind",cadOw.coords[[i]]))[,1])))
  ##could also do similar with coords...
  # xy<- cadOw.coords[1][[1]] %>% as.data.frame()
  # names(xy) <- c("V1", "V2")
  # for (i in 2:  length(cadOw.coords))   xy<- rbind(xy, as.data.frame(do.call("rbind",cadOw.coords[[i]]))  )
  ## but below is simpler

  cadOw.coords.xy <- coordinates(cadOw) %>% sapply( function(x) do.call("rbind", x)) %>% do.call("rbind",.)
  ##test for lentgth equivalence
  length(cadOw.coords.xy )/2 == length(z)
  cadOw.pts <-  data.frame(x=cadOw.coords.xy[ ,1], y=cadOw.coords.xy[ ,2], z= z)
  cad.sp <- SpatialPointsDataFrame(cadOw.pts[,c('x','y')] , data= data.frame(z=cadOw.pts$z)   )
  return(cad.sp)
}

# head(cadOw.pts)
# head(cadOw.pts[1:length(y),1])
# SpatialPoints(cadOw.pts)
# grd.pts = SpatialPixels(SpatialPoints(cadOw.pts))


#r <- raster(ncols=36, nrows=18)


# https://gis.stackexchange.com/questions/79062/how-to-make-raster-from-irregular-point-data-without-interpolation


#' Title
#'
#' @param pal
#' @param stretch
#' @param n
#' @param alpha
#'
#' @return
#' @export
#'
#' @examples
stretch_pal <- function(pal="terrain.colors", stretch=.5, n=255, alpha=1 ){

  stretch.diff <- c(0, stretch, 1) %>% diff()

  my.cols<-eval(parse(text=paste0(pal, "(10)")))

  inds= seq(10/length(stretch.diff), 10, 10/length(stretch.diff)) %>% round()
  print(stretch.diff)
  print(inds)
  my.pal<- colorRampPalette(my.cols[1:inds[1]], alpha=alpha)(round(n*stretch.diff[1]))
  for (i in 2:length(inds)){
    my.pal<- c(my.pal, colorRampPalette(my.cols[inds[i-1]:inds[i]], alpha=alpha)(round(n*stretch.diff[i])))
    #my.pal<- c(my.pal, colorRampPalette(my.cols[inds[i-1]:inds[i]])(round(n*stretch[i])))
  }
  my.pal
}

#' Title
#'
#' @param pal
#' @param stretch
#' @param n
#'
#' @return
#' @export
#'
#' @examples
stretch_brewer <- function(pal="RdYlGn", stretch=.5, n=255 ){
  stretch.diff <- c(0, stretch, 1) %>% diff()
  my.cols<-RColorBrewer::brewer.pal(10,pal)

  inds= seq(10/length(stretch.diff), 10, 10/length(stretch.diff)) %>% round()
  print(stretch.diff)
  print(inds)
  my.pal<- colorRampPalette(my.cols[1:inds[1]])(round(n*stretch.diff[1]))
  for (i in 2:length(inds)){
    my.pal<- c(my.pal, colorRampPalette(my.cols[inds[i-1]:inds[i]])(round(n*stretch.diff[i])))
    #my.pal<- c(my.pal, colorRampPalette(my.cols[inds[i-1]:inds[i]])(round(n*stretch[i])))
  }
  my.pal
}


#stretch_brewer(stretch = c(.3,.5))
#   for (i in 1:length(stretch.diff){
#     ind =
#   }
#   pal<-c( colorRampPalette(my.cols[inds[i-1:5])(n*stretch),
#           colorRampPalette(my.cols[5:10])(n*(1-stretch)))
#   pal
# }



#' Title
#'
#' @param ras
#' @param lims
#'
#' @return
#' @export
#'
#' @examples
stretch_ras<- function(ras, lims=c(.05,.95)){
  diff.ras= max(ras[], na.rm =T) -min(ras[], na.rm =T)
  ras[ras< min(ras[], na.rm =T)+diff.ras*lims[1]] <- min(ras[], na.rm =T)+diff.ras*lims[1]
  ras[ras> max(ras[], na.rm =T)-diff.ras*(1-lims[2])] <- max(ras[], na.rm =T)-diff.ras*(1-lims[2])
  ras
}



#http://www.fabioveronesi.net/DEMderiv_blog.r
#http://r-video-tutorial.blogspot.com.au/2012/10/terrain-attributes-with-raster-package.html

#' Title
#'
#' @param data
#' @param attr
#' @param method
#'
#' @return
#' @export
#'
#' @examples
DEMderiv<-function(data,attr="plan.curvature",method="evans"){
  #Title: Function for computing terrain attributes from a raster
  #Author: Fabio Veronesi
  #License: Creative Commons - Attribution-NonCommercial (CC BY-NC) - http://creativecommons.org/



  cellValue=raster::res(data)[1]

  evans<-function(neighb){  #method proposed by Evans (1980) as it is described in Florisky (1998)
    #Node numbering for the 3x3 window
    #Z1    Z2    Z3
    #Z4    Z5    Z6
    #Z7    Z8    Z9
    z1<-neighb[1]
    z2<-neighb[2]
    z3<-neighb[3]
    z4<-neighb[4]
    z6<-neighb[6]
    z7<-neighb[7]
    z8<-neighb[8]
    z9<-neighb[9]
    z5<-neighb[5]
    r=(z1+z3+z4+z6+z7+z9-(2*(z2+z5+z8)))/(3*(cellValue^2))
    t=(z1+z2+z3+z7+z8+z9-(2*(z4+z5+z6)))/(3*(cellValue^2))
    s=(z3+z7-z1-z9)/(4*(cellValue^2))
    p=(z3+z6+z9-z1-z4-z7)/(6*cellValue)
    q=(z1+z2+z3-z7-z8-z9)/(6*cellValue)
    if(paste(attr)=="s.slope"){result= q }
    else{
      if(paste(attr)=="e.slope"){result= p }
      else{if(paste(attr)=="slope"){result= atan(sqrt(p^2+q^2)) }
    else{
      if(paste(attr)=="aspect"){result= 180-atan2(q,p)+90*(p/abs(p)) }
      else{
        if(paste(attr)=="plan.curvature"){result= -(q^2*r-2*p*q*s+p^2*t)/((p^2+q^2)*sqrt(1+p^2+q^2))}
        else{
          if(paste(attr)=="prof.curvature"){result= -(p^2*r+2*p*q*s+q^2*t)/((p^2+q^2)*sqrt(1+p^2+q^2)^3)}
        }}}}}
    c(result)
  }

#' Title
#'
#' @param neighb
#'
#' @return
#' @export
#'
#' @examples
  shary<-function(neighb){  #method proposed by Shary (1995) as it is described in Florisky (1998)
    #Node numbering for the 3x3 window
    #Z1    Z2    Z3
    #Z4    Z5    Z6
    #Z7    Z8    Z9
    z1<-neighb[1]
    z2<-neighb[2]
    z3<-neighb[3]
    z4<-neighb[4]
    z6<-neighb[6]
    z7<-neighb[7]
    z8<-neighb[8]
    z9<-neighb[9]
    z5<-neighb[5]
    r=(z1+z3+z7+z9+3*(z4+z6)-2*(z2+3*z5+z8))/(5*(cellValue^2))
    t=(z1+z3+z7+z9+3*(z2+z8)-2*(z4+3*z5+z6))/(5*(cellValue^2))
    s=(z3+z7-z1-z9)/(4*(cellValue^2))
    p=(z3+z6+z9-z1-z4-z7)/(6*cellValue)
    q=(z1+z2+z3-z7-z8-z9)/(6*cellValue)
    if(paste(attr)=="s.slope"){result= -q }
    else{
      if(paste(attr)=="e.slope"){result= p }
      else{if(paste(attr)=="slope"){result= atan(sqrt(p^2+q^2)) }
    else{
      if(paste(attr)=="aspect"){result= 180-atan2(q,p)+90*(p/abs(p)) }
      else{
        if(paste(attr)=="plan.curvature"){result= -(q^2*r-2*p*q*s+p^2*t)/((p^2+q^2)*sqrt(1+p^2+q^2)) }
        else{
          if(paste(attr)=="prof.curvature"){result= -(p^2*r+2*p*q*s+q^2*t)/((p^2+q^2)*sqrt(1+p^2+q^2)^3) }
        }}}}}
    c(result)
  }


#' Title
#'
#' @param neighb
#'
#' @return
#' @export
#'
#' @examples
  zev.tho<-function(neighb){   #method proposed by Zevenbergen and Thorne (1987) as it is described in Florisky (1998)
    #Node numbering for the 3x3 window
    #Z1    Z2    Z3
    #Z4    Z5    Z6
    #Z7    Z8    Z9
    z1<-neighb[1]
    z2<-neighb[2]
    z3<-neighb[3]
    z4<-neighb[4]
    z6<-neighb[6]
    z7<-neighb[7]
    z8<-neighb[8]
    z9<-neighb[9]
    z5<-neighb[5]
    p=(z6-z4)/(2*cellValue)
    q=(z2-z8)/(2*cellValue)
    r=(z4+z6-2*z5)/(2*(cellValue^2))
    s=(z3+z7-z1-z9)/(4*(cellValue^2))
    t=(z2+z8-2*z5)/(2*(cellValue^2))
    if(paste(attr)=="s.slope"){result= -q }
    else{
      if(paste(attr)=="e.slope"){result= p }
      else{ if(paste(attr)=="slope"){result= atan(sqrt(p^2+q^2)) }
    else{
      if(paste(attr)=="aspect"){result= 180-atan2(q,p)+90*(p/abs(p)) }
      else{
        if(paste(attr)=="plan.curvature"){result= -(q^2*r-2*p*q*s+p^2*t)/((p^2+q^2)*sqrt(1+p^2+q^2))  }
        else{
          if(paste(attr)=="prof.curvature"){result= -(p^2*r+2*p*q*s+q^2*t)/((p^2+q^2)*sqrt(1+p^2+q^2)^3) }
        }}}}}
    c(result)
  }



#' Title
#'
#' @param neighb
#'
#' @return
#' @export
#'
#' @examples
  moore<-function(neighb){   #method proposed by Moore et al. (1993) as it is described in Florisky (1998)
    #Node numbering for the 3x3 window
    #Z1    Z2    Z3
    #Z4    Z5    Z6
    #Z7    Z8    Z9
    z1<-neighb[1]
    z2<-neighb[2]
    z3<-neighb[3]
    z4<-neighb[4]
    z6<-neighb[6]
    z7<-neighb[7]
    z8<-neighb[8]
    z9<-neighb[9]
    z5<-neighb[5]
    p=(z6-z4)/(2*cellValue)
    q=(z2-z8)/(2*cellValue)
    r=(z4+z6-2*z5)/(cellValue^2)
    s=(z3+z7-z1-z9)/(4*(cellValue^2))
    t=(z2+z8-2*z5)/(cellValue^2)
    if(paste(attr)=="s.slope"){result=-q }
    else{
      if(paste(attr)=="e.slope"){result= p }
    else{ if(paste(attr)=="slope"){result= atan(sqrt(p^2+q^2)) }
    else{
      if(paste(attr)=="aspect"){result= 180-atan2(q,p)+90*(p/abs(p)) }
      else{
        if(paste(attr)=="plan.curvature"){result= -(q^2*r-2*p*q*s+p^2*t)/((p^2+q^2)*sqrt(1+p^2+q^2)) }
        else{
          if(paste(attr)=="prof.curvature"){result= -(p^2*r+2*p*q*s+q^2*t)/((p^2+q^2)*sqrt(1+p^2+q^2)^3) }
        }}}}}
    c(result)
  }

  w=as.matrix(cbind(c(1,1,1),c(1,1,1),c(1,1,1)))  #sandiford add

  if(paste(method)=="evans"){crop(focal(data,w=w,fun=evans,pad=T,padValue=0),y=c(data@extent@xmin+cellValue,data@extent@xmax-cellValue,data@extent@ymin+cellValue,data@extent@ymax-cellValue))}
  else{
    if(paste(method)=="zev.tho"){crop(focal(data,w=w,fun=zev.tho,pad=T,padValue=0),y=c(data@extent@xmin+cellValue,data@extent@xmax-cellValue,data@extent@ymin+cellValue,data@extent@ymax-cellValue))}
    else{
      if(paste(method)=="shary"){crop(focal(data,w=w,fun=shary,pad=T,padValue=0),y=c(data@extent@xmin+cellValue,data@extent@xmax-cellValue,data@extent@ymin+cellValue,data@extent@ymax-cellValue))}
      else{
        if(paste(method)=="moore"){crop(focal(data,w=w,fun=moore,pad=T,padValue=0),y=c(data@extent@xmin+cellValue,data@extent@xmax-cellValue,data@extent@ymin+cellValue,data@extent@ymax-cellValue))}
      }}}


  #REFERENCES
  #Evans, I. S. (1980). An Integrated System of Terrain Analysis and Slope Mapping. Zeitschrift für Geomorphologie, Suppl. Bd. 36, 274-295.
  #Florinsky, I. V. (1998). Accuracy of Local Topographic Variables Derived from Digital Elevation Models. International Journal of Geographical Informaation Science, 12:1, 47-62.
  #Shary, P. A. (1991). The Second Derivative Topographic Method. In: The Geometry of the Earth Surface Structures, edited by Stepanov, I. N., 15-29 (in Russian).
  #Wilson, J. P. & Gallant, J. C. (2000). Terrain Analysis - Principles and Applications. Wiley.
  #Zevenbergen, L. W. & Thorne, C. R. (1987). Quantitative Analysis of Land Surface Topography. Earth Surface Processes and Landforms, vol. 12, 47-56.
  #Moore, I. D.; Gessler, P. E.; Nielsen, G. A. & Paterson, G. A. (1993). Soil Attribute Prediction Using Terrain Analysis. Soil Science Society of America Journal, vol. 57, 443-452.
}




#' Title
#'
#' @param dds
#' @param geo
#' @param minute
#' @param sec
#'
#' @return
#' @export
#'
#' @examples
dd2deg <-function(dds, geo="S", minute=TRUE, sec=FALSE ){

  # returns an expression of latlons in degree minutes,
  # sec not implemented yet
  test10 <- function (dds) {
    minum <- (dds%%1)*.6*100
    if(minum==0 ) lab<- "plain(00)" else
      if(minum<10 ) lab<-paste("plain(0",round(minum,1), ")", sep="") else
        lab<-paste(round(minum))
      return(lab)
  }

  signDeg <- function(dds) {
    if (dds < 0) sign<- "-" else sign <- ""
    return(sign)
  }
  signs<-purrr::map(dds, signDeg)

  dds= abs(dds)
  if (minute==TRUE) parse(text = paste(signs, floor(dds), "*degree * ~ ",   purrr::map(dds, test10) , "*minute  ",   sep = " ")) else
    parse(text = paste(signs, floor(dds), "*degree ",    sep = " "))
  # text.str <- paste(signs, floor(dds), "*degree * ~ ",   purrr::map(dds, test10) , "*minute  ",   sep = "")
  #  text.str

}

# dd2deg(c(41,141.1, 142.5, 14.9))
#' Title
#'
#' @param ras
#'
#' @return
#' @export
#'
#' @examples
setClip <-function(ras){
  clip(extent(ras)[1], extent(ras)[2], extent(ras)[3], extent(ras)[4])
}



#' Title
#'
#' @param e.ras
#' @param axis.width
#' @param double
#' @param ndiscr
#' @param crs
#'
#' @return
#' @export
#'
#' @examples
set_axes_trad<-function( e.ras,
                    axis.width=0.007,
                    double=FALSE,
                    ndiscr = 100,
                    crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"){
  # e.ras <- raster::extent(ras)
  print(axis.width)
  if (str_detect(class(e.ras),"Raster")) e.ras <- raster::extent(e.ras)

  e.sp <-as(e.ras, 'SpatialPolygons')
  proj4string(  e.sp )<-crs
  # e.ras.bbox <- bbox(e.ras)
  if (double){
    gl.mar = sp::gridlines(e.sp,
                           easts = c(e.ras[1:2],  e.ras[1:2] + diff(e.ras[3:4])*c(axis.width,-axis.width)),
                           norths =c(e.ras[3:4],  e.ras[3:4] + diff(e.ras[3:4])*c(axis.width,-axis.width) ),
                           ndiscr = ndiscr)
  } else{

    gl.mar = sp::gridlines(e.sp,
                           easts = c( e.ras[1:2] + diff(e.ras[3:4])*c(axis.width,-axis.width)),
                           norths =c(  e.ras[3:4] + diff(e.ras[3:4])*c(axis.width,-axis.width) ),
                           ndiscr = ndiscr)
  }
  return(gl.mar )
}



#' Title
#'
#' @param ras
#' @param ax.ext
#' @param col
#' @param n.minor
#' @param cex
#' @param lwd
#' @param tick.sep
#' @param lwd.ticks
#' @param lwd.ticks.minor
#' @param add
#' @param major
#' @param minor
#' @param asp
#' @param mar
#' @param clip
#' @param minute
#' @param sec
#' @param legend
#' @param trad
#' @param axes
#' @param mgp
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
msPlot<- function(ras,
                  ax.ext=.015,
                  col=stretch_brewer(stretch=c(.6, .85, .95)) %>% rev(), #terrain.colors(255),
                  n.minor=10,
                  cex=2.5,
                  lwd=2,
                  tick.sep=5,
                  lwd.ticks=1.5,
                  lwd.ticks.minor=1.5,
                  add=F,
                  major=.01,
                  minor=.004,
                  asp=1,
                  mar=c(5.1, 4.1, 4.1, 2.1),
                  clip=0,
                  minute=TRUE,
                  sec=FALSE,
                  legend=FALSE,
                  trad=NA,
                  axes=FALSE,
                  mgp=c(3,2,0),
                  rgb=F,
                  ...)
{

 # print(par()$mar)
 #
   if( add==TRUE) {
    par(mar=mar, mgp=mgp,new=TRUE, xpd=NA)
  } else
  {
    par( mar=mar, mgp=mgp, xpd=NA)

  }

#  print(par()$mar)
  #
  # if (clip >0) {
  #   usr <- par("usr")
  #  my.ext<- extent(dt)
  #   diff.x<-diff(my.ext[1:2])*clip
  #   diff.y<-diff(my.ext[3:4])*clip
  #   new.ext<- extent(my.ext[1]+diff.x  , my.ext[2]-diff.x ,my.ext[3]+diff.y , my.ext[4]-diff.y )
  #   print( ras)
  #
  # ras1<- raster::crop(ras,  new.ext )
  # print(ras1)
  #
  # } else ras1<- ras
  if (str_detect(class(ras),"Raster")) {
     raster::plot(ras [[1]]  ,
         col=col,
         # col=grey.colors(255, alpha = 1),
         axes=FALSE, legend=legend, box=FALSE, asp=1,
         cex.axis=2 , cex.lab=2, interpolate=T,  legend.width =2.2,
         legend.mar=4,
         axis.args = list(cex.axis = 2),  horizontal=T ,...)
    if (rgb)
          raster::plotRGB(ras,
                 # col=grey.colors(255, alpha = 1),
              #   axes=FALSE,
                 #legend=legend,
              #   box=FALSE,
                 # asp=1,
                 # cex.axis=2 ,
                 # cex.lab=2,
                 add=T, ...
                 # interpolate=T,
                 # legend.width =2.2,
                 # legend.mar=4,
              #   axis.args = list(cex.axis = 2),
             #    horizontal=T ,...
             )

    }

  if (str_detect(class(ras)[1],"Spatial") ) sp::plot(ras  ,
                  col=col,
                  # col=grey.colors(255, alpha = 1),
                  axes=FALSE,
                  legend=legend, box=FALSE, asp=1,
                  cex.axis=2 , cex.lab=2, interpolate=T,  legend.width =2.2,
                  legend.mar=4,
                  axis.args = list(cex.axis = 2),  horizontal=T,   ...)
 if (str_detect(class(ras)[1],"sf") )  plot(ras  ,
                                                                    col=col,
                                                                    # col=grey.colors(255, alpha = 1),
                                                                    axes=FALSE,
                                                                    legend=legend, box=FALSE, asp=1,
                                                                    cex.axis=2 , cex.lab=2, interpolate=T,  legend.width =2.2,
                                                                    legend.mar=4,
                                                                    axis.args = list(cex.axis = 2),  horizontal=T,   ...)

   if(  axes==TRUE) {
     par(xpd=NA)
   #  clip(x.ax[1]-400, x.ax[2]+400, y.ax[1]-400, y.ax[2]+400)
      plot_axes ( ras, ax.ext=ax.ext, n.minor=n.minor,
                cex=cex,lwd=lwd,tick.sep=tick.sep,lwd.ticks=lwd.ticks,
                lwd.ticks.minor=lwd.ticks, major=major,minor=minor, minute=minute, sec=sec,add=TRUE)
   }

  # clip(x.ras.lims[1] , x.ras.lims[2] , y.ras.lims[1] , y.ras.lims[2] )
  # par(xpd=FALSE)
  #

  #if (clip>0) do.call("clip", as.list(usr))  # reset to plot region setClip(ras)
}


# pArrows <- function(G, lwd=4, length = .4, N=3,  angle =30,  h=2,  ant=T, arrows=c(1,2), kdiv=20, fault=F, fold=T, extent=NA) {
#
#   G$line=getsplineG(G$x, G$y, kdiv=kdiv)
#
#   lines(G$line, lwd=lwd)
#   if ( 1 %in% arrows) arrows(G$line$x[1], G$line$y[1], G$line$x[2], G$line$y[2], length = length, angle = angle, code = 1,   lwd=lwd)
#   la <-  length(G$line$x)
#   if ( 2 %in% arrows) arrows(G$line$x[ la ], G$line$y[ la ], G$line$x[la-1], G$line$y[ la -1], length = length, angle = angle, code = 1,   lwd=lwd)
#
#   #  if (syn==T){}
#   if (  fault==FALSE){
#     GeoMap::SynAnticline(G$line$x , G$line$y, N = N, h1=h, h2=h, r1=5, r2=5 ,   endtol=.3, col = "black", lwd=lwd)
#   }
#
#   if (fault==T)  GeoMap::thrust(G$line$x , G$line$y, N = N, h=h,   REV=TRUE)
# }
pArrows <- function(G, lwd=4, length = .4, N=3,  angle =30,  h=2,
                    ant=T, arrows=c(1,2), kdiv=20, fault=F, fold=T,
                    extent=NA, lty=1, col="black") {
  G = as.list(G)
  G$line=GEOmap::getsplineG(G$x, G$y, kdiv=kdiv)

  #lines(G$line, lwd=lwd)
  if ( 1 %in% arrows) arrows(G$line$x[1], G$line$y[1], G$line$x[2], G$line$y[2], length = length, angle = angle, code = 1,  col=col, lty=lty, lwd=lwd)
  la <-  length(G$line$x)
  if ( 2 %in% arrows) arrows(G$line$x[ la ], G$line$y[ la ], G$line$x[la-1], G$line$y[ la -1], length = length, angle = angle, code = 1,   col=col, lty=lty, lwd=lwd)

  #  if (syn==T){}
  if (  fault==FALSE){
    GEOmap::SynAnticline(G$line$x , G$line$y, N = N, h1=h, h2=h, r1=5, r2=5 ,
                         endtol=.3, col = col,
                         lwd=lwd, lty=lty)
  }

  if (fault==T)  GEOmap::thrust(G$line$x , G$line$y, N = N, h=h,  col = col,
                                lwd=lwd, lty=lty, REV=TRUE)
}


#' Title
#'
#' @param ras
#' @param rmax
#'
#' @return
#' @export
#'
#' @examples
set_which<- function(ras, rmax) {
  ras[ras>rmax]<-rmax
  return(ras)
}
#t.raster <- raster::raster(filename)

#' Title
#'
#' @param ras
#' @param shade
#' @param shade.marine
#' @param thresh
#' @param topo
#'
#' @return
#' @export
#'
#' @examples
build_raster_stack <- function(ras,  #raster obj.
                               shade=c(300,10,320), #vex, sun.el, sun.az.
                               shade.marine=c(10,20,320), #shade setings for marine part
                               thresh=0, #can be a two vec to limit (c(lower,higer))
                               topo=FALSE
){

  #' create a raster stack with elements, topo - raw raster .
  #' topo.thresh - thesholded  raster according to theresh params
  #' shade - hill shaded  raw raster using shade params
  #' shade.marine - hill shaded  raster < thrsh[1] using shade.marine params
  #' shade.theesh - hill shaded  raster > thrsh[1] using shade.marine params
  #' @param ras  raw raster.
  #' @param shade length 3 vector, vertical.exag, sun.elevation, sun.azimumuth.
  #' @param.marine shade length 3 vector, vertical.exag, sun.elevation, sun.azimuth.
  #' @param thresh length 1 c(lower) or two c(lower, upper) vector for threshold settings.
  #' @param topo  only clauclates toposhade
  #' @return raster stack of 4 layers.
  #' @examples
  #' ras<-build_raster_stack(bt.1d.median  %>% set_which(rmax=1500), shade=c(600,5,320))

  if (length(shade)==1)  shae= c(shade, 10,320)
  if (length(shade)==2)  shae= c(shade,  320)

  flowd1 <- raster::terrain(ras, opt='tri')
  slope1 <- raster::terrain(ras*shade[1], opt='slope')
  aspect1 <- raster::terrain(ras*shade[1], opt='aspect')
  hill1 <- raster::hillShade(slope1, aspect1,shade[2], shade[3])

  if(!topo) {mask.lim<-thresh[1]
  m<-ras
  m[m >=mask.lim] <- NA
  mp<-ras
  mp[m <=mask.lim] <- NA
  hill3 <- mask(hill1,mp)

  if (!is.na(shade.marine[1])){
    flowd1 <- raster::terrain(ras, opt='tri')
    slope1 <- raster::terrain(ras*shade.marine[1], opt='slope')
    aspect1 <- raster::terrain(ras*shade.marine[1], opt='aspect')
    hill1 <- raster::hillShade(slope1, aspect1,shade.marine[2], shade.marine[3])
  }
  hill2 <- mask(hill1,m)
  ras.thresh <- ras

  if(!is.na(thresh[1])) {
    ras.thresh[ras<thresh[1]]=thresh[1]
    if  (length(thresh)>1 & thresh[1]<thresh[2]) ras.thresh[ras>thresh[2]]=thresh[2]
  }
  b<-brick(ras, nl=5) %>% addLayer(hill1) %>% addLayer(ras.thresh)  %>% addLayer(hill2) %>% addLayer(hill3)
  names(b) <- c("topo","shade", "topo.thresh",  "shade.marine", "shade.land")}

  else {
    b<-brick(ras, nl=2) %>% addLayer(hill1)
    names(b) <- c("topo","shade" )

  }
  return(b)
}


#' Title
#'
#' @param e.ras
#' @param tick.int
#' @param axis.width
#' @param nseq
#' @param test
#' @param crs
#'
#' @return
#' @export
#'
#' @examples
trad_axes_poly <- function(e.ras, #etxent  or raster class
                      tick.int=c(5,5),  #length of segments
                      axis.width=0.007,
                      nseq=6,
                      test=TRUE,
                      crs= CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")){

  if (class(e.ras) %in% c("Raster", "RasterStack", "RasterLayer")) e.ras <- raster::extent(e.ras)
 #print(e.ras)
   ax.width.plot <- min( diff(e.ras[1:2])*axis.width, diff(e.ras[3:4])*axis.width)

  if (length(tick.int)==1) tick.int=c(tick.int, tick.int)

  xint <- c(seq(ceiling(e.ras[1]/tick.int[1])*tick.int[1],e.ras[2], tick.int[1]), e.ras[2])
  nx<-floor(length(xint)/2  )
  yint <- c(seq(ceiling(e.ras[3]/tick.int[2])*tick.int[2],e.ras[4], tick.int[2]), e.ras[4])
  ny<-floor(length(yint)/2  )


  #other version

  # xint <- seq(ceiling(e.ras[1]/tick.int[1])*tick.int[1],e.ras[2], tick.int[1])
  # nx<-length(xint)/2 %>% floor()
  # yint <- seq(ceiling(e.ras[3]/tick.int[2])*tick.int[2],e.ras[4], tick.int[2])
  # ny<-length(yint)/2 %>% floor()
  #

  xis<-seq(1, length(xint),2)


  if (!test){

  x.poly<- matrix(c(xint[xis],
                    xint[ xis+1],
                    xint[ xis+1],
                    xint[ xis] , xint[xis] ),
                  ncol=5, nrow=length(xis) )  %>%t()

  y.poly.bottom <- matrix( c(e.ras[3] ,
                             e.ras[3] ,
                             e.ras[3] +  ax.width.plot ,
                             e.ras[3] +  ax.width.plot,
                             e.ras[3]  ),
                           ncol=length(xis), nrow=5 )
  y.poly.top<- matrix( c(e.ras[4] ,
                         e.ras[4] ,
                         e.ras[4] -  ax.width.plot ,
                         e.ras[4] -  ax.width.plot,
                         e.ras[4]  ),
                       ncol=length(xis), nrow=5 )

  yis<-seq(1, length(yint),2)
  y.poly<- matrix(c(yint[yis],
                    yint[ yis+1],
                    yint[ yis+1],
                    yint[ yis],
                    yint[yis]  ),
                  ncol=5, nrow=length(yis) ) %>% t()
  x.poly.left<- matrix( c(e.ras[1] ,
                          e.ras[1] ,
                          e.ras[1] +  ax.width.plot,
                          e.ras[1] +  ax.width.plot,
                          e.ras[1] ),
                        ncol=length(yis), nrow=5 )

  x.poly.right<- matrix( c(e.ras[2] ,
                           e.ras[2] ,
                           e.ras[2] -  ax.width.plot,
                           e.ras[2] -  ax.width.plot,
                           e.ras[2] ),
                         ncol=length(yis), nrow=5 )

  } else{

    xis<-seq(1, length(xint),2)
  # x.poly<- rbind(mapply(seq,head(xint[xis], -1), head(xint[xis+1],-1), length.out=nseq ),
  #                mapply(seq, head(xint[xis+1],-1),head(xint[xis], -1), length.out=nseq ))
  xint[xis+1][is.na(xint[xis+1])] <- e.ras[2] #max(xint[xis])
  x.poly<- rbind(mapply(seq, xint[xis] ,  xint[xis+1] , length.out=nseq ),
                 mapply(seq,  xint[xis+1] , xint[xis] , length.out=nseq ))
   x.poly <- rbind(x.poly, x.poly[1,])%>% signif( digits = 5)

  npoly.r<-dim(x.poly)[1]
  npoly.c<-dim(x.poly)[2]

  ysb <-rep(c(rep(e.ras[3],  times=nseq),  rep(e.ras[3]+  ax.width.plot,  times=nseq),e.ras[3]), npoly.c)

  y.poly.bottom <-  matrix(ysb, nrow = npoly.r, ncol = npoly.c)  %>% signif(  digits = 5)

  yst <-rep(c(rep(e.ras[4],  times=nseq),  rep(e.ras[4]-  ax.width.plot,  times=nseq),e.ras[4]), npoly.c)

  y.poly.top <-  matrix(yst, nrow = npoly.r, ncol = npoly.c)  %>% signif(  digits = 5)
  yis<-seq(1, length(yint),2)
  yint[yis+1][is.na(yint[yis+1])] <- e.ras[4] #max(xint[xis])

  y.poly<- rbind(mapply(seq, yint[yis] ,  yint[yis+1] , length.out=nseq ),
                 mapply(seq,  yint[yis+1] , yint[yis], length.out=nseq ))
  y.poly <- rbind(y.poly, y.poly[1,]) %>% signif(  digits = 5)

  npoly.r<-dim(y.poly)[1]
  npoly.c<-dim(y.poly)[2]
  xsl <-rep(c(rep(e.ras[1],  times=nseq),  rep(e.ras[1] +  ax.width.plot,  times=nseq),e.ras[1]), npoly.c)
  x.poly.left <-  matrix(xsl, nrow = npoly.r, ncol = npoly.c)  %>% signif( digits = 5)
  ysr <-rep(c(rep(e.ras[2],  times=nseq),  rep(e.ras[2] - ax.width.plot,  times=nseq),e.ras[2]), npoly.c)
  x.poly.right <-  matrix(ysr, nrow = npoly.r, ncol = npoly.c)  %>% signif(  digits = 5)
  }

 poly.ax.x <- function(i,x.poly, y.poly ) sp::Polygon(matrix(c(x.poly[,i],y.poly[,i] ),
                                                              ncol=2,
                                                              nrow=length(y.poly[,i])))
  poly.ax.y <- function(i,x.poly, y.poly ) sp::Polygon(matrix(c(x.poly[,i],y.poly[,i] ),
                                                              ncol=2,
                                                              nrow=length(x.poly[,i])))

    my1.poly <- lapply(1:nx, poly.ax.x, x.poly, y.poly.bottom ) %>%
    Polygons(ID="ax1")


  #   message(x.poly)
  #
  #   print(x.poly)
  # message(y.poly.bottom)
  # print(y.poly.bottom)
  # message(y.poly.top)
  # print(y.poly.top)
  # message(y.poly)
  # print(y.poly)
  # message(x.poly.left)
  # print(x.poly.left)
  # message(x.poly.right)
  # print(x.poly.right)

  my3.poly <- lapply(1:nx, poly.ax.x, x.poly, y.poly.top ) %>%
    Polygons(ID="ax3")
  my2.poly <- lapply(1:ny, poly.ax.y, x.poly.left, y.poly ) %>%
    Polygons(ID="ax2")
  my4.poly <- lapply(1:ny, poly.ax.y, x.poly.right, y.poly ) %>%
    Polygons(ID="ax4")

   print(list(my1.poly, my2.poly, my3.poly ,my4.poly ) %>%
           as.SpatialPolygons.PolygonsList(proj4string= crs)
         )
  # return(list(my1.poly, my2.poly, my3.poly ,my4.poly )  )

  return(list(my1.poly, my2.poly, my3.poly ,my4.poly ) %>%
           as.SpatialPolygons.PolygonsList(proj4string= crs))
}


#' Title
#'
#' @param e.ras
#' @param axis.width
#' @param width
#' @param crs
#' @param nseq
#' @param extend
#'
#' @return
#' @export
#'
#' @examples
trad_axes_poly_back <- function(e.ras, #etxent  or raster class
                            #length of segments
                           axis.width=0.007,
                           width=2,
                           crs= CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"),
                           nseq=51,
                           extend=0.002){

  if(class(e.ras) != "Extent") e.ras <- raster::extent(e.ras)

  ax.width.plot <- min( diff(e.ras[1:2])*axis.width, diff(e.ras[3:4])*axis.width)*width

  x1<- e.ras[1]
  x2 <- e.ras[2]
  y1<- e.ras[3]
  y2<- e.ras[4]

  #(extend white space by .2% - or extend - outside plots to clip ragged edges on projected rasters)
  x1e <- x1-(extend*diff(e.ras[1:2]))
  x2e <- x2+(extend*diff(e.ras[1:2]))
  y1e <- y1-(extend*diff(e.ras[3:4]))
  y2e <- y2+(extend*diff(e.ras[3:4]))

  x.seq <- c(seq(x1,x2, length.out=nseq),rev(seq(x1,x2, length.out=nseq)), x1)
  y.seq <- c(seq(y1,y2, length.out=nseq), rev(seq(y1,y2, length.out=nseq)), y1)

  x.left<- c(seq(x1e,x1e, length.out=nseq),rev(seq(x1+ax.width.plot,x1+ax.width.plot, length.out=nseq)), x1e)
  x.right<- c(seq(x2e,x2e, length.out=nseq),rev(seq(x2-ax.width.plot,x2-ax.width.plot, length.out=nseq)), x2e)
  y.bottom<- c(seq(y1e,y1e, length.out=nseq),rev(seq(y1+ax.width.plot,y1+ax.width.plot, length.out=nseq)), y1e)
  y.top<- c(seq(y2e,y2e, length.out=nseq),rev(seq(y2-ax.width.plot,y2-ax.width.plot, length.out=nseq)), y2e)

  my1.poly <-sp::Polygon(matrix(c(x.seq, y.bottom ),
                                ncol=2,
                                nrow= 2*nseq+1)) %>%  list()%>%
    Polygons(ID="ax1")

  my3.poly <-sp::Polygon(matrix(c(x.seq, y.top ),
                                ncol=2,
                                nrow=2*nseq+1) )%>%  list()%>%
    Polygons(ID="ax3")

  my2.poly <-sp::Polygon(matrix(c(x.left, y.seq ),
                                ncol=2,
                                nrow=2*nseq+1) )%>%  list()%>%
    Polygons(ID="ax2")
  my4.poly <-sp::Polygon(matrix(c(x.right, y.seq ),
                                ncol=2,
                                nrow=2*nseq+1) )%>%  list()%>%
    Polygons(ID="ax4")

  return( list(my1.poly, my2.poly, my3.poly,my4.poly ) %>%
           as.SpatialPolygons.PolygonsList(proj4string= crs))
}



#' Title
#'
#' @param xlim
#' @param ylim
#' @param l.out
#' @param clip
#'
#' @return
#' @export
#'
#' @examples
maps2sp = function(xlim, ylim, l.out = 100, clip = TRUE) {
  stopifnot(require(maps))
  m = maps::map(xlim = xlim, ylim = ylim, plot = FALSE, fill = TRUE)
  p = rbind(cbind(xlim[1], seq(ylim[1],ylim[2],length.out = l.out)),
            cbind(seq(xlim[1],xlim[2],length.out = l.out),ylim[2]),
            cbind(xlim[2],seq(ylim[2],ylim[1],length.out = l.out)),
            cbind(seq(xlim[2],xlim[1],length.out = l.out),ylim[1]))
  LL = CRS("+init=epsg:4326")
  bb = SpatialPolygons(list(Polygons(list(Polygon(list(p))),"bb")), proj4string = LL)
  IDs <- sapply(strsplit(m$names, ":"), function(x) x[1])
  stopifnot(require(maptools))
  m <- map2SpatialPolygons(m, IDs=IDs, proj4string = LL)
  if (!clip)
    m
  else {
    stopifnot(require(rgeos))
    gIntersection(m, bb) # cut map slice in WGS84
  }
}



#' Title
#'
#' @param ras
#' @param ax.ext
#' @param n.minor
#' @param lwd
#' @param tick.sep
#' @param lwd.ticks
#' @param lwd.ticks.minor
#' @param major
#' @param minor
#' @param clip
#' @param minute
#' @param sec
#' @param trad
#' @param cex
#' @param add
#' @param grid
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
plot_axes <- function (ras,
                       ax.ext=.015,
                       n.minor=10,
                       lwd=2,
                       tick.sep=5,
                       lwd.ticks=1.5,
                       lwd.ticks.minor=1.5,
                       major=.01,
                       minor=.004,
                       clip=T,
                       minute=TRUE,
                       sec=FALSE,
                       trad=NA,
                       cex=2.5,
                       add=F,
                       grid=T,

                       ...){
  par(xpd=NA)
  if (length(ax.ext) < 2) ax.ext<-c(ax.ext,ax.ext)
  if (str_detect(class(ras)[1], "sf"))
  {

    message('class sf')

    if (add==FALSE) plot(sf::st_bbox(ras)[c(1,3,2,4)], axes=F,  lwd=0.0, col="white")#oma=c(2,2,2,2), mar=c(5,4,4,4))

    x.ras.lims <- sf::st_bbox(ras)[c(1,3)]
     y.ras.lims <- sf::st_bbox(ras)[c(2,4)]
     x.ras.cell.size <-  diff(x.ras.lims)/1000
     y.ras.cell.size <- diff(y.ras.lims)/1000

  } else{
    if (add==FALSE) plot(extent(ras), axes=F,   lwd=0.0, col="white")#oma=c(2,2,2,2), mar=c(5,4,4,4))
    x.ras.lims <- extent(ras)[1:2]
    y.ras.lims <- extent(ras)[3:4]
    x.ras.cell.size <-  diff(x.ras.lims)/ras@ncols
    y.ras.cell.size <- diff(y.ras.lims)/ras@nrows

  }
    print(c(x.ras.cell.size,y.ras.cell.size))

  x.ax.ext <- diff(x.ras.lims)*ax.ext[1]
  y.ax.ext <- diff(y.ras.lims)*ax.ext[2]

  x.ax.lims <- x.ras.lims + c(-x.ax.ext,  x.ax.ext)
  x.ax.lims <- x.ras.lims + c(- x.ax.ext,  x.ax.ext)


  y.ax.lims <- y.ras.lims + c(-y.ax.ext, y.ax.ext)
  y.ax.lims <- y.ras.lims + c(-y.ax.ext, y.ax.ext)

  x.ax <- c(seq(x.ax.lims[1], x.ax.lims[2] , x.ras.cell.size),x.ax.lims[2])
  y.ax <- c(seq(y.ax.lims[1], y.ax.lims[2] , y.ras.cell.size),y.ax.lims[2])

  ras.axes <- list( ax1=data.frame(x=x.ax,y=y.ax.lims[1]),
                    ax2=data.frame(y=y.ax, x=x.ax.lims[1] ),
                    ax3=data.frame(x=x.ax,y=y.ax.lims[2]),
                    ax4=data.frame(y=y.ax, x=x.ax.lims[2] )
  )


  # if (!is.na(trad[1])){
  #   x.ax.ext.trad <-diff(x.ras.lims)*trad
  #   y.ax.ext.trad <-diff(y.ras.lims)*trad
  #   # x.ax.trad <-c(x.ax.lims +c(-x.ax.ext.trad,x.ax.ext.trad),
  #   #               rev(x.ax.lims)+c(x.ax.ext.trad,-x.ax.ext.trad), x.ax.lims[1]-x.ax.ext.trad)
  #   # y.ax.trad <- c(y.ax.lims[1]-x.ax.ext.trad,y.ax.lims[1]-x.ax.ext.trad,
  #   #                y.ax.lims[2]+x.ax.ext.trad, y.ax.lims[2]+x.ax.ext.trad, y.ax.lims[1]-x.ax.ext.trad)
  #   #
  #   trad.ras.axes <-  ras.axes
  #
  #   trad.ras.axes$ax1$y =trad.ras.axes$ax1$y+y.ax.ext.trad
  #   trad.ras.axes$ax2$x =trad.ras.axes$ax2$x+x.ax.ext.trad
  #   trad.ras.axes$ax3$y =trad.ras.axes$ax3$y-y.ax.ext.trad
  #   trad.ras.axes$ax4$x =trad.ras.axes$ax4$x-x.ax.ext.trad
  # }
  #

  for (i in 1:length(ras.axes ) ) lines(ras.axes[[i]]$x , ras.axes[[i]]$y,   lwd=lwd, xpd=NA)
  # x.ras.lims<- c(min(ras.axes$ax1$x), max(ras.axes$ax1$x))
  # y.ras.lims<- c(min(ras.axes$ax2$y), max(ras.axes$ax2$y))
  # print(x.ras.lims)# lines(x.ax, y.ax, lwd=lwd)
  # print(y.ras.lims)# lines(x.ax, y.ax, lwd=lwd)
  if (grid==T){

  #  if (!is.na(trad[1]))    for (i in 1:length(trad.ras.axes ) )     lines(trad.ras.axes[[i]]$x , trad.ras.axes[[i]]$y,   lwd=lwd)
  #lines(x.ax.trad, y.ax.trad, lwd=lwd) #draw ticks
  if(is.na(tick.sep)) x.ticks <- axTicks(side=1) else
    x.ticks= seq(ceiling(x.ras.lims[1]/tick.sep)*tick.sep, x.ras.lims[2], tick.sep)

  x.ticks <- x.ticks[x.ticks>x.ax.lims[1] & x.ticks<x.ax.lims[2]]
  axis(side=1,pos=y.ax.lims[1], labels= dd2deg(x.ticks, minute=minute), at=x.ticks,lwd=0, lwd.ticks=lwd.ticks, tck = major, cex.axis=cex, xpd=NA, ...)
  axis(side=3,pos=y.ax.lims[2], labels=FALSE, at=x.ticks,lwd=0, lwd.ticks=lwd.ticks, tck = major, cex.axis=cex, xpd=NA,  line=0)

  x.minor.space <- diff(x.ticks[1:2])/n.minor
  x.minor.ticks <- seq(ceiling(x.ras.lims[1]/x.minor.space)*x.minor.space,
                       floor(x.ras.lims[2]/x.minor.space)*x.minor.space,
                       x.minor.space)
  axis(side=1,pos=y.ax.lims[1], at=x.minor.ticks , labels=FALSE,lwd=0, lwd.ticks=lwd.ticks.minor, tck = minor, cex.axis=cex, xpd=NA)
  axis(side=3,pos=y.ax.lims[2], at=x.minor.ticks , labels=FALSE,lwd=0, lwd.ticks=lwd.ticks.minor, tck = minor, cex.axis=cex, xpd=NA)

  if(is.na(tick.sep))   y.ticks <- axTicks(side=2) else
    y.ticks= seq(ceiling(y.ras.lims[1]/tick.sep)*tick.sep, y.ras.lims[2], tick.sep)

  print(y.ticks)
  y.ticks <- y.ticks[y.ticks>y.ax.lims[1] & y.ticks<y.ax.lims[2]]
  axis(side=2,at=y.ticks, pos=x.ax.lims[1], labels= dd2deg(y.ticks, minute=minute ),lwd=0, lwd.ticks=1, tck =major, cex.axis=cex, xpd=NA, ...)
  axis(side=4,at=y.ticks, pos=x.ax.lims[2],labels=FALSE, lwd=0, lwd.ticks=1, tck =major, cex.axis=cex, xpd=NA)

  y.minor.space <- diff(y.ticks[1:2])/n.minor
  y.minor.ticks <- seq(ceiling(y.ras.lims[1]/y.minor.space)*y.minor.space,
                       floor(y.ras.lims[2]/y.minor.space)*y.minor.space,
                       y.minor.space)
  axis(side=2,pos=x.ax.lims[1], at=y.minor.ticks , labels=FALSE,lwd=0, lwd.ticks=lwd.ticks.minor, tck = minor, cex.axis=cex, xpd=NA)
  axis(side=4,pos=x.ax.lims[2], at=y.minor.ticks , labels=FALSE,lwd=0, lwd.ticks=lwd.ticks.minor, tck = minor, cex.axis=cex, xpd=NA)

  }

}

# sring.c needed for axes labels
# to trim/round  strings when their  corrdinate transform
# ... takes an argument from labels {sp}  returns an updated sp
# ... with  @data$labels updates
# ... used in plot_axes_trad

#' Title
#'
#' @param my.labs
#'
#' @return
#' @export
#'
#' @examples
string.c <-function(my.labs){
  message(" assumes NAs are 0s")

  strings<-my.labs@data$labels

  # print(strings)
  sdf<-str_split(strings,"\\*degree\\*") %>% as.data.frame()
  #sdf

  n<-sdf[1,] %>% as.numeric() %>% round(digits =2)  %>% as.character()

  n[is.na(n)] <-"0"

 # print(str_c(n, sdf[2,], sep="*degree*"))
  my.labs@data$labels[n !="0"  ]<- str_c(n[n !="0" ], sdf[2,n !="0" ], sep="*degree*")
    my.labs@data$labels[n=="0"   ] <- "0*degree" #str_c(n[n!="0" & !is.na(n) ],"*degree", sep="")
  my.labs@data$labels[ is.na(n) ]<-  "0*degree"
 #print(my.labs@data$labels)
 my.labs
}

#' Title
#'
#' @param e.ras
#' @param proj
#' @param ax.int
#' @param add
#' @param lwd
#' @param col
#' @param fg
#' @param cex
#' @param grid
#' @param width
#' @param crs
#' @param legend.crs
#' @param extend
#' @param axis.width
#' @param stringc
#' @param degrees
#' @param double
#' @param poly
#' @param test
#'
#' @return
#' @export
#'
#' @examples
plot_axes_trad <- function(e.ras, proj=NA, ax.int=5,  add = FALSE, lwd=1, col="black", fg="black", cex=3, grid=F,width=2,
                      crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                      legend.crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                      extend=0.02, axis.width=0.007, stringc=TRUE, degrees=TRUE, double=TRUE, poly=TRUE, test=TRUE){

  #width specifiec width of white bcgorund extend ing ibto the plot fiels
 if (length(ax.int)==1) ax.int=c(ax.int, ax.int)
 par(xpd=NA) # plotting clipped to device region

  if(class(e.ras) %in% c("Raster", "RasterStack", "RasterBrick", "RasterLayer")) e.ras <- raster::extent(e.ras)

 if (poly)  ax.poly <- trad_axes_poly(e.ras, tick.int=ax.int, axis.width=axis.width, crs=CRS(crs), test=test)
  ax.poly.back <- trad_axes_poly_back(e.ras, width=width, extend=extend)
  ax.lines <-  set_axes_trad(e.ras, double=double, crs=crs, axis.width=axis.width)

 # print(head(ax.poly))
  #print(ax.lines)

  e.sp <-as(e.ras, 'SpatialPolygons')
  proj4string(e.sp) <- crs
  gl = gridlines(e.sp, easts = seq(ceiling(e.ras[1]/ax.int[1])*ax.int[1],e.ras[2], ax.int[1]) ,
                 norths = seq(ceiling(e.ras[3]/ax.int[2])*ax.int[2],e.ras[4], ax.int[2]))

  if (!is.na(proj)) {
    ax.poly.back<- ax.poly.back %>% spTransform(  proj)
    if (poly)   ax.poly<-ax.poly %>% spTransform(  proj)
    ax.lines<-ax.lines %>% spTransform(  proj)
    e.sp <- e.sp %>% spTransform(  proj)
    gl <- gl %>% spTransform(  proj)
  }

  plot(ax.poly.back, add = add, lwd=0.0001, col="white", fg="white")
   # print(ax.poly.back %>% str())
   if (poly)   plot(ax.poly, add = add, lwd=lwd, col=col, fg=fg)
  plot(ax.lines, add = TRUE, lwd=lwd,  col=col)

  #print(labels(gl, CRS(legend.crs), side = 2:3 ))

  if (degrees==TRUE){
  if (stringc) g.labs <-labels(gl, CRS(legend.crs),side = 2:3) %>% string.c else
      g.labs <-labels(gl, CRS(legend.crs),side = 2:3)} else{
      g.labs <-labels(gl,  CRS( proj@projargs),side = 2:3)

      z.labels.inds  <- which( g.labs@data$pos  ==2)  # 2 equals left side should be corrected to passed var
      x.labels.inds  <- which( g.labs@data$pos  == 3) #as above
      x.left <-   as.numeric(g.labs@data$labels[x.labels.inds[1]])
      print(g.labs@data$labels[z.labels.inds] )
      print(g.labs@data$labels[x.labels.inds] )
      print("---")
      print(x.left)

      z.top <- max( 6378137,max(as.numeric(g.labs@data$labels[z.labels.inds])))
      print(z.top)
      g.labs@data$labels[x.labels.inds] <-  round((as.numeric(g.labs@data$labels[x.labels.inds]) -x.left)/1000)
      g.labs@data$labels[z.labels.inds] <-   round((as.numeric(g.labs@data$labels[z.labels.inds]) -z.top)/1000)

     #    g.labs@data$labels <- floor(as.numeric(g.labs@data$labels)/10000)*10 -6540

    print(g.labs@data$labels[z.labels.inds] )
    print(g.labs@data$labels[x.labels.inds] )
      }
  #CRS( proj)@projargs
  #print(g.labs@data$labels  )
  #graphics::text(labels(gl, CRS(legend.crs), side = 2:3 ), cex=cex )
  graphics::text(g.labs, cex=cex )
  #graphics::text(labels(gl,   side = 2:3 ), cex=cex )
  if(grid==TRUE) sp:: plot(gl, add = TRUE, col="grey55", lty=1, lwd=.5)
  par(xpd=FALSE) # plotting clipped to plot region
}


#' Title
#'
#' @param flows
#' @param from
#' @param to
#' @param add
#' @param by
#' @param arr.max
#' @param arr.min
#' @param scale
#' @param lwd
#' @param type
#' @param NAcol
#' @param col
#' @param alpha
#'
#' @return
#' @export
#'
#' @examples
quiverTransform<- function(flows,
                           from=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"), to=laea,
                           add = T ,
                           by=5, arr.max=.3,arr.min=.3,  scale=.3, lwd=8,
                           type="simple",NAcol=rgb(1,1,1, alpha=0) , col="white", alpha=.7

){
  OceanView::quiver2D(x = flows$lons, y = flows$lats,  u = flows$u, v = flows$v,
                      by=by, arr.max=arr.max,arr.min=arr.min,  scale=scale, lwd=lwd,
                      type=type,NAcol=NAcol , col=col, alpha=alpha, plot=F) ->flows.latlons



  flows.sp.transform0<-SpatialPoints(cbind(flows.latlons$x0%>% as.numeric(), flows.latlons$y0 %>% as.numeric()),  proj4string=from) %>%spTransform( to)
  flows.sp.transform1<-SpatialPoints(cbind(flows.latlons$x1%>% as.numeric(), flows.latlons$y1 %>% as.numeric()),  proj4string=from) %>%spTransform( to)

  flows.transform<-flows.latlons
  flows.latlons$x0 %>% dim() -> dims
  flows.transform$x0 <- flows.sp.transform0@coords[,1]  %>% matrix(ncol=dims[1], nrow=dims[2])
  flows.transform$y0 <- flows.sp.transform0@coords[,2]  %>% matrix(ncol=dims[1], nrow=dims[2])

  flows.transform$x1 <- flows.sp.transform1@coords[,1]  %>% matrix(ncol=dims[1], nrow=dims[2])
  flows.transform$y1 <- flows.sp.transform1@coords[,2]  %>% matrix(ncol=dims[1], nrow=dims[2])

  return(flows.transform)
}


#' Title
#'
#' @param flows
#' @param add
#' @param length
#' @param scale
#' @param lwd
#' @param code
#' @param col
#'
#' @return
#' @export
#'
#' @examples
plotQuiver<- function(flows, add=T,  length=.1,  scale=.45, lwd=3, code=2, col="black"){
  arrows(flows$x0, flows$y0, flows$x1, flows$y1, length=length, code=code, lwd=lwd, col=col)
}



# clips plotting rgeion to ras extent

#' Title
#'
#' @param ras
#'
#' @return
#' @export
#'
#' @examples
set_clip<-function(ras){

    clip.region<-(extent(ras))[1:4]
  clip(clip.region[1],clip.region[2],clip.region[3],clip.region[4])
}

# convert given x and y points in one CRS to anlother CRS. default is to retuen data.frmae with x and y.
# set xy=F to retern spatial point

#' Title
#'
#' @param limsx
#' @param limsy
#' @param from
#' @param to
#'
#' @return
#' @export
#'
#' @examples
convertLims <- function(limsx, limsy, from=CRS("+init=epsg:4326"), to=CRS("+proj=laea +lat_0=-45.7 +lon_0=132.9 +ellps=WGS84")){
  lims.df <- data.frame(x=limsx, y=limsy) %>% convertPts(from=from, to= to)
  print(lims.df)
  return(list(limsx=lims.df$x, limsy=lims.df$y))

}
#' Title
#'
#' @param x
#' @param y
#' @param from
#' @param to
#' @param xy
#' @param single
#'
#' @return
#' @export
#'
#' @examples
convertPts <- function(x,y=y, from=CRS("+init=epsg:4326"), to=CRS("+proj=laea +lat_0=-45.7 +lon_0=132.9 +ellps=WGS84") , xy=T, single=F){
  if( is.list(x) | is.data.frame(x)) {  d= as.data.frame(x)
  names(d)<- c("lon", "lat")}
else   d <- data.frame(lon=x, lat=y)

 d1 <- d
 inds <- which(!is.na(d[,1]))
  na.inds <- which(is.na(d[,1]))

  d <- d[inds,]
  coordinates(d) <- c("lon", "lat")
  proj4string(d) <-from # WGS 84

  # d.con<- d
  # inds <- which(!is.na(d.con[,1]))
 # print(d@coords)
  d.con<- spTransform( d, to)

   if (xy)  {
     # if(length(na.inds)>=1) {
     #
     #  d.con2<-data.frame(lon=x, lat=y)
     #  d.con2[inds,1]<-d.con@coords[,1]
     #  d.con2[inds,2]<-d.con@coords[,2]}
      d.con=data.frame(x=d.con@coords[,1], y=d.con@coords[,2] )
      if(length(na.inds)>=1){

        d1[inds,]<-d.con
        d.con<- d1
      }
     }
    if (single ) d.con=c(d.con[1], d.con[2]) %>% as.numeric()
  return(d.con)
}


#' Title
#'
#' @param ras
#' @param x
#' @param y
#' @param n
#' @param plot
#' @param df
#' @param km
#' @param gc
#'
#' @return
#' @export
#'
#' @examples
extractLines<-function( ras, x=c(150,140), y=c(-11.5,-43.5), n=100, plot=TRUE, df=TRUE , km=TRUE, gc=T){

  # default is to sue great circle form geosphere
 if (!gc)
   line.df <- data.frame(x=seq(x[1],x[2], length.out=n), y=seq(y[1],y[2], length.out=n))
      else {
    line.df <- geosphere::gcIntermediate(c(x[1],y[1]), c(x[2],y[2]),n)

    line.df<- rbind(c(x[1],y[1]), line.df)
    line.df<- rbind(  line.df, c(x[2],y[2]) ) %>% as.data.frame()
     names(line.df) <- c("x", "y") }

 # print(str(line.df))
  my.lines<-extract(ras, line.df, df=TRUE)
#  print(head(my.lines))
  line.df <- bind_cols(line.df, extract(ras, line.df, df=TRUE))

  #                 line.df$z<-  extract(ras, line.df, df=TRUE)

  line.df$distance <- geosphere::distGeo(as.matrix(line.df[,1:2]), as.matrix(line.df[1,1:2]))
  if (km==TRUE) line.df$distance <-line.df$distance/1000
  if (class(ras) %in% c("RasterBrick", "RasterStack"))  line.df<- gather(line.df %>% dplyr::select(-ID),label, z, -x,-y, -distance)

    return(line.df)

}



#' Title
#'
#' @param col
#' @param alpha
#'
#' @return
#' @export
#'
#' @examples
add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2,
        function(x)
          rgb(x[1], x[2], x[3], alpha=alpha))
}

#' Title
#'
#' @param blues
#' @param reds
#' @param reverse
#' @param drop
#'
#' @return
#' @export
#'
#' @examples
BlRe.colours <- function(blues=6, reds=6, reverse=F, drop=NA){
  colfunc <- colorRampPalette(c("blue", "white"))
  colfunc1 <- colorRampPalette(c("white", "red"))

  if (!is.na(drop)) cols <- c(colfunc(blues) %>% head(-drop),colfunc1(reds) %>% tail(-drop))
 else cols =c(colfunc(blues),colfunc1(reds) )


  if( reverse==TRUE) return(cols %>% rev()) else return(cols)

}

# plot_axes <- function(ras, proj=NA, ax.int=5, add = FALSE, lwd=1, col="black", fg="black",
#                       crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
#                       legend.crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"){
#
#   ax.poly <- trad.axes(ras)
#     ax.lines <-  set.axes(ras, double=T, crs=crs)
#     print(ax.lines)
#
#
#    e.sp <-as(e.ras, 'SpatialPolygons')
#    proj4string(  e.sp )<-crs
#
#   if (!is.na(proj)) {
#     ax.poly<-ax.poly%>%spTransform(  proj)
#     ax.lines<-ax.lines%>%spTransform(  proj)
#     e.sp <- e.sp%>%spTransform(  proj)
#   }
#   plot(ax.poly, add = add, lwd=lwd, col=col, fg=fg)
#   plot(ax.lines, add = TRUE, lwd=lwd,  col=col)
#   #print(e.sp)
#
#   gl = gridlines(e.sp, easts = seq(ceiling(e.ras[1]/ax.int)*ax.int,e.ras[2], ax.int) , norths = seq(ceiling(e.ras[3]/ax.int)*ax.int,e.ras[4], ax.int))
#
#
#   graphics::text(labels(gridlines(e.sp), legend.crs, side = 2:3 ), cex=3 )
#
#
# }
