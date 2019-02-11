


# library(tidyverse)
# library(raster)
# library(spatstat)
# library(rgeos)
# library(maptools)
# library(RColorBrewer)
# library(maptools)


#source('~/Dropbox/msandifo/documents/programming/r/2017/water/geofabric/raster_functions.R')
# test=F
# if (test) {
#   slab.name="izu"
#   extend.lims =c(-1,1,-1,1)
#   slab.dir ="/Users/msandifo/Dropbox/msandifo/documents/programming/r/2017/ecuador/data/slab1/allslabs/"
#   bathy.resolution=1
# } else {
# rm(slab.name, limsx, limsy, full.extent)
#   rm(topo)
#   rm(slab)
# extend.lims =NA }
#
#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_proj <- function(...){set_proj(...)}
#' Title
#'
#' @param slab
#' @param proj
#'
#' @return
#' @export
#'
#' @examples
set_proj<- function(slab, proj="laea"){
  # add check for CRS validity ....
  if (proj=="laea"){ mproj <-CRS(paste0("+proj=laea +lat_0=",
                       0, " +lon_0=",
                       round(mean(raster::extent(slab$slab)[1:2]), 1)))}
  else mproj=proj
  return(mproj)
}

#' Title
#'
#' @param slab
#' @param proj
#' @param ppp
#'
#' @return
#' @export
#'
#' @examples
proj_slab <- function(slab, proj="laea", ppp=T){

  p = get_proj(slab, proj=proj)
  slab$slab %>% raster::projectRaster( crs= p)  ->  slab$slab
  # slab$cmt@coords <-  slab$cmt@coords %>% as.data.frame() %>% convertPts(p) %>% as.matrix()
  # slab$ehb@coords <-  slab$ehb@coords %>% as.data.frame() %>% convertPts(p) %>% as.matrix()
  # slab$hf@coords <-  slab$hf@coords %>% as.data.frame() %>% convertPts(p) %>% as.matrix()
  # slab$vo@coords <-  slab$vo@coords %>% as.data.frame() %>% convertPts(p) %>% as.matrix()
  # slab$wsm@coords <-  slab$wsm@coords %>% as.data.frame() %>% convertPts(p) %>% as.matrix()

  slab$cmt   <-  slab$cmt  %>% spTransform( p)
  slab$ehb  <-  slab$ehb %>% spTransform( p)
  slab$hf  <-  slab$hf %>% spTransform( p)
  slab$vo  <-  slab$vo %>% spTransform( p)
  slab$wsm  <-  slab$wsm %>% spTransform( p)
  slab$coastlines<- slab$coastlines %>% st_as_sf() %>% st_transform(p@projargs) %>% as("Spatial")
    slab$coasts <-slab$coasts %>% spTransform( p)
  slab$rivers <-slab$rivers %>% spTransform( p)
  slab$plates <-slab$plates %>% spTransform(p)
  slab$borders <-slab$borders %>% spTransform( p)
   if(ppp) slab2pp(slab) -> slab
  slab

}


# p<-get_proj(sam, proj="laea")
# sam$cmt@coords <-sam$cmt@coords %>% as.data.frame() %>% convertPts(p) %>% as.matrix()
# sam$cmt@coords@data
# library(sf)


#' Title
#'
#' @param slab
#' @param slab.name
#' @param extend.lims
#' @param slab.dir
#' @param etopo
#' @param fact
#' @param detail
#' @param ppp
#' @param simplify
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
assemble_slab <- function(slab = NA,
                          slab.name = "izu",

                          extend.lims = c(-1, 1, -1, 1),
                          slab.dir = "/Volumes/data/data/global/slabs/slab1/allslabs/",
                          etopo = "~/Dropbox/data/global/topography/etopo/ETOPO1_Bed_g_geotiff.tif",
                          slab2 =F,
                          fact = 1,
                          detail="h",
                          borders=T,
                          plates=T,
                          rivers=T,
                          coasts=T,
                          ppp=T, # for extarcting coasts, rivers etc.
                          simplify=F,  #limits regiems in cmt to NF, TF , SS- still need to modify ..... wsm
                          ...) {
  #assemble slab 1.0 rasters
if(slab2==T)                  slab.dir   <-str_replace(   slab.dir ,"slab1", "slab2")
print(slab.dir)
print(slab.name)
  if (missing(slab.name)) slab.name=slab.name
  message("still need to systematise all the spatial outputs as similar classes")
  if (!is.raster(slab) )  {
    if (!slab2){
    slab.depth <-
      raster(paste0(slab.dir, slab.name, "_slab1.0_clip.grd"))
    slab.dip <-
      raster(paste0(slab.dir, slab.name, "_slab1.0_dipclip.grd"))
    slab.strike <-
      raster(paste0(slab.dir, slab.name, "_slab1.0_strclip.grd"))
} else {
  files <- list.files(slab.dir)

  slab.depth <-
    raster(paste0(slab.dir, files[files %>% str_detect(paste0(slab.name,"_slab2_dep"))]),ncdf=TRUE)
  slab.dip <-
    raster(paste0(slab.dir, files[files %>% str_detect(paste0(slab.name,"_slab2_dip"))]),ncdf=TRUE)
  slab.strike <-
    raster(paste0(slab.dir, files[files %>% str_detect(paste0(slab.name,"_slab2_str"))]),ncdf=TRUE)
}


    # print(extent(slab.depth)); print(extent(slab.dip));print(extent(slab.strike))

    slab <- stack(slab.depth, nl = 9)   %>%
      addLayer(slab.dip) %>%
      addLayer(slab.strike)
  }
  names(slab) <- c("depth", "dip", "strike")
  crs(slab) <- "+init=epsg:4326"

  if ((extent(slab)[2]) > 180)  {
    slab <- setExtent(slab, c(extent(slab)[1:2] - 360, extent(slab)[3:4]))
  }

  full.extent <- extent(slab) + extend.lims

  limsx <- full.extent[1:2];  limsy <- full.extent[3:4]

  message("extent is ", full.extent[1:4] %>% str_c(", "))
  # topo <- getNOAA.bathy(lon1=limsx[1],lon2=limsx[2],lat1=limsy[2],lat2=limsy[1],  resolution= fact) %>%
  #   marmap::as.raster() %>%

  topo <- raster(etopo) %>%
    crop(full.extent)

  crs(topo) <- "+init=epsg:4326"
  if (fact > 1)   topo <- aggregate(topo, fact = fact)

  slab <- slab %>% resample(topo) #%>% addLayer(topo)
  topo <- topo %>% build_raster_stack(thresh = c(0, 6000))
  slab<- addLayer(slab, topo)

  message("... loading cmt")
  read_cmt(limsx = limsx, limsy = limsy, simplify=simplify) %>%
    ppp2spdf( full.extent) %>%
    add_slab_depth(  slab, anom=T) ->
    cmt

  message("... loading wsm")
  wsm_ppp(limsx = limsx, limsy = limsy) %>%
    ppp2spdf( full.extent) %>%
    add_slab_depth(  slab, anom=T)->
    wsm

  message("... heat flow")
  read_hf(limsx = limsx, limsy = limsy) %>%
   ppp2spdf (full.extent) %>%
   add_slab_depth(  slab )->
    hf

  message("... volcanoes")
  read_vo(limsx = limsx, limsy = limsy )%>%
    ppp2spdf (full.extent) %>%
    add_slab_depth(  slab )->
    vo


  message("... loading ehb")
  read_ehb(limsx = limsx, limsy = limsy) %>%
    ppp2spdf (full.extent) %>%
    add_slab_depth(  slab, anom=T) ->
  ehb


  #--geography
case=1

  message("... loading coasts")
  library(maptools)
  #coasts <- get_coasts( full.extent, sp=T, detail="h")
if(coasts) {
if (case==1)  {coastlines<-   rnaturalearthhires::coastline10 %>%
  st_as_sfc() %>%
  st_crop( extent(slab))  %>%
  as( 'Spatial')

  coasts <- get_sf(get_gshhg_file(), slab, sp=T) %>%  as( 'SpatialLinesDataFrame')
  }
  else
  if (case==2)
 coasts <- rgdal::readOGR(get_gshhg_file()) %>%
   fortify() %>%subset(long >= limsx[1] & long <= limsx[2] & lat >= limsy[1] & lat<= limsy[2]) %>% #crop(borders,full.extent)
   df2spline()
} else {coasts=NA; coastlines=NA}
  message("loading rivers")
  #rivers <- get_rivers( full.extent, sp=T, detail="h")
 if(rivers) {
   if (case==1)
    rivers <- rbind(get_sf(get_wdbii_file(river=T, level=1, res="i"), slab, sp=F),
                     get_sf(get_wdbii_file(river=T, level=2, res="i"), slab, sp=F),
                     get_sf(get_wdbii_file(river=T, level=3, res="i"), slab, sp=F),
                     get_sf(get_wdbii_file(river=T, level=4, res="i"), slab, sp=F)) %>%
    as("Spatial")

  else   rivers<-rgdal::readOGR(get_wdbii_file(river = T,level=2)) %>% fortify() %>%subset(long >= limsx[1] & long <= limsx[2] & lat >= limsy[1] & lat<= limsy[2]) %>% #crop(borders,full.extent)
    df2spline()
} else rivers=NA

if (borders){
  message("... loading borders")
  if (case==1){
    borders = rbind(get_sf(get_wdbii_file(river=F, level=3, res="i"), slab, sp=F),
                   get_sf(get_wdbii_file(river=F, level=2, res="i"), slab, sp=F),
                    get_sf(get_wdbii_file(river=F, level=1, res="i"), slab, sp=F)) %>%
    as("Spatial")
    bordersi =  get_sf(get_wdbii_file(river=F, level=2, res="i"), slab, sp=F)  %>%
      as("Spatial")
  }
  else {
      borders<-get_borders_TM() %>%
    subset(LON >= limsx[1] & LON <= limsx[2] & LAT >= limsy[1] & LAT<= limsy[2])
  }} else{
    borders=NA
  bordersi=NA}
  #crop(borders,full.extent)

  message("... loading plates")

  #plates= get_plates()#crop( full.extent)

  if(plates){
  if (case==1){
   plates= get_sf("~/Dropbox/data/global/polygons/tectonicplates-master/PB2002_boundaries.shp" , slab, sp=F) %>%
     add_column(., ID=as.numeric(row.names(.)), .before = "LAYER") %>%
   as("Spatial")

  } else plates= get_plates() %>%
    fortify() %>%
    subset(long >= limsx[1] & long <= limsx[2] & lat >= limsy[1] & lat<= limsy[2]) %>%
    #crop(borders,full.extent)
    df2spline()
  message("plate boudary ids : ", get_plate_boundary_ids(plates) %>% str_c( collapse = ", "))
} else plates=NA




  my.slab <-list(
    slab = slab,
     hf = hf,
    vo = vo,
    ehb = ehb,
     wsm=wsm,
    cmt=cmt,
     coasts=coasts,
    coastlines=coastlines,
     rivers=rivers,
     plates=plates,
     borders=borders,
    bordersi=bordersi,
     name = slab.name
  )

  if(ppp) slab2pp(my.slab, case=case) -> my.slab
#  message (my.slab %>% str(max.level=1) )

   return(  my.slab )

}

is.ras <- function(ras) {
  class(ras)[1] %in% c("Raster", "RasterLayer", "RasterBrick", "RasterStack")

}


#' Title
#'
#' @param slab
#' @param fact
#' @param extend.lims
#' @param etopo
#'
#' @return
#' @export
#'
#' @examples
add_topo_stack <-
  function(slab,
           fact = 1,
           extend.lims = c(-1, 1, -1, 1),
           etopo = "/Volumes/data/data/global/topography/etopo/ETOPO1_Bed_g_geotiff.tif") {
    full.extent <- extent(slab) + extend.lims

 #   print(full.extent)
    # limsx <- full.extent[1:2]; limsy<- full.extent[3:4]
    # print(limsx) ; print(limsy)
    # topo <- getNOAA.bathy(lon1=limsx[1],lon2=limsx[2],lat1=limsy[2],lat2=limsy[1],  resolution= fact) %>%
    #   marmap::as.raster() %>%

    topo <- raster(etopo) %>%
      crop(full.extent)

    crs(topo) <- "+init=epsg:4326"
    if (fact > 1)
      topo <- aggregate(topo, fact = fact)
    topo <- topo %>% build_raster_stack(thresh = c(0, 6000))

    slab %>% resample(topo) %>% addLayer(topo)


  }

#- --
#' Title
#'
#' @param slabs
#' @param extend.lims
#' @param slab.name
#'
#' @return
#' @export
#'
#' @examples
merge_slabs <-
  function(slabs = list(mex, sam),
           extend.lims = c(0, 0, 0, 0),
           slab.name = "mex.sam") {
    s1 <- slabs[[1]]$slab %>% dropLayer(4:8)
    s2 <- slabs[[2]]$slab %>% dropLayer(4:8)
    res.ratio <- res(s2) / res(s1)
    print(res.ratio)
    if (res.ratio[1] > 1.1)
      s1 <- aggregate(s1, res.ratio[1])
    if (res.ratio[1] < .9)
      s2 <- aggregate(s2, 1 / res.ratio[1])
    e1 <-    extent(s1)
    e2 <-    extent(s2)
    e <- extent(min(e1[1], e2[1]),
                max(e1[2], e2[2]),
                min(e1[3], e2[3]),
                max(e1[4], e2[4]))

    s1 <- extend(s1, e)
    s2 <- extend(s2, e)

    s <- raster::merge(s1, s2, tolerance = 1)
    names(s) <- c("depth", "dip", "strike")
    if (res.ratio > 1)
      fact = res.ratio[1]
    else
      fact = 1
    slab <- add_topo_stack(slab = s,
                           extend.lims = extend.lims,
                           fact = fact)

    full.extent <- extent(slab)

    limsx <- full.extent[1:2]
    limsy <- full.extent[3:4]
    wsm <- wsm_ppp(limsx = limsx, limsy = limsy)
    hf <- read_hf(limsx = limsx, limsy = limsy)
    hf <-
      crop(hf %>% maptools::as.SpatialPointsDataFrame.ppp(),
           full.extent)
    crs(hf) <- "+init=epsg:4326"
    hf$dip <- hf %>% raster::extract(slab$dip, .)
    hf$depth <- hf %>% raster::extract(slab$depth, .)
    hf$strike <- hf %>% raster::extract(slab$strike, .)

    vo <- read_vo(limsx = limsx, limsy = limsy)
    ehb <-  read_ehb(limsx = limsx, limsy = limsy)
    ehb <-
      crop(ehb %>% maptools::as.SpatialPointsDataFrame.ppp(),
           full.extent)
    crs(hf) <- "+init=epsg:4326"
    ehb$dip <- ehb %>% raster::extract(slab$dip, .)
    ehb$depth2slab <- ehb %>% raster::extract(slab$depth, .)
    ehb$strike <- ehb %>% raster::extract(slab$strike, .)
    ehb$depth.anom <-
      ehb$marks.depth + ehb$depth2slab  # posiitive is below slab posiiton

    cmt<-read_cmt(limsx = limsx, limsy = limsy)

    wsm <- wsm_ppp(  limsx = limsx, limsy = limsy)

    # vo= rbind(slabs[[1]]$vo, slabs[[2]]$vo)
    # # izu<- list(  slab=slab1, hf=hf, vo=vo, name=slab.name)
    # return(list(  slab=slab,
    #               hf=rbind(slabs[[1]]$hf, slabs[[2]]$hf),
    #               vo=rbind(slabs[[1]]$vo, slabs[[2]]$vo),
    #               name=slab.name))
    return(list(
      slab = slab,
      hf = hf,
      vo = vo,
      ehb = ehb,
      wsm=wsm,
      cmt=cmt,
      name = slab.name
    ))

  }

#ms<- merge_slabs()
#rbind(mex$hf, sam$hf)


#- ----- -

#' Title
#'
#' @param slab.name
#' @param slab.dir
#'
#' @return
#' @export
#'
#' @examples
load_slab <- function(slab.name = "izu",
                      slab.dir = "/Users/msandifo/Dropbox/msandifo/documents/programming/r/2017/ecuador/data/slab1/allslabs/") {
  load(paste0(slab.dir, slab.dir, ".RData"))
}

#-

#' Title
#'
#' @param ras
#' @param method
#' @param fact
#' @param foc
#' @param stretch.plan
#'
#' @return
#' @export
#'
#' @examples
ras_plancurv <- function(ras,  method="evans", fact=1, foc=25, stretch.plan=c(.25,.75)){
  if (fact>1) ras1<- ras %>% raster::aggregate( fact=fact) %>% raster::focal(w=matrix(1, foc,foc), mean) else
    ras1<- ras  %>% raster::focal(w=matrix(1, foc,foc), mean)
  DEMderiv(ras1,"plan.curvature",method) %>% resample(ras) %>% stretch_ras(stretch.plan)
}

#' Title
#'
#' @param vfile
#' @param limsx
#' @param limsy
#' @param ppp
#' @param to
#'
#' @return
#' @export
#'
#' @examples
read_vo <- function(vfile = '~/Dropbox/data/global/volcanoes/volcano1.csv',
                    limsx = c(-180, 180),
                    #can be a raster, extent object or 2-length vector
                    limsy = c(-90, 90),
                    #only used if limsx is a 2-length vec
                    ppp = TRUE,
                    to = NA) {
  if (is.ras(limsx)) {
    limsx <-
       extent(raster)[1:2]
    limsy <-  extent(raster)[3:4]
  }
  if (length(limsx) >= 4) {
    limsy = limsx[3:4]
    limsx = limsx[1:2]
  }
  v <- read.csv(vfile)
  v$LONGITUDE = v$LONGITUDE * v$EW
  v$LATITUDE = v$LATITUDE * v$NS
  v <-
    subset(v,
           LONGITUDE >= limsx[1] &
             LONGITUDE <= limsx[2] &
             LATITUDE >= limsy[1] & LATITUDE <= limsy[2])

  if (!is.na(to)) {
    v1 <-
      data.frame (long = v$LONGITUDE, lat = v$LATITUDE)  %>% convertPts(to = to)
    v$LONGITUDE <- v1[, 1]
    v$LATITUDE <- v1[, 2]
  }
 # print(head(v))
  names(v)[match(c("LONGITUDE", "LATITUDE"), names(v))] <-
    c("long", "lat")
   names(v) <- str_to_lower(names(v)) %>% str_replace_all(" ","")

   names(v)[match(c("name"), names(v))] <-
     c("vo.name")
   message("volcano names : ", str_c(names(v), ", "))
  if (!ppp)
    return(v)
  else
    return(spatstat::ppp(v$long, v$lat, limsx, limsy, marks = v ))#[, c(1, 2, 3, 4, 5, 6, 7, 8, 11,13)]))

}




#' Title
#'
#' @param ghf.file
#' @param limsx
#' @param limsy
#' @param ppp
#'
#' @return
#' @export
#'
#' @examples
read_hf <-
  function(ghf.file = '~/Dropbox/data/global/heatflow/Global2010.csv',
           limsx = c(-360, 360),
           limsy = c(-90, 90),
           ppp  = TRUE) {
    ghf.all <-
      read_csv(
        ghf.file,
        skip = 1,
        na = "",
        #header=T#,  encoding = "UTF-8", stringsAsFactors=F,
        col_names = c(
          "Number",
          "Codes",
          "Name",
          "lat",
          "long",
          "Elevation",
          "minD",
          "maxD",
          "nTemps",
          "Gradient",
          "nCond",
          "Conductivity",
          "nHeatProd",
          "HeatProd",
          "HeatFlow",
          "nSites",
          "YearPub",
          "Reference",
          "Comments",
          "X",
          "x1"
        ),
        col_types = c("iccnnnnninininniiccc")
      )

    # ghf<- ghf.all[,c(5,4,15)]
    # names(ghf)<-c('lon','lat','hf')
    # ghf$hf <- as.numeric(ghf$hf, NA.RM=T)
    # ghf$lat <- as.numeric(ghf$lat, NA.RM=T)
    # ghf$lon <- as.numeric(ghf$lon, NA.RM=T)
    ihf <-
      subset(ghf.all, lat > limsy[1] &
               lat < limsy[2] & long > limsx[1] & long < limsx[2])

    names(ihf) <- str_to_lower(names(ihf))
    if (ppp == TRUE)
      return(spatstat::ppp(ihf$long, ihf$lat, limsx, limsy, marks = ihf[, c(1:21)]))
    else
      return(ihf)
  }


#' Title
#'
#' @param dirname
#' @param save
#' @param ppp
#'
#' @return
#' @export
#'
#' @examples
prepare_ehb <-
  function(dirname = "/Volumes/data/data/global/quakes/ehb",
           save = T, ppp=T) {
    hdf.files <-
      list.files(path = dirname,
                 pattern = "\\.hdf$",
                 full.names = T)

    print(hdf.files)
    ehb.format <-
      list(
        c(
          "a1",
          "a3",
          "a2",
          "i2",
          "i3",
          "i3",
          "i4",
          "i3",
          "f6.2",
          "a1",
          "f8.0",
          "f8.0",
          "2f6.0",
          "3f4.0",
          "4i4",
          "3f8.0",
          "3f6.0",
          "4i4",
          "f5.0"
        )
      )

    col.classes= c("character",  "character", "character",
                   "numeric", "numeric", "numeric", "numeric", "numeric",  "numeric",
                   "character",
                   "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",  "numeric",
                   "numeric", "numeric", "numeric",  "numeric",
                   "numeric", "numeric", "numeric",
                   "numeric", "numeric", "numeric",  "numeric",  "numeric",
                   "numeric", "numeric" , "numeric"

    )
    col.names = c(
      "ahyp",
      "isol",
      "iseq",
      "iyr",
      "mon",
      "iday",
      "ihr",
      "min",
      "sec",
      "ad",
      "lat",
      "long",
      "depth",
      "iscdep",
      "mb",
      "ms",
      "mw",
      "ntot",
      "ntel",
      "ndep",
      "igreg",
      "se",
      "ser",
      "sedep",
      "rstadel",
      "openaz1",
      "openaz2",
      "az1",
      "flen1",
      "az2",
      "flen2",
      "avh"
    )
    # ehb<-read.fortran(fname, format=ehb.format, as.is = TRUE, colClasses = NA, col.names=col.names)
    # max( ehb$glon)
    # head( ehb)
    #

    ehb <-
      map_dfr(
        hdf.files,
        read.fortran,
        format = ehb.format,
        as.is = FALSE,
        colClasses =  col.classes,
        col.names = col.names
      )
    ehb$c <-
      "19"
    ehb$c[ehb$iyr < 50] <- "200" #used to get full years in prep for time
    ehb$time <-
      lubridate::ymd_hms(paste0(
        paste(paste0(ehb$c, ehb$iyr), ehb$mon, ehb$iday, sep = "/"),
        " ",
        paste(ehb$ihr, ehb$min, ehb$sec, sep = ":")
      ))
    if (ppp) ehb  <-
      spatstat::ppp(ehb$long, ehb$lat, c(-180, 180), c(-90, 90), marks = ehb[, c(1:3, 10, 11:32, 34)])

    if (save)
      save(ehb , file = paste0(dirname, "/ehb.Rdata"))
    return(ehb)
  }

#' Title
#'
#' @param ehb
#' @param limsx
#' @param limsy
#' @param ppp
#' @param to
#' @param fname
#'
#' @return
#' @export
#'
#' @examples
read_ehb <- function(ehb = NA,
                     limsx = c(-180, 180),
                     #can be a raster, extent object or 2-length vector
                     limsy = c(-90, 90),
                     #only used if limsx is a 2-length vec
                     ppp = TRUE,
                     to = NA,
                     fname = "~/Dropbox/data/global/quakes/ehb/ehb.Rdata") {
  if (is.na(ehb))
    load(fname)
  if (is.ras(limsx)) {
    extent.lim <- extent(limsx)
    print(extent.lim)
    limsy <-  extent.lim[3:4]
    limsx <- extent.lim[1:2]
  }
  if (length(limsx) >= 4) {
    limsy = limsx[3:4]
    limsx = limsx[1:2]
  }
    ehb$marks$long <-  ehb$x
    ehb$marks$lat <-  ehb$y
  ehb[ehb$x >= limsx[1] &
        ehb$x <= limsx[2]  & ehb$y >= limsy[1]  & ehb$y <= limsy[2]]
}

#' Title
#'
#' @param fname
#' @param sep
#'
#' @return
#' @export
#'
#' @examples
read_wsm <-
  function(fname = "~/Dropbox/data/global/wsm/wsm2016.csv", sep=",") {
    message('note: raw file need to be saved at UTF-8')
   message("reading ", fname)
   return(read.csv(fname, sep = sep))

  }

#' Title
#'
#' @param wsm
#' @param fname
#' @param sep
#' @param limsx
#' @param limsy
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
wsm_ppp <- function(wsm = NA,
                    fname = "~/Dropbox/data/global/wsm/wsm2016.csv",
                    sep=",",
                    limsx = c(-180, 180),
                    limsy = c(-90, 90),...) {
  if (is.ras(limsx))  {
    extent.lim <- extent(limsx)
    print(extent.lim)
    limsy <-  extent.lim[3:4]
    limsx <- extent.lim[1:2]
  }
  if (length(limsx) >= 4) {
    limsy = limsx[3:4]
    limsx = limsx[1:2]
  }
  if (is.na(wsm))
    wsm <- read_wsm(fname= fname,sep=sep)
  names(wsm) <- str_to_lower(names(wsm))
  names(wsm)[names(wsm)=="lon"]<-"long"
   print(names(wsm))
  wsm <-
    subset(wsm, long >= limsx[1] &
             long <= limsx[2] & lat >= limsy[1] & lat <= limsy[2])

  wsm <- subset(wsm, is.finite(lat) & is.finite(long) & is.finite(azi))
  wsm <- subset(wsm,  azi !=999)

  return(ppp(wsm$long, wsm$lat, window=owin(limsx,limsy),
                marks =data.frame(lat=wsm$lat, long=wsm$long,azi=wsm$azi, regime=wsm$regime, depth=wsm$depth)))


  # return(ppp(wsm$LON, wsm$LAT,
  #            owin(limsx, limsy),
  #            marks = wsm[, c(1:2, 5:61)]))
}


#' Title
#'
#' @param fname
#' @param limsx
#' @param limsy
#' @param ppp
#' @param simplify
#'
#' @return
#' @export
#'
#' @examples
read_cmt <-
  function(fname = "~/Dropbox/data/global/quakes/cmt/cmt1.RData",
           limsx = c(-180, 180),
           limsy = c(-90, 90), ppp=T, simplify=F) {
    if (is.ras(limsx))  {
      extent.lim <- extent(limsx)
      print(extent.lim)
      limsy <-  extent.lim[3:4]
      limsx <- extent.lim[1:2]
    }
    if (length(limsx) >= 4) {
      limsy = limsx[3:4]
      limsx = limsx[1:2]
    }
     load(fname)
    # cmt <-
    #   cmtrecord[, c(2, 5, 4, 6, 46, 47, 49, 50, 52, 53, 54, 55, 56, 57, 58, 59, 60)]
    # names(cmt) <- c(
    #   'date',
    #   'long',
    #   'lat',
    #   'depth',
    #   'tp',
    #   't',
    #   'bp',
    #   'b',
    #   'pp',
    #   'p',
    #   'scalarmoment',
    #   'np1strike',
    #   'np1dip',
    #   'np1rake',
    #   'NP2strike',
    #   'np2dip',
    #   'np2rake'
    # )
    #
 cmt <- cmtrecord
 names(cmt) <- str_to_lower(names(cmt))
 #message(cmt %>% str())
  cmt %>% names() %>% str_replace("eigenvalue1", "t") -> names( cmt)
  cmt %>% names() %>% str_replace("eigenvalue2", "b") -> names( cmt)
  cmt %>% names() %>% str_replace("eigenvalue3", "p") -> names( cmt)
  cmt %>% names() %>% str_replace("plunge", "p") -> names( cmt)
  cmt %>% names() %>% str_replace("azimuth", "a") -> names( cmt)
  cmt %>% names() %>% str_replace("latitude", "lat") -> names( cmt)
  cmt %>% names() %>% str_replace("longitude", "long") -> names( cmt)
  message("cmt records : ",str_c(names(cmt), ", "))

    cmt <-
      subset(cmt, long >= limsx[1] &
               long <= limsx[2] & lat >= limsy[1] & lat <= limsy[2])

                 cmt$date <- as.POSIXct(cmt$date)

    #see Stress derivation from earthquake focal mechanisms
    # A. Barth, J. Reinecker and O. Heidbach


                 message("cmt regimes set according to wsm guidlelines as per http://www.world-stress-map.org/data/")
    cmt$sh <- cmt$pa
    if (simplify)  cmt$regime <- "U" else cmt$regime <- "U"

    cmt$quality <- "E"

    inds <- which(cmt$pp >= 52 & cmt$tp <= 35)
    cmt$sh[inds] <- cmt$ba[inds]
    cmt$regime[inds] <- "NF"
    cmt$quality[inds] <- "C"

    inds <- which(cmt$pp < 52 &  cmt$pp >= 40 & cmt$tp <= 20)
    cmt$sh[inds] <- cmt$ta[inds] + 90
    if (simplify)  cmt$regime[inds] <-"SS" else cmt$regime[inds] <- "NS"
    cmt$quality[inds] <- "C"


    inds <- which(cmt$pp < 40 &  cmt$bp >= 45 & cmt$tp <= 20)
    cmt$sh[inds] <- cmt$ta[inds] + 90
    cmt$regime[inds] <- "SS"
    cmt$quality[inds] <- "C"

    inds <- which(cmt$pp <= 20 & cmt$bp >= 45 & cmt$tp < 40)
    cmt$regime[inds] <- "SS"
    cmt$quality[inds] <- "C"

    inds <-  which(cmt$pp <= 20 &  cmt$tp >= 40 & cmt$tp < 52)  #cmt$AZI[inds] <-cmt$P[inds]
   if (simplify)  cmt$regime[inds] <- "TF" else cmt$regime[inds] <- "TS"
    cmt$quality[inds] <- "C"

    inds <-
      which(cmt$pp <= 35 & cmt$tp >= 52)  #cmt$AZI[inds] <-cmt$P[inds]
    cmt$regime[inds] <- "TF"
    cmt$quality[inds] <- "C"
    cmt$sh <- cmt$sh %% 180

    message( "setting cmt aizmuth to shmax (p-axis) by default")
    cmt$azi <- cmt$sh  #sets AZI <- SH by default

    if (ppp==TRUE)
      return( # modulu
 ppp(cmt$long, cmt$lat, window=owin(limsx,limsy),  marks =cmt)) else
   return(cmt) #data.frame(AZI=cmt$AZI, REGIME=cmt$REGIME, DEPTH=cmt$DEPTH, P=cmt$P, T=cmt$T, B=cmt$B))) else

  }

#' Title
#'
#' @param limsx
#' @param limsy
#'
#' @return
#' @export
#'
#' @examples
getOwin <- function(limsx, limsy) {
  owinObj <- owin(xrange = limsx, yrange = limsy)
  return(owinObj)
}



#' Title
#'
#' @param wsm
#' @param scale
#' @param psp
#' @param segment
#' @param norm
#' @param simplify
#'
#' @return
#' @export
#'
#' @examples
getlines<- function(wsm,   scale=1, psp=F, segment=F, norm=T, simplify=T){

  names(wsm) <- stringr::str_to_lower(names(wsm))
  #returns stress trajectories
  #scale=1
  if (is.null( wsm$marks$dip))   wsm$marks$dip <-0
  if (norm) scale<- cos(wsm$marks$dip*pi/180)*scale #norm means scale according dip
  sinazi<- sin(wsm$marks$azi*pi/180)*scale
  cosazi<- cos(wsm$marks$azi*pi/180)*scale
  x<- matrix(ncol=length(wsm$y),nrow=3)
  y<- matrix(ncol=length(wsm$y),nrow=3)
  x[1,]<-wsm$x+ sinazi
  x[2,]<-wsm$x- sinazi
  y[1,]<-wsm$y+ cosazi
  y[2,]<-wsm$y- cosazi
  #
  # print(x)
  # print(y)
  #
  # cols<- vector("character",length(wsm$regime))
  # cols[wsm$regime=="NF"] <-"Red"
  # cols[wsm$regime=="NS"] <-"Red"
  # cols[wsm$regime=="SS"] <-"Green"
  # cols[wsm$regime=="TF"] <-"Blue"
  # cols[wsm$regime=="TS"] <-"Blue"
  # cols[wsm$regime=="U"] <-"Black"
  # cols<- rep(cols, each=3)
  if (simplify){
    wsm$marks$regime[wsm$marks$regime=="TS"]<-"SS"
 # wsm$marks$regime[wsm$marks$regime=="U"]<-"NF"
  wsm$marks$regime[wsm$marks$regime=="NS"]<-"SS"
  }
  if (psp==T & segment==F ){
    xrange<- c(min(x[2,])-scale, max(x[1,])+scale)
    yrange<- c(min(y[2,])-scale, max(y[1,])+scale)
    print(xrange)
    print(yrange)

    return(psp(x[1,], y[1,], x[2,], y[2,], owin(xrange,yrange), marks =wsm$marks$regime))
  } else if (segment==T)  {

    data.frame(x=wsm$x, y=wsm$y,xstart=x[2,], xend=x[1,], ystart=y[2,], yend=y[1,], regime=wsm$marks$regime, plunge=wsm$marks$dip )
  } else
    {
    return(data.frame(x=as.vector(x), y=as.vector(y), regime=rep(wsm$marks$regime, each=3)))
  }
}




#' Title
#'
#' @param my.points
#' @param slab
#' @param anom
#'
#' @return
#' @export
#'
#' @examples
add_slab_depth <- function(my.points, slab, anom=NULL){
  my.points$slab.dip <-  my.points %>% raster::extract(slab$dip, .)
  my.points$slab.depth <-  my.points %>% raster::extract(slab$depth, .)
  my.points$slab.strike <- my.points %>% raster::extract(slab$strike, .)
   if(!is.null(anom))  my.points$depth.anom <-  my.points$depth + my.points$slab.depth
  return(my.points)

}

#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
slab2ppp <- function(...){
  slab2pp(...)
}

#' Title
#'
#' @param slab
#' @param verbose
#' @param case
#'
#' @return
#' @export
#'
#' @examples
slab2pp <- function(slab, verbose=F, case =1){
 if (case ==1){
   slab$borders %>% as.psp() -> slab$borders.psp
   slab$bordersi %>% as.psp() -> slab$bordersi.psp}
  else
   slab$borders %>% spp2spl( ) %>% as.psp() -> slab$borders.psp
    slab$coastlines     %>% as.psp() -> slab$coastlines.psp
    # slab$plates  %>% spp2spl( ) %>% as.psp() -> slab$plates.ppp
    slab$plates    %>% as.psp() -> slab$plates.ppp
  slab$vo %>% as.ppp() -> slab$vo.ppp
  slab$hf %>% as.ppp() -> slab$hf.ppp
  slab$cmt %>% as.ppp() -> slab$cmt.ppp
  slab$wsm %>% as.ppp() -> slab$wsm.ppp
  slab$ehb %>% as.ppp() -> slab$ehb.ppp
  if (verbose) print(slab %>% str(max =1))
  slab
}


