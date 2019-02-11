library(tidyverse)
library(raster)
library(slab)
library(spatstat)
slab.name = "sam"

extend.lims = c(-1, 1, -1, 1)
slab.dir = "/Users/msandifo/Dropbox/msandifo/documents/programming/r/2017/ecuador/data/slab1/allslabs/"
etopo = "~/data/global/topography/etopo/ETOPO1_Bed_g_geotiff.tif"
fact = 1
detail="h"
ppp=T  # for extarcting coasts, rivers etc.
simplify=F   #limits regiems in cmt to NF, TF , SS- still need to modify ..... wsm

  #assemble slab 1.0 rasters

  if (missing(slab.name)) slab.name=slab.name
  message("still need to systematise all the spatial outputs as similar classes")
      slab.depth <-
      raster(paste0(slab.dir, slab.name, "_slab1.0_clip.grd"))
    slab.dip <-
      raster(paste0(slab.dir, slab.name, "_slab1.0_dipclip.grd"))
    slab.strike <-
      raster(paste0(slab.dir, slab.name, "_slab1.0_strclip.grd"))

    # print(extent(slab.depth)); print(extent(slab.dip));print(extent(slab.strike))

    slab <- stack(slab.depth, nl = 9)   %>%
      addLayer(slab.dip) %>%
      addLayer(slab.strike)

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
  wsm_ppp(fname = '~/Documents/programming/r/2013/yogi/wsm2008.csv', sep=";",limsx = limsx, limsy = limsy) %>%
    ppp2spdf( full.extent) %>%
    add_slab_depth(  slab, anom=T)->
    wsm

  message("... volcanoes")
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

    message("... loading coasts")
    library(maptools)
    coasts <- get_coasts( full.extent, sp=T, detail="h")

    message("loading rivers")
    rivers <- get_rivers( full.extent, sp=T, detail="h")

    message("... loading borders")
    borders<-get_borders_TM()
    borders<-  borders %>%subset(LON >= limsx[1] & LON <= limsx[2] & LAT >= limsy[1] & LAT<= limsy[2]) #crop(borders,full.extent)

    message("... loading plates")

    plates= get_plates() %>% fortify()%>%subset(long >= limsx[1] & long <= limsx[2] & lat >= limsy[1] & lat<= limsy[2]) %>% #crop(borders,full.extent)
      points_to_line() #"long","lat", id_field = "id")

    get_plates() %>% spldf_crop(full.extent )

    # ebbox <- function( ext=  full.extent){
    #                   p<-Polygon(cbind(ext[c(1:2,2:1)],ext[c(3,3,4,4)]))
    #                   ps<-Polygons(list(p), "s")
    #                   SpatialPolygons(list(ps) ,proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 " ))
    # }
    #
    # plates@data <- cbind(plates@data,coordinates(plates) %>% unlist() %>% matrix( ncol=2, byrow=F) %>%as.data.frame())
    #
    #
    # ebbox <- function( ext=  full.extent){
    #   p<-Polygon(cbind(ext[c(1:2,2:1)],ext[c(3,3,4,4)]))
    #   ps<-Polygons(list(p), "s")
    #   SpatialPolygons(list(ps) ,proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 " ))
    # }
    #


    # %>%subset(x >= limsx[1] & x <= limsx[2] & y >= limsy[1] & y<= limsy[2]) #crop(borders,full.extent)
    # plates<-crop(plates,full.extent)
    # plates<- crop(plates, c(limsx, limsy))
    # print("---")
  # message("Bird plate boudary ids : " , plates$id %>%   unique  %>% str_c( collapse = ", "))

    message("plate boudary ids : ", get_plate_boundary_ids(plates) %>% str_c( collapse = ", "))
    message("plate boudary ids : ", plates$id %>% unique() %>%str_c( collapse = ", "))

    my.slab <-list(
      slab = slab,
      hf = hf,
      vo = vo,
      ehb = ehb,
      wsm=wsm,
      cmt=cmt,
      coasts=coasts,
      rivers=rivers,
      plates=plates,
      borders=borders,
      name = slab.name
    )

    if(ppp) slab2pp(my.slab) -> my.slab

