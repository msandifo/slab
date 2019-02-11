library(slab)
fn <-get_gshhg_file()
# dir="/Volumes/data/data/global/coastines/gshhg-shp-2/"
#
# type="GSHHS"
# resolution="i"
# level=2
# xlim=c(138, 152)
# ylim=c(-41.5, -33.)
# fn = paste0(dir,"/",
#            type, "_shp/",
#            resolution,
#            "/GSHHS_", resolution,"_L",level,".shp")
# print(fn)
# print("/Volumes/data/data/global/polygons/gshhg-shp-2/GSHHS_shp/i/GSHHS_i_L2.shp")
print(file.exists(fn))

box =  extent(sam1$slab)
library(sf)
st_read(fn)  %>% st_crop(  box)  %>% st_transform(  get_proj(sam1)@projargs) -> bounds
st_read(fn)  %>% st_crop(  box)    -> bounds

ggplot(  )  + geom_map()+
   # # scale_colour_manual(values = c("red3",  "orange","green4","blue4", "black", "brown" ))+
#
# lims2 <- lims
# lims<-(sam$slab %>% extent())[1:4]
#
# my.stretch= c(.35,.9)
# if (shade) ptemp.cont <- ptemp.cont +  # ggmap::inset_raster(sam$slab$shade %>% as.raster(col= grey.colors(200), maxpixels=511*451), xmin= e.ex@xmin, xmax=e.ex@xmax, ymin= e.ex@ymin, ymax=e.ex@ymax )+
  ggmap::inset_raster(sam1$slab$shade   %>%   stretch_ras(my.stretch) %>%
                        # raster::projectRaster( crs=proj.laea  ) %>%
                        as.raster(col= grey.colors(200),                                                                                                                                                                                                                   maxpixels=sam$slab$shade@ncols*sam$slab$shade@nrows),
                      xmin=lims[1], xmax=lims[2], ymin= lims[3], ymax=lims[4] )

my.stretch= c(.35,.9)
ggplot()+ geom_sf(data=bounds) +geom_raster(data=sam1$slab$shade   %>%   stretch_ras(my.stretch) %>%
                                              as.raster(col= grey.colors(200)) %>% fortify())


  ptemp.cont + geom_sf(data=bounds)

  ggmap::inset_raster( sam1$slab$shade   %>%   stretch_ras(my.stretch) %>%
                        as.raster(col= grey.colors(200),
                                  maxpixels=sam1$slab$shade@ncols*sam1$slab$shade@nrows),
                      xmin=box[1], xmax=box[2], ymin= box[3], ymax=box[4] )

get_ghss2 <- function( gshhg = "/Users/msandifo/Dropbox/data/global/coastlines/gshhg-bin-2/",
                      xlim=c(138, 152),
                      ylim=c(-41.5, -33.),
                      grid=c(4,2),
                      detail="i",
                      factor=.25,
                      projection=NULL,level=1,no.clip=F,
                      properly=T,
                      checkPolygons=F,
                      rivers=F,
                      borders=F,
                      shift =F,
                      ...
){
  # library(maps)
  library(rgeos)
  # library(stringr)
  # library(ncdf4)
  # library(reshape2)
  # library(ggplot2)
  # library(mapdata)
  # library(maptools)
  library(PBSmapping)
  # library(magrittr)
  #xy.ratio <- diff(ylim)/diff(xlim)
  expand_lims<- function(lims, factor=.2) lims+ c(-1,1)*(1+diff(lims))*factor  # function ...
  useWest=F

  if (!rgeosStatus()) gpclibPermit()
  #gshhg <- "/Users/msandifo/Desktop/gshhg-bin-2/"
  if (!rivers & !borders) gshhg.f.b <- paste0(gshhg,"gshhs_",detail,".b")  else
    if (!borders)  gshhg.f.b <- paste0(gshhg,"wdb_rivers_",detail,".b") else
      gshhg.f.b <- paste0(gshhg,"wdb_borders_",detail,".b")
#  shore <- st_read( gshhg.f.b)
  #gshhs.f.b <- system.file("share/gshhs_c.b", package="maptools")
  #shore <- importGSHHS(gshhg.f.b , xlim=xlim , ylim=ylim , maxLevel=level)

  # shore<-getRgshhsMap(gshhg.f.b ,  xlim=xlim , ylim=ylim , maxLevel=level)
  #wdb_borders.i.b <- paste0(gshhg,"wdb_borders_",detail,".b")
  # shore <- getRgshhsMap(gshhg.f.b, xlim =  (xlim ),
  #                       ylim =  (ylim ), level=level , no.clip=no.clip, checkPolygons=checkPolygons,
  #                       properly=properly )

  shore <-  getRgshhsMap (gshhg.f.b, xlim =  (xlim ),
                          ylim =  (ylim ), level=level , no.clip=no.clip, checkPolygons=checkPolygons,
                          properly=properly,shift=shift, avoidGEOS=F )

    shore <-  getRgshhsMap (gshhg.f.b, xlim =  (xlim ),
                          ylim =  (ylim ), level=level , no.clip=no.clip, checkPolygons=checkPolygons,
                         properly=properly,shift=shift, avoidGEOS=F )
  # #borders <- getRgshhsMap(wdb_borders.i.b, xlim = expand_lims(xlim), ylim = expand_lims(ylim)  ) %>% fortify()  ??? why doenst it work?
  shore

}


rivers1 <- rbind(get_sf(get_wdbii_file(river=T, level=1, res="i"), sam1$slab, sp=F),
         get_sf(get_wdbii_file(river=T, level=2, res="i"), sam1$slab, sp=F),
         get_sf(get_wdbii_file(river=T, level=3, res="i"), sam1$slab, sp=F),
         get_sf(get_wdbii_file(river=T, level=4, res="i"), sam1$slab, sp=F)) %>%
  as("Spatial")
plot(rivers1)
