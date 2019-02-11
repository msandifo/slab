# shap_funcs
# functions for readinga nd manipulating shape files, as need for example for slab assembla.'




#' Computes a spatile lines object froma  data frame with long lat and id columns
#' afatped from https://rpubs.com/walkerke/points_to_line
#' @param data
#'
#' @return SpatialLines
#' @export
#'
#' @examples
#'  plates= get_plates()
#'  df_to spline(plates)
#'
df2spline <- function(df  , proj.string="+init=epsg:4326") {

  coordinates(df) <- ~long+lat
  ids<- df$id  %>%  unique()
  paths <- sp::split(df, df$id)
  sp_lines <- SpatialLines(list(Lines(list(Line(paths[[1]])), ids[1])))
  for (p in 2:length(paths)) {
    l <- SpatialLines(list(Lines(list(Line(paths[[p]])), ids[p])))
    sp_lines <- spRbind(sp_lines, l)
  }

  proj4string(sp_lines) = CRS("+init=epsg:4326")
  return(sp_lines)

}

#' Title
#'
#' @param spldf
#' @param ext
#' @param proj.string
#'
#' @return
#' @export
#'
#' @examples
spldf_crop<- function(spldf, ext,  proj.string= "+init=epsg:4326"){
  spldf  %>% fortify() %>%
    subset(long >= ext[1] & long <= ext[2] & lat >= ext[3] & lat<= ext[4]) %>%
    df2spline(  proj.string=proj.string)

}



#' Title
#'
#' @param dir
#' @param type
#' @param resolution
#' @param level
#' @param river
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_wdbii_file <- function( dir="~/data/global/coastlines/gshhg-shp-2/",
                            type="WDBII",
                            resolution="i",
                            level=1,
                            river=F,
                            ...
){

  if (type=="WDBII") {if (river ==TRUE)
  {type=paste0(type,"_river")
  inset="0"} else  {type=paste0(type,"_border")
  inset=""
  }
  }

  fn = paste0(dir,"/",
              "WDBII", "_shp/", resolution,"/",
              type, "_", resolution,"_L",inset,level,".shp")
  print(file.exists(fn))
  return(fn)
}


#' Title
#'
#' @param dir
#' @param type
#' @param resolution
#' @param level
#'
#' @return
#' @export
#'
#' @examples
get_gshhg_file <- function( dir="~/data/global/coastlines/gshhg-shp-2/",
                            type="GSHHS",
                            resolution="i",
                            level=1
){



  fn = paste0(dir,"/",
              type, "_shp/",
              resolution,
              "/GSHHS_", resolution,"_L",level,".shp")
  print(file.exists(fn))
  return(fn)
}
# #
# xlim=c(138, 152),
# ylim=c(-41.5, -33.),
# grid=c(4,2),
# detail="i",
# factor=.25,
# projection=NULL,level=1,no.clip=F,
# properly=T,
# checkPolygons=F,
# rivers=F,
# borders=F,
# shift =F,
# ...
# ){
#   # library(maps)
#   library(rgeos)
#   # library(stringr)
#   # library(ncdf4)
#   # library(reshape2)
#   # library(ggplot2)
#   # library(mapdata)
#   # library(maptools)
#   library(PBSmapping)
#   # library(magrittr)
#   #xy.ratio <- diff(ylim)/diff(xlim)
#   expand_lims<- function(lims, factor=.2) lims+ c(-1,1)*(1+diff(lims))*factor  # function ...
#   useWest=F
#
#   if (!rgeosStatus()) gpclibPermit()
#   #gshhg <- "/Users/msandifo/Desktop/gshhg-bin-2/"
#   if (!rivers & !borders) gshhg.f.b <- paste0(gshhg,"gshhs_",detail,".b")  else if (!borders)  gshhg.f.b <- paste0(gshhg,"wdb_rivers_",detail,".b") else gshhg.f.b <- paste0(gshhg,"wdb_borders_",detail,".b")
#
#   #gshhs.f.b <- system.file("share/gshhs_c.b", package="maptools")
#   shore <- importGSHHS(gshhg.f.b , xlim=xlim , ylim=ylim , maxLevel=level)
#
#   # shore<-getRgshhsMap(gshhg.f.b ,  xlim=xlim , ylim=ylim , maxLevel=level)
#   #wdb_borders.i.b <- paste0(gshhg,"wdb_borders_",detail,".b")
#   # shore <- getRgshhsMap(gshhg.f.b, xlim =  (xlim ),
#   #                       ylim =  (ylim ), level=level , no.clip=no.clip, checkPolygons=checkPolygons,
#   #                       properly=properly )
#   #  shore <-  getRgshhsMap (gshhg.f.b, xlim =  (xlim ),
#   #                       ylim =  (ylim ), level=level , no.clip=no.clip, checkPolygons=checkPolygons,
#   #                       properly=properly,shift=shift, avoidGEOS=F )
#   # #borders <- getRgshhsMap(wdb_borders.i.b, xlim = expand_lims(xlim), ylim = expand_lims(ylim)  ) %>% fortify()  ??? why doenst it work?
#   shore
#
# }



#' Title
#'
#' @param fn
#' @param xlim
#' @param ylim
#' @param factor
#' @param level
#' @param no.clip
#' @param properly
#' @param checkPolygons
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_ghss <- function( fn = get_gshhg_file() ,
                      xlim=c(138, 152),
                      ylim=c(-41.5, -33.),

                      factor=.25,
                      level=1,
                      no.clip=F,
                      properly=T,
                      checkPolygons=F,
                      ...
){

  expand_lims<- function(lims, factor=.2) lims+ c(-1,1)*(1+diff(lims))*factor  # function ...
  useWest=F

  if (!rgeosStatus()) gpclibPermit()

  shore <- getRgshhsMap(gshhg.f.b, xlim =  (xlim ),
                        ylim =  (ylim ), level=level , no.clip=no.clip, checkPolygons=checkPolygons,
                        properly=properly )

  # shore <- importGSHHS(fn , xlim=xlim , ylim=ylim , maxLevel=level)

  shore

}


#' Title
#'
#' @param gshhg
#' @param xlim
#' @param ylim
#' @param grid
#' @param detail
#' @param factor
#' @param projection
#' @param level
#' @param no.clip
#' @param properly
#' @param checkPolygons
#' @param rivers
#' @param borders
#' @param shift
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_ghss1 <- function( gshhg = "~/data/global/coastlines/gshhg-bin-2/",
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
  if (!rivers & !borders) gshhg.f.b <- paste0(gshhg,"gshhs_",detail,".b")  else if (!borders)  gshhg.f.b <- paste0(gshhg,"wdb_rivers_",detail,".b") else gshhg.f.b <- paste0(gshhg,"wdb_borders_",detail,".b")

  #gshhs.f.b <- system.file("share/gshhs_c.b", package="maptools")
  shore <- importGSHHS(gshhg.f.b , xlim=xlim , ylim=ylim , maxLevel=level)

  # shore<-getRgshhsMap(gshhg.f.b ,  xlim=xlim , ylim=ylim , maxLevel=level)
  #wdb_borders.i.b <- paste0(gshhg,"wdb_borders_",detail,".b")
  # shore <- getRgshhsMap(gshhg.f.b, xlim =  (xlim ),
  #                       ylim =  (ylim ), level=level , no.clip=no.clip, checkPolygons=checkPolygons,
  #                       properly=properly )
  #  shore <-  getRgshhsMap (gshhg.f.b, xlim =  (xlim ),
  #                       ylim =  (ylim ), level=level , no.clip=no.clip, checkPolet_wdbii_filygons=checkPolygons,
  #                       properly=properly,shift=shift, avoidGEOS=F )
  # #borders <- getRgshhsMap(wdb_borders.i.b, xlim = expand_lims(xlim), ylim = expand_lims(ylim)  ) %>% fortify()  ??? why doenst it work?
  shore

}




#' Title
#'
#' @param fn
#'
#' @return
#' @export
#'
#' @examples
get_plates <- function(fn = "~/data/global/polygons/tectonicplates-master/PB2002_boundaries.shp" ){
  # maptools::readShapeLines(fn, repair=T, delete_null_obj=T, proj4string=crs("+init=epsg:4326"))
  raster::shapefile(fn)
}




#' Title
#'
#' @param lims
#' @param spatial
#' @param detail
#'
#' @return
#' @export
#'
#' @examples
get_coasts<- function(lims=c(-107, -88,  117,  22), spatial=T, detail="i"){

  my.coast1 <-get_ghss(xlim= 360+lims[1:2], ylim=lims[3:4], detail=detail, factor=1, level=3, rivers=F, borders=F)
  names(my.coast1)
  if (spatial) return(PolySet2SpatialPolygons(my.coast1, close_polys=TRUE)  ) else return(my.coast1 )
}

#' Title
#'
#' @param lims
#' @param spatial
#' @param detail
#'
#' @return
#' @export
#'
#' @examples
get_borders<- function(lims=c(-107, -88,  117,  22), spatial=T, detail="i"){
  my.coast1 <-get_ghss(xlim= 360+lims[1:2], ylim=lims[3:4], detail=detail, factor=1, level=3, rivers=F, borders=T)
  if (spatial) return(PolySet2SpatialPolygons(my.coast1, close_polys=TRUE)  ) else return(my.coast1 )
}


#
# get_borders<- function(countries, borders=NA) {
#   if (is.na(borders)) borders <- get_borders_TM()
#   border.select <- borders %>% subset(NAME %in% countries)
#   get_i_borders <- function(country) borders %>% subset(NAME %in% countries) %>% fortify %>% mutate(country=country)
#   purrr::map_df(countries, get_i_borders)  %>% rbind  %>% arrange(country,id, piece)
# }

#q
# get_borders_TM <- function(fn = "/Volumes/data/data/global/coastlines/TM_WORLD_BORDERS-0/TM_WORLD_BORDERS-0.3.shp"){
#   shapefile(fn)
# }


#' Title
#'
#' @return
#' @export
#'
#' @examples
get_borders_TM <- function(){
  shapefile("~/data/global/coastlines/TM_WORLD_BORDERS-0/TM_WORLD_BORDERS-0.3.shp")
}

#' Title
#'
#' @param lims
#' @param spatial
#' @param detail
#'
#' @return
#' @export
#'
#' @examples
get_rivers<- function(lims=c(-107, -88,  117,  22), spatial=T, detail="i"){
  my.coast1 <-get_ghss(xlim= 360+lims[1:2], ylim=lims[3:4], detail=detail, factor=1, level=3, rivers=T, borders=F)
  if (spatial) return(PolySet2SpatialPolygons(my.coast1, close_polys=TRUE)  ) else return(my.coast1 )
}


#' Title
#'
#' @param fn
#' @param slab
#' @param sp
#' @param multi
#' @param factors
#'
#' @return
#' @export
#'
#' @examples
get_sf<- function(fn, slab, sp=T, multi=T, factors=F)
  { if (sp) st_read(fn, promote_to_multi=multi, stringsAsFactors=factors)  %>%
  st_crop( extent(slab))  %>%
  st_transform(  crs(slab)@projargs) %>%
  as( "Spatial") else st_read(fn)  %>%
    st_crop( extent(slab))  %>%
    st_transform(  crs(slab)@projargs)
}




