#' Title
#'
#' @param slab
#' @param inc
#'
#' @return
#' @export
#'
#' @examples
graticule <- function(slab, inc=c(5,5)) {
  lims <- extent(slab$slab)[1:4]
  longs <- seq(ceiling(lims[1]/inc[1])*inc[1],
               floor(lims[2]/inc[1])*inc[1], inc[1])

  lats <- seq( ceiling(lims[3]/inc[2])*inc[2],
               floor(lims[4]/inc[2])*inc[2], inc[2]
  )
  list(longs=longs, lats=lats)
}

#' Title
#'
#' @param label
#' @param labs
#'
#' @return
#' @export
#'
#' @examples
lsign<- function(label,  labs = c("°W","  ", "°E")){
  vec.sign = round(label/abs(label))
  vec.sign[is.na(vec.sign)] =0
  str_c(label, labs[vec.sign+2])

}

#' Title
#'
#' @param vec
#' @param inc
#' @param proj4string
#'
#' @return
#' @export
#'
#' @examples
graticule_longs <- function(vec, inc=.1, proj4string= CRS("+init=epsg:4326")){
  labs = c("°W","  ", "°E")

  build_df <- function(i)  {  data.frame(x= i,  y=seq(-90,90, inc), id=i)
  }
  map(vec$longs, build_df) %>% do.call("rbind", .) %>% mutate(label=lsign(id, labs=labs))
}


#' Title
#'
#' @param vec
#' @param inc
#' @param sp
#' @param proj4string
#'
#' @return
#' @export
#'
#' @examples
graticule_lats <- function(vec, inc=.1, sp=F , proj4string= CRS("+init=epsg:4326")){

  labs = c("°S","  ", "°N")


  build_df <- function(i, inc)  data.frame(x=seq(-180,180,inc),  y=i,  ID=lsign(i, labs))

  build_ls <- function(i,inc)  Lines(Line(cbind(long=seq(-180,180,inc),  lat=rep(i,(360/inc)+1) )),   ID=lsign(i, labs))

  grat <- purrr::map_df(vec$lats, build_df, inc=inc) #%>% do.call("rbind", .) # %>% mutate(label=lsign(ID, labs=labs))

  if (sp)  grat <- purrr::map(vec$lats, build_ls, inc=inc)  %>%  SpatialLines(proj4string=proj4string  )
return(grat)
}

