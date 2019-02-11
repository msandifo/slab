
mex1 <-assemble_slab(slab.name="mex", extend.lims = c(-2, -7, 5.5, .5),  simplify=T)


mex1$cmt.ppp$marks %>% subset(ms>7.8 & lubridate::year(date)==2017) %>% dplyr::select(c( long, lat)) -> cmt.e

isc.ppp$marks %>% subset(m>7.5 & lubridate::year(date)==2017) %>% dplyr::select(c(long, lat))

get_slab_details(mex1$slab ,longlat = c(-94.11, 14.85 ),plates= bird.plates.psp)

get_slab_details(mex$slab, longlat=cmt.e ,plates= bird.plates.psp)
cbind(mex1$cmt.ppp$marks %>% subset(ms>7.8 & lubridate::year(date)==2017),
      get_slab_details(mex$slab, longlat=cmt.e ,plates= bird.plates.psp))


purrr::map_dfr(cmt.e, get_slab_details(mex$slab, longlat=. ,plates= bird.plates.psp))

