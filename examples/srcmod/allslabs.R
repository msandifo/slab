slab.dir <- "/Volumes/data/data/global/slabs/slab1/allslabs/"
out.file <- "/Volumes/data/data/global/slabs/slab1/slabs.Rdata"

prep.data<-T

if (prep.data){
  list.files(slab.dir) -> grd1.files
for (typ in c("depth", "strike", "dip")){
if (typ=="depth") grd.files <-grd1.files[str_detect(grd1.files,"0_clip.grd")] else
if (typ=="strike") grd.files <-grd1.files[str_detect(grd1.files,"0_strclip.grd")] else
 grd.files <-grd1.files[str_detect(grd1.files,"0_dipclip.grd")]

 mslab  <-
   raster(paste0(slab.dir,grd.files[1]))
 mslab.1 <-
   raster(paste0(slab.dir,grd.files[2]))

 mslab <- merge(  mslab ,  mslab.1 )

 for (i in 3:length(grd.files ))  {
   mslab.1 <-
     raster(paste0(slab.dir,grd.files[i]))
   mslab<- merge( mslab, mslab.1 )
 }
 if (typ=="depth") depth <- raster::rotate(mslab)
 if (typ=="strike") strike <- raster::rotate(mslab)
 if (typ=="dip") dip <- raster::rotate(mslab)
}

 # print(extent(slab.depth)); print(extent(slab.dip));print(extent(slab.strike))

 slab <- stack(depth, nl = 9)   %>%
   addLayer(dip) %>%
   addLayer(strike)
 names(slab) <- c("depth", "dip", "strike")
 crs(slab) <- "+init=epsg:4326"


 save(slab, file=out.file)
} else load(out.file)

plot(slab)
