slab.dir <- "/Volumes/data/data/global/slabs/slab1/allslabs/"
 list.files(slab.dir) -> grd.files

 grd.files <-grd.files[str_detect(grd.files,"0_clip.grd")]


 slab.depth <-
   raster(paste0(slab.dir,grd.files[1]))
 slab.depth.1 <-
   raster(paste0(slab.dir,grd.files[2]))

 slab.depth<- merge( slab.depth, slab.depth.1 )

 for (i in 3:length(grd.files ))  {
   slab.depth.1 <-
     raster(paste0(slab.dir,grd.files[i]))
   slab.depth<- merge( slab.depth, slab.depth.1 )
 }
