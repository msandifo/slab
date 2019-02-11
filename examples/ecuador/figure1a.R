proj.laea = CRS(paste0("+proj=laea +lat_0=", round(min(raster::extent(mex$slab$depth)[3:4]),1), " +lon_0=", round(mean(raster::extent(mex$slab$depth)[1:2]), 1)))
mex$slab$depth %>% raster::projectRaster( crs= proj.laea)  %>% aggregate( fact=5) -> mex.dep.laea
lex <- extent(mex.dep.laea)
extent(mex.dep.laea) <-c(lex[1], lex[2], lex[3], lex[4])/1000
sumaco <- c( -77.625833,-0.538056)
sumaco.laea <- convertPts(sumaco[1],sumaco[2], from="+init=epsg:4326" , to=proj.laea)/1000

ehb.slab <- mex$ehb.ppp$marks %>%
  subset(depth.anom >= -5    | (depth.anom >= -20  & depth > 70 )) %>%
  dplyr::arrange(depth.anom)
#ehb1.slab <- mex$ehb.ppp$marks %>% subset(depth.anom >=0)
vo.slab <- mex$vo.ppp$marks

#ehb1 <-convertPts(ehb1.slab$long, ehb1.slab$lat,  from="+init=epsg:4326" , to=proj.laea)/1000
vo <-convertPts(vo.slab$long, vo.slab$lat,  from="+init=epsg:4326" , to=proj.laea)/1000
bird <- convertPts(bird.plates$long, bird.plates$lat,  from="+init=epsg:4326" , to=proj.laea)/1000
bird$z <- -5
bird$id <- bird.plates$id
# image(mex.dep.laea)
# points(sumaco.laea$x, sumaco.laea$y)
#ehb.slab$depth.anom %>% range() %>% diff()



#set colouring
ehb.slab$depth.anom.r <-round(ehb.slab$depth.anom)
ehb.slab$depth.anom.r[ehb.slab$depth.anom.r> 30 ]<- 30


cols<-c(colorRampPalette(c( "red",  "white" ))(abs(min(ehb.slab$depth.anom.r))+1),
        colorRampPalette(c(     "white" , "blue"))(abs(max(ehb.slab$depth.anom.r))+2))

ehb.slab$cols <- cols[c(ehb.slab$depth.anom.r+abs(min(ehb.slab$depth.anom.r))+1)]
tear.zone=list()



tear.zone$x=c(-78.6, -78, -77,-76.5, -76.35)
tear.zone$y=c(-.7, -.6, -.2, -.1, -0)-.3

tear.zone.laea =(GEOmap::getsplineG(tear.zone$x, tear.zone$y, kdiv=20) %>%  convertPts(to=proj.laea))/1000

mex.dep.laea1 <- mex.dep.laea
na.cells <-extract(mex.dep.laea, tear.zone.laea[20:30,] , cellnumbers=T, buffer=20)
mex.dep.laea1[(do.call("rbind",na.cells))[,1]] <- NA
na.cells <-extract(mex.dep.laea, tear.zone.laea[10:40,], cellnumbers=T, buffer=14)
mex.dep.laea1[(do.call("rbind",na.cells))[,1]] <- NA
na.cells1 <-extract(mex.dep.laea, tear.zone.laea , cellnumbers=T, buffer=10)
mex.dep.laea1[(do.call("rbind",na.cells1))[,1]] <- NA

v <- ehb.slab$depth.anom
v[ehb.slab$depth>300]=0
v[ehb.slab$depth<70]=0
#### Thin plate spline model
ehb <-convertPts(ehb.slab$long, ehb.slab$lat,  from="+init=epsg:4326" , to=proj.laea)/1000
# ehb1 = rbind(ehb,convertPts(c(-76, -76, -76, -75.5, -75.7, -75.5, -75, -75, -75) ,
#                            c( -1, -.7, -.3, -.5,  -.2, .1, 0.1,.3,.5)+.3, from="+init=epsg:4326" , to=proj.laea)/1000)
# v= c(v, 140, 160, 0, 120, 150, 0, 90,100,0)

V.zone= GEOmap::getsplineG(tear.zone$x, tear.zone$y, kdiv=40)
V.zone$z=V.zone$y*0

inc=.5
ehb1 = rbind(ehb,convertPts(c(  V.zone$x,V.zone$x, V.zone$x,V.zone$x) ,
                            c( V.zone$y+inc, V.zone$y+inc/2,V.zone$y-inc/2, V.zone$y-inc) , from="+init=epsg:4326" , to=proj.laea)/1000)
v= c(v,   V.zone$z-20,V.zone$z-30, V.zone$z+300, V.zone$z+120)

inds <- which(ehb.slab$m >= 5)

tps <- Tps(ehb1[inds,], v[inds] , method="REML")
        
# p <- mex.dep.laea
# plot(r)
p <- interpolate(mex.dep.laea, tps)
p <- mask(p, mex.dep.laea)

mex1 <- mex.dep.laea
mex1[p<7]<-NA
mex1[is.na(mex.dep.laea1)]<-NA

mex2 <- mex.dep.laea
mex2[p > -7 ]<-NA
mex2[is.na(mex.dep.laea1)]<-NA


qm <- quadmesh::quadmesh(mex.dep.laea)
qm2 <- quadmesh::quadmesh(mex1)
qm2a <- quadmesh::quadmesh(mex2)
qm1 <- quadmesh::quadmesh(mex.dep.laea1-p)

# rgl.clear()
# shade3d(qm)
# rglwidget()
par3d("mouseMode" = c("trackball","polar" ,"zoom" , "pull" ))
rgl.clear()
rgl.spheres(sumaco.laea[1], sumaco.laea[2], 0, r = 15, color = "orange3")
rgl.lines(c(sumaco.laea[1],sumaco.laea[1]), c(sumaco.laea[2],sumaco.laea[2]), c(-200,0), col="orange3", lwd=2)
for (i in 1:length(vo$x)){
  rgl.lines(c(vo$x[i],vo$x[i]), c(vo$y[i],vo$y[i]), c(-100,0), col="yellow", lwd=1, alpha=.5)
}
rgl.spheres(vo$x, vo$y, 0, r = 8, color = "yellow")

wire3d(qm2, col = "grey70",  lit = FALSE, alpha=.6)
wire3d(qm2a, col = "yellow2",  lit = FALSE, alpha=.6)
wire3d(qm1, col = "#D95F02",  lit = FALSE, alpha=.1)
shade3d(qm1, col = "#D95F02", alpha=.5 )
bg3d("grey30")
rgl.spheres(ehb$x, ehb$y, -ehb.slab$depth, r = pmax(ehb.slab$m-3.5,.5)*4, color =  ehb.slab$cols) #colorRampPalette(c( "red",  "white", "blue"))(length(ehb$x)))

