
s.widths<-read_csv(fpath, skip=1)
#s.widths$W <-
s.widths$W<- round(s.widths$W/2.)*2.
lm(formula = Mw ~  log10(W), data = s.widths) -> l1
lm(formula =    log10(W) ~Mw, data = s.widths) -> l2

coeff=coefficients(l1) %T>% glimpse()


rdip= c(20,90,1)
dip = seq(rdip[1],rdip[2], rdip[3])
rz= c(10,70,1)
z= seq(rz[1],rz[2],rz[3])
a.dip = array( rep(dip, each=length(z))  , dim=c(length(z), length(dip))) %T>% glimpse()
a.z = array( rep(rev(z),  length(dip)) , dim=c(length(z), length(dip))) %T>% glimpse()
  w <-  a.z/(a.dip*pi/180)

Mw= raster( log10(w)*coeff[2]+coeff[1],  xmn=rdip[1], xmx=rdip[2], ymn=rz[1], ymx=rz[2])
wr<- raster(w,  xmn=rdip[1], xmx=rdip[2], ymn=rz[1], ymx=rz[2])
  plot( wr)
 contour( Mw, levels=seq(5,9, .25), add=T)
 RStoolbox::ggR(Mw, ggObj=F) -> Mw.df
 RStoolbox::ggR(wr, ggObj=F) -> wr.df

 RStoolbox::ggR(Mw,  geom_raster = TRUE)+
   scale_fill_gradient2(high="red" ,low="blue", mid="white", midpoint=7.5) +
   geom_contour(data=Mw.df, aes(x,y, z= layer), binwidth=.5)

 #  geom_contour(data=wr.df, aes(x,y, z= layer), binwidth=10, col="red")+
   labs(x="dip -  degrees", y="z - kms")


 scale_fill_gradientn(name = "mojo", colours = rainbow(10))predict(l1, data.frame(W=c(n36,n48,nmax, n48p)),  se.fit=T, scale=NULL, interval="prediction", level=.995) %T>% glimpse()-> pW

eq = paste0("W = ",round(coeff[1],2), " + ", round(coeff[2],2), " x log10(Mw) ")
x=s.widths$Mw
y=s.widths$W
lm.out <- lm(y ~ log10(x))
newx = seq(5.,9.5,by = 0.001)


predict(l1, data.frame(W=c(n36,n48,nmax, n48p)),  se.fit=T, scale=NULL, interval="prediction", level=.995) %T>% glimpse()-> pW


pred.w.plim <- predict(lm.out, newdata=data.frame(x=newx ), se.fit=T,interval = "prediction")

n.conf <- .995
conf_interval <- predict(lm.out, newdata=data.frame(x=newx ), interval="confidence",
                         level = n.conf)
# conf_interval9 <- predict(lm.out, newdata=data.frame(x=newx ), interval="confidence",
#                           level = 0.9)
slab.thick <- 24
n36 <-slab.thick/sin(55*pi/180)
n48 <-slab.thick/sin(39*pi/180)
n48p <- (12+slab.thick)/sin(42*pi/180)
mmax<- 24.35
amax <-24
nmax<- slab.thick/sin(mmax*pi/180)

predict(l1, data.frame(W=c(n36,n48,nmax, n48p)),  se.fit=T, scale=NULL, interval="prediction", level=.995) %T>% glimpse()-> pW

predict(l2, data.frame(Mw=c(7.09, 7.51,8.2)),  se.fit=T, interval="confidence", level=.995) %T>% glimpse()-> pMw
