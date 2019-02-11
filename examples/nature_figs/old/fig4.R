#  figure 4
#
fpath <-"/Users/msandifo/Dropbox/msandifo/documents/programming/r/2018/geophys/faultscaling/data/strasser_.csv"

s.widths<-read_csv(fpath, skip=1)
lm(formula = Mw ~  log10(W), data = s.widths) -> l1
coeff=coefficients(l1)
eq = paste0("W = ",round(coeff[1],2), " + ", round(coeff[2],2), " x log10(Mw) ")
x=s.widths$Mw
y=s.widths$W
lm.out <- lm(y ~ log10(x))
newx = seq(5.,9.5,by = 0.001)
pred.w.plim <- predict(lm.out, newdata=data.frame(x=newx ), se.fit=T,interval = "prediction")

n.conf <- .995
conf_interval <- predict(lm.out, newdata=data.frame(x=newx ), interval="confidence",
                         level = n.conf)
# conf_interval9 <- predict(lm.out, newdata=data.frame(x=newx ), interval="confidence",
#                           level = 0.9)

n36 <-30/sin(55*pi/180)
n48 <-30/sin(39*pi/180)
i36 <- c( which.min(abs(pred.w.plim$fit[,1]-n36)),
                    which.min(abs(pred.w.plim$fit[,1]+ (pred.w.plim$se.fit*2)-n36)),
          which.min(abs(pred.w.plim$fit[,1]- (pred.w.plim$se.fit*2)-n36)))
i36.df <- data.frame( x=newx[i36], y=pred.w.plim$fit[i36,1])
i36.poly <- data.frame(x=i36.df$x[c(1,2,2,1,1)+1], y=i36.df$y[c(1,1,2,2,1)+1] )


i48 <- c( which.min(abs(pred.w.plim$fit[,1]-n48)),
          which.min(abs(pred.w.plim$fit[,1]+ (pred.w.plim$se.fit*2)-n48)),
          which.min(abs(pred.w.plim$fit[,1]- (pred.w.plim$se.fit*2)-n48)))
i48.df <- data.frame( x=newx[i48], y=pred.w.plim$fit[i48,1])
i48.poly <- data.frame(x=i48.df$x[c(1,2,2,1,1)+1], y=i48.df$y[c(1,1,2,2,1)+1] )


n <- 5
#
pdf(file = "fig4.pdf", width=20, height=10)
par(mfrow=c(1,2))
par(mar = c(5,5,2, 2) , oma=c(3,3,0,0), mgp=c(3,1,0))
plot(x,y, xlab="Mw", ylab="W",  cex=.2, cex.axis=2,cex.lab=2.2, xlim=c(5.5, 8.8), ylim=c(5,83), col="white")
polygon( c(newx,newx %>% rev()), c(conf_interval[,2],  conf_interval[,3] %>% rev()), col="grey90", bo="white" )
polygon( i48.poly$x,  i48.poly$y, col="lightblue", bo="blue2" )
polygon( i36.poly$x,  i36.poly$y, col="orange", bo="red2" )

#points(x,y, xlab="Mw", ylab="W",  cex=.2, cex.axis=2,cex.lab=2.5, xlim=c(5.5, 8.8), ylim=c(5,83), add=T)
points(head(x, -n), head(y, -n),   cex=3)
points(tail(x, n), tail(y, n),    cex=3, pch=0 )
#abline(coeff=coeff,, col="lightblue")
lines(newx, pred.w.plim$fit[,1], col="grey60", lty=1, lwd=.2)
lines(newx, pred.w.plim$fit[,1]+pred.w.plim$se.fit*2, col="grey30", lty=5, lwd=.4)
lines(newx, pred.w.plim$fit[,1]-pred.w.plim$se.fit*2, col="grey30", lty=5, lwd=.4)
#lines(newx, pred.w.plim[,3], col="black", lty=1)
# lines(newx, conf_interval[,2], col="blue", lty=3)
# lines(newx, conf_interval[,3], col="blue", lty=3)

# lines(newx, conf_interval[,3], col="blue", lty=3)


abline(30/sin(39*pi/180), 0, col="blue2", lty=5)
#abline(30/sin(45*pi/180), 0, col="red4", lty=5)
abline(30/sin(55*pi/180), 0, col="red2", lty=5)

abline(v=i48.df$x[1],  col="blue2", lty=5)
#abline(v=7.75+.35,  col="blue2", lty=5)
abline(v=i36.df$x[1],  col="red2", lty=5)
#abline(v=7.27+.25,  col="red2", lty=5)
text(6.4, 79, eq, cex=2.)
text(5.9, 49, "MFS N-dipping nodal set", cex=1.2,col="blue2")
text(5.9, 38, "MFS S-dipping nodal set", cex=1.2,col="red2")
text(7.3, 28, "Puebla", cex=1.2)
text(8.4, 68, "Chaipas", cex=1.2)
text(8.8, 82, expression(paste("2", sigma)), cex=.8)
text(8.8, 54, "0.995\nconf.", cex=.8, col="grey60")
text(i48.df$x, i48.df$y[1], round(i48.df$x,2), cex=.7, col="blue2", adj=c(-.15,-.5))
text(i36.df$x, i36.df$y[1], round(i36.df$x,2), cex=.7, col="red2", adj=c(-.15,-.5))

# lines(newx, conf_interval95[,2],
i48 <- which.min(abs(pred.w.plim$fit[,1]- (pred.w.plim$se.fit*2)-36))
pred.w.plim$fit[i48,1]
newx[i48]

# secon pane;

image(KT$x, KT$y, KT$z, add=F, col=c(rep('#ffffff',22),terrain.colors(190) %>% rev))
contour(KT$x, KT$y, KT$z, add=T, col="grey80", lwd=1, nlevels=8)
points(TZZ$x, TZZ$y, cex=3, pch=2)
points(PZZ$x, PZZ$y, cex=3, pch=4, col="black")
contour(NP2$x, NP2$y, NP2$z, add=T, col="red", lwd=1, nlevels=4)
contour(NP1$x, NP1$y, NP1$z, add=T, col="blue2", lwd=1, nlevels=4)
net(add=1, col="grey70", border="white", lwd = .2)
GEOmap::antipolygon(CC$x,CC$y,col="white")
pcirc(gcol = "grey50", border = "black",   ndiv = 360)
text(0,1.05, "N", xpd=NA, cex=1.8)
text(0,-1.05, "S", xpd=NA, cex=1.8)
text( -1.05,0, "W", xpd=NA, cex=1.8)
text( 1.05,0, "E", xpd=NA, cex=1.8)

np2m <- c(
  (cmt1.ppp[p2]$marks$np2strike[cmt1.ppp[p2]$marks$distance > 150 ]) %>% as.double() %>% median() +90,
  cmt1.ppp[p2]$marks$np2dip[cmt1.ppp[p2]$marks$distance > 150 ] %>% as.double() %>%median(),
  cmt1.ppp[p2]$marks$np2dip[cmt1.ppp[p2]$marks$distance > 150 ] %>% as.double() %>%sd())


np1m <-c(
  (cmt1.ppp[p2]$marks$np1strike[cmt1.ppp[p2]$marks$distance > 150 ] %>% median() +90)%%360,
  (cmt1.ppp[p2]$marks$np1dip[cmt1.ppp[p2]$marks$distance > 150 ]) %>% median(),
  (cmt1.ppp[p2]$marks$np1dip[cmt1.ppp[p2]$marks$distance > 150 ]) %>% sd())

np2m.r <- c(min(cmt1.ppp[p2]$marks$np2dip[cmt1.ppp[p2]$marks$distance > 150 ] ), max(cmt1.ppp[p2]$marks$np2dip[cmt1.ppp[p2]$marks$distance > 150 ]  ))
np1m.r <- c(min(cmt1.ppp[p2]$marks$np1dip[cmt1.ppp[p2]$marks$distance > 150 ] ), max(cmt1.ppp[p2]$marks$np1dip[cmt1.ppp[p2]$marks$distance > 150 ]  ))



#mfocpoint(np1m[1], np1m[2],  pch=24, cex=3., bg="yellow", lab =paste0(" ", np1m[2],"°=>", np1m[1],"°"), tsize=0)
mfocpoint(np1m[1]-30, np1m[2]-10,  pch=24, cex=0., bg="yellow", lab =paste0("M(np1)=",np1m[2],"\n(", np1m.r[1] ,"-",np1m.r[2] ,")"), tcol="blue2", tsize=1.5, adj=c(.5,.5))
#mfocpoint(np2m[1], np2m[2],  pch=24, cex=3., bg="yellow", lab =paste0(" ", np2m[2],"°=>", np2m[1],"°"), tsize=1.7)

mfocpoint(np2m[1]-70, np2m[2]-3,  pch=24, cex=0., bg="yellow", lab =  paste0("M(np2)=",np2m[2],"\n(", np2m.r[1] ,"-",np2m.r[2] ,")"), tcol="red2", tsize=1.5, adj=c(.5,.5))

mfocpoint(110-90, 42, pch=22, cex=3., bg="yellow", lab =paste0(" Puebla"), tsize=1.25, adj=c(-.1,1))
dev.off()

par(mfrow=c(1,1))

