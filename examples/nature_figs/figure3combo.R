cpfun <- function() {
   par(mfrow=c(1,2))
  par(mar = c(5,5,2, 2) , oma=c(3,3,0,0), mgp=c(3,1,0))
  plot(x,y, xlab="Mw", ylab="W",  cex=.2, cex.axis=2,cex.lab=2.2, xlim=c(5.5, 8.8), ylim=c(5,83), col="white")

  #.995 conf
  #polygon( c(newx,newx %>% rev()), c(conf_interval[,2],  conf_interval[,3] %>% rev()), col="grey90", bo="white" )
  #text(6.64, 8, "0.995\nconf.int.", cex=1.1, col="grey60")


  # polygon( i48.poly$x,  i48.poly$y, col="lightblue", bo="blue2" )
  # polygon( i36.poly$x,  i36.poly$y, col="orange", bo="red2" )

  library(car)
  #lines(ellipse::ellipse(cov(s.widths[, 1:2]),   level = 0.5, centre=c(i48.df$x[1], ( i48.df$y[1]))))

  MASS::cov.trob(s.widths[, 1:2]) -> shap
  # car::ellipse(c(i48.df$x[1], ( i48.df$y[1])), shap$cov, rad=.6, col="blue2", lwd=.8, bg="lightblue")
  #  car::ellipse(c(i36.df$x[1], ( i36.df$y[1])), shap$cov, rad=.55, col="red2", lwd=.8)
  # le <- ellipse::ellipse(l1, center =c(i48.poly$x[1], log10( i48.poly$y[1])))
  #   le[,1]+le[,2]*i48.poly$x[1]

  #polygon( imax.poly$x,  imax.poly$y, col="grey70", bo="black" )

  #points(x,y, xlab="Mw", ylab="W",  cex=.2, cex.axis=2,cex.lab=2.5, xlim=c(5.5, 8.8), ylim=c(5,83), add=T)
  points(head(x, -n), head(y, -n),   cex=2.25)
  points(tail(x, n), tail(y, n),    cex=2.25, pch=0 )
  #abline(coeff=coeff,, col="lightblue")
  #error_bars(PMw, PW)
  lines(newx, pred.w.plim$fit[,1], col="grey40", lty=1, lwd=.2)
  lines(newx, pred.w.plim$fit[,1]+pred.w.plim$se.fit*2, col="grey80", lty=5, lwd=.8)
  lines(newx, pred.w.plim$fit[,1]-pred.w.plim$se.fit*2, col="grey80", lty=5, lwd=.8)
  #lines(newx, pred.w.plim[,3], col="black", lty=1)
  # lines(newx, conf_interval[,2], col="blue", lty=3)
  # lines(newx, conf_interval[,3], col="blue", lty=3)

  # lines(newx, conf_interval[,3], col="blue", lty=3)

  #
  # abline(slab.thick/sin(39*pi/180), 0, col="blue2", lty=5)
  # #abline(30/sin(45*pi/180), 0, col="red4", lty=5)
  # abline(slab.thick/sin(55*pi/180), 0, col="red2", lty=5)
  # abline(slab.thick/sin(mmax*pi/180), 0, col="black", lty=5)
  #
  # abline(v=i48.df$x[1],  col="blue2", lty=5)
  # #abline(v=7.75+.35,  col="blue2", lty=5)
  # abline(v=i36.df$x[1],  col="red2", lty=5)
  # abline(v=imax.df$x[1],  col="black", lty=5)

  #abline(v=7.27+.25,  col="red2", lty=5)
  text(6.8, 79, eq, cex=1.7)
  text(6.8, 74, paste("seismogenic core =", slab.thick, "kms"),  cex=1.4)
  text(6.85, n48+12, "N-dipping\nnodal set", cex=1.,col="blue2")
  text(6.35, n36+6, "S-dipping\nnodal set", cex=1.,col="red2")
  text(8.3, n48p-12, "MFS limit\n> slab curv.", cex=1.,col="black")
  text(8.45, 77, #nmax+.4,
       paste0(signif(mmax,2), "° dipping\nnodal plane"), cex=1.,col="maroon")
  text(7.38, 30, "Puebla", cex=1.1)
  text(8.52, 64, "Chiapas", cex=1.1)
  text(8.8, 82, expression(paste("2", sigma)), cex=1.1)

  latex2exp::TeX('$\\alpha  x^\\alpha$, where $\\alpha \\in 1\\ldots 5$')
  e1<-latex2exp::TeX(sprintf("$%.1f^{+%.2f}_{%.2f}$", i48.df[1,1],  i48.df[3,1]-i48.df[1,1],   i48.df[2,1]-i48.df[1,1]  ))
  e2<-latex2exp::TeX(sprintf("$%.1f^{+%.2f}_{%.2f}$", i36.df[1,1],  i36.df[3,1]-i36.df[1,1],   i36.df[2,1]-i36.df[1,1]  ))
  e3<-latex2exp::TeX(sprintf("$%.1f^{+%.2f}_{%.2f}$", imax.df[1,1],  imax.df[3,1]-imax.df[1,1],   imax.df[2,1]-imax.df[1,1]  ))
  e4<-latex2exp::TeX(sprintf("$%.1f^{+%.2f}_{%.2f}$", i48p.df[1,1],  i48p.df[3,1]-i48p.df[1,1],   i48p.df[2,1]-i48p.df[1,1]  ))
  #latex2exp::TeX('$\\alpha  x^\\alpha$, where $\\alpha \\in 1\\ldots 5$')

  # text(i48.df$x[c(3)], i48.df$y[1], round(i48.df$x[c(3)],2), cex=1.2, col="blue2", adj=c(-.15,-.5))
  # text(i36.df$x[c(3)], i36.df$y[1], round(i36.df$x[c(3)],2), cex=1.2, col="red2", adj=c(-.15,-.5))
  # text(i48.df$x[c(2)], i48.df$y[1], round(i48.df$x[c(2)],2), cex=1.2, col="blue2", adj=c(.15,-.5))
  # text(i36.df$x[c(2)], i36.df$y[1], round(i36.df$x[c(2)],2), cex=1.2, col="red2", adj=c(.15,-.5))
  text(6.85, n48+6, #i48.df$x[c(1)]+.1, 30,#i48.df$y[1],
       e1,#round(i48.df$x[c(1)],1),
       cex=1.4, col="blue2")#, adj=c(-.25,-.5))
  text(6.35, n36-1, #i36.df$x[c(1)], 15, #i36.df$y[1],
       e2, #round(i36.df$x[c(1)],1),
       cex=1.4, col="red2")#, adj=c(-.25,-.5))
  text(8.45, 71, #imax.df$x[c(1)], 43, #i36.df$y[1],
       e3, #round(imax.df$x[c(1)],1),
       cex=1.4, col="maroon")#, adj=c(-.25,-.5))
  text(8.3, n48p-18, #imax.df$x[c(1)], 43, #i36.df$y[1],
       e4, #round(imax.df$x[c(1)],1),
       cex=1.4, col="black")#, adj=c(-.25,-.5))
  error_bars(i48.df, offset=c(.06, 1.2), lwd=4, col="blue3")
  error_bars(i36.df, offset=c(.06, 1.2), lwd=4, col="red3")
  error_bars(imax.df, offset=c(.06, 1.2), lwd=2,lty=2, col="maroon")
  error_bars(i48p.df, offset=c(.06, 1.2), lwd=4,  col="black")

  # lines(newx, conf_interval95[,2],
  i48 <- which.min(abs(pred.w.plim$fit[,1]- (pred.w.plim$se.fit*2)-36))
  pred.w.plim$fit[i48,1]
  newx[i48]

  # second pane;

  # need to source
  #source('~/Dropbox/msandifo/documents/programming/r/packages/slab/examples/fig.stereoplot.R')

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
  mfocpoint(np1m[1]-35, np1m[2]-10,  pch=24, cex=0., bg="yellow", lab =paste0("M=",np1m[2],"\n(", np1m.r[1] ,"-",np1m.r[2] ,")"), tcol="blue2", tsize=1.5, adj=c(.5,.5))
  #mfocpoint(np2m[1], np2m[2],  pch=24, cex=3., bg="yellow", lab =paste0(" ", np2m[2],"°=>", np2m[1],"°"), tsize=1.7)

  mfocpoint(np2m[1]-70, np2m[2]-4,  pch=24, cex=0., bg="yellow",
            lab =  paste0("M=",np2m[2],"\n(", np2m.r[1] ,"-",np2m.r[2] ,")"), tcol="red2", tsize=1.5, adj=c(.5,.5))

  mfocpoint(110-90, 42, pch=22, cex=3., bg="yellow", lab =paste0(" Puebla"), tsize=1.5, adj=c(1.,1.4))

  }


library(gridGraphics)

pdf(file = "/Users/msandifo/Dropbox/msandifo/documents/programming/r/packages/slab/examples/nature_figs/figures/fig3combo.pdf", width=15, height=15)
grid.newpage()
pushViewport(viewport(y = 0, height = .5, just = "bottom")) > grid.echo(cpfun, newpage = FALSE, prefix = "cp")
upViewport()
pushViewport(viewport(y = 1, height = .5, x=.04, width=.94, just = c(0,1)))
print(
  tplot + theme(text = element_text(size=28), legend.position = c(.93,.2)) + scale_radius(range=c(6,14))+
        guides(fill= "none", shape="none")+
        labs(y="inferred temperature", x="distance from trench -kms") , newpage = FALSE)
upViewport()
dev.off()

