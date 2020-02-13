
#' Main slab  plotting fucntion
#'
#' @param slab
#' @param slab.col
#' @param col
#' @param out.size
#' @param stretch.ras
#' @param stretch.plan
#' @param alpha.plan
#' @param mgp
#' @param smallplot
#' @param cex
#' @param legcex
#' @param alpha
#' @param contours
#' @param ax.ext
#' @param hf
#' @param ehb
#' @param wsm
#' @param cmt
#' @param wscale
#' @param wlwd
#' @param Pax
#' @param Tax
#' @param depth
#' @param ends
#' @param shade
#' @param plancurv
#' @param trad
#' @param shallow
#' @param dip
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
plot_slab <- function(slab,
                      slab.col =  stretch_brewer(pal = "Spectral"  , stretch =
                                                   .5) ,
                      col= NA,
                      out.size = 1200,
                      stretch.ras = c(.15, .5),
                      stretch.plan =c(.6,.8),
                      alpha.plan=1,
                      mgp = c(.5, 0, 0),
                      smallplot = c(.35, .65, .04, .055),
                      cex = 2,
                      legcex = 2,
                      alpha = .75,
                      contours=c(-10, -30,-70,-(seq(100,600,100))),
                      ax.ext = c(.015, .015),
                      hf = FALSE,
                      ehb = TRUE,
                      wsm=TRUE,
                      cmt=TRUE,
                      wscale=.5,
                      wlwd=2,
                      Pax=T,
                      Tax=F,
                      depth=NA,
                      ends=T,
                      shade=T,
                      plancurv=F,
                      trad=T,
                      shallow=F,
                      dip=T,...){
  full.extent <- extent(slab$slab)
  aspect.ratio <- diff(full.extent[3:4]) / diff(full.extent[1:2])

  wid <- out.size
  hei <- out.size * 1.05
  if (aspect.ratio > 1)
    hei <- hei * aspect.ratio
  else
    wid <- wid / aspect.ratio
  png(
    file = paste0(slab$name, "-wedge.png"),
    width = wid,
    height = hei
  )

  if (is.na(col)) col <- stretch_brewer(stretch=stretch.ras) %>% rev()
  #par(oma=c(2,2,2,2)*0, mar=c(1,1,1,1), mgp=mgp )
  par(mgp = mgp)
  #msPlot(e.ras$topo %>% crop(  full.extent),   col=grey.colors(255) %>% add.alpha(.85))
  msPlot(
    slab$slab$topo.thresh %>% crop(full.extent), # %>% stretch_ras(stretch.ras),
    col= col#   grey.colors(255) #stretch_brewer(stretch=stretch.ras) %>% rev() #col = terrain.colors(255)
  )
  msPlot(
    slab$slab$shade.marine %>% crop(full.extent),
    add = T,
    col = grey.colors(255) %>% add.alpha(1)
  )
  # msPlot(
  #   slab$slab$shade.land %>% crop(full.extent),
  #   add = T,
  #   col = grey.colors(255) %>% add.alpha(.25)
  # )

 # message(print(names(slab$slab)))

#  if (plancurv %in% names(slab$slab)) slab$slab$slab<- slab$slab[[which(slab$slab %>% names() ==plancurv)]]  %>%
    if (plancurv==T )   slab$slab$slab<- slab$slab$depth %>%
    ras_plancurv(foc=25, stretch.plan=stretch.plan) else
      if (dip==T)
      slab$slab$slab<-  abs(slab$slab$dip) else  slab$slab$slab<-  abs(slab$slab$depth)

  msPlot(
    slab$slab$slab,
    axes = F,
    add = T,
    col = slab.col %>% add.alpha(alpha.plan),
    legend = F
  )
  msPlot(slab$slab$shade.land %>% crop(  full.extent), add=T, col=grey.colors(255) %>% add.alpha(.4))
  #  msPlot(slab$slab$shade.marine %>% crop(  full.extent),  add=T, col=grey.colors(255) %>% add.alpha(.2))



  if (hf) {
    message("... hf")
    i70 <- which(slab$hf$depth > -30)
    symbols(
      slab$hf@coords    ,
      circles =  slab$hf$marks.HeatFlow / 5 ,
      inches = 1 / 6,
      ann = F,
      bg = "white",
      fg = "black",
      add = T,
      lwd = 1
    )

    symbols(
      slab$hf@coords[i70, ]    ,
      circles =  slab$hf$marks.HeatFlow[i70] / 1 ,
      inches = 1 / 6,
      ann = F,
      bg = "white",
      fg = "red",
      add = T,
      lwd = 1.5
    )
  }
  if (wsm) {
    message("... wsm")
    plot_wsm(wsm=slab$wsm, limsx=slab$slab, scale=wscale, lwd=wlwd, Tax=F, Pax=F,ends=F, shade=F)
  }
  if (cmt) {
    message("... cmt")
    if (!is.na(depth)) {
      if (length(depth)<2) depth= c(depth, 900)
      slab$cmt <- subset(slab$cmt, depth >=depth[1] & depth <depth[2])
    }
    if (Pax==TRUE)  { slab$cmt$azi <-slab$cmt$pa%%180
    #  plot_wsm(wsm=slab$cmt, limsx=slab$slab, scale=wscale, lwd=wlwd*1.5, col="white")
    plot_wsm(wsm=slab$cmt, limsx=slab$slab, scale=wscale, lwd=wlwd, Pax=T, Tax=F,ends=ends, shade=shade)}
    if (Tax==TRUE)  {slab$cmt$azi <-slab$cmt$ta %% 180
    #  plot_wsm(wsm=slab$cmt, limsx=slab$slab, scale=wscale, lwd=wlwd*1.5, col="white")
    plot_wsm(wsm=slab$cmt, limsx=slab$slab, scale=wscale, lwd=wlwd, lty=1,   Pax=F, Tax=T,ends=ends, shade=shade)}
    #   if (Pax ==FALSE &  Tax==FALSE )  {
    #  #   plot_wsm(wsm=slab$cmt, limsx=slab$slab, scale=wscale, lwd=wlwd*1.5, col="white")
    # #    plot_wsm(wsm=slab$cmt, limsx=slab$slab, scale=wscale, lwd=wlwd,  Pax=F, Tax=F,ends=ends, shade=shade)}
  }


  # print(slab$ehb$depth.anom)
  if (ehb) {
    message("... ehb")

    i70.b <- which(slab$ehb$marks.depth  <= 70)
    if (shallow) points(
      slab$ehb@coords[i70.b, ],
      pch = 20,
      cex = .5,
      ann = F,
      bg = "black" %>% add.alpha(.6),
      fg = "black",
      add = T,
      lwd = 1.5
    )
    i70 <- which(slab$ehb$depth.anom < 0 & slab$ehb$marks.depth > 70)
    points(
      slab$ehb@coords[i70, ],
      pch = 24,
      cex = 1.4,
      ann = F,
      bg = "white",
      fg = "red",
      add = T,
      lwd = 1.5
    )
    i70.1 <-
      which(slab$ehb$depth.anom >= 0 & slab$ehb$marks.depth > 70)
    points(
      slab$ehb@coords[i70.1, ],
      pch = 25,
      cex = 1.4,
      ann = F,
      bg = "red" %>% add.alpha(.6),
      fg = "black",
      add = T,
      lwd = 1.5
    )

  }

  plotvo(
    limsx = extent(slab$slab)[1:2],
    limsy = extent(slab$slab)[3:4],
    cex = 3,
    pch = 24,
    col = "grey30",
    bg = "Yellow",
    to = NA,
    lwd = 1.5, ppp=F
  )
  contour(
    slab$slab$depth,
    levels = contours,
    add = T,
    labcex = 1
  )

  if (!trad)  plot_axes (
    slab$slab$shade.marine %>% crop(full.extent),
    add = T,
    ax.ext = ax.ext,
    cex = cex,
    mgp = mgp,
    major = .02,
    minor = .008 ,
    mgp = c(2, .6, 0)
  ) else
    plot_axes_trad(slab$slab$shade.marine, proj=NA,
                   cex=1.5,
                   grid=T, add=T ,
                   width=2,
                   axis.width=.007, stringc=T,...)


  raster::plot(
    slab$slab$slab,
    horizontal = TRUE,
    smallplot = smallplot,
    col = slab.col,
    legend.only = TRUE,
    axis.args = list(cex.axis = legcex, mfp = c(0, 1, 0)),
    legend.args = list(
      text = NULL,
      #" Slab 1.0 dip\n",
      cex = legcex  ,
      mgp = c(0, 1, 0)
    )
  )
  dev.off()
  system(paste0("open ", slab$name, "-wedge.png"))

}



#' Plot world stree map data
#'
#' @param wsm
#' @param scale
#' @param lines
#' @param limsx
#' @param limsy
#' @param lwd
#' @param lty
#' @param shade
#' @param Tax
#' @param Pax
#' @param ends
#'
#' @return
#' @export
#'
#' @examples
plot_wsm <-
  function(wsm = NA,
           scale = 1,
           lines = T,
           limsx = c(-180, 180),
           limsy = c(-90,90),
           lwd=1,
           lty=1,
           shade=TRUE,
           Tax=F,
           Pax=F,
           ends=TRUE) {
    if (is.ras(limsx)) {
      extent.lim <- extent(limsx)
      limsy <-  extent.lim[3:4]
      limsx <- extent.lim[1:2]
    }

    message("class wsm is ",class(wsm))
    print(wsm$t)
    print(wsm$p)
    if (is.na(wsm)){  wsm <- read_wsm()
    }



    if (class(wsm) != 'ppp')
      #wsm <- wsm_ppp(wsm, limsx = limsx, limsy = limsy, Tax=F, Pax=F)  # convert to ppp
    wsm %>% projNA()%>% as.ppp() -> wsm

    if (lines == T) {
      ismp <- wsm_lines(wsm, scale = scale, Tax=Tax, Pax=Pax)
      message ("plot_wsm ",names(ismp))
      regimes <- c('NF', 'NS', 'SS', 'TF', 'TS', 'U')
      for (reg in regimes) {
        ismp1 <- subset(ismp, regime == reg)
        if (reg == 'NF' | reg == 'NS')
          col <- "Red2"
        if (reg == 'SS')
          col <- "DarkGreen"
        if (reg == 'TF' | reg == 'TS')
          col <- "Blue2"
        if (reg == 'U')
          col <- "Black"
        if (shade) lines(ismp1$x, ismp1$y, col=add.alpha("white",.6), lwd=lwd*2 )


        if (Pax & ends)  points(ismp1$x[ismp1$z==1], ismp1$y[ismp1$z==1], col = col, cex= .66*lwd, pch=0)
        if (Tax & ends)  points(ismp1$x[ismp1$z==1], ismp1$y[ismp1$z==1], col = col, cex= .66*lwd, pch=1)
        lines(ismp1$x, ismp1$y, col = col, lwd = lwd, lty=lty)
        #		points(wsm$LON, wsm$LAT, pch=16,cex=.3, col="WHITE")
      }
    } else {
      points(wsm$x, wsm$y)
    }
  }



plotvo <- function(...)
  plot_vo(...)

#' Plot volcanoes
#'
#' @param vo
#' @param limsx
#' @param limsy
#' @param cex
#' @param col
#' @param pch
#' @param bg
#' @param to
#' @param lwd
#' @param doplot
#' @param ppp
#'
#' @return
#' @export
#'
#' @examples
plot_vo <- function(vo = NA,
                    limsx = c(-180, 180),
                    #can be a raster, extent object or 2-length vector
                    limsy = c(-90, 90),
                    #only used if limsx is a 2-length vec
                    cex = 1,
                    col = "BLACK",
                    pch = 24,
                    bg = "yellow",
                    to = NA,
                    lwd = 2,
                    doplot = TRUE, ppp=T) {
  # if (is.raster(limsx)) {limsx<- limsx(extent(raster)[1:2]); limsy<- limsx(extent(raster)[3:4])}
  # if (length(limsx) >= 4 ) { limsy=limsx[3:4]; limsx=limsx[1:2]}
  if (!is.ppp(vo))
    v <- read_vo(limsx = limsx,
                 limsy = limsy,
                 to = to, ppp=ppp)
  if (!is.ppp(vo))
    points(
      v$long, v$lat,
      pch = pch,
      cex = cex,
      col = col,
      bg = bg,
      lwd = lwd
    ) else points(
      v ,
      pch = pch,
      cex = cex,
      col = col,
      bg = bg,
      lwd = lwd
    )

}


#' plot earthquake data kernel
#'
#' @param df
#' @param cuts
#' @param min
#' @param xlim
#' @param ylim
#' @param position
#' @param alpha
#' @param bw
#' @param stroke
#' @param kernel
#' @param legend.position
#' @param show.leg
#' @param points
#' @param mscale
#' @param mlimit
#'
#' @return
#' @export
#'
#' @examples
plot_kernel <- function(df, cuts= c(3.5, 6.4,10), min=0,  xlim=c(0,400),
                        ylim=c(0,0.016), position=NA , alpha=.3, bw="SJ",
                        stroke=0.35,
                        kernel="triangular",
                        legend.position=c(.9, .75),
                        show.leg=T, points=F, mscale=2, mlimit=4  ){
  df$m <-pmax(df$ms, df$mb )
  df <- df %>% subset(m >min)
  df$Mw <-  cut(df$m, cuts)

  p1 <- ggplot()+
    hrbrthemes::theme_ipsum(axis = TRUE, ticks = F,grid=c("XY"),axis_text_size=14, axis_title_size=18)+
    theme(panel.grid.minor=element_blank() )+
    geom_density(data=df, aes(boundary.distance ), fill="white", linetype=2, size=.3, show.legend = F, bw=bw, kernel=kernel)+
    theme(legend.position = legend.position ) +
    labs(x="distance - kms", y="kernel density estimate")
    # geom_vline( xintercept=c(100,300), size=.2, linetype=2)+


  if (points) p1<-p1+
    geom_point(data=df, aes(x= boundary.distance, y = (m-mlimit)/(1000/mscale), size=depth ), show.legend=T, shape=1, stroke=stroke)+
    scale_radius(range=c(1,10)) + scale_y_continuous(sec.axis = sec_axis(~.*(1000/mscale) +mlimit, name="Mw"), expand = c(0,0),limits=ylim) else
      p1 <- p1+  scale_x_continuous(expand = c(0,0), limits=xlim)

  if (is.na(position)) p1+ geom_density(data=df %>% subset(!is.na(Mw)), aes(boundary.distance , fill=Mw),alpha=alpha , bw=bw) else
    p1+ geom_density(data=df %>% subset(!is.na(Mw)), aes(boundary.distance ,fill=Mw), position = position, alpha=alpha, show.legend = F, kernel=kernel)
}

project_kernel1 <- function(...){
  project_kernel(...)
}

#' Title
#'
#' @param df
#' @param cuts
#' @param min
#' @param xlim
#' @param ylim
#' @param position
#' @param alpha
#' @param bw
#' @param stroke
#' @param size
#' @param kernel
#' @param legend.position
#' @param show.leg
#' @param points
#' @param mscale
#' @param mlimit
#' @param regime
#'
#' @return
#' @export
#'
#' @examples
plot_kernel_old <- function(df, cuts= c(3.5, 6.4,10), min=0,  xlim=c(0,400),
                        ylim=c(0,0.016), position=NA , alpha=.3, bw="SJ",stroke=.35,size=.5,
                        kernel="triangular", legend.position=c(.9, .75), show.leg=T, points=F, mscale=2, mlimit=4, regime=F){

  if (length(size==1)) size= c(size, size*.66)
  df$m <-pmax(df$ms, df$mb )
  df <- df %>% subset(m >min)
  df$Mw <-  cut(df$m, cuts)

  p1 <- ggplot()+
    hrbrthemes::theme_ipsum(axis = TRUE, ticks = F,grid=c("XY"),axis_text_size=14, axis_title_size=18)+
    theme(panel.grid.minor=element_blank() )+
    geom_density(data=df, aes( distance ), fill="white", linetype=2, size=size[2], show.legend = F, bw=bw, kernel=kernel)+
    theme(legend.position = legend.position ) +
    labs(x="distance - kms", y="kernel density estimate")
  # geom_vline( xintercept=c(100,300), size=.2, linetype=2)+


  if (points) p1<-p1+geom_point(data=df, aes(x=  distance, y = (m-mlimit)/(1000/mscale), size=depth, colour=regime ), show.legend=T, shape=1, stroke=stroke)+
    scale_radius(range=c(1,10)) + scale_y_continuous(sec.axis = sec_axis(~.*(1000/mscale) +mlimit, name="Mw"), expand = c(0,0),limits=ylim)+  scale_x_continuous(expand = c(0,0), limits=xlim) else
      p1 <- p1+  scale_x_continuous(expand = c(0,0), limits=xlim)

  if (is.na(position)) p1+ geom_density(data=df %>% subset(!is.na(Mw)), aes( distance , fill=Mw),alpha=alpha , size=size[1], bw=bw) else
    p1+ geom_density(data=df %>% subset(!is.na(Mw)), aes( distance ,fill=Mw), position = position, alpha=alpha, show.legend = F, kernel=kernel)
}


