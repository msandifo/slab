# require(stats); require(graphics)
# bSpline(women$height, df = 5)
# bs(women$height, df = 5)
# summary(fm1 <- lm(women$weight ~ bSpline( women$height , knots = 5, degree = 2) ))
# summary(fm2 <- lm(weight ~ bs(height, df = 5), data = women))
#
# ## example of safe prediction
# plot(women, xlab = "Height (in)", ylab = "Weight (lb)")
# ht <- seq(57, 73, length.out = 200)
# ht1 <- seq(57, 73, length.out = 15)
# lines(ht1, predict(fm1, data.frame(height = ht)))
# lines(ht, predict(fm2, data.frame(height = ht)), col="red")


mex.slab.pro <- rbind( data.frame(x=c(0, 10, 55, 110, 150,   200,  260, 400), y= -c(0, 1,12, 38,43, 47,50,   200), profile="p1"),
                       data.frame(x=c(0, 15, 55, 110, 150,   290,  330, 360), y= -c(5, 7,16, 38,43, 45,62,   75), profile="p2"),
                       data.frame(x=c(0, 10, 55, 120, 170,   250,  300, 550), y= -c(0, 1,12, 35,55, 100, 130, 300), profile="p3"),
                       data.frame(x=c(0, 10, 55, 110, 150,   200,  290, 360)*1.1, y= -c(0, 2,18, 50,80, 130, 230, 300), profile="p4"))


library(splines)
x <- mex.slab.pro$x[mex.slab.pro$profile=="p4"]
y <- mex.slab.pro$y[mex.slab.pro$profile=="p4"]


mod1 <-read.csv("/Users/msandifo/Dropbox/msandifo/documents/programming/r/packages/slab/examples/slabTop.csv", col.names=c("x", "depth")) %>%
  subset(x>= -750) %>%
  mutate(x= x+600, depth = -depth)

mod1a <- mod1 %>% mutate(x=x*.9, y=depth*.9) %>% dplyr::select(c("x", "y")) %>% subset(x<400)

x <- mod1a$x
y <- mod1a$y

n=101
sg.length=3
sn<-spline(x,y, n=n, method="natural" )
f_of_x <- splinefun(x,y )

sn1<-spline(sn$x, f_of_x(sn$x, deriv = 1), n=n ) %>% smooth.spline()
sn1a<-spline(sn$x, f_of_x(sn$x, deriv = 1), n=n )

ff_of_x  <- splinefun(sn1$x, f_of_x(sn1$x, deriv = 1) )
fff_of_x  <- splinefun(sn2$x, f_of_x(sn2$x, deriv = 1) )





#sn2<-spline(sn1$x, ff_of_x(sn1$x, deriv = 1), n=n/4)
# sn1$y<- pracma::savgol(y, sg.length, dorder=1, forder = 2)
# sn1$x<- x
sn2$y<- pracma::savgol(sn1$y, sg.length, dorder=1, forder = 4) # %>% smooth.spline()
sn2$x<- sn1$x
sn3<-spline(sn2$y, fff_of_x(sn2$y, deriv = 2), n=n*4 )
sn3$y<- pracma::savgol(sn1$y, sg.length, dorder=2, forder = 4)

#sn3$y <- mdatools::prep.savgol(sn2$y %>% as.matrix(ncol=1), 5, dorder=1, porder = 1)

sn3$x<- sn1$x

sn2$x <- head(sn2$x , -sg.length) %>% tail(-sg.length)
sn2$y <- head(sn2$y , -sg.length) %>% tail(-sg.length)

sn3$x <- head(sn3$x , -sg.length) %>% tail(-sg.length)
sn3$y <- head(sn3$y , -sg.length) %>% tail(-sg.length)
# plot(sn2$x,f_of_x(sn2$x, deriv = 1)/max(abs(f_of_x(sn2$x, deriv = 1))))
# lines(sn2$x,f_of_x(sn2$x, deriv = 1)/max(abs(f_of_x(sn2$x, deriv = 1))), ylim=c(-1,1))
plot(sn1$x, sn1$y/max(abs(sn1$y)), ylim=c(-1,1))
lines(sn1$x, sn1$y/max(abs(sn1$y)), ylim=c(-1,1))
lines(sn1$x, sn1$y/max(abs(sn1$y)))
#points(sn$x, sn$y/max(abs(sn$y)), col="red", add=T)
lines(sn$x, sn$y/max(abs(sn$y)), col="red")

lines(sn2$x, sn2$y/max(abs(sn2$y)), col="blue")
#points(sn2$x, sn2$y/max(abs(sn2$y)), col="blue")

lines(sn3$x, sn3$y/max(abs(sn3$y)), col="darkgreen")
#points(sn3$x, sn3$y/max(abs(sn3$y)), col="darkgreen")

lines(sn3$x, abs( sn3$y/max(abs(sn3$y))), col="darkgreen", lty=2)
#points(sn3$x, abs(sn3$y/max(abs(sn3$y))), col="darkgreen")

