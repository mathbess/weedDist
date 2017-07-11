################################################
## Distribution and form of each dispersal curve
##
## first version: July 10 2017
## last modification: July 10 2017
## Willian Vieira
##
## R version 3.4.0 (2017-04-21)
## Platform: x86_64-apple-darwin15.6.0
## Running under: OS X 10.12.5 (Sierra)
################################################

#Curves equation
Weibull = function(x, m, c) {
  b = (c - 1) / (c * m ^ c)
  z = b * c * abs(x) ^ (c - 1) * exp(-b * abs(x) ^ c)
  return(z)
}

Gaussian = function(x, sigma) {
  z = (1 / (sigma * sqrt(2 * pi))) * exp(-(x ^ 2) / (2 * sigma ^ 2))
  return(z)
}

Exponential = function(x, lambda) {
  z = 1 / (2 * pi * (lambda ^ 2)) * exp(-abs(x) / lambda)
  return(z)
}

f2dt <- function(x, a, b) {
  z = ((b - 1) / (pi * a * a)) * (1 + ((x * x) / (a * a))) ^ (-b)
  return(z)
}

#plot
library(graphicsutils)
x <- seq(-50, 50, 0.1)

#Gaussian
pdf("./figure/gaussian.pdf", width = 5, height = 3)
par(mar = c(0, 0, 0, 0))
plot0(c(0, 1000),c(0, .033))
points(Gaussian(x = x, sigma = 12), type = "l", lwd = 6.5)
dev.off()

#Weibull
pdf("./figure/weibull.pdf", width = 5, height = 3)
par(mar = c(0, 0, 0, 0))
plot0(c(0, 1000),c(0, .11))
points(Weibull(x = x, m = 5, c = 1.9), type = "l", lwd = 6.5)
dev.off()

#Exponential
pdf("./figure/exponential.pdf", width = 5, height = 3)
par(mar = c(0, 0, 0, 0))
plot0(c(0, 1000),c(0, .00155))
points(Exponential(x = x, lambda = 10), type = "l", lwd = 6.5)
dev.off()

#2Dt
pdf("./figure/2dt.pdf", width = 5, height = 3)
par(mar = c(0, 0, 0, 0))
plot0(c(0, 1000),c(0, .0023))
points(f2dt(x = x, a = 6.5, b = 1.3), type = "l", lwd = 6.5)
dev.off()
