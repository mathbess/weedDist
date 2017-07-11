myWD <- '/Users/wvieira/Dropbox/Cursos/INRA - Agroecologie/'

#Test of parameters before applying the filter
{
#####################
##     Weibull     ##
#####################

Weibull = function(x, m, c) {
  b = (c - 1) / (c * m ^ c)
  z = b * c * abs(x) ^ (c - 1) * exp(-b * abs(x) ^ c)
  return(z)
}

#m
x = seq(from = 0, to = 300, by = 0.2)
listm = c(2, 6, 10, 14, 18)
plot(
  0,
  xlim = c(0, 30),
  ylim = c(0, 0.04),
  xlab = 'Distance (m)',
  ylab = 'z',
  pch = "",
  main = "Weibull - c = 1.5"
)
for (k in 1:5) {
  z = Weibull(x, m = listm[k], c = 1.5)
  z[!is.finite(z)] <- 0
  if (sum(z) != 0)
    z = z / sum(z)
  lines(x, z, col = k)
}
legend(
  'topright',
  legend = listm,
  lty = 1,
  col = 1:5,
  cex = .95
)

#c
x = seq(from = 0, to = 50, by = 0.2)
listc = c(0.2, 0.7, 1.2, 1.7, 2.2, 2.7)
plot(
  0,
  xlim = c(0, 10),
  ylim = c(0, 0.13),
  xlab = 'Distance (m)',
  ylab = 'z',
  pch = "",
  main = "Weibull - m = 4.5"
)
for (k in 1:length(listc)) {
  z = Weibull(x, m = 4, c = listc[k])
  z[!is.finite(z)] <- 0
  if (sum(z) != 0)
    z = z / sum(z)
  lines(x, z, col = k)
}
legend(
  'topright',
  legend = listc,
  lty = 1,
  col = 1:length(listc),
  cex = .95
)

{
  #Finding Bound
  x = seq(from = 0, to = 300, by = 0.2)
  listm = seq(2, 20, 0.1)
  listc = seq(1.2, 2, 0.1)
  nb = length(listm) * length(listc)
  dat = data.frame(matrix(NA, nrow = nb, ncol = 3))
  names(dat) = c('m', 'c', 'max')
  dat$m = listm
  dat$c = rep(listc, each = length(listm))
  for (k in 1:nb) {
    z = Weibull(x, m = dat$m[k], c = dat$c[k])
    z[!is.finite(z)] <- 0
    if (sum(z) != 0)
      z = z / sum(z)
    z = round(z, 4)
    x0 = min(x[z == 0 & x != 0])
    dat$max[k] = x0
  }
  
  #regression
  reg <- lm(dat$max ~ m + c, data = dat)
  summary(reg)
  
  #test function
  #values come from regression summary where the first three values are the Estimate
  #and the last value is the max residual value, with this one we are sure the last seed will be in the area
  ff <- function(m, c) {
    z = 6.5820 * m - (147.7906 * c) + 244.9198 + 97.789
    return(z)
  }
  dat[which(dat$m == 10 & dat$c == 1.2), ]
  ff(10, 1.2)
}

{
  #plot (have to add ~100m for safety)
  ab = summary(reg)
  bound <- data.frame(predict(reg, int = 'c'))
  or <- order(bound$fit)
  plot(dat$max[or],
       ylab = 'Maximum dispersal',
       pch = '',
       ylim = c(min(bound), max(dat$max)))
  points(dat$max[or], pch = 16, cex = 0.5)
  lines(bound$fit[or], col = "red", lwd = 2)
  lines(bound$lwr[or], lty = 3, col = "blue")
  lines(bound$upr[or], lty = 3, col = "blue")
  legend(
    "topleft",
    c("observation", "fitted curve", "bound"),
    cex = 1.2,
    col = c('black', 'red', 'blue'),
    lty = 1:3,
    lwd = 2,
    bty = "n"
  )
  r2 = round(ab$adj.r.squared, digits = 2)
  legend('bottomright', paste('R2 = ', r2, sep = ""))
}

#####################
##     Weibull  2  ##
#####################

Weibull2 = function(x, m, c) {
  z = (c / m) * ((x / m) ^ (c - 1)) * exp(-(x / m) ^ c)
  return(z)
}
#m
x = seq(from = 0, to = 300, by = 0.2)
listm = c(2, 6, 10, 14, 18)
plot(
  0,
  xlim = c(0, 20),
  ylim = c(0, 0.04),
  xlab = 'Distance (m)',
  ylab = 'z',
  pch = "",
  main = "Weibull - c = 1.5"
)
for (k in 1:5) {
  z = Weibull2(x, m = 1.2, c = 0.8)
  z[!is.finite(z)] <- 0
  if (sum(z) != 0)
    z = z / sum(z)
  lines(x, z, col = 1)
}
legend(
  'topright',
  legend = listm,
  lty = 1,
  col = 1:9,
  cex = .95
)

#c
x = seq(from = 0, to = 50, by = 0.2)
listc = c(0.2, 0.7, 1.2, 1.7, 2.2, 2.7)
plot(
  0,
  xlim = c(0, 10),
  ylim = c(0, 0.13),
  xlab = 'Distance (m)',
  ylab = 'z',
  pch = "",
  main = "Weibull - m = 4.5"
)
for (k in 1:length(listc)) {
  z = Weibull2(x, m = 4, c = listc[k])
  z[!is.finite(z)] <- 0
  if (sum(z) != 0)
    z = z / sum(z)
  lines(x, z, col = k)
}
legend(
  'topright',
  legend = listc,
  lty = 1,
  col = 1:length(listc),
  cex = .95
)


{
  #Finding Bound
  x = seq(from = 0, to = 300, by = 0.2)
  listm = seq(0.1, 5, 0.1)
  listc = seq(0.2, 2, 0.1)
  nb = length(listm) * length(listc)
  dat = data.frame(matrix(NA, nrow = nb, ncol = 3))
  names(dat) = c('m', 'c', 'max')
  dat$m = listm
  dat$c = rep(listc, each = length(listm))
  for (k in 1:nb) {
    z = Weibull2(x, m = dat$m[k], c = dat$c[k])
    z[!is.finite(z)] <- 0
    if (sum(z) != 0)
      z = z / sum(z)
    z = round(z, 4)
    x0 = min(x[z == 0 & x != 0])
    dat$max[k] = x0
  }
  
  #regression
  reg <- lm(dat$max ~ m + c, data = dat)
  summary(reg)
  
  #test function
  #values come from regression summary where the first three values are the Estimate
  #and the last value is the max residual value, with this one we are sure the last seed will be in the area
  ff <- function(m, c) {
    z = 4.7221 * m - (71.8989 * c) + 101.5627 + 79.345
    return(z)
  }
  dat[which(dat$m == 20 & dat$c == 0.5), ]
  ff(20, .5)
}

{
  #plot (have to add ~100m for safety)
  ab = summary(reg)
  bound <- data.frame(predict(reg, int = 'c'))
  or <- order(bound$fit)
  plot(dat$max[or],
       ylab = 'Maximum dispersal',
       pch = '',
       ylim = c(min(bound), max(dat$max)))
  points(dat$max[or], pch = 16, cex = 0.5)
  lines(bound$fit[or], col = "red", lwd = 2)
  lines(bound$lwr[or], lty = 3, col = "blue")
  lines(bound$upr[or], lty = 3, col = "blue")
  legend(
    "topleft",
    c("observation", "fitted curve", "bound"),
    cex = 1.2,
    col = c('black', 'red', 'blue'),
    lty = 1:3,
    lwd = 2,
    bty = "n"
  )
  r2 = round(ab$adj.r.squared, digits = 2)
  legend('bottomright', paste('R2 = ', r2, sep = ""))
}


#####################
##    Gaussian     ##
#####################

Gaussian = function(x, sigma) {
  z = (1 / (sigma * sqrt(2 * pi))) * exp(-(x ^ 2) / (2 * sigma ^ 2))
  return(z)
}

x = seq(from = 0, to = 50, by = 0.2)
lists = c(0.7, 1, 2, 4)
plot(
  0,
  xlim = c(0, 10),
  ylim = c(0, 0.2),
  xlab = 'Distance (m)',
  ylab = 'z',
  pch = "",
  main = "Gaussian"
)
for (k in 1:length(lists)) {
  z = Gaussian(x, sigma = lists[k])
  z[!is.finite(z)] <- 0
  if (sum(z) != 0)
    z = z / sum(z)
  lines(x, z, col = k)
}
legend(
  'topright',
  legend = lists,
  lty = 1,
  col = 1:length(lists),
  cex = .95
)

  {
    #Finding Bound
    
    x = seq(from = 0, to = 300, by = 0.2)
    lists = seq(0.5, 50, 0.1)
    nb = length(lists)
    dat = data.frame(matrix(NA, nrow = nb, ncol = 2))
    names(dat) = c('s', 'max')
    dat$s = lists
    for (k in 1:nb) {
      z = Gaussian(x, sigma = dat$s[k])
      z[!is.finite(z)] <- 0
      if (sum(z) != 0)
        z = z / sum(z)
      z = round(z, 4)
      x0 = min(x[z == 0 & x != 0])
      dat$max[k] = x0
    }
    #regression
    reg <- lm(dat$max ~ s, data = dat)
    summary(reg)
    
    #test function
    #values come from regression summary where the first three values are the Estimate
    #and the last value is the max residual value, with this one we are sure the last seed will be in the area
    ff <- function(s) {
      z = (2.829490 * s) + 5.57001 + 1.7662
      return(z)
    }
    dat[which(dat$s == 20), ]
    ff(20)
    
    {
      #plot
      ab = summary(reg)
      plot(dat$s, dat$max)
      abline(reg, col = 2)
      r2 = round(ab$adj.r.squared, digits = 3)
      legend('bottomright', paste('R2 = ', r2, sep = ""))
    }
    
  }
  
  #####################
  ##   Exponential   ##
  #####################
  
  Exponential = function(x, lambda)
  {
    z = 1 / (2 * pi * (lambda ^ 2)) * exp(-abs(x) / lambda)
    return(z)
  }
  
  x = seq(from = 0, to = 50, by = 0.2)
  listl = c(0.5, 1, 2, 4, 6, 8)
  plot(
    0,
    xlim = c(0, 10),
    ylim = c(0, 0.35),
    xlab = 'Distance (m)',
    ylab = 'z',
    pch = "",
    main = "Exponential"
  )
  for (k in 1:length(listl))
  {
    z = Exponential(x, lambda = listl[k])
    z[!is.finite(z)] <- 0
    if (sum(z) != 0)
      z = z / sum(z)
    lines(x, z, col = k)
  }
  legend(
    'topright',
    legend = listl,
    lty = 1,
    col = 1:length(listl),
    cex = .95
  )
  
  #Finding Bound
  x = seq(from = 0, to = 300, by = 0.2)
  listl = seq(0.5, 25, 0.1)
  nb = length(listl)
  dat = data.frame(matrix(NA, nrow = nb, ncol = 2))
  names(dat) = c('l', 'max')
  dat$l = listl
  for (k in 1:nb)
  {
    z = Exponential(x, lambda = dat$l[k])
    z[!is.finite(z)] <- 0
    if (sum(z) != 0)
      z = z / sum(z)
    z = round(z, 4)
    x0 = min(x[z == 0 & x != 0])
    dat$max[k] = x0
  }
  #regression
  reg <- lm(dat$max ~ l, data = dat)
  summary(reg)
  
  #test function
  #values come from regression summary where the first three values are the Estimate
  #and the last value is the max residual value, with this one we are sure the last seed will be in the area
  ff <- function(l)
  {
    z = (4.87324 * l) + 8.90765 + 2.425
    return(z)
  }
  dat[which(dat$l == 20), ]
  ff(20)
  
  {
    #plot
    ab = summary(reg)
    plot(dat$l, dat$max)
    abline(reg, col = 2)
    r2 = round(ab$adj.r.squared, digits = 3)
    legend('bottomright', paste('R2 = ', r2, sep = ""))
  }
  
  
  #####################
  ##      2Dt        ##
  #####################
  
  f2dt <- function(x, a, b)
  {
    z = ((b - 1) / (pi * a * a)) * (1 + ((x * x) / (a * a))) ^ (-b)
    return(z)
  }
  #a
  x = seq(from = 0, to = 100, by = 1)
  lista = seq(0.5, 40.5, 5)
  plot(
    0,
    xlim = c(0, 10),
    ylim = c(0, 0.4),
    xlab = 'Distance (m)',
    ylab = 'z',
    pch = "",
    main = "2Dt - b = 1.5"
  )
  for (k in 1:length(lista))
  {
    z = f2dt(x, a = lista[k], b = 5)
    z[!is.finite(z)] <- 0
    #	if(sum(z)!=0) z=z/sum(z)
    lines(x, log(z), col = k)
  }
  legend(
    'topright',
    legend = lista,
    lty = 1,
    col = 1:length(lista),
    cex = .95
  )
  
  #b
  x = seq(from = 0, to = 50, by = 0.2)
  listb = c(1.1, 1.2, 1.5, 2, 2.5, 3, 3.5)
  plot(
    0,
    xlim = c(0, 100),
    ylim = c(0, 0.08),
    xlab = 'Distance (m)',
    ylab = 'z',
    pch = "",
    main = "2Dt - a = 4"
  )
  for (k in 1:length(listb))
  {
    z = f2dt(x, a = 20, b = 1.9)
    z[!is.finite(z)] <- 0
    if (sum(z) != 0)
      z = z / sum(z)
    lines(x, z, col = k)
  }
  legend(
    'topright',
    legend = listb,
    lty = 1,
    col = 1:length(listb),
    cex = .95
  )
  
  #Finding Bound
  x = seq(from = 0, to = 300, by = 0.2)
  lista = seq(2, 20, 1)
  listb = seq(1.1, 4, 0.1)
  nb = length(lista) * length(listb)
  dat = data.frame(matrix(NA, nrow = nb, ncol = 3))
  names(dat) = c('a', 'b', 'max')
  dat$a = lista
  dat$b = rep(listb, each = length(lista))
  for (k in 1:nb)
  {
    z = f2dt(x, a = dat$a[k], b = dat$b[k])
    z[!is.finite(z)] <- 0
    if (sum(z) != 0)
      z = z / sum(z)
    z = round(z, 4)
    x0 = min(x[z == 0 & x != 0])
    dat$max[k] = x0
  }
  #regression
  reg <- lm(dat$max ~ a + b, data = dat)
  summary(reg)
  
  #test function
  ff <- function(a, b)
  {
    z = 3.2160 * a - (28.00092 * b) + 82.6865 + 76.20381
    return(z)
  }
  dat[which(dat$a == 20 & dat$b == 1.1), ]
  ff(20, 1.1)
  
  {
    #plot (difference ~ 100m)
    ab = summary(reg)
    bound <- data.frame(predict(reg, int = 'c'))
    or <- order(bound$fit)
    plot(
      dat$max[or],
      ylab = 'Maximum dispersal',
      pch = '',
      ylim = c(min(bound), max(dat$max))
    )
    points(dat$max[or], pch = 16, cex = 0.5)
    lines(bound$fit[or], col = "red", lwd = 2)
    lines(bound$lwr[or], lty = 3, col = "blue")
    lines(bound$upr[or], lty = 3, col = "blue")
    legend(
      "topleft",
      c("observation", "fitted curve", "bound"),
      cex = 1.2,
      col = c('black', 'red', 'blue'),
      lty = 1:3,
      lwd = 2,
      bty = "n"
    )
    r2 = round(ab$adj.r.squared, digits = 2)
    legend('bottomright', paste('R2 = ', r2, sep = ""))
  }
  
  #========================================================================
}

#Getting a function with estimate the a and b parameter for FlorSys
{  
  x <-
    c(
      0.01,
      0.1,
      0.2,
      0.5,
      1,
      1.5,
      2,
      2.5,
      3,
      3.5,
      4,
      4.5,
      5,
      6,
      7,
      8,
      9,
      10,
      15,
      20,
      25,
      30,
      35,
      40,
      45,
      50,
      60,
      70,
      80,
      90,
      100,
      150,
      200,
      250,
      300
    )
  
  x <- c(seq(0.1, 9.8, 0.2), 10:300)
  a = seq(0.1, 60.1, 0.1)
  b = seq(1.1, 7.1, 0.1)
  tot = length(a) * length(b)
  dat <- data.frame(matrix(NA, nrow = tot, ncol = 4))
  names(dat) <- c('a', 'b', 'mean', 'max')
  dat$a = a
  dat$b = rep(b, each = length(a))
  
  for (i in 1:tot)
  {
    z = f2dt(x, a = dat$a[i], b = dat$b[i])
    if (sum(z) != 0)
      z = z / sum(z)
    zz = cumsum(z)
    x1 = x[which(abs(zz - 0.5) == min(abs(zz - 0.5)))]
    x0 = x[which(abs(zz - 0.9) == min(abs(zz - 0.9)))]
    dat$max[i] = x0
    dat$mean[i] = x1
    
    progress(i * 100 / tot)
  }
  
  # Lm parameter A
  
  reg <- lm(dat$a ~ dat$mean + dat$max + dat$mean * dat$max)
  summary(reg)
  reg <-
    lm(log(dat$a) ~ dat$mean + dat$max + dat$mean * dat$max) #a in log
  summary(reg)
  reg <-
    lm(dat$a ~ log(dat$mean) + log(dat$max) + log(dat$mean) * log(dat$max)) #mean and max in log
  summary(reg)
  reg <-
    lm(log(dat$a) ~ log(dat$mean) + log(dat$max) + log(dat$mean) * log(dat$max)) #both in log
  summary(reg)
  
  #plot
  bound <- data.frame(predict(reg, int = 'c'))
  or <- order(bound$fit)
  plot(log(dat$a)[or])
  lines(bound$fit[or], col = "red", lwd = 2)
  lines(bound$lwr[or], lty = 3, col = "blue")
  lines(bound$upr[or], lty = 3, col = "blue")
  legend(
    "bottomright",
    c("observation", "fitted curve", "bound"),
    cex = 1.2,
    col = c('black', 'red', 'blue'),
    lty = 1:3,
    lwd = 2,
    bty = "n"
  )
  r2 = round(summary(reg)$adj.r.squared, digits = 2)
  legend('topleft', paste('R2 = ', r2, sep = ""))
  
  
  # Lm parameter B
  
  reg <- lm(dat$b ~ dat$mean + dat$max + dat$mean * dat$max)
  summary(reg)
  reg <-
    lm(log(dat$b) ~ dat$mean + dat$max + dat$mean * dat$max) #b in log
  summary(reg)
  reg <-
    lm(dat$b ~ log(dat$mean) + log(dat$max) + log(dat$mean) * log(dat$max)) #mean and max in log
  summary(reg)
  reg <-
    lm(log(dat$b) ~ log(dat$mean) + log(dat$max) + log(dat$mean) * log(dat$max)) #both in log
  summary(reg)
  
  reg <-
    lm(
      log(dat$b) ~ log(dat$a) + log(dat$mean) + log(dat$max) + log(dat$mean) *
        log(dat$max) + log(dat$mean) * log(dat$a) + log(dat$max) * log(dat$a)
    ) #with A
  summary(reg)
  
  #plot
  bound <- data.frame(predict(reg, int = 'c'))
  or <- order(bound$fit)
  plot(log(dat$b)[or])
  lines(bound$fit[or], col = "red", lwd = 2)
  lines(bound$lwr[or], lty = 3, col = "blue")
  lines(bound$upr[or], lty = 3, col = "blue")
  legend(
    "bottomright",
    c("observation", "fitted curve", "bound"),
    cex = 1.2,
    col = c('black', 'red', 'blue'),
    lty = 1:3,
    lwd = 2,
    bty = "n"
  )
  r2 = round(summary(reg)$adj.r.squared, digits = 2)
  legend('topleft', paste('R2 = ', r2, sep = ""))
  
}

#Test of parameters and resolution after filter
{
  WDmodel = paste(myWD, "Willian/R/dynWeed/Model/", sep = '')
  WDsimul = paste(myWD, "Willian/R/dynWeed/Tests/", sep = '')
  WDinput = paste(myWD, "Willian/R/dynWeed/", sep = '')
  WDoutput = paste(myWD, "Willian/R/dynWeed/output/", sep = '')
  
  # loading model
  source(paste(WDmodel, "DynWeed4.3.1.r", sep = ""))
  # loading dispersal Curves
  source(paste(WDinput, "Dispersal/DispersalCurves.r", sep = ""))
  
  
  #####################
  ##     Weibull     ##
  #####################
  
  #Getting data: variation du M, C et Resolution
  res = seq(from = 10, to = 30, by = 2)
  listm = seq(from = 1, to = 30, by = 0.1)
  listc = seq(from = 0.2, to = 1.5, by = 0.1)
  ntotal = length(res) * length(listm) * length(listc)
  tab = data.frame(matrix(nrow = ntotal, ncol = 7))
  names(tab) = c('res', 'm', 'c', 'seedPropDep', 'mean', 'dist50', 'dist99')
  tab$m = listm
  tab$c = rep(listc, each = length(listm))
  tab$res = rep(res, each = length(listm) * length(listc))
  for (i in 1:ntotal) {
    rst = newRst(
      xmin = 1000,
      xmax = 3000,
      ymin = 1000,
      ymax = 3000,
      res = tab$res[i],
      nbBand = 1,
      bandNames = c("seed"),
      bandVal = c(0)
    )
    rst = setMiddleVal(rst, "seed", 1)
    Zm = getFilter(
      typCurve = "Weibull",
      pCurve = list(m = tab$m[i], c = tab$c[i]),
      r = rst@grid@cellsize[1]
    )
    rstDisp = dispCurve(rst, Zm, spList = "seed")
    rstDisp = addDistFromMiddle(rstDisp)
    tab$seedPropDep[i] = sum(rstDisp@data$seed[rstDisp@data$dist <= tab$res[i]])
    tab$mean[i] = getMeanDist(rstDisp)
    tab$dist50[i] = getDistProp(rstDisp, 0.5)
    tab$dist99[i] = getDistProp(rstDisp, 0.99)
    progress(100 * i / ntotal)
  }
  head(tab)
  tail(tab)
  
  setwd(paste(myWD, 'Willian/R/DynWeed/Dispersal/', sep = ''))
  #write.table(tab,"tab_weibull.txt",row.names=F,dec=".",quote=F,sep="\t")
  #tab=read.table('tab_weibull.txt',head=T)
  
  #plot 3D (resolution = 20m)
  library(latticeExtra)
  outTab = tab[which(tab$res == 20), ]
  #mean distance
  cloud(
    outTab$mean ~ outTab$c + outTab$m,
    ,
    col.facet = 'grey',
    xbase = 0.4,
    ybase = 0.4,
    scales = list(arrows = FALSE, col = 1),
    par.settings = list(axis.line = list(col = "transparent"))
  )
  #max distance
  windows()
  cloud(
    outTab$dist99 ~ outTab$c + outTab$m,
    ,
    col.facet = 'grey',
    xbase = 0.4,
    ybase = 0.4,
    scales = list(arrows = FALSE, col = 1),
    par.settings = list(axis.line = list(col = "transparent"))
  )
  
  #projection (resolution = 20m)
  #parameter c
  listm = seq(from = 3, to = 20, by = 0.5)
  listc = seq(from = 1.2, to = 2, by = 0.1)
  plot(
    listm,
    outTab$mean[which(outTab$c == 1.2)],
    ylim = c(20, 140),
    type = 'n',
    xlab = 'm',
    ylab = 'Mean Distance'
  )
  for (i in 1:9) {
    points(listm,
           outTab$mean[which(outTab$c == listc[i])],
           type = 'l',
           col = i,
           pch = i)
    points(listm,
           outTab$mean[which(outTab$c == listc[i])],
           col = i,
           pch = i,
           cex = 0.6)
  }
  legend(
    'topleft',
    legend = listc,
    lty = 1,
    col = 1:9,
    pch = 1:9,
    cex = 0.9
  )
  #parameter m
  plot(
    listc,
    outTab$mean[which(outTab$m == 20)],
    type = 'n',
    ylim = c(20, 150),
    xlab = 'c',
    ylab = 'Mean Distance'
  )
  for (i in 1:length(listm)) {
    points(listc,
           outTab$mean[which(outTab$m == listm[i])],
           type = 'l',
           col = i,
           cex = 0.9)
  }
  
  #second representation
  plot(
    outTab$seedPropDep ~ outTab$m,
    pch = c(1:9)[as.numeric(as.factor(outTab$c))],
    col = c(1:9)[as.numeric(as.factor(outTab$c))],
    xlab = 'parameter M',
    ylab = 'Seed Proportion on the pixel',
    cex = 0.65
  )
  legend(
    'bottomleft',
    legend = c('1.2', '1.3', '1.4', '1.5', '1.6', '1.7', '1.8', '1.9', '2'),
    pch = 1:9,
    col = 1:9
  )
  boxplot(outTab$seedPropDep ~ outTab$c,
          xlab = 'parameter C',
          ylab = 'Seed Proportion on the pixel')
  
  #graphic with resolution variation
  par(mfrow = c(2, 2))
  hist(
    tab$seedPropDep[which(tab$res == 10)],
    ylim = c(0, 110),
    main = 'Res = 10',
    xlab = 'Seed proportion on the pixel'
  )
  hist(
    tab$seedPropDep[which(tab$res == 16)],
    ylim = c(0, 110),
    main = 'Res = 16',
    xlab = 'Seed proportion on the pixel'
  )
  hist(
    tab$seedPropDep[which(tab$res == 20)],
    ylim = c(0, 110),
    main = 'Res = 20',
    xlab = 'Seed proportion on the pixel'
  )
  hist(
    tab$seedPropDep[which(tab$res == 28)],
    ylim = c(0, 110),
    main = 'Res = 28',
    xlab = 'Seed proportion on the pixel'
  )
  
  
  #projection mean
  #parameter c
  res = seq(from = 10, to = 30, by = 2)
  plot(
    tab$c[which(tab$res == 10)],
    tab$mean[which(tab$res == 10)],
    ylim = c(10, 80),
    pch = '',
    xlab = 'parameter C',
    ylab = 'Mean dispersal'
  )
  for (i in 1:11) {
    m = lm(tab$mean[which(tab$res == res[i])] ~ tab$c[which(tab$res == res[i])] +
             I(tab$c[which(tab$res == res[i])] ^ 2))
    points(tab$c[which(tab$res == res[i])],
           predict(m),
           col = i,
           pch = i,
           cex = 0.9)
    lines(tab$c[which(tab$res == res[i])], predict(m), col = i, pch = i)
  }
  legend(
    'topright',
    legend = res,
    lty = 1,
    pch = 1:11,
    col = 1:11,
    cex = .95
  )
  
  #parameter m
  res = seq(from = 10, to = 30, by = 2)
  plot(
    tab$m[which(tab$res == 10)],
    tab$mean[which(tab$res == 10)],
    ylim = c(10, 70),
    pch = '',
    xlab = 'parameter M',
    ylab = 'Mean dispersal'
  )
  for (i in 1:11) {
    m = lm(tab$mean[which(tab$res == res[i])] ~ tab$m[which(tab$res == res[i])] +
             I(tab$m[which(tab$res == res[i])] ^ 2))
    points(tab$m[which(tab$res == res[i])],
           predict(m),
           col = i,
           pch = i,
           cex = 0.85)
  }
  legend(
    'topleft',
    legend = res,
    lty = 1,
    pch = 1:11,
    col = 1:11,
    cex = .95
  )
  
  #projection seed Proportion
  #parameter c
  res = seq(from = 10, to = 30, by = 2)
  plot(
    tab$c[which(tab$res == 10)],
    tab$seedPropDep[which(tab$res == 10)],
    ylim = c(0, .9),
    pch = '',
    xlab = 'parameter C',
    ylab = 'Seed Proportion on the pixel'
  )
  for (i in 1:11) {
    m = lm(tab$seedPropDep[which(tab$res == res[i])] ~ tab$c[which(tab$res ==
                                                                     res[i])] + I(tab$c[which(tab$res == res[i])] ^ 2))
    points(tab$c[which(tab$res == res[i])],
           predict(m),
           col = i,
           pch = i,
           cex = 0.85)
    lines(tab$c[which(tab$res == res[i])], predict(m), col = i)
  }
  legend(
    'topleft',
    legend = res,
    lty = 1,
    pch = 1:11,
    col = 1:11,
    cex = 0.95
  )
  
  #parameter m
  res = seq(from = 10, to = 30, by = 2)
  plot(
    tab$m[which(tab$res == 10)],
    tab$seedPropDep[which(tab$res == 10)],
    ylim = c(0, 1),
    pch = '',
    xlab = 'parameter M',
    ylab = 'Seed Proportion on the pixel'
  )
  for (i in 1:11) {
    m = lm(tab$seedPropDep[which(tab$res == res[i])] ~ tab$m[which(tab$res ==
                                                                     res[i])] + I(tab$m[which(tab$res == res[i])] ^ 2))
    points(tab$m[which(tab$res == res[i])],
           predict(m),
           col = i,
           pch = i,
           cex = 0.85)
  }
  legend(
    'topright',
    legend = res,
    lty = 1,
    col = 1:11,
    pch = 1:11,
    cex = 0.95
  )
  
  #selecting the best parameter (and resolution)
  summary(tab)
  summary(tab[which(tab$seedPropDep > 0.70 &
                      tab$seedPropDep < 0.80 &
                      tab$res > 18 & tab$res < 24), ])
  summary(tab[which(tab$mean < 30 & tab$dist99 > 65), ])
  
  #####################
  ##    Gaussian     ##
  #####################
  
  res = seq(from = 10, to = 30, by = 2)
  lists = seq(from = 1, to = 100, by = 0.5)
  ntotal = length(res) * length(lists)
  tab = data.frame(matrix(nrow = ntotal, ncol = 6))
  names(tab) = c('res', 's', 'seedPropDep', 'mean', 'dist50', 'dist99')
  tab$s = lists
  tab$res = rep(res, each = length(lists))
  for (i in 1:ntotal) {
    rst = newRst(
      xmin = 1000,
      xmax = 3000,
      ymin = 1000,
      ymax = 3000,
      res = tab$res[i],
      nbBand = 1,
      bandNames = c("seed"),
      bandVal = c(0)
    )
    rst = setMiddleVal(rst, "seed", 1)
    Zm = getFilter(
      typCurve = "Gaussian",
      pCurve = list("sigma" = tab$s[i]),
      r = rst@grid@cellsize[1]
    )
    rstDisp = dispCurve(rst, Zm, spList = "seed")
    rstDisp = addDistFromMiddle(rstDisp)
    tab$seedPropDep[i] = sum(rstDisp@data$seed[rstDisp@data$dist <= tab$res[i]])
    tab$mean[i] = getMeanDist(rstDisp)
    tab$dist50[i] = getDistProp(rstDisp, 0.5)
    tab$dist99[i] = getDistProp(rstDisp, 0.99)
    progress(100 * i / ntotal)
  }
  head(tab)
  tail(tab)
  
  #setwd(paste(myWD,'Willian/R/DynWeed/Dispersal/', sep = ''))
  #write.table(tab,"tab_gaussian.txt",row.names=F,dec=".",quote=F,sep="\t")
  #tab=read.table('tab_gaussian.txt',head=T)
  
  {
    #Plot sigma variation
    plot(
      tab$s,
      tab$dist99,
      pch = '',
      xlab = 'Sigma',
      ylab = '',
      main = 'Sigma variation test (Gaussian)'
    )
    lines(tab$s[which(tab$res == 20)], tab$dist99[which(tab$res == 20)], lty =
            1, col = 2)
    lines(tab$s[which(tab$res == 20)], tab$dist95[which(tab$res == 20)], col =
            3)
    lines(tab$s[which(tab$res == 20)], tab$mean[which(tab$res == 20)], col =
            4)
    legend(
      "topleft",
      c("dist99", "dist95", "distMean"),
      col = c(2, 3, 4),
      lty = 1,
      lwd = 2,
      bty = "n"
    )
    
    #Graphic with resolution
    #Sigma projection for each resolution
    #mean
    res = seq(from = 10, to = 30, by = 2)
    plot(
      tab$s[which(tab$res == 10)],
      tab$mean[which(tab$res == 10)],
      pch = '',
      ylim = c(0, max(tab$mean)),
      xlim = c(0, 100),
      xlab = 'Sigma',
      ylab = 'Mean dispersal'
    )
    for (i in 1:11) {
      lines(tab$s[which(tab$res == res[i])],
            tab$mean[which(tab$res == res[i])],
            type = 'l',
            col = i,
            pch = i)
    }
    
    res = seq(from = 10, to = 30, by = 2)
    plot(
      tab$s[which(tab$res == 10)],
      tab$mean[which(tab$res == 10)],
      pch = '',
      xlim = c(0, 30),
      ylim = c(0, 40),
      xlab = 'Sigma',
      ylab = 'Mean dispersal'
    )
    for (i in 1:11) {
      lines(
        tab$s[which(tab$res == res[i])],
        tab$mean[which(tab$res == res[i])],
        type = 'l',
        col = i,
        pch = i,
        cex = 0.9
      )
      points(tab$s[which(tab$res == res[i])],
             tab$mean[which(tab$res == res[i])],
             col = i,
             pch = i,
             cex = 0.5)
    }
    legend(
      'bottomright',
      legend = res,
      lty = 1,
      pch = 1:11,
      col = 1:11,
      cex = .95
    )
    
    #Seed proportion
    res = seq(from = 10, to = 30, by = 2)
    plot(
      tab$s[which(tab$res == 10)],
      tab$seedPropDep[which(tab$res == 10)],
      pch = '',
      ylim = c(0, 1),
      xlim = c(0, 100),
      xlab = 'Sigma',
      ylab = 'Seed Proportion on the pixel'
    )
    for (i in 1:11) {
      lines(
        tab$s[which(tab$res == res[i])],
        tab$seedPropDep[which(tab$res == res[i])],
        type = 'l',
        col = i,
        pch = i,
        ylim = c(0, 250)
      )
    }
    
    res = seq(from = 10, to = 30, by = 2)
    plot(
      tab$s[which(tab$res == 10)],
      tab$seedPropDep[which(tab$res == 10)],
      pch = '',
      xlim = c(1, 30),
      ylim = c(0, 1),
      xlab = 'Sigma',
      ylab = 'Seed Proportion on the pixel'
    )
    for (i in 1:11) {
      lines(
        tab$s[which(tab$res == res[i])],
        tab$seedPropDep[which(tab$res == res[i])],
        type = 'l',
        col = i,
        pch = i,
        cex = 0.9
      )
      points(
        tab$s[which(tab$res == res[i])],
        tab$seedPropDep[which(tab$res == res[i])],
        col = i,
        pch = i,
        cex = 0.5
      )
    }
    legend(
      'bottomleft',
      legend = res,
      lty = 1,
      pch = 1:11,
      col = 1:11,
      cex = .95
    )
    
    #selecting the best parameter (and resolution)
    summary(tab)
    summary(tab[which(tab$seedPropDep > 0.70 &
                        tab$seedPropDep < 0.80 &
                        tab$res > 18 & tab$res < 24), ])
    summary(tab[which(tab$mean < 30 & tab$dist99 > 65), ])
    
    
    
    #####################
    ##   Exponential   ##
    #####################
    
    # Lambda and resolution variation
    listl = seq(from = 4, to = 20, by = 0.1)
    res = seq(from = 10, to = 30, by = 2)
    nbm = length(listl) * length(res)
    tab = data.frame(matrix(nrow = nbm, ncol = 7))
    names(tab) = c("res",
                   "lambda",
                   "seedPropDep",
                   "mean",
                   "dist50",
                   "dist95",
                   "dist99")
    tab$lambda = listl
    tab$res = rep(res, each = length(listl))
    for (k in 1:nbm) {
      rst = newRst(
        xmin = 1000,
        xmax = 3000,
        ymin = 1000,
        ymax = 3000,
        res = tab$res[k],
        nbBand = 1,
        bandNames = c("seed"),
        bandVal = c(0)
      )
      rst = setMiddleVal(rst, "seed", 1)
      Zm = getFilter(
        typCurve = "Exponential",
        pCurve = list("lambda" = tab$lambda[k]),
        r = rst@grid@cellsize[1]
      )
      rstDisp = dispCurve(rst, Zm, spList = "seed")
      rstDisp = addDistFromMiddle(rstDisp)
      tab$seedPropDep[k] = sum(rstDisp$seed[which(rstDisp$dist <= tab$res[k])])
      tab$mean[k] = getMeanDist(rstDisp)
      tab$dist50[k] = getDistProp(rstDisp, 0.5)
      tab$dist95[k] = getDistProp(rstDisp, 0.95)
      tab$dist99[k] = getDistProp(rstDisp, 0.99)
      progress(100 * k / nbm)
    }
    
    head(tab)
    tail(tab)
    
    #setwd(paste(myWD,'Willian/R/DynWeed/Dispersal/', sep = ''))
    #write.table(tab,"tab_exponential.txt",row.names=F,dec=".",quote=F,sep="\t")
    #tab=read.table('tab_exponential.txt',head=T)
    
    #plot
    #Plot lambda variation
    plot(
      tab$lambda,
      tab$dist99,
      pch = '',
      xlab = 'Lambda',
      ylab = '',
      main = 'Lambda variation test (Exponential)'
    )
    lines(tab$lambda[which(tab$res == 20)], tab$dist99[which(tab$res == 20)], lty =
            1, col = 2)
    lines(tab$lambda[which(tab$res == 20)], tab$dist95[which(tab$res == 20)], col =
            3)
    lines(tab$lambda[which(tab$res == 20)], tab$mean[which(tab$res == 20)], col =
            4)
    legend(
      "topleft",
      c("dist99", "dist95", "distMean"),
      col = c(2, 3, 4),
      lty = 1,
      lwd = 2,
      bty = "n"
    )
    
    #mean
    
    res = seq(from = 10, to = 30, by = 2)
    plot(
      tab$lambda,
      tab$mean,
      pch = '',
      ylim = c(0, max(tab$mean)),
      xlab = 'Lambda',
      ylab = 'Dispersion Mean'
    )
    for (i in 1:11) {
      lines(tab$lambda[which(tab$res == res[i])], tab$mean[which(tab$res == res[i])], col =
              i, cex = 0.8)
      points(
        tab$lambda[which(tab$res == res[i])],
        tab$mean[which(tab$res == res[i])],
        col = i,
        pch = 1:20,
        cex = 0.6
      )
    }
    legend(
      'bottomright',
      legend = res,
      lty = 1,
      pch = 1:11,
      col = res,
      cex = 0.9
    )
    
    #Seed proportion
    res = seq(from = 10, to = 30, by = 2)
    plot(
      tab$lambda,
      tab$seedPropDep,
      pch = '',
      xlab = 'Lambda',
      ylab = 'Seed Proportion on the pixel'
    )
    for (i in 1:11) {
      lines(tab$lambda[which(tab$res == res[i])],
            tab$seedPropDep[which(tab$res == res[i])],
            col = i,
            cex = 0.8)
      points(
        tab$lambda[which(tab$res == res[i])],
        tab$seedPropDep[which(tab$res == res[i])],
        col = i,
        pch = 1:20,
        cex = 0.6
      )
    }
    legend(
      'bottomleft',
      legend = res,
      lty = 1,
      pch = 1:11,
      col = res,
      cex = 0.9
    )
    
    #selecting the best parameter (and resolution)
    summary(tab)
    summary(tab[which(tab$seedPropDep > 0.70 &
                        tab$seedPropDep < 0.80 &
                        tab$res > 18 & tab$res < 24), ])
    summary(tab[which(tab$mean < 30 & tab$dist99 > 65), ])
    
    #####################
    ##      2Dt        ##
    #####################
    
    # Variation de a, b et resolution
    lista = seq(from = 5, to = 10, by = 0.1)
    listb = seq(from = 1.1, to = 1.5, by = 0.1)
    res = seq(from = 10, to = 30, by = 2)
    nbm = length(lista) * length(res) * length(listb)
    tab = data.frame(matrix(nrow = nbm, ncol = 8))
    names(tab) = c("res",
                   "a",
                   "b",
                   "seedPropDep",
                   "mean",
                   "dist50",
                   "dist95",
                   "dist99")
    tab$a = lista
    tab$b = rep(listb, each = length(lista))
    tab$res = rep(res, each = length(lista) * length(listb))
    for (k in 1:nbm) {
      rst = newRst(
        xmin = 1000,
        xmax = 3000,
        ymin = 1000,
        ymax = 3000,
        res = tab$res[k],
        nbBand = 1,
        bandNames = c("seed"),
        bandVal = c(0)
      )
      rst = setMiddleVal(rst, "seed", 1)
      Zm = getFilter(
        typCurve = "f2dt",
        pCurve = list(a = tab$a[k], b = tab$b[k]),
        r = rst@grid@cellsize[1]
      )
      rstDisp = dispCurve(rst, Zm, spList = "seed")
      rstDisp = addDistFromMiddle(rstDisp)
      tab$seedPropDep[k] = sum(rstDisp$seed[which(rstDisp$dist <= tab$res[k])])
      tab$mean[k] = getMeanDist(rstDisp)
      tab$dist50[k] = getDistProp(rstDisp, 0.5)
      tab$dist95[k] = getDistProp(rstDisp, 0.95)
      tab$dist99[k] = getDistProp(rstDisp, 0.99)
      progress(100 * k / nbm)
    }
    
    head(tab)
    tail(tab)
    
    #setwd(paste(myWD,'Willian/R/DynWeed/Dispersal/', sep = ''))
    #write.table(tab,"tab_f2dt.txt",row.names=F,dec=".",quote=F,sep="\t")
    #tab=read.table('tab_f2dt.txt',head=T)
    
    #plot 3D (resolution = 20m)
    library(latticeExtra)
    outTab = tab[which(tab$res == 20), ]
    #mean distance
    cloud(
      outTab$mean ~ outTab$b + outTab$a,
      col.facet = 'grey',
      xbase = 0.4,
      ybase = 0.4,
      scales = list(arrows = FALSE, col = 1),
      par.settings = list(axis.line = list(col = "transparent"))
    )
    #max distance
    windows()
    cloud(
      outTab$dist99 ~ outTab$b + outTab$a,
      col.facet = 'grey',
      xbase = 0.4,
      ybase = 0.4,
      scales = list(arrows = FALSE, col = 1),
      par.settings = list(axis.line = list(col = "transparent"))
    )
    
    #projection (resolution = 20m)
    #parameter a
    lista = seq(from = 5, to = 10, by = 0.1)
    listb = seq(from = 1.1, to = 1.5, by = 0.1)
    plot(
      listb,
      outTab$mean[which(outTab$a == 5)],
      xlim = c(min(listb), max(listb)),
      ylim = c(15, max(tab$mean)),
      type = 'n',
      xlab = 'Parameter b',
      ylab = 'Mean Distance',
      main = 'Resolution = 20 m'
    )
    for (i in 1:length(lista)) {
      points(listb,
             outTab$mean[which(outTab$a == lista[i])],
             type = 'l',
             col = i,
             pch = i)
      points(listb,
             outTab$mean[which(outTab$a == lista[i])],
             col = i,
             pch = i,
             cex = 0.6)
    }
    #parameter b
    plot(
      lista,
      outTab$mean[which(outTab$b == 1.5)],
      ylim = c(min(outTab$mean), max(outTab$mean)),
      type = 'n',
      xlab = 'Parameter a',
      ylab = 'Mean Distance',
      main = 'Resolution = 20 m'
    )
    for (i in 1:length(listb)) {
      points(lista,
             outTab$mean[which(outTab$b == listb[i])],
             type = 'l',
             col = i,
             pch = i)
      points(lista,
             outTab$mean[which(outTab$b == listb[i])],
             col = i,
             pch = i,
             cex = 0.6)
    }
    legend(
      'topleft',
      legend = listb,
      lty = 1,
      col = 1:length(listb),
      pch = 1:length(listb),
      cex = 0.9
    )
    
    #second representation
    plot(
      outTab$seedPropDep ~ outTab$a,
      pch = c(1:5)[as.numeric(as.factor(outTab$b))],
      col = c(1:5)[as.numeric(as.factor(outTab$b))],
      xlab = 'parameter a',
      ylab = 'Seed Proportion on the pixel',
      cex = 0.65,
      main = 'Resolution = 20 m'
    )
    legend(
      'bottomleft',
      legend = c('1.2', '1.3', '1.4', '1.5', '1.6', '1.7', '1.8', '1.9', '2'),
      pch = 1:9,
      col = 1:9
    )
    boxplot(
      outTab$seedPropDep ~ outTab$b,
      xlab = 'parameter b',
      ylab = 'Seed Proportion on the pixel',
      ylab = 'Seed Proportion on the pixel',
      cex = 0.65,
      main = 'Resolution = 20 m'
    )
    
    #projection mean
    #parameter a
    res = seq(from = 10, to = 30, by = 2)
    plot(
      tab$a[which(tab$res == 10)],
      tab$mean[which(tab$res == 10)],
      ylim = c(5, 45),
      pch = '',
      xlab = 'parameter a',
      ylab = 'Mean dispersal'
    )
    for (i in 1:11) {
      m = lm(tab$mean[which(tab$res == res[i])] ~ tab$a[which(tab$res == res[i])] +
               I(tab$a[which(tab$res == res[i])] ^ 2))
      points(tab$a[which(tab$res == res[i])],
             predict(m),
             col = i,
             pch = i,
             cex = 0.9)
    }
    legend(
      'topleft',
      legend = res,
      lty = 1,
      pch = 1:11,
      col = 1:11,
      cex = .95
    )
    
    #parameter b
    res = seq(from = 10, to = 30, by = 2)
    plot(
      tab$b[which(tab$res == 10)],
      tab$mean[which(tab$res == 10)],
      ylim = c(5, 45),
      pch = '',
      xlab = 'parameter b',
      ylab = 'Mean dispersal'
    )
    for (i in 1:11) {
      m = lm(tab$mean[which(tab$res == res[i])] ~ tab$b[which(tab$res == res[i])] +
               I(tab$b[which(tab$res == res[i])] ^ 2))
      points(
        tab$b[which(tab$res == res[i])],
        predict(m),
        type = 'l',
        col = i,
        pch = i,
        cex = 0.85
      )
      points(tab$b[which(tab$res == res[i])],
             predict(m),
             col = i,
             pch = i,
             cex = 0.85)
    }
    legend(
      'topright',
      legend = res,
      lty = 1,
      pch = 1:11,
      col = 1:11,
      cex = .95
    )
    
    #projection seed Proportion
    #parameter a
    res = seq(from = 10, to = 30, by = 2)
    plot(
      tab$a[which(tab$res == 10)],
      tab$seedPropDep[which(tab$res == 10)],
      ylim = c(0.1, 1),
      pch = '',
      xlab = 'parameter a',
      ylab = 'Seed Proportion on the pixel'
    )
    for (i in 1:11) {
      m = lm(tab$seedPropDep[which(tab$res == res[i])] ~ tab$a[which(tab$res ==
                                                                       res[i])] + I(tab$a[which(tab$res == res[i])] ^ 2))
      points(tab$a[which(tab$res == res[i])],
             predict(m),
             col = i,
             pch = i,
             cex = 0.85)
    }
    legend(
      'bottomleft',
      legend = res,
      lty = 1,
      col = 1:11,
      pch = 1:11,
      cex = 0.95
    )
    
    #parameter b
    res = seq(from = 10, to = 30, by = 2)
    plot(
      tab$b[which(tab$res == 10)],
      tab$seedPropDep[which(tab$res == 10)],
      ylim = c(0.1, 1),
      pch = '',
      xlab = 'parameter b',
      ylab = 'Seed Proportion on the pixel'
    )
    for (i in 1:11) {
      m = lm(tab$seedPropDep[which(tab$res == res[i])] ~ tab$b[which(tab$res ==
                                                                       res[i])] + I(tab$b[which(tab$res == res[i])] ^ 2))
      points(tab$b[which(tab$res == res[i])],
             predict(m),
             col = i,
             pch = i,
             cex = 0.85)
      lines(tab$b[which(tab$res == res[i])], predict(m), col = i)
    }
    legend(
      'bottomright',
      legend = res,
      lty = 1,
      pch = 1:11,
      col = 1:11,
      cex = 0.95
    )
    
    #selecting the best parameter (and resolution)
    summary(tab)
    summary(tab[which(tab$seedPropDep > 0.70 &
                        tab$seedPropDep < 0.80 &
                        tab$res >= 18 & tab$res <= 24), ])
    summary(tab[which(tab$mean < 30 & tab$dist99 > 65), ])
    
    
  }
}

#Dispersion Type
{  
  setwd(paste(myWD, 'Willian/R/', sep = ''))
  dat <- read.table(
    'dispType.txt',
    head = T,
    dec = ',',
    na.strings = 'NA'
  )
  names(dat)
  
  reg <- lm(mean ~ h + m + BfMode + BeMode + FrMode + D3, dat)
  summary(reg)
  reg <- lm(mean ~ h + m + BfMode + BeMode + dia_type + vterm, dat)
  summary(reg)
  
  
  plot(dat$m, dat$dia_mass)
  text(
    dat$m,
    dat$dia_mass,
    labels = dat$sp,
    pos = 1,
    offset = 0.5,
    cex = 0.7
  )
  
  par(mfrow = c(2, 2), mar = c(2, 4, 2.5, 1))
  a <-
    boxplot(
      dat$mean ~ dat$BfMode,
      varwidth = T,
      main = 'Base flor',
      ylim = c(0, 8),
      ylab = 'Mean distance (m)'
    )
  text(1:length(a$n), a$stats[5, ] + 1, paste("n =", a$n), cex = 0.75)
  b <- boxplot(
    dat$mean ~ dat$BeMode,
    varwidth = T,
    main = 'Benoit',
    ylim = c(0, 8)
  )
  text(1:length(b$n), b$stats[5, ] + 1, paste("n =", b$n), cex = 0.75)
  c <-
    boxplot(
      dat$mean ~ dat$D3,
      varwidth = T,
      main = 'D3',
      ylim = c(0, 8),
      ylab = 'Mean distance (m)'
    )
  text(1:length(c$n), c$stats[5, ] + 1, paste("n =", c$n), cex = 0.75)
  d <- boxplot(
    dat$mean ~ dat$FrMode,
    varwidth = T,
    main = 'Fried',
    ylim = c(0, 8)
  )
  text(1:length(d$n), d$stats[5, ] + 1, paste("n =", d$n), cex = 0.75)
  
}

#Simulation plan
{
  WDmodel = paste(myWD, "Willian/R/dynWeed/Model/", sep = '')
  WDsimul = paste(myWD, "Willian/R/dynWeed/Tests/", sep = '')
  WDinput = paste(myWD, "Willian/R/dynWeed/", sep = '')
  WDoutput = paste(myWD, "Willian/R/dynWeed/output/", sep = '')
  
  # loading model
  source(paste(WDmodel, "DynWeed4.3.1.r", sep = ""))
  
  # loading function for parallel computation
  #__________________________________________________________________
  # utilities for parallel computation
  library(snowfall)
  
  # fonctions pour suivre l'avanc??e en parall??le
  # fonction de cr??ation et alimentation du log
  fillLog = function(k, WD = NULL) {
    CD = getwd()
    if (is.null(WD)) {
      WD = getwd()
    } else{
      setwd(WD)
    }
    if (length(dir()[dir() == "log"]) != 1)
      dir.create("log", showWarnings = F)
    if (substr(WD, nchar(WD), nchar(WD)) != "/")
      WD = paste(WD, "/", sep = "")
    setwd(paste(WD, "log", sep = ""))
    write.table(NA, paste("iteration", k, "done"))
    setwd(CD)
  }
  
  # fonction de nettoyage du log
  clearLog = function(WD = NULL) {
    CD = getwd()
    if (is.null(WD)) {
      WD = getwd()
    } else{
      setwd(WD)
    }
    if (length(dir()[dir() == "log"]) == 1) {
      if (substr(WD, nchar(WD), nchar(WD)) != "/")
        WD = paste(WD, "/", sep = "")
      setwd(paste(WD, "log", sep = ""))
      file.remove(dir())
    }
    setwd(CD)
  }
  
  
  # loading FieldMaps (raster)
  nbRst = 9
  RST = vector("list", nbRst)
  RST[[1]] = readGDAL(paste(
    WDinput,
    "Input/FieldMap1_raster/fieldmap1/w001001.adf",
    sep = ""
  ))
  RST[[2]] = readGDAL(paste(
    WDinput,
    "Input/FieldMap2_raster/fieldmap2/w001001.adf",
    sep = ""
  ))
  RST[[3]] = readGDAL(paste(
    WDinput,
    "Input/Fenay2013Etendu_raster/rstfenayet/w001001.adf",
    sep = ""
  ))
  RST[[4]] = readGDAL(paste(
    WDinput,
    "Input/FieldMap4_raster/fieldmap4/w001001.adf",
    sep = ""
  ))
  RST[[5]] = readGDAL(paste(
    WDinput,
    "Input/FieldMap5_raster/fieldmap5/w001001.adf",
    sep = ""
  ))
  RST[[6]] = readGDAL(paste(WDinput, "Input/FieldMap6_raster/w001001.adf", sep =
                              ""))
  RST[[7]] = readGDAL(paste(WDinput, "Input/FieldMap7_raster/w001001.adf", sep =
                              ""))
  RST[[8]] = readGDAL(paste(WDinput, "Input/FieldMap8_raster/w001001.adf", sep =
                              ""))
  RST[[9]] = readGDAL(paste(WDinput, "Input/FieldMap9_raster/w001001.adf", sep =
                              ""))
  
  for (k in 1:nbRst) {
    names(RST[[k]]@data)[1] = "Field"
  }
  # loading FieldMaps (shpaefile)
  nbShp = 9
  SHP = vector("list", nbShp)
  SHP[[1]] = readShp(paste(WDinput, "Input/FieldMap1_shapefile/FieldMap1.shp", sep =
                             ""))
  SHP[[2]] = readShp(paste(WDinput, "Input/FieldMap2_shapefile/FieldMap2.shp", sep =
                             ""))
  SHP[[3]] = readShp(paste(
    WDinput,
    "Input/Fenay2013Etendu_shapefile/FenayEtendu.shp",
    sep = ""
  ))
  SHP[[4]] = readShp(paste(WDinput, "Input/FieldMap4_shapefile/FieldMap4.shp", sep =
                             ""))
  SHP[[5]] = readShp(paste(WDinput, "Input/FieldMap5_shapefile/FieldMap5.shp", sep =
                             ""))
  SHP[[6]] = readShp(paste(WDinput, "Input/FieldMap6_shapefile/FieldMap6.shp", sep =
                             ""))
  SHP[[7]] = readShp(paste(WDinput, "Input/FieldMap7_shapefile/FieldMap7.shp", sep =
                             ""))
  SHP[[8]] = readShp(paste(WDinput, "Input/FieldMap8_shapefile/FieldMap8.shp", sep =
                             ""))
  SHP[[9]] = readShp(paste(WDinput, "Input/FieldMap9_shapefile/FieldMap9.shp", sep =
                             ""))
  
  # loading parameters
  paramTable = read.table(
    paste(WDinput, "Input/paramTable.txt", sep = ""),
    header = T,
    stringsAsFactors = F
  )
  iniTable = read.table(
    paste(WDinput, "Input/iniTableDisp.txt", sep = ""),
    header = T,
    stringsAsFactors = F
  )
  load(paste(WDinput, "Input/landscapeList.rdata", sep = ""))
  
  # chargement table de valeurs de ref pour calcul des scores
  IFT_SC = read.table(paste(WDinput, "Input/IFT_SC.txt", sep = ""), header =
                        T)
  
  # species names
  spList = c("WinterSmall", "WinterLarge", "SpringSmall", "SpringLarge")
  # paramet set
  #table 1
  nbS = 9 * 7 * 10 * 32
  listLU = c("Conv", "Org", "PM")
  param = data.frame(matrix(nrow = nbS, ncol = 12))
  names(param) = c(
    "Num",
    "Curve",
    "NameP1",
    "NameP2",
    "P1",
    "P2",
    "FieldMap",
    "MainLandUse",
    "MLUprop",
    "Spatial",
    "Rep",
    "Landscape"
  )
  param$Num = 1:nbS
  we = rep('Weibull', 10080)
  f2d = rep('f2dt', 10080)
  param$Curve = c(we, f2d)
  param$NameP1 = rep(c('m', 'a'), c(10080, 10080))
  param$NameP2 = rep(c('c', 'b'), c(10080, 10080))
  param$P1 = rep(c(4, 8, 12, 15, 5.5, 6.5, 8, 9),
                 c(2520, 2520, 2520, 2520, 2520, 2520, 2520, 2520))
  weibC = rep(c(1.2, 1.4, 1.5, 1.7), each = 630)
  weibCt = rep(weibC, 4)
  f2dC = rep(c(1.2, 1.3, 1.4, 1.5), each = 630)
  f2dCt = rep(f2dC, 4)
  param$P2 = c(weibCt, f2dCt)
  param$FieldMap = rep(1:9, each = 70)
  param$MainLandUse = rep(listLU, c(30, 30, 10))
  param$Rep = rep(1:10)
  param$Spatial = "Random"
  param$Landscape = rep(1:630)
  param$MLUprop[which(param$MainLandUse == 'PM')] = 1
  prop = c(1, 0.97, 0.5)
  param$MLUprop[which(param$MainLandUse == 'Conv')] = rep(prop, each = 10)
  param$MLUprop[which(param$MainLandUse == 'Org')] = rep(prop, each = 10)
  
  #dir.create(paste(WDoutput,'Step1',sep=''))
  #write.table(param,paste(WDoutput,"/Step1/param.txt",sep=""),sep="\t",row.names=F,quote=F)
  #setwd(paste(WDoutput,'Step1',sep=''))
  #param=read.table('param.txt',head=T)
  
  #__________________________________________________________________
  # identitifying the central field
  
  # for(kFM in 1:9){
  # x11()
  # RST[[kFM]]=rstCheckMatrix(RST[[kFM]])
  # map(RST[[kFM]],type="fieldShape",label=T,main=paste("FM",kFM),polyg=SHP[[kFM]])
  # }
  
  # #Create a list of landscape distribuitions to repeat at each step
  #landscapeList=as.list(rep(NA,nbS))
  # for(k in 1:nbS) {
  
  # kFM=param$FieldMap[k]
  
  # rst=rstCheckMatrix(RST[[kFM]])
  # rst=extendRst(rst,w=300,val=0)
  
  # MLU=param$MainLandUse[k]
  
  # LUnames=c("Conv","Org","DD","PM")
  # proportion=rep(0,4)
  
  # if(param$MLUprop[k] == 1) {
  # proportion[LUnames==MLU]=1
  # }else{
  # proportion[LUnames==MLU]=param$MLUprop[k]
  # proportion[LUnames=='PM']=1-param$MLUprop[k]
  # }
  
  # landscape=createLandscape(rst,LUnames,proportion)
  
  # while(centralLU!=MLU){
  # landscape=createLandscape(rst,LUnames,proportion)
  # centralLU=landscape$LandUse[landscape$Field==centralF[kFM]]
  # }
  
  # landscapeList[[k]]=landscape
  # progress(100*k/nbS)
  # }
  
  # save(landscapeList, file = "landscapeList.RData")
  
  
  centralF = c(177, 35, 136, 19, 270, 58, 317, 142, 250)
  
  #runing function
  runStep8 = function(k) {
    #for(k in 1:nbS) {
    
    kFM = param$FieldMap[k]
    
    rst = rstCheckMatrix(RST[[kFM]])
    rst = extendRst(rst, w = 300, val = 0)
    
    MLU = param$MainLandUse[k]
    
    LUnames = c("Conv", "Org", "DD", "PM")
    proportion = rep(0, 4)
    
    if (param$MLUprop[k] == 1) {
      proportion[LUnames == MLU] = 1
    } else{
      proportion[LUnames == MLU] = param$MLUprop[k]
      proportion[LUnames == 'PM'] = 1 - param$MLUprop[k]
    }
    
    j = param$Landscape[k]
    landscape = landscapeList[[j]]
    centralLU = landscape$LandUse[landscape$Field == centralF[kFM]]
    
    rst = initialize(rst, landscape, iniTable, spList, Fill = F)
    rst@data[, spList] = 0
    
    mat = asMat(rst)
    Ind = which(mat@data$Field == centralF[kFM], arr.ind = T)
    
    rMin = min(Ind[, 1])
    rMax = max(Ind[, 1])
    rC = round(rMin + (rMax - rMin) / 2, 0)
    
    cMin = min(Ind[, 2])
    cMax = max(Ind[, 2])
    cC = round(cMin + (cMax - cMin) / 2, 0)
    
    rNoC = seq(from = rC - 2,
               to = rC + 2,
               by = 1)
    cNoC = seq(from = cC - 2,
               to = cC + 2,
               by = 1)
    
    PA = rst@grid@cellsize[1] * rst@grid@cellsize[1]
    mat@data$WinterSmall[rNoC, cNoC] = PA * as.numeric(iniTable[iniTable$LandUse ==
                                                                  MLU, 2])
    mat@data$WinterLarge[rNoC, cNoC] = PA * as.numeric(iniTable[iniTable$LandUse ==
                                                                  MLU, 3])
    mat@data$SpringSmall[rNoC, cNoC] = PA * as.numeric(iniTable[iniTable$LandUse ==
                                                                  MLU, 4])
    mat@data$SpringLarge[rNoC, cNoC] = PA * as.numeric(iniTable[iniTable$LandUse ==
                                                                  MLU, 5])
    
    rst = asRst(mat)
    
    pCurve = list(param$P1[k], param$P2[k])
    names(pCurve) = c(param$NameP1[k], param$NameP2[k])
    
    set.seed(randomSeeds[k])
    dynWeed(
      rst = rst,
      RT = 50,
      spList = spList,
      iniType = "manual",
      LUnames = NULL,
      proportion = NULL,
      iniTable = NULL,
      landscape = landscape,
      paramTable = paramTable,
      drCurve = 1,
      typCurve = param$Curve[k],
      pCurve = pCurve,
      drMatrix = 0,
      pMatrix = NULL,
      w = 0,
      dyn = T,
      saveRdata = T,
      saveLandscapeMap = T,
      colorList = setLandUseColor(LUnames, proportion),
      saveSeedMap = T,
      WD = paste(WDoutput, "Step1/", sep = ""),
      simName = paste("Sim", k, sep = ""),
      RTlist = c(10, 20, 30, 50),
      overwrite = T,
      polyg = SHP[[kFM]],
      calcScore = T,
      IFT_SC = IFT_SC
    )
    #progress(100*k/nbS)
    fillLog(k, paste(WDoutput, "Step1/", sep = ""))
    return(k)
  }
  #__________________________________________________________________
  
  # set random seeds
  lr = 0
  while (lr != nbS) {
    randomSeeds = ceiling(runif(nbS, 0, 1000 * nbS))
    lr = length(unique(randomSeeds))
  }
  
  
  sfInit(parallel = T, cpus = 8)
  sfExport(list = ls(all = TRUE))
  sfLibrary(rgdal)
  sfLibrary(shapefiles)
  OUTPUT = sfClusterApplyLB(20161, runStep8)
  
  sfStop()
  library(snowfall)
  ?snowfall
  
  # nettoyage du dossier log
  clearLog(paste(WDoutput, "Step1/", sep = "")) #Change step
  
}

#Simulation plan 2
{
  WDmodel = paste(myWD, "Willian/R/dynWeed/Model/", sep = '')
  WDsimul = paste(myWD, "Willian/R/dynWeed/Tests/", sep = '')
  WDinput = paste(myWD, "Willian/R/dynWeed/", sep = '')
  WDoutput = paste(myWD, "Willian/R/dynWeed/output/", sep = '')
  
  # loading model
  source(paste(WDmodel, "DynWeed4.3.1.r", sep = ""))
  
  # loading function for parallel computation
  #__________________________________________________________________
  # utilities for parallel computation
  library(snowfall)
  
  # fonctions pour suivre l'avanc??e en parall??le
  # fonction de cr??ation et alimentation du log
  fillLog = function(k, WD = NULL) {
    CD = getwd()
    if (is.null(WD)) {
      WD = getwd()
    } else{
      setwd(WD)
    }
    if (length(dir()[dir() == "log"]) != 1)
      dir.create("log", showWarnings = F)
    if (substr(WD, nchar(WD), nchar(WD)) != "/")
      WD = paste(WD, "/", sep = "")
    setwd(paste(WD, "log", sep = ""))
    write.table(NA, paste("iteration", k, "done"))
    setwd(CD)
  }
  
  # fonction de nettoyage du log
  clearLog = function(WD = NULL) {
    CD = getwd()
    if (is.null(WD)) {
      WD = getwd()
    } else{
      setwd(WD)
    }
    if (length(dir()[dir() == "log"]) == 1) {
      if (substr(WD, nchar(WD), nchar(WD)) != "/")
        WD = paste(WD, "/", sep = "")
      setwd(paste(WD, "log", sep = ""))
      file.remove(dir())
    }
    setwd(CD)
  }
  
  
  # loading FieldMaps (raster)
  nbRst = 9
  RST = vector("list", nbRst)
  RST[[1]] = readGDAL(paste(
    WDinput,
    "Input/FieldMap1_raster/fieldmap1/w001001.adf",
    sep = ""
  ))
  RST[[2]] = readGDAL(paste(
    WDinput,
    "Input/FieldMap2_raster/fieldmap2/w001001.adf",
    sep = ""
  ))
  RST[[3]] = readGDAL(paste(
    WDinput,
    "Input/Fenay2013Etendu_raster/rstfenayet/w001001.adf",
    sep = ""
  ))
  RST[[4]] = readGDAL(paste(
    WDinput,
    "Input/FieldMap4_raster/fieldmap4/w001001.adf",
    sep = ""
  ))
  RST[[5]] = readGDAL(paste(
    WDinput,
    "Input/FieldMap5_raster/fieldmap5/w001001.adf",
    sep = ""
  ))
  RST[[6]] = readGDAL(paste(WDinput, "Input/FieldMap6_raster/w001001.adf", sep =
                              ""))
  RST[[7]] = readGDAL(paste(WDinput, "Input/FieldMap7_raster/w001001.adf", sep =
                              ""))
  RST[[8]] = readGDAL(paste(WDinput, "Input/FieldMap8_raster/w001001.adf", sep =
                              ""))
  RST[[9]] = readGDAL(paste(WDinput, "Input/FieldMap9_raster/w001001.adf", sep =
                              ""))
  
  for (k in 1:nbRst) {
    names(RST[[k]]@data)[1] = "Field"
  }
  # loading FieldMaps (shpaefile)
  nbShp = 9
  SHP = vector("list", nbShp)
  SHP[[1]] = readShp(paste(WDinput, "Input/FieldMap1_shapefile/FieldMap1.shp", sep =
                             ""))
  SHP[[2]] = readShp(paste(WDinput, "Input/FieldMap2_shapefile/FieldMap2.shp", sep =
                             ""))
  SHP[[3]] = readShp(paste(
    WDinput,
    "Input/Fenay2013Etendu_shapefile/FenayEtendu.shp",
    sep = ""
  ))
  SHP[[4]] = readShp(paste(WDinput, "Input/FieldMap4_shapefile/FieldMap4.shp", sep =
                             ""))
  SHP[[5]] = readShp(paste(WDinput, "Input/FieldMap5_shapefile/FieldMap5.shp", sep =
                             ""))
  SHP[[6]] = readShp(paste(WDinput, "Input/FieldMap6_shapefile/FieldMap6.shp", sep =
                             ""))
  SHP[[7]] = readShp(paste(WDinput, "Input/FieldMap7_shapefile/FieldMap7.shp", sep =
                             ""))
  SHP[[8]] = readShp(paste(WDinput, "Input/FieldMap8_shapefile/FieldMap8.shp", sep =
                             ""))
  SHP[[9]] = readShp(paste(WDinput, "Input/FieldMap9_shapefile/FieldMap9.shp", sep =
                             ""))
  
  # loading parameters
  paramTable = read.table(
    paste(WDinput, "Input/paramTable.txt", sep = ""),
    header = T,
    stringsAsFactors = F
  )
  iniTable = read.table(
    paste(WDinput, "Input/iniTableDisp.txt", sep = ""),
    header = T,
    stringsAsFactors = F
  )
  load(paste(WDinput, "Input/landscapeList.rdata", sep = ""))
  
  # chargement table de valeurs de ref pour calcul des scores
  IFT_SC = read.table(paste(WDinput, "Input/IFT_SC.txt", sep = ""), header =
                        T)
  
  # species names
  spList = c("WinterSmall", "WinterLarge", "SpringSmall", "SpringLarge")
  # paramet set
  
  #table 2
  nbS = 9 * 7 * 10 * 8
  listLU = c("Conv", "Org", "PM")
  param2 = data.frame(matrix(nrow = nbS, ncol = 12))
  names(param2) = c(
    "Num",
    "Curve",
    "NameP1",
    "NameP2",
    "P1",
    "P2",
    "FieldMap",
    "MainLandUse",
    "MLUprop",
    "Spatial",
    "Rep",
    "Landscape"
  )
  param2$Num = 20161:(20160 + nbS)
  ga = rep('Gaussian', 2520)
  ex = rep('Exponential', 2520)
  param2$Curve = c(ga, ex)
  param2$NameP1 = rep(c('sigma', 'Lambda'), c(2520, 2520))
  param2$NameP2 = rep(NA, length(nbS))
  param2$P1 = rep(c(10, 15, 20, 25, 10, 15, 20, 25),
                  c(630, 630, 630, 630, 630, 630, 630, 630))
  param2$P2 = rep(NA, length(nbS))
  param2$FieldMap = rep(1:9, each = 70)
  param2$MainLandUse = rep(listLU, c(30, 30, 10))
  param2$Rep = rep(1:10)
  param2$Spatial = "Random"
  param2$Landscape = rep(1:630)
  param2$MLUprop[which(param2$MainLandUse == 'PM')] = 1
  prop = c(1, 0.97, 0.5)
  param2$MLUprop[which(param2$MainLandUse == 'Conv')] = rep(prop, each = 10)
  param2$MLUprop[which(param2$MainLandUse == 'Org')] = rep(prop, each = 10)
  
  #dir.create(paste(WDoutput,'Step1',sep=''))
  #write.table(param,paste(WDoutput,"/Step1/param2.txt",sep=""),sep="\t",row.names=F,quote=F)
  #setwd(paste(WDoutput,'Step1',sep=''))
  #param2=read.table('param2.txt',head=T)
  
  #__________________________________________________________________
  # identitifying the central field
  
  # for(kFM in 1:9){
  # x11()
  # RST[[kFM]]=rstCheckMatrix(RST[[kFM]])
  # map(RST[[kFM]],type="fieldShape",label=T,main=paste("FM",kFM),polyg=SHP[[kFM]])
  # }
  
  # #Create a list of landscape distribuitions to repeat at each step
  #landscapeList=as.list(rep(NA,nbS))
  # for(k in 1:nbS) {
  
  # kFM=param$FieldMap[k]
  
  # rst=rstCheckMatrix(RST[[kFM]])
  # rst=extendRst(rst,w=300,val=0)
  
  # MLU=param$MainLandUse[k]
  
  # LUnames=c("Conv","Org","DD","PM")
  # proportion=rep(0,4)
  
  # if(param$MLUprop[k] == 1) {
  # proportion[LUnames==MLU]=1
  # }else{
  # proportion[LUnames==MLU]=param$MLUprop[k]
  # proportion[LUnames=='PM']=1-param$MLUprop[k]
  # }
  
  # landscape=createLandscape(rst,LUnames,proportion)
  
  # while(centralLU!=MLU){
  # landscape=createLandscape(rst,LUnames,proportion)
  # centralLU=landscape$LandUse[landscape$Field==centralF[kFM]]
  # }
  
  # landscapeList[[k]]=landscape
  # progress(100*k/nbS)
  # }
  
  # save(landscapeList, file = "landscapeList.RData")
  
  
  centralF = c(177, 35, 136, 19, 270, 58, 317, 142, 250)
  k = 1
  
  #runing function
  runStep8 = function(k) {
    #for(k in 1:2) {
    
    kFM = param2$FieldMap[k]
    
    rst = rstCheckMatrix(RST[[kFM]])
    rst = extendRst(rst, w = 300, val = 0)
    
    MLU = param2$MainLandUse[k]
    
    LUnames = c("Conv", "Org", "DD", "PM")
    proportion = rep(0, 4)
    
    if (param2$MLUprop[k] == 1) {
      proportion[LUnames == MLU] = 1
    } else{
      proportion[LUnames == MLU] = param2$MLUprop[k]
      proportion[LUnames == 'PM'] = 1 - param2$MLUprop[k]
    }
    
    j = param2$Landscape[k]
    landscape = landscapeList[[j]]
    centralLU = landscape$LandUse[landscape$Field == centralF[kFM]]
    
    rst = initialize(rst, landscape, iniTable, spList, Fill = F)
    rst@data[, spList] = 0
    
    mat = asMat(rst)
    Ind = which(mat@data$Field == centralF[kFM], arr.ind = T)
    
    rMin = min(Ind[, 1])
    rMax = max(Ind[, 1])
    rC = round(rMin + (rMax - rMin) / 2, 0)
    
    cMin = min(Ind[, 2])
    cMax = max(Ind[, 2])
    cC = round(cMin + (cMax - cMin) / 2, 0)
    
    rNoC = seq(from = rC - 2,
               to = rC + 2,
               by = 1)
    cNoC = seq(from = cC - 2,
               to = cC + 2,
               by = 1)
    
    PA = rst@grid@cellsize[1] * rst@grid@cellsize[1]
    mat@data$WinterSmall[rNoC, cNoC] = PA * as.numeric(iniTable[iniTable$LandUse ==
                                                                  MLU, 2])
    mat@data$WinterLarge[rNoC, cNoC] = PA * as.numeric(iniTable[iniTable$LandUse ==
                                                                  MLU, 3])
    mat@data$SpringSmall[rNoC, cNoC] = PA * as.numeric(iniTable[iniTable$LandUse ==
                                                                  MLU, 4])
    mat@data$SpringLarge[rNoC, cNoC] = PA * as.numeric(iniTable[iniTable$LandUse ==
                                                                  MLU, 5])
    
    rst = asRst(mat)
    
    pCurve = list(param2$P1[k])
    names(pCurve) = param2$NameP1[k]
    
    #	set.seed(randomSeeds[k])
    dynWeed(
      rst = rst,
      RT = 50,
      spList = spList,
      iniType = "manual",
      LUnames = NULL,
      proportion = NULL,
      iniTable = NULL,
      landscape = landscape,
      paramTable = paramTable,
      drCurve = 1,
      typCurve = param2$Curve[k],
      pCurve = pCurve,
      drMatrix = 0,
      pMatrix = NULL,
      w = 0,
      dyn = T,
      saveRdata = T,
      saveLandscapeMap = T,
      colorList = setLandUseColor(LUnames, proportion),
      saveSeedMap = T,
      WD = paste(WDoutput, "Step2/", sep = ""),
      simName = paste("Sim", param2$Num[k], sep = ""),
      RTlist = c(10, 20, 30, 50),
      overwrite = T,
      polyg = SHP[[kFM]],
      calcScore = T,
      IFT_SC = IFT_SC
    )
    #progress(100*k/nbS)
    fillLog(k, paste(WDoutput, "Step2/", sep = ""))
    #return(k)
  }
  
  
  #__________________________________________________________________
  
  # set random seeds
  lr = 0
  while (lr != nbS) {
    randomSeeds = ceiling(runif(nbS, 0, 1000 * nbS))
    lr = length(unique(randomSeeds))
  }
  
  
  sfInit(parallel = T, cpus = 8)
  sfExport(list = ls(all = TRUE))
  sfLibrary(rgdal)
  sfLibrary(shapefiles)
  OUTPUT = sfClusterApplyLB(seq(1, 5040), runStep8)
  
  sfStop()
  
  
  
  # nettoyage du dossier log
  clearLog(paste(WDoutput, "Step1/", sep = "")) #Change step
  
}
  
#pre analyse (data mining)
{
  WDmodel = paste(myWD, "Willian/R/dynWeed/Model/", sep = '')
  WDsimul = paste(myWD, "Willian/R/dynWeed/Tests/", sep = '')
  WDinput = paste(myWD, "Willian/R/dynWeed/", sep = '')
  WDoutput = paste(myWD, "Willian/R/dynWeed/output/", sep = '')
  
  load(paste(myWD, 'Willian/R/data/dat.Rdata', sep = ''))
  load(paste(myWD, 'Willian/R/data/parcClist.Rdata', sep = ''))
  
  source(paste(WDmodel, "DynWeed4.3.1.r", sep = ""))
  source(paste(myWD, 'Willian/R/Script/myNativeFunctions.r', sep = ""))
  
  table
  dat = param
  dat$WD = rep(NA, length(dat$MLUprop))
  dat$TDAF = rep(NA, length(dat$MLUprop))
  dat$TDMLU = rep(NA, length(dat$MLUprop))
  dat$TDPM = rep(NA, length(dat$MLUprop))
  dat$WSAF = rep(NA, length(dat$MLUprop))
  dat$WSMLU = rep(NA, length(dat$MLUprop))
  dat$WSPM = rep(NA, length(dat$MLUprop))
  dat$WLAF = rep(NA, length(dat$MLUprop))
  dat$WLMLU = rep(NA, length(dat$MLUprop))
  dat$WLPM = rep(NA, length(dat$MLUprop))
  dat$SSAF = rep(NA, length(dat$MLUprop))
  dat$SSMLU = rep(NA, length(dat$MLUprop))
  dat$SSPM = rep(NA, length(dat$MLUprop))
  dat$SLAF = rep(NA, length(dat$MLUprop))
  dat$SLMLU = rep(NA, length(dat$MLUprop))
  dat$SLPM = rep(NA, length(dat$MLUprop))
  
  #legend of data table
  #WD: weed diversity
  #TDAF: total weed density in all fields
  #TDMLU: total weed density in the main land use
  #TDPM: total weed density in PM
  #WSAF: WinterSmall seed density in all fields
  #WSMLU: WinterSmall seed density in the main land use
  #WSPM: WinterSmall seed density in PM
  #WLAF: WinterLarge all fields
  #WLMLU: WinterLarge main land use
  #WLPM: WinterLarge PM
  #SSAF: SpringSmall all fields
  #SSMLU: SpringSmall main land use
  #SSPM: SpringSmall PM
  #SLAF: SpringLarge all fields
  #SLMLU: SpringLarge main land use
  #SLPM: SpringLarge PM
  #dA: dispersal accumulated on time (number of fields with 0,1; 1 and 5% of the maximum seed quantity)
  #v: speed of seed dispersal in meters/year
  
  
  WDoutputStep1 = paste(myWD, "Willian/R/dynWeed/output/Step1/", sep = '')
  for (i in 20161:length(dat$Num)) {
    #load Simulation
    simName = paste('Sim', i, sep = '')
    load(paste(WDoutputStep1, simName, '/', simName, '.Rdata', sep = ''))
    dat$WD[i] = output$Score$WD[50]
    dat$TDAF[i] = sum(as.numeric(output$LUstat[[51]][which(output$LUstat[[51]]$LandUse == "AllFields"), c(3:6)]))
    dat$TDMLU[i] = sum(as.numeric(output$LUstat[[51]][which(output$LUstat[[51]]$LandUse == dat$MainLandUse[i]), c(3:6)]))
    if (dat$MLUprop[i] == 1) {
      dat$TDPM[i] = NA
    } else {
      dat$TDPM[i] = sum(as.numeric(output$LUstat[[51]][which(output$LUstat[[51]]$LandUse == 'PM'), c(3:6)]))
    }
    dat$WSAF[i] = as.numeric(output$LUstat[[51]][which(output$LUstat[[51]]$LandUse == "AllFields"), 3])
    dat$WSMLU[i] = as.numeric(output$LUstat[[51]][which(output$LUstat[[51]]$LandUse == dat$MainLandUse[i]), 3])
    if (dat$MLUprop[i] == 1) {
      dat$WSPM[i] = NA
    } else {
      dat$WSPM[i] = as.numeric(output$LUstat[[51]][which(output$LUstat[[51]]$LandUse == 'PM'), 3])
    }
    dat$WLAF[i] = as.numeric(output$LUstat[[51]][which(output$LUstat[[51]]$LandUse == "AllFields"), 4])
    dat$WLMLU[i] = as.numeric(output$LUstat[[51]][which(output$LUstat[[51]]$LandUse == dat$MainLandUse[i]), 4])
    if (dat$MLUprop[i] == 1) {
      dat$WLPM[i] = NA
    } else {
      dat$WLPM[i] = as.numeric(output$LUstat[[51]][which(output$LUstat[[51]]$LandUse == 'PM'), 4])
    }
    dat$SSAF[i] = as.numeric(output$LUstat[[51]][which(output$LUstat[[51]]$LandUse == "AllFields"), 5])
    dat$SSMLU[i] = as.numeric(output$LUstat[[51]][which(output$LUstat[[51]]$LandUse == dat$MainLandUse[i]), 5])
    if (dat$MLUprop[i] == 1) {
      dat$SSPM[i] = NA
    } else {
      dat$SSPM[i] = as.numeric(output$LUstat[[51]][which(output$LUstat[[51]]$LandUse == 'PM'), 5])
    }
    dat$SLAF[i] = as.numeric(output$LUstat[[51]][which(output$LUstat[[51]]$LandUse == "AllFields"), 6])
    dat$SLMLU[i] = as.numeric(output$LUstat[[51]][which(output$LUstat[[51]]$LandUse == dat$MainLandUse[i]), 6])
    if (dat$MLUprop[i] == 1) {
      dat$SLPM[i] = NA
    } else {
      dat$SLPM[i] = as.numeric(output$LUstat[[51]][which(output$LUstat[[51]]$LandUse == 'PM'), 6])
    }
    
    progress(100 * i / length(dat$Num))
  }
  
  
  #Get the distance from the central Field
  
  CF = c(177, 35, 136, 19, 270, 58, 317, 142, 250)
  for (i in 1:9) {
    polyg = SHP[[i]]
    colName = 'IDfield'
    centralF = CF[i]
    
    fieldList = sort(polyg$data[, colName])
    nbF = length(fieldList)
    DCF = data.frame(matrix(nrow = nbF, ncol = 2))
    colnames(DCF) = c('IDField', 'centralF')
    for (k2 in 1:nbF) {
      id1 = centralF
      field1 = polyg$shape[[which(polyg$data[, colName] == id1)]]
      id2 = fieldList[k2]
      field2 = polyg$shape[[which(polyg$data[, colName] == id2)]]
      
      DCF[k2, 2] = distPolyg(field1, field2)
      DCF[k2, 1] = id2
    }
    assign(paste('DCF', i, sep = ''), DCF)
    progress(100 * i / 9)
  }
  
  distCentralF = list(DCF1, DCF2, DCF3, DCF4, DCF5, DCF6, DCF7, DCF8, DCF9)
  save(distCentralF, file = 'distCentralF.Rdata')
  
  load(paste(myWD, 'D:/Willian/R/data/distCentralF.Rdata', sep = ''))
  for (k in 1:9) {
    assign(paste("DCF", k, sep = ""), distCentralF[[k]])
  }
  
  #accumulated dispersion
  
  dat$dA = rep(NA, length(dat$Field))
  dispClist = vector("list", length(dat$Field))
  WDoutputStep1 = "D:/Willian/R/dynWeed/output/Step1/"
  nm = rep(NA, 51)
  for (a in 1:51) {
    nm[a] = paste('TG', a, sep = '')
  }
  
  for (i in 1:length(dat$Num)) {
    #load Simulation
    simName = paste('Sim', i, sep = '')
    load(paste(WDoutputStep1, simName, '/', simName, '.Rdata', sep = ''))
    
    nbF = length(distCentralF[[dat$Field[i]]]$IDField)
    dispC = data.frame(matrix(NA, nrow = nbF, ncol = 53))
    names(dispC) = c(nm, 'IDField', 'centralF')
    dispC$IDField = distCentralF[[dat$Field[i]]]$IDField
    dispC$centralF = distCentralF[[dat$Field[i]]]$centralF
    
    for (j in 1:51) {
      for (l in 1:nbF) {
        dispC[which(dispC$IDField == l), j] = sum(
          output$fieldStat[[j]]$WinterSmallS[which(output$fieldStat[[j]]$Field == l)],
          output$fieldStat[[j]]$WinterLargeS[which(output$fieldStat[[j]]$Field == l)],
          output$fieldStat[[j]]$SpringSmallS[which(output$fieldStat[[j]]$Field == l)],
          output$fieldStat[[j]]$SpringLargeS[which(output$fieldStat[[j]]$Field == l)]
        )
      }
    }
    
    #organizing the table
    dispCo = dispC[order(dispC$centralF), ]
    
    #getting the accumulated dispersion
    #0,1% of the max seed density
    ma = max(dispCo, na.rm = TRUE)
    ma1 = ma / 100
    
    #table over time with the number of fields with more than 'ma1'
    tim = seq(1, 50, 1)
    nbFc = rep(NA, 50)
    parcC = data.frame(tim, nbFc)
    for (k in 1:50) {
      l = k + 2
      parcC[k, 2] = length(dispC$IDField[which(dispC[, l] > ma1)])
    }
    
    #time where 'ma1' arrived in 95% of the fields
    nbF95 = nbF * 0.95
    td = parcC$tim[which(abs(parcC$nbFc - nbF95) == min(abs(parcC$nbFc - nbF95)))]
    tdisp = td[1]
    dat$dA[i] = tdisp
    
    #saving the dispC table
    dispClist[[i]] = dispC
    
    #progress
    progress(100 * i / length(dat$Num))
  }
  
  
  #getting the dA for differents % (0,1%; 1%; 5%)
  for (i in 1:length(dat$Num)) {
    dispCo = dispClist[[i]][order(dispClist[[i]]$centralF), ]
    ma = max(dispCo[, c(1:51)], na.rm = TRUE)
    ma01 = ma * 0.001
    ma1 = ma * 0.01
    ma5 = ma * 0.05
    
    Time = seq(1, 51, 1)
    nbFc01 = rep(NA, 51)
    nbFc1 = rep(NA, 51)
    nbFc5 = rep(NA, 51)
    parcC = data.frame(Time, nbFc01, nbFc1, nbFc5)
    
    for (k in 1:51) {
      parcC[k, 2] = length(dispClist[[i]]$IDField[which(dispClist[[i]][, k] > ma01)])
      parcC[k, 3] = length(dispClist[[i]]$IDField[which(dispClist[[i]][, k] > ma1)])
      parcC[k, 4] = length(dispClist[[i]]$IDField[which(dispClist[[i]][, k] > ma5)])
    }
    
    nbF = length(dispClist[[i]]$IDField)
    nbF95 = nbF * 0.95
    td01 = parcC$Time[which(abs(parcC$nbFc01 - nbF95) == min(abs(parcC$nbFc01 -
                                                                   nbF95)))]
    tdisp01 = td01[1]
    dat$dA01[i] = tdisp01
    td1 = parcC$Time[which(abs(parcC$nbFc1 - nbF95) == min(abs(parcC$nbFc1 -
                                                                 nbF95)))]
    tdisp1 = td1[1]
    dat$dA1[i] = tdisp1
    td5 = parcC$Time[which(abs(parcC$nbFc5 - nbF95) == min(abs(parcC$nbFc5 -
                                                                 nbF95)))]
    tdisp5 = td5[1]
    dat$dA5[i] = tdisp5
    
    progress(100 * i / length(dat$Num))
  }
  
  
  #saving dA table (for graphic)
  
  parcClist = vector("list", length(dat$Num))
  for (i in 1:length(dat$Num)) {
    dispCo = dispClist[[i]][order(dispClist[[i]]$centralF), ]
    ma = max(dispCo[, c(1:51)], na.rm = TRUE)
    ma01 = ma * 0.001
    ma1 = ma * 0.01
    ma5 = ma * 0.05
    
    Time = seq(1, 51, 1)
    nbFc01 = rep(NA, 51)
    nbFc1 = rep(NA, 51)
    nbFc5 = rep(NA, 51)
    parcC = data.frame(Time, nbFc01, nbFc1, nbFc5)
    
    for (k in 1:51) {
      parcC[k, 2] = length(dispClist[[i]]$IDField[which(dispClist[[i]][, k] > ma01)])
      parcC[k, 3] = length(dispClist[[i]]$IDField[which(dispClist[[i]][, k] > ma1)])
      parcC[k, 4] = length(dispClist[[i]]$IDField[which(dispClist[[i]][, k] > ma5)])
    }
    
    parcClist[[i]] = parcC
    progress(100 * i / length(dat$Num))
  }
  
  #save(parcClist,file='parcClist.Rdata')
  #save(dispClist,file='dispClist.Rdata')
  #write.table(dat,paste(WDoutput,"/Step1/dat.txt",sep=""),sep="\t",row.names=F,quote=F)
  #dat=read.table('D:/Willian/R/dynWeed/output//Step1/dat.txt',head=T)
  save(dat, file = 'dat.Rdata')
  
  
  # getting speed of dispersal (distance/time)
  
  #mean distance of the 4 further fields
  load('distCentralF.Rdata')
  furtherDist <- data.frame(matrix(NA, nrow = 9, ncol = 2))
  names(furtherDist) <- c('FieldMap', 'dist')
  for (i in 1:9) {
    furtherDist[i, 1] = i
    furtherDist[i, 2] = max(distCentralF[[i]]$centralF)
    
  }
  
  dat$v01 <- as.vector(rep(NA, length(dat$Num)))
  dat$v1 <- as.vector(rep(NA, length(dat$Num)))
  dat$v5 <- as.vector(rep(NA, length(dat$Num)))
  for (i in 1:length(dat$Num)) {
    dat$v01[i] = furtherDist$dist[which(furtherDist$FieldMap == dat$FieldMap[i])] /
      dat$dA01[i]
    dat$v1[i] = furtherDist$dist[which(furtherDist$FieldMap == dat$FieldMap[i])] /
      dat$dA1
    dat$v5[i] = furtherDist$dist[which(furtherDist$FieldMap == dat$FieldMap[i])] /
      dat$dA5
  }
  
  boxplot(dat$v01[which(dat$MainLandUse == 'PM')] ~ dat$Curve[which(dat$MainLandUse == 'PM')])
  
  #Weed Diversity and Seed accumulated on time
  
  WDSeed = vector("list", length(dat$Field))
  WDoutputStep1 = "D:/Willian/R/dynWeed/output/Step1/"
  
  for (i in 23419:length(dat$Num)) {
    #load Simulation
    simName = paste('Sim', i, sep = '')
    load(paste(WDoutputStep1, simName, '/', simName, '.Rdata', sep = ''))
    
    tempTab = data.frame(matrix(NA, nrow = 50, ncol = 1))
    names(tempTab) = c('WD')
    tempTab$WD = output$Score$WD
    tempTab$WinterSmallS = output$Score$WinterSmallS
    tempTab$WinterLargeS = output$Score$WinterLargeS
    tempTab$SpringSmallS = output$Score$SpringSmallS
    tempTab$SpringLargeS = output$Score$SpringLargeS
    tempTab$allSeed = output$Score$WinterSmallS + output$Score$WinterLargeS +
      output$Score$SpringSmallS + output$Score$SpringLargeS
    
    WDSeed[[i]] = tempTab
    
    #progress
    progress(100 * i / length(dat$Num))
  }
  
  #save(WDSeed,file='WDSeed.Rdata')
  load(paste(myWD, 'Willian/R/data/WDSeed.Rdata', sep = ''))
  
  
  
  for (i in 1:length(dat$Num)) {
    simName = paste('Sim', i, sep = '')
    load(paste(WDoutputStep1, simName, '/', simName, '.Rdata', sep = ''))
    
    temp_dat = data.frame(matrix(NA, nrow = 50, ncol = 2))
    names(temp_dat) = c('TDMLU', 'TDPM')
    for (j in 2:51) {
      J = j - 1
      temp_dat$TDMLU[J] = sum(as.numeric(output$LUstat[[j]][which(output$LUstat[[j]]$LandUse == dat$MainLandUse[i]), c(3:6)]))
    }
    if (dat$MLUprop[i] == 1) {
      dat$TDPM[i] = NA
    } else {
      for (k in 2:51) {
        K = k - 1
        temp_dat$TDPM[K] = sum(as.numeric(output$LUstat[[k]][which(output$LUstat[[k]]$LandUse == 'PM'), c(3:6)]))
      }
    }
    
    WDSeed[[i]]$TDMLU = temp_dat$TDMLU
    WDSeed[[i]]$TDPM = temp_dat$TDPM
    
    progress(i * 100 / length(dat$Num))
  }
  
  
  #graphic
  
  plot(dispCo$centralF,
       dispCo$TG50,
       pch = '',
       main = s)
  for (i in seq(1, 50, by = 5)) {
    j = i + 2
    lines(dispCo$centralF, dispCo[, j], col = i)
  }
  legend(
    'bottomright',
    legend = seq(1, 50, 5),
    lty = 1,
    col = seq(1, 50, 5),
    cex = 0.8
  )
  
  
}

#Data analyse
{
  #mining
  {
  WDmodel = paste(myWD, "Willian/R/dynWeed/Model/", sep = '')
  WDsimul = paste(myWD, "Willian/R/dynWeed/Tests/", sep = '')
  WDinput = paste(myWD, "Willian/R/dynWeed/", sep = '')
  WDoutput = paste(myWD, "Willian/R/dynWeed/output/", sep = '')
  
  load(paste(myWD, 'Willian/R/data/dat.Rdata', sep = ''))
  load(paste(myWD, 'Willian/R/data/parcClist.Rdata', sep = ''))
  
  source(paste(WDmodel, "DynWeed4.3.1.r", sep = ""))
  source(paste(myWD, 'Willian/R/Script/myNativeFunctions.r', sep = ''))
  }
  
  #PCA
  {
  #accumulated dispersion, Weed diversity and Number of seed for each curve
  library(ggplot2)
  
  load(paste(myWD, "Willian/R/data/distCentralF.Rdata", sep = ""))
  load(paste(myWD, "Willian/R/data/parcClist.Rdata", sep = ""))
  
  #data selection for PCA Analysis
  #PM
  dat.PM = dat[which(dat$MLUprop == c(0.97, 0.5)), ]
  
  #Without PM
  dat.spm = dat[, c(seq(1, 15), 17, 18, 19, 20, 22, 23, 25, 26, 29, 30, 31)]
  
  dat.p = dat.spm[, c(2, 3, 4, 5, 6, 7, 8, 9, 11, 12, c(13:23), 25)]
  dat.pa = dat.p[, c(11:22)]
  dat.pa1 = dat.pa[, c(1, 2, 3, 12)]
  
  # apply PCA
  dat.pca = prcomp(dat.pa1, center = TRUE, scale. = TRUE)
  
  # plot method
  plot(dat.pca, type = "l")
  
  summary(dat.pca)
  dat.pca$rotation
  
  #	ing the outliers
  bxpdat <- boxplot(dat.pca$rotation[, c(1, 2)])
  text(bxpdat$group,
       bxpdat$out,
       rownames(dat.pca$rotation[, c(1, 2)])[which(dat.pca$rotation[, c(1, 2)] == bxpdat$out, arr.ind =
                                                     TRUE)[, 1]],
       pos = 4)
  
  #comp 1 vs comp 2
  plot(dat.pca$rotation[, 1], dat.pca$rotation[, 2])
  #identify(dat.pca$rotation[,1],dat.pca$rotation[,2], labels=row.names(dat.pca$rotation))
  
  #plot
  install.packages('devtools')
  library(devtools)
  install_github("ggthemes", "vqv/ggthemes")
  library(ggbiplot)
  
  g <- ggbiplot("data",
                groups = "Groups",
                ellipse = TRUE,
                circle = TRUE)
  #g <- g + scale_color_manual(values=c('brown4','aquamarine4','darkolivegreen2'))
  # g <- g + scale_color_discrete(name = '')
  # g <- g + theme(legend.direction = 'horizontal',
  # legend.position = 'top')
  
  g <- g + theme_bw()
  
  # g <- g + geom_rangeframe() +
  # theme_tufte() +
  # scale_x_continuous(breaks = extended_range_breaks()(mtcars$wt)) +
  # scale_y_continuous(breaks = extended_range_breaks()(mtcars$mpg))
  
  print(g)
  
  #=====================================================================
  #PCA for separated groups (11 different groups)
  
  #data without PM
  dat.spm = dat[, c(seq(1, 15), 17, 18, 19, 20, 22, 23, 25, 26, 29, 30, 31)]
  #data with PM
  dat.p = dat.spm[, c(2, 3, 4, 5, 6, 7, 8, 9, 11, 12, c(13:23), 25)]
  
  nm = 'group 1 - Conv 50%'
  dg1 = dat.p[which(dat.spm$MainLandUse == 'Conv' &
                      dat.spm$MLUprop == 0.5), ]
  dg1a = dg1[, c(11:22)]
  dg1b = dg1a[, c(1, 2, 3, 12)]
  
  nm = 'group 2 - Org 50%'
  dg2 = dat.p[which(dat.spm$MainLandUse == 'Org' &
                      dat.spm$MLUprop == 0.5), ]
  dg2a = dg2[, c(11:22)]
  dg2b = dg2a[, c(1, 2, 3, 12)]
  
  nm = 'group 3 - Conv 97%'
  dg3 = dat.p[which(dat.spm$MainLandUse == 'Conv' &
                      dat.spm$MLUprop == 0.97), ]
  dg3a = dg3[, c(11:22)]
  dg3b = dg3a[, c(1, 2, 3, 12)]
  
  nm = 'group 4 - Org 97%'
  dg4 = dat.p[which(dat.spm$MainLandUse == 'Org' &
                      dat.spm$MLUprop == 0.97), ]
  dg4a = dg4[, c(11:22)]
  dg4b = dg4a[, c(1, 2, 3, 12)]
  
  nm = 'group 5 - Conv 100%'
  dg5 = dat.p[which(dat.spm$MainLandUse == 'Conv' &
                      dat.spm$MLUprop == 1), ]
  dg5a = dg5[, c(11:22)]
  dg5b = dg5a[, c(1, 2, 3, 12)]
  
  nm = 'group 6 - Org 100%'
  dg6 = dat.p[which(dat.spm$MainLandUse == 'Org' &
                      dat.spm$MLUprop == 1), ]
  dg6a = dg6[, c(11:22)]
  dg6b = dg6a[, c(1, 2, 3, 12)]
  
  nm = 'group 7 - Org'
  dg7 = dat.p[which(dat.spm$MainLandUse == 'Org'), ]
  dg7a = dg7[, c(11:22)]
  dg7b = dg7a[, c(1, 2, 3, 12)]
  
  nm = 'group 8 - Conv'
  dg8 = dat.p[which(dat.spm$MainLandUse == 'Conv'), ]
  dg8a = dg8[, c(11:22)]
  dg8b = dg8a[, c(1, 2, 3, 12)]
  
  nm = 'group 9 - 100%'
  dg9 = dat.p[which(dat.spm$MLUprop == 1), ]
  dg9a = dg9[, c(11:22)]
  dg9b = dg9a[, c(1, 2, 3, 12)]
  
  nm = 'group 10 - 97%'
  dg10 = dat.p[which(dat.spm$MLUprop == 0.97), ]
  dg10a = dg10[, c(11:22)]
  dg10b = dg10a[, c(1, 2, 3, 12)]
  
  nm = 'group 11 - 50%'
  dg11 = dat.p[which(dat.spm$MLUprop == 0.5), ]
  dg11a = dg11[, c(11:22)]
  dg11b = dg11a[, c(1, 2, 3, 12)]
  }
  
  #pirate plot (violin plot)
  {
    library("yarrr")
  
  x11(width = 4.5, height = 4)
  par(family = 'serif',
      mar = c(2, 3.4, 1, 0) + .5,
      cex = 0.8)
  pirateplot(
    formula = v01 ~ MLUprop,
    data =  dat[which(dat$MainLandUse != "PM"),],
    main = "Proportion of permanent meadows",
    pal = 'black',
    xlab = "",
    ylab = "Propagation speed (m/year)",
    point.pch = 16,
    point.o = .1,
    hdi.o = .6,
    bar.o = 0,
    line.o = .5
  )
  box()
  }
  
  #Anova for the groups
  {
  library(car)
  
  #data without PM
  dat.spm <-
    dat[, c(seq(1, 15), 17, 18, 19, 20, 22, 23, 25, 26, 29, 30, 31, 32, 33)]
  dat.spm <- dat.spm[which(dat$MainLandUse != "PM"),]
  
  #data with PM
  dat.p <-
    dat.spm[, c(2, 3, 4, 5, 6, 7, 8, 9, 11, 12, c(13:23), 25)]
  
  # Number of seeds - All fields
  # Call anova - everything
  avTDAF <-
    Anova(
      lm1 <- lm(
        TDAF ~ Curve + MainLandUse + MLUprop + FieldMap +
          Curve * MainLandUse + Curve * MLUprop + Curve * FieldMap +
          MainLandUse * FieldMap + FieldMap * MLUprop,
        data = dat.spm
      ),
      singular.ok = FALSE,
      type = "III"
    )
  plot(lm1, which = 2)
  avTDAF
  summary(lm1)
  om <- calcOmegaSq(avTDAF); om
  shapiro.test(sample(dat$TDAF, 5000))
  
  # Number of Seeds - MLU
  # Call anova
  avTDAF1 <-
    Anova(
      lm1 <- lm(
        TDMLU ~ Curve + MainLandUse + MLUprop + FieldMap +
          Curve * MainLandUse + Curve * MLUprop + Curve * FieldMap +
          MainLandUse * FieldMap + FieldMap * MLUprop,
        data = dat.spm
      ),
      singular.ok = FALSE,
      type = "III"
    )
  plot(lm1, which = 2)
  avTDAF1
  summary(lm1)
  om1 <- calcOmegaSq(anovaTable = avTDAF1); om1
  shapiro.test(sample(dat$TDMLU, 5000))
  
  # Weed diversity
  # Call anova
  avTDAF2 <-
    Anova(
      lm1 <- lm(
        sqrt(WD) ~ Curve + MainLandUse + MLUprop + FieldMap +
          Curve * MainLandUse + Curve * MLUprop + Curve * FieldMap +
          MainLandUse * FieldMap + FieldMap * MLUprop,
        data = dat.spm
      ),
      singular.ok = FALSE,
      type = "III"
    )
  plot(lm1, which = 2)
  avTDAF2
  summary(lm1)
  om2 <- calcOmegaSq(anovaTable = avTDAF2)
  shapiro.test(sample(dat$WD, 5000))
  
  # Propagation speed
  # Call anov
  avTDAF3 <-
    Anova(
      lm1 <- lm(
        v01 ~ Curve + MainLandUse + MLUprop + FieldMap +
          Curve * MainLandUse + Curve * MLUprop + Curve * FieldMap +
          MainLandUse * FieldMap + FieldMap * MLUprop,
        data = dat.spm
      ),
      singular.ok = FALSE,
      type = "III"
    )
  plot(lm1, which = 2)
  avTDAF3
  summary(lm1)
  om3 <- calcOmegaSq(anovaTable = avTDAF3)
  shapiro.test(sample(log(dat$v01), 5000))
  
  #Test for MLUprop and curve
  kruskal.test(dat$v01 ~ dat$MLUprop, data = dat.spm)
  kruskal.test(dat$v01 ~ dat$Curve, data = dat.spm)
  
  #medians
    #propagation speed
  bx <- boxplot(dat.spm$v01[which(dat.spm$MLUprop == 0.5)] ~ dat.spm$Curve[which(dat.spm$MLUprop == 0.5)])
  bx$stats[3,] #median
  
    #number of seed
      #MLU
  median(c(median(dat$TDMLU[which(dat$MainLandUse == "PM")]), median(dat$TDPM[which(dat$MLUprop == c(0.97, 0.5))]))) #PM
  median(dat$TDMLU[which(dat$MainLandUse == "Conv")]) #Conv
  median(dat$TDMLU[which(dat$MainLandUse == "Org")]) #Org
      #MLUprop
  median(dat$TDAF[which(dat$MLUprop == 0.97 & dat$MainLandUse == "Conv")])
  median(dat$TDAF[which(dat$MLUprop == 0.97 & dat$MainLandUse == "Org")])
  
  median(dat$TDAF[which(dat$MLUprop == 0.5 & dat$MainLandUse == "Conv")])
  median(dat$TDAF[which(dat$MLUprop == 0.5 & dat$MainLandUse == "Org")])
  }
      #curve
  median(dat$TDAF[which(dat$Curve == "Exponential")])
  median(dat$TDAF[which(dat$Curve == "f2dt")])
  median(dat$TDAF[which(dat$Curve == "Gaussian")])
  median(dat$TDAF[which(dat$Curve == "Weibull")])
  
  #Landscape correlation
  {
  FMinf <- read.table(paste(myWD, "Willian/R/data/FMinf.txt", sep = ""), head = TRUE)
  FMinfo <- FMinf[order(FMinf$nbF), ] #ordering by nbF
  FMinfo2 <- FMinf[order(FMinf$meanArea), ] #ordering by meanArea
  
  #data frame for Weed diversity and Number of seed
  FMWD <- data.frame(matrix(NA, ncol = 9, nrow = 400))
  FMTDAF <- data.frame(matrix(NA, ncol = 9, nrow = 400))
  
  for (i in 1:9) {
    Sim = g1$Num[which(g1$FieldMap == i)]
    j = 1
    for (j in 1:length(Sim)) {
      s = Sim[j]
      FMWD[j, i] = g1$WD[which(g1$Num == s)]
      FMTDAF[j, i] = g1$TDAF[which(g1$Num == s)]
    }
    progress(100 * i / 9)
  }
  FMWDo <- FMWD[, FMinfo$fieldMap]
  FMTDAFo <- FMTDAF[, FMinfo$fieldMap]
  
  plot(0,
       pch = '',
       xlim = c(200, 850),
       ylim = c(min(FMTDAF), max(FMTDAF)))
  for (i in 1:length(FMWDo$X1)) {
    lines(FMinf$nbF,
          FMTDAFo[i, ],
          col = addTrans('grey', 20),
          lwd = 2.5)
  }
  }
}

#Extras
{    
    {
      #facny plot
      library('plot3D')
      scatter3D_fancy <- function(x, y, z, ..., colvar = z)
      {
        panelfirst <- function(pmat) {
          XY <- trans3D(x, y, z = rep(min(z), length(z)), pmat = pmat)
          scatter2D(
            XY$x,
            XY$y,
            colvar = colvar,
            pch = ".",
            cex = 2,
            add = TRUE,
            colkey = FALSE
          )
          
          XY <- trans3D(x = rep(min(x), length(x)), y, z, pmat = pmat)
          scatter2D(
            XY$x,
            XY$y,
            colvar = colvar,
            pch = ".",
            cex = 2,
            add = TRUE,
            colkey = FALSE
          )
        }
        scatter3D(
          x,
          y,
          z,
          ...,
          colvar = colvar,
          panel.first = panelfirst,
          colkey = list(
            length = 0.5,
            width = 0.5,
            cex.clab = 0.75
          )
        )
      }
      #plot
      scatter3D_fancy(
        outTab$c,
        outTab$m,
        outTab$mean,
        pch = 16,
        ticktype = "detailed",
        theta = 45,
        d = 2,
        main = "Iris data",
        clab = c("Mean dispersal (cm)")
      )
      
      #TESTS (Regression)
      ##For Weibull data :
      setwd(paste(myWD, 'Willian/R/DynWeed/Dispersal/', sep = ''))
      tab = read.table('tab_weibull.txt', head = T)
      
      reg1 <- lm(seedPropDep ~ res + m + c, tab)
      summary(reg1)
      reg2 <- lm(mean ~ res + m + c, tab)
      summary(reg2)
      reg <- lm(dist99 ~ res + m + c, tab)
      summary(reg)
      
      #plots of the test
      #seed proportion
      ab = summary(reg1)
      bound <- data.frame(predict(reg1, int = 'c'))
      or <- order(bound$fit)
      plot(
        tab$seedPropDep[or],
        ylab = 'Seed proportion on the pixel',
        pch = '',
        ylim = c(min(bound), max(bound))
      )
      points(tab$seedPropDep[or], pch = 16, cex = 0.5)
      lines(bound$fit[or], col = "red", lwd = 2)
      lines(bound$lwr[or], lty = 3, col = "blue")
      lines(bound$upr[or], lty = 3, col = "blue")
      legend(
        "topleft",
        c("observation", "fitted curve", "bound"),
        cex = 1.2,
        col = c('black', 'red', 'blue'),
        lty = 1:3,
        lwd = 2,
        bty = "n"
      )
      r2 = round(ab$adj.r.squared, digits = 2)
      legend('bottomright', paste('R2 = ', r2, sep = ""))
      #mean
      ab = summary(reg2)
      bound <- data.frame(predict(reg2, int = 'c'))
      or <- order(bound$fit)
      plot(
        tab$mean[or],
        ylab = 'Mean dispersal',
        pch = '',
        ylim = c(min(bound), max(bound))
      )
      points(tab$mean[or], pch = 16, cex = 0.5)
      lines(bound$fit[or], col = "red", lwd = 2)
      lines(bound$lwr[or], lty = 3, col = "blue")
      lines(bound$upr[or], lty = 3, col = "blue")
      legend(
        "topleft",
        c("observation", "fitted curve", "bound"),
        cex = 1.2,
        col = c('black', 'red', 'blue'),
        lty = 1:3,
        lwd = 2,
        bty = "n"
      )
      r2 = round(ab$adj.r.squared, digits = 2)
      legend('bottomright', paste('R2 = ', r2, sep = ""))
      #Dist99
      ab = summary(reg)
      bound <- data.frame(predict(reg, int = 'c'))
      or <- order(bound$fit)
      plot(
        tab$dist99[or],
        ylab = 'Maximum distance',
        pch = '',
        ylim = c(min(bound), max(bound))
      )
      points(tab$dist99[or], pch = 16, cex = 0.5)
      lines(bound$fit[or], col = "red", lwd = 2)
      lines(bound$lwr[or], lty = 3, col = "blue")
      lines(bound$upr[or], lty = 3, col = "blue")
      legend(
        "topleft",
        c("observation", "fitted curve", "bound"),
        cex = 1.2,
        col = c('black', 'red', 'blue'),
        lty = 1:3,
        lwd = 2,
        bty = "n"
      )
      r2 = round(ab$adj.r.squared, digits = 2)
      legend('bottomright', paste('R2 = ', r2, sep = ""))
    }
    
    #Curve test plot
    
    {
      #Exponential
      f = function(x, l) {
        z = l * exp(-l * x)
        return(z)
      }
      x = seq(from = -150,
              to = 150,
              by = 1)
      plot(
        0,
        xlim = c(-150, 50),
        ylim = c(0, 1000),
        pch = ''
      )
      for (i in x) {
        l = 0.1
        z = f(i, l)
        points(i, z, cex = 0.6)
      }
      
      f1 = function(x, l) {
        z = 1 / (2 * pi * (l ^ 2)) * exp(-x / l)
        return(z)
      }
      #x=seq(from=-150,to=150,by=1)
      #plot(0,xlim=c(-150,150),ylim=c(0,300),pch='')
      for (i in x) {
        l = 7.5
        z = f1(i, l)
        points(i,
               z,
               col = 2,
               pch = 13,
               cex = 0.6)
      }
      
      f2 = function(x, l) {
        z = ((l ^ 2) / 2 * pi) * exp(-l * x)
        return(z)
      }
      #x=seq(from=-150,to=150,by=1)
      #plot(0,xlim=c(-150,50),ylim=c(0,1000),pch='')
      for (i in x) {
        l = 0.1
        z = f2(i, l)
        points(i, z, cex = 0.6, pch = 17)
      }
    }
    
    {
      #2Dt
      f = function(a, b, x) {
        z = (b - 1) / (pi * a) * (1 + ((x ^ 2) / a)) ^ (-b)
        return(z)
      }
      x = seq(from = -150,
              to = 150,
              by = 1)
      plot(0,
           xlim = c(-10, 10),
           ylim = c(0, 10),
           pch = '')
      for (i in x) {
        a = 15
        b = 400
        z = f(a, b, i)
        points(i, z, cex = 0.6)
      }
      
      f1 = function(a, b, x) {
        z = (((b - 2) * (b - 1)) / 2 * pi) * (1 + (x / a)) ^ (-b)
        return(z)
      }
      x = seq(from = -150,
              to = 150,
              by = 1)
      plot(
        0,
        xlim = c(-50, 50),
        ylim = c(0, 1000),
        pch = ''
      )
      for (i in x) {
        a = 10
        b = 15
        z = f1(a, b, i)
        points(i,
               z,
               col = 2,
               pch = 13,
               cex = 0.6)
      }
      
      f2 = function(x, l) {
        z = ((l ^ 2) / 2 * pi) * exp(-l * x)
        return(z)
      }
      #x=seq(from=-150,to=150,by=1)
      #plot(0,xlim=c(-150,50),ylim=c(0,1000),pch='')
      for (i in x) {
        l = 0.1
        z = f2(i, l)
        points(i, z, cex = 0.6, pch = 17)
      }
    }
    
    {
      #Animation GIF
      WDmodel = paste(myWD, "Willian/R/dynWeed/Model/", sep = '')
      WDsimul = paste(myWD, "Willian/R/dynWeed/Tests/", sep = '')
      WDinput = paste(myWD, "Willian/R/dynWeed/", sep = '')
      WDoutput = paste(myWD, "Willian/R/dynWeed/output/", sep = '')
      
      # loading model
      source(paste(WDmodel, "DynWeed4.3.1.r", sep = ""))
      
      
    # loading FieldMaps (raster)
    nbRst = 9
    RST = vector("list", nbRst)
    RST[[1]] = readGDAL(paste(
      WDinput,
      "Input/FieldMap1_raster/fieldmap1/w001001.adf",
      sep = ""
    ))
    RST[[2]] = readGDAL(paste(
      WDinput,
      "Input/FieldMap2_raster/fieldmap2/w001001.adf",
      sep = ""
    ))
    RST[[3]] = readGDAL(
      paste(
        WDinput,
        "Input/Fenay2013Etendu_raster/rstfenayet/w001001.adf",
        sep = ""
      )
    )
    RST[[4]] = readGDAL(paste(
      WDinput,
      "Input/FieldMap4_raster/fieldmap4/w001001.adf",
      sep = ""
    ))
    RST[[5]] = readGDAL(paste(
      WDinput,
      "Input/FieldMap5_raster/fieldmap5/w001001.adf",
      sep = ""
    ))
    RST[[6]] = readGDAL(paste(WDinput, "Input/FieldMap6_raster/w001001.adf", sep =
                                ""))
    RST[[7]] = readGDAL(paste(WDinput, "Input/FieldMap7_raster/w001001.adf", sep =
                                ""))
    RST[[8]] = readGDAL(paste(WDinput, "Input/FieldMap8_raster/w001001.adf", sep =
                                ""))
    RST[[9]] = readGDAL(paste(WDinput, "Input/FieldMap9_raster/w001001.adf", sep =
                                ""))
    
    for (k in 1:nbRst) {
      names(RST[[k]]@data)[1] = "Field"
    }
    # loading FieldMaps (shpaefile)
    nbShp = 9
    SHP = vector("list", nbShp)
    SHP[[1]] = readShp(paste(WDinput, "Input/FieldMap1_shapefile/FieldMap1.shp", sep =
                               ""))
    SHP[[2]] = readShp(paste(WDinput, "Input/FieldMap2_shapefile/FieldMap2.shp", sep =
                               ""))
    SHP[[3]] = readShp(paste(
      WDinput,
      "Input/Fenay2013Etendu_shapefile/FenayEtendu.shp",
      sep = ""
    ))
    SHP[[4]] = readShp(paste(WDinput, "Input/FieldMap4_shapefile/FieldMap4.shp", sep =
                               ""))
    SHP[[5]] = readShp(paste(WDinput, "Input/FieldMap5_shapefile/FieldMap5.shp", sep =
                               ""))
    SHP[[6]] = readShp(paste(WDinput, "Input/FieldMap6_shapefile/FieldMap6.shp", sep =
                               ""))
    SHP[[7]] = readShp(paste(WDinput, "Input/FieldMap7_shapefile/FieldMap7.shp", sep =
                               ""))
    SHP[[8]] = readShp(paste(WDinput, "Input/FieldMap8_shapefile/FieldMap8.shp", sep =
                               ""))
    SHP[[9]] = readShp(paste(WDinput, "Input/FieldMap9_shapefile/FieldMap9.shp", sep =
                               ""))
    
    # loading parameters
    paramTable = read.table(
      paste(WDinput, "Input/paramTable.txt", sep = ""),
      header = T,
      stringsAsFactors = F
    )
    iniTable = read.table(
      paste(WDinput, "Input/iniTableDisp.txt", sep = ""),
      header = T,
      stringsAsFactors = F
    )
    load(paste(WDinput, "Input/landscapeList.rdata", sep = ""))
    
    # chargement table de valeurs de ref pour calcul des scores
    IFT_SC = read.table(paste(WDinput, "Input/IFT_SC.txt", sep = ""), header =
                          T)
    
    # species names
    spList = c("WinterSmall",
               "WinterLarge",
               "SpringSmall",
               "SpringLarge")
    
    setwd(paste(WDoutput, 'Step1', sep = ''))
    param = read.table('param.txt', head = T)
    
    centralF = c(177, 35, 136, 19, 270, 58, 317, 142, 250)
    
    #runing function
    
    for (k in 1:nbS) {
      k = 12654
      kFM = param$FieldMap[k]
      
      rst = rstCheckMatrix(RST[[kFM]])
      rst = extendRst(rst, w = 300, val = 0)
      
      MLU = param$MainLandUse[k]
      
      LUnames = c("Conv", "Org", "DD", "PM")
      proportion = c(0.4, 0.4, 0, 0.2)
      
      
      landscape = createLandscape(rst, LUnames, proportion)
      
      spList = c("WinterSmall", "WinterLarge", "SpringSmall", "SpringLarge")
      
      rst = initialize(rst, landscape, iniTable, spList)
      rst@data[, spList] = 0
      
      mat = asMat(rst)
      Ind = which(mat@data$Field == centralF[kFM], arr.ind = T)
      
      rMin = min(Ind[, 1])
      rMax = max(Ind[, 1])
      rC = round(rMin + (rMax - rMin) / 2, 0)
      
      cMin = min(Ind[, 2])
      cMax = max(Ind[, 2])
      cC = round(cMin + (cMax - cMin) / 2, 0)
      
      rNoC = seq(from = rC - 2,
                 to = rC + 2,
                 by = 1)
      cNoC = seq(from = cC - 2,
                 to = cC + 2,
                 by = 1)
      
      PA = rst@grid@cellsize[1] * rst@grid@cellsize[1]
      mat@data$WinterSmall[rNoC, cNoC] = PA * as.numeric(iniTable[iniTable$LandUse ==
                                                                    MLU, 2])
      mat@data$WinterLarge[rNoC, cNoC] = PA * as.numeric(iniTable[iniTable$LandUse ==
                                                                    MLU, 3])
      mat@data$SpringSmall[rNoC, cNoC] = PA * as.numeric(iniTable[iniTable$LandUse ==
                                                                    MLU, 4])
      mat@data$SpringLarge[rNoC, cNoC] = PA * as.numeric(iniTable[iniTable$LandUse ==
                                                                    MLU, 5])
      
      rst = asRst(mat)
      
      pCurve = list(param$P1[k], param$P2[k])
      names(pCurve) = c(param$NameP1[k], param$NameP2[k])
      
      dynWeed(
        rst = rst,
        RT = 40,
        spList = spList,
        iniType = "manual",
        LUnames = NULL,
        proportion = NULL,
        iniTable = NULL,
        landscape = landscape,
        paramTable = paramTable,
        drCurve = 1,
        typCurve = param$Curve[k],
        pCurve = list(a = 5, b = 1.2),
        drMatrix = 0,
        pMatrix = NULL,
        w = 0,
        dyn = T,
        saveRdata = T,
        saveLandscapeMap = T,
        colorList = setLandUseColor(LUnames, proportion),
        saveSeedMap = T,
        WD = paste(WDoutput, "Test1/", sep = ""),
        simName = paste("Sim", k, sep = ""),
        RTlist = c(1:40),
        overwrite = T,
        polyg = SHP[[kFM]],
        calcScore = T,
        IFT_SC = IFT_SC
      )
      
      
    }
    
    #graphic accumulated dispersion
    
    load("D:/Willian/R/data/distCentralF.Rdata")
    
    #accumulated dispersion
    
    dat$dA = rep(NA, length(dat$Field))
    dispClist = vector("list", length(dat$Field))
    nm = rep(NA, 50)
    for (a in 1:50) {
      nm[a] = paste('TG', a, sep = '')
    }
    names(dispC) = c('IDField', 'centralF', nm)
    
    nbF = length(distCentralF[[dat$Field[i]]]$IDField)
    dispC = data.frame(matrix(NA, nrow = nbF, ncol = 52))
    names(dispC) = c(nm, 'IDField', 'centralF')
    dispC$IDField = distCentralF[[dat$Field[i]]]$IDField
    dispC$centralF = distCentralF[[dat$Field[i]]]$centralF
    
    for (j in 1:51) {
      for (l in 1:nbF) {
        dispC[which(dispC$IDField == l), j] = sum(
          output$fieldStat[[j]]$WinterSmallS[which(output$fieldStat[[j]]$Field == l)],
          output$fieldStat[[j]]$WinterLargeS[which(output$fieldStat[[j]]$Field == l)],
          output$fieldStat[[j]]$SpringSmallS[which(output$fieldStat[[j]]$Field == l)],
          output$fieldStat[[j]]$SpringLargeS[which(output$fieldStat[[j]]$Field == l)]
        )
      }
    }
    
    #organizing the table
    dispCo = dispC[order(dispC$centralF),]
    
    #getting the dispersion accumulated
    #0,1% of the max seed density
    ma = max(dispCo, na.rm = TRUE)
    ma1 = ma / 100
    
    #table over time with the number of fields with more than 'ma1'
    tim = seq(1, 50, 1)
    nbFc = rep(NA, 50)
    parcC = data.frame(tim, nbFc)
    for (k in 1:50) {
      l = k + 2
      parcC[k, 2] = length(dispC$IDField[which(dispC[, l] > ma1)])
    }
    
    
    #plot
    x = seq(1, 50)
    for (i in 1:39) {
      jpeg(file = paste(
        'D:/Willian/R/DynWeed/output/Test1/Sim12654/year',
        i,
        '.jpeg',
        sep = ''
      ))
      plot(
        parcC[1:i, ],
        type = 'l',
        xlim = c(1, 39),
        ylim = c(0, 230),
        lwd = 3
      )
      dev.off()
    }
    
  }
}
