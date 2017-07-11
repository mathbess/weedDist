myWD <- "/Users/wvieira/dropbox/Cursos/INRA - Agroecologie/"

# Graphs for rapport 

	#color codes : 
		# Conv = #8B2323
		# Org = #458B74
		# PM = #BCEE68
		
		# Weibull = 'red'
		# F2Dt = '#00A600'
		# Gaussian = 'yellow'
		# Exponential = 'blue'

WDmodel = paste(myWD,"Willian/R/dynWeed/Model/", sep = '')
load(paste(myWD,'Willian/R/data/dat.Rdata', sep = ''))
source(paste(myWD,"Willian/R/Script/myNativeFunctions.r", sep = ""))

#Material and Methods
{
###################################
#  			FIGURE 7			  #
###################################

	#Figure 7 A.
	
Gaussian=function(x,sigma){
  z=(1/(sigma*sqrt(2*pi)))*exp(-(x^2)/(2*sigma^2))
  return(z)
}

x <- 1:100
z <- Gaussian(x, 12)
if(sum(z)!=0) z=z/sum(z)

myplot(x, z,
	   xlab = 'Distance (m)',
	   Title = 'Gaussian (Sigma = 12)',
	   type = 'l',
	   xlim = c(0,60),
	   aAdjy = 2,
	   aAdjx = - 1,
	   width = 2.958333,
	   height = 2.572917,
	   NewWindow = FALSE
	   )
#mean = 8 m
#max = 31 m


# Polygon 95%
zz1 <- z[1:31]
zz1 <- c(-0.0025, zz1, -0.0025)
polygon(c(1, 1:31, 31), zz1, col = 'ivory3')
# Polygon 50%
zz <- z[1:8]
zz <- c(-0.0025, zz, -0.0025)
polygon(c(1, 1:8, 8), zz, col = 'ivory2')

segments(-2, 0.05507222, 8, 0.05507222, lty = 5)
segments(8, 0.05507222, 8, -0.0025, lty = 5)
segments(-2, 0.002445041, 31, 0.002445041, lty = 5)
segments(31, 0.002445041, 31, -0.0025, lty = 5)

#add text
mtext(
  '50%',
  side = 2,
  line = 0,
  outer = FALSE,
  at = 0.05507222,
  las = 2,
  adj =  1.15
)
mtext(
  '95%',
  side = 2,
  line = 0,
  outer = FALSE,
  at = 0.002445041,
  las = 2,
  adj =  1.15
)

#Figure 7 B.
	
Gaussian=function(x,sigma){
  z=(1/(sigma*sqrt(2*pi)))*exp(-(x^2)/(2*sigma^2))
  return(z)
}


x <- 1:100
lists = c(1, 5, 10, 15, 20)
myplot(
  x,
  x,
  xlim = c(0, 60),
  ylim = c(0, 0.1),
  pch = '',
  aAdjy = 2,
  aAdjx = -1,
  xlab = 'Distance (m)',
  Title = "Gaussian",
  width = 2.958333,
  height = 2.572917
)

for (k in 1:length(lists)) {
  z = Gaussian(x, sigma = lists[k])
  if (sum(z) != 0)
    z = z / sum(z)
  lines(x, z, col = k)
}
legend(
  'topright',
  legend = lists,
  lty = 1,
  col = (1:length(lists)),
  cex = .7
)

#Figure 7 C.

showDisp2 = function(typCurve,
                     pCurve,
                     r,
                     dimension = "3D",
                     maxDist = NULL,
                     Title = NULL,
                     xaxt = NULL,
                     yaxt = NULL,
                     lwd = NULL) {
  if (typCurve == "Gaussian") {
    sigma = pCurve$sigma
    DF = Gaussian
    bound = (2.829490 * sigma) + 5.57001 + 1.76623 #Values come from regression between max distance and paramters. Last value is the max residual
  }
  if (typCurve == "Weibull") {
    m = pCurve$m
    c = pCurve$c
    DF = Weibull
    bound = 6.5820 * m - (147.7906 * c) + 244.9198 + 97.789
  }
  if (typCurve == "Exponential") {
    lambda = pCurve$lambda
    DF = Exponential
    bound = (4.87324 * lambda) + 8.90765 + 2.425
  }
  if (typCurve == "f2dt") {
    a = pCurve$a
    b = pCurve$b
    DF = f2dt
    bound = (3.2160 * a) - (28.00092 * b) + 82.6865 + 76.20381
  }
  
  
  if (!is.null(maxDist))
    bound = maxDist
  
  bound = round(bound, 0)
  bound = r * (1 + (bound - bound %% r) / r) # move to the next value divisible by r
  
  if (is.null(Title)) {
    Title = paste(typCurve, paste(
      names(pCurve),
      rep("=", length(pCurve)),
      pCurve,
      sep = "",
      collapse = " "
    ))
  } else{
    Title = Title
  }
  
  if (dimension == "2D") {
    xu = seq(from = -bound,
             to = bound,
             by = r)
    z1D = DF(xu, pCurve)
    if (sum(z1D) != 0)
      z1D = z1D / sum(z1D)
    plot(
      xu,
      z1D,
      pch = "",
      main = Title,
      xaxt = xaxt,
      yaxt = yaxt
    )
    lines(xu, z1D, lwd = lwd)
  }
  
  if (dimension == "3D") {
    xu = seq(from = -bound,
             to = bound,
             by = r)
    yu = xu
    grd = expand.grid(xu, yu)
    nbP = nrow(grd)
    X = grd[, 1]
    Y = grd[, 2]
    D = sqrt(outer(X, 0, "-") * outer(X, 0, "-") + outer(Y, 0, "-") * outer(Y, 0, "-"))
    z2D = matrix(DF(D, pCurve), nrow = length(yu), ncol = length(xu))
    if (sum(z2D) != 0)
      z2D = z2D / sum(z2D)
    persp(xu, yu, z2D, theta = 30, phi = 20)
    mtext(
      side = 3,
      Title,
      adj = 0.5,
      line = 0.05,
      cex = 1.5,
      font = 3
    )
  }
}

par(family = 'serif')
showDisp2(
  typCurve = "Gaussian",
  pCurve = list("sigma" = 12),
  dimension = '3D',
  r = 2,
  Title = 'Sigma = 12 \n R??solution = 2 m'
)

#Figure 7 D.

source("D:/Willian/R/dynWeed/Model/raster.r")
source("D:/Willian/R/dynWeed/Dispersal/dispFunc.r")
library(svMisc)
colori = sort(heat.colors(400), decreasing = T)

# generate the matrix filter
Zm = getFilter(typCurve = "Gaussian",
               pCurve = list("sigma" = 12),
               r = 20)
x11(width = 2.895833, height = 3.072917)
par(family = 'serif',
    mar = c(1.5, 1, 2.2, 1),
    cex = 1.3)
image(
  Zm,
  col = colori,
  xaxt = 'n',
  yaxt = 'n',
  ylim = c(0.1, 0.9),
  xlim = c(0.1, 0.9)
)
box()
at = c(0.25, 0.416, 0.583, 0.748)
xx = axis(1,
          labels = FALSE,
          at = at,
          tck = -0.02)
yy = axis(2,
          labels = FALSE,
          at = at,
          tck = -0.02)

#x and y lab list
label <- c(0, 20, 40, 60)
axis(
  1,
  at = at,
  line = -0.95,
  labels = label,
  tck = -0.04,
  lwd = 0
)
axis(
  2,
  at = at,
  line = -0.95,
  labels = label,
  tck = -0.04,
  lwd = 0
)

#Title
mtext(
  side = 3,
  'Sigma = 12 \n R??solution = 20 m',
  line = 0.05,
  cex = 1.4,
  font = 3
)

# Curves distribution to use in the table
showDisp2(
  typCurve = 'Gaussian',
  pCurve = list("sigma" = 22.5),
  dimension = '2D',
  xaxt = 'n',
  yaxt = 'n',
  r = 2,
  lwd = 10
) #function is above
showDisp2(
  typCurve = 'Exponential',
  pCurve = list("lambda" = 22.5),
  dimension = '2D',
  xaxt = 'n',
  yaxt = 'n',
  r = 2,
  lwd = 10
)
showDisp2(
  typCurve = 'f2dt',
  pCurve = list("a" = 20, "b" = 1.5),
  dimension = '2D',
  xaxt = 'n',
  yaxt = 'n',
  r = 2,
  lwd = 10
)
showDisp2(
  typCurve = 'Weibull',
  pCurve = list(m = 20, c = 1.5),
  dimension = '2D',
  xaxt = 'n',
  yaxt = 'n',
  r = 2,
  lwd = 10
)

}

#Results
{
  #Effet des variables biologiques sur la dispersion des adventices
{
setwd('D:/Willian/R')
dispDat <- read.table(
  'dispType.txt',
  head = T,
  dec = ',',
  na.strings = 'NA'
)
names(dispDat)

#Figure 11.

myboxplot(
  x = log(dispDat$mean),
  y = dispDat$BfMode,
  Interaction = 1,
  Rotation = TRUE,
  Graus = 20,
  Adj = 0.35,
  xlab = '',
  ylim = c(-1.6, 2.2),
  ylab = 'Mean distance (log)',
  width = 4.4,
  height = 4.13,
  N = TRUE,
  lablistx = c(
    'Anemochory',
    'Barochory',
    'Endozoochory',
    'Epizoochory',
    'Myrmecochory'
  )
)
myboxplot(
  x = log(dispDat$max),
  y = dispDat$BfMode,
  Interaction = 1,
  Rotation = TRUE,
  Graus = 20,
  Adj = 0.35,
  xlab = '',
  ylim = c(0, 6),
  ylab = 'Maximum distance (log)',
  width = 4.4,
  height = 4.13,
  N = TRUE,
  lablistx = c(
    'Anemochory',
    'Barochory',
    'Endozoochory',
    'Epizoochory',
    'Myrmecochory'
  )
)

#Figure 12.

myboxplot(
  x = log(dispDat$mean),
  y = dispDat$BfMode,
  Interaction = 1,
  Rotation = TRUE,
  Graus = 20,
  Adj = 0.35,
  xlab = '',
  ylim = c(-1.6, 2.2),
  ylab = 'Mean distance (log)',
  width = 4.4,
  height = 4.13,
  N = TRUE,
  Title = 'Base Flor',
  lablistx = c(
    'Anemochory',
    'Barochory',
    'Endozoochory',
    'Epizoochory',
    'Myrmecochory'
  )
)
myboxplot(
  x = log(dispDat$mean),
  y = dispDat$BeMode,
  Interaction = 1,
  Rotation = TRUE,
  Graus = 20,
  Adj = 0.35,
  xlab = '',
  ylim = c(-1.6, 2.2),
  width = 4.4,
  height = 4.13,
  Title = 'Issanchou',
  N = TRUE,
  atTicks = c(1, 2, 3),
  lablistx = c('Anemochory', 'Barochory', 'Zoochory')
)
myboxplot(
  x = log(dispDat$mean),
  y = dispDat$D3,
  Interaction = 1,
  Rotation = TRUE,
  Graus = 20,
  Adj = 0.35,
  xlab = '',
  ylim = c(-1.6, 2.2),
  ylab = 'Mean distance (log)',
  width = 4.4,
  height = 4.13,
  N = TRUE,
  Title = 'D3',
  lablistx = c(
    'Anemochory',
    'Endozoochory',
    'Epizoochory',
    'hemerochory'
  )
)
myboxplot(
  x = log(dispDat$mean),
  y = dispDat$FrMode,
  Interaction = 1,
  Rotation = TRUE,
  Graus = 20,
  Adj = 0.35,
  xlab = '',
  ylim = c(-1.6, 2.2),
  width = 4.4,
  height = 4.13,
  Title = 'Fried',
  N = TRUE,
  atTicks = c(1, 2, 3),
  lablistx = c('Anemochory', 'Barochory', 'Zoochory')
)

d <- boxplot(
  dat$mean ~ dat$FrMode,
  varwidth = T,
  main = 'Fried',
  ylim = c(0, 8)
)
text(1:length(d$n), d$stats[5, ] + 1, paste("n =", d$n), cex = 0.75)

}

  # Before simulation
{  
  #Exemple with Weibull
  
  WDmodel = "D:/Willian/R/dynWeed/Model/"
  WDinput = "D:/Willian/R/dynWeed/"
  # loading model
  source(paste(WDmodel, "DynWeed4.3.1.r", sep = ""))
  # loading dispersal Curves
  source(paste(WDinput, "Dispersal/dispFunc.r", sep = ""))
  
  #Getting data: variation du M, C
  listm = seq(from = 1, to = 40, by = 1)
  listc = seq(from = 1.2, to = 3, by = 0.02)
  ntotal = length(listm) * length(listc)
  tab = data.frame(matrix(nrow = ntotal, ncol = 6))
  names(tab) = c('m', 'c', 'seedPropDep', 'mean', 'dist50', 'dist99')
  tab$m = listm
  tab$c = rep(listc, each = length(listm))
  
  for (i in 1:ntotal) {
    rst = newRst(
      xmin = 1000,
      xmax = 3000,
      ymin = 1000,
      ymax = 3000,
      res = 20,
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
    tab$seedPropDep[i] = sum(rstDisp@data$seed[rstDisp@data$dist <= 20])
    tab$mean[i] = getMeanDist(rstDisp)
    tab$dist50[i] = getDistProp(rstDisp, 0.5)
    tab$dist99[i] = getDistProp(rstDisp, 0.99)
    progress(100 * i / ntotal)
  }
  head(tab)
  tail(tab)
  
  #plot 3D (resolution = 20m)
  library(latticeExtra)
  
  #mean distance
  cloud(
    tab$mean ~ tab$c + tab$m,
    ,
    col.facet = 'grey',
    xbase = 0.4,
    ybase = 0.4,
    scales = list(arrows = FALSE, col = 1),
    par.settings = list(axis.line = list(col = "transparent"))
  )
  
  #projection (resolution = 20m)
  
  lism = c(4, 8, 12, 15)
  lism[1] = 1
  lisc = c(1.2, 1.4, 1.5, 1.7)
  
  #parameter c
  myplot(
    x = listm,
    y = tab$mean[which(tab$c == 1.2)],
    ylim = c(20, 90),
    xlim = c(0, 15),
    type = 'n',
    ylab = '',
    pch = '',
    Title = '',
    aAdjx = -0.9
  )
  
  for (i in 1:length(lisc)) {
    points(tab$m[which(tab$c == lisc[i])],
           tab$mean[which(tab$c == lisc[i])],
           type = 'l',
           col = i,
           pch = i)
  }
  legend(
    'topleft',
    legend = lisc,
    lty = 1,
    col = 1:length(lisc),
    cex = 0.8
  )
  
  #parameter m
  myplot(
    x = listc,
    y = tab$mean[which(tab$m == 40)],
    ylim = c(20, 300),
    xlim = c(1.2, 2.3),
    pch = '',
    Title = 'Parameter m variation',
    aAdjx = -0.9
  )
  
  for (i in 1:length(lism)) {
    points(tab$c[which(tab$m == lism[i])],
           tab$mean[which(tab$m == lism[i])],
           type = 'l',
           col = i,
           cex = 0.9)
  }
  legend(
    'topright',
    legend = lism,
    lty = 1,
    col = 1:10,
    cex = 0.9
  )
  
  #Seed proportion in the pixel
  # parameter m
  myplot(
    x = tab$m[which(tab$c == lisc[1])],
    y = tab$seedPropDep[which(tab$c == lisc[1])],
    pch = '',
    xlab = 'Parameter m',
    aAdjx = -.9,
    ylab = 'Seed proportion in the pixel'
  )
  for (i in 1:length(lisc)) {
    points(
      tab$m[which(tab$c == lisc[i])],
      y = tab$seedPropDep[which(tab$c == lisc[i])],
      type = 'l',
      col = i,
      pch = i
    )
  }
  legend(
    'topright',
    legend = lisc,
    lty = 1,
    col = 1:length(lisc),
    cex = 0.8
  )
  
  #parameter c
  myplot(
    x = tab$c[which(tab$m == lism[9])],
    y = tab$seedPropDep[which(tab$m == lism[9])],
    pch = '',
    ylim = c(0, 0.5),
    xlim = c(1.2, 2.7),
    xlab = 'Parameter c',
    aAdjx = -.9,
    ylab = 'Seed proportion in the pixel'
  )
  for (i in 1:length(lism)) {
    points(
      tab$c[which(tab$m == lism[i])],
      tab$seedPropDep[which(tab$m == lism[i])],
      type = 'l',
      col = i,
      cex = 0.9
    )
  }
  legend(
    'topright',
    legend = lism,
    lty = 1,
    col = 1:10,
    cex = 0.9
  )
  
}
  
  # After simlulation
{    
    #PCA
  {
    
    library(ggbiplot)
    
    #PCA for separated groups (11 different groups)
    
    #data without PM
    dat.spm = dat[, c(seq(1, 15), 17, 18, 19, 20, 22, 23, 25, 26, 29, 30, 31)]
    #data with PM
    dat.p = dat.spm[, c(2, 3, 4, 5, 6, 7, 8, 9, 11, 12, c(13:23), 25)]
    
    variables = dat.p[, c(11:22)]
    varib = variables[, c(1, 2, 3, 12)]
    
    # apply PCA
    dat.pca = prcomp(varib, center = TRUE, scale. = TRUE)
    
    # plot B
    g <- ggbiplot(
      dat.pca,
      groups = dat$MainLandUse,
      ellipse = TRUE,
      circle = TRUE,
      alpha = 0.1,
      family = 'serif'
    )
    g <-
      g + scale_color_manual(values = c('brown4', 'aquamarine4', 'darkolivegreen2'))
    g <- g + theme_bw(base_family = "serif")
    g <- g + theme(text = element_text(size = 15))
    
    print(g)
    
    # plot A
    g <- ggbiplot(
      dat.pca,
      groups = dat$MLUprop,
      ellipse = TRUE,
      circle = TRUE,
      alpha = 0.1,
      family = 'serif'
    )
    g <- g + theme_bw(base_family = "serif")
    g <- g + theme(text = element_text(size = 15))
    
    print(g)
    
    #Boxplot to put next to the PCA
    WDsc = as.vector(scale(dat.spm$WD, center = TRUE, scale = TRUE))
    TDMLUsc = as.vector(scale(dat.spm$TDMLU, center = TRUE, scale = TRUE))
    TDAFsc = as.vector(scale(dat.spm$TDAF, center = TRUE, scale = TRUE))
    dA1sc = as.vector(scale(dat.spm$dA1, center = TRUE, scale = TRUE))
    
    x11(width = 1.8, height = 7.05)
    boxplot(
      WDsc,
      TDMLUsc,
      width = c(0.5, 0.5),
      ylim = c(-2.5, 4.1),
      cex = 0.5
    )
    boxplot(
      WDsc,
      TDMLUsc,
      width = c(0.5, 0.5),
      ylim = c(-2.5, 4.1),
      cex = 0.5,
      frame.plot = FALSE,
      xaxt = 'n',
      yaxt = 'n'
    )
    
    x11(width = 1.8, height = 7.05)
    boxplot(
      TDAFsc,
      dA1sc,
      width = c(0.5, 0.5),
      ylim = c(-2, 2),
      cex = 0.5
    )
    boxplot(
      TDAFsc,
      dA1sc,
      width = c(0.5, 0.5),
      ylim = c(-2, 2),
      cex = 0.5,
      frame.plot = FALSE,
      xaxt = 'n',
      yaxt = 'n'
    )
    
    #PCA for separated groups (group 2 and 5)
    
    nm = 'group 2 - Org 50%'
    dg2 = dat.p[which(dat.spm$MainLandUse == 'Org' &
                        dat.spm$MLUprop == 0.5), ]
    dg2a = dg2[, c(11:22)]
    dg2b = dg2a[, c(1, 2, 12)]
    dat.pca2 = prcomp(dg2b, center = TRUE, scale. = TRUE)
    
    nm = 'group 5 - Conv 100%'
    dg5 = dat.p[which(dat.spm$MainLandUse == 'Conv' &
                        dat.spm$MLUprop == 1), ]
    dg5a = dg5[, c(11:22)]
    dg5b = dg5a[, c(1, 2, 12)]
    dat.pca5 = prcomp(dg5b, center = TRUE, scale. = TRUE)
    
    # plot
    #	Group 2
    g <- ggbiplot(
      dat.pca2,
      groups = dg5$Curve,
      ellipse = TRUE,
      circle = TRUE,
      alpha = 0.12,
      family = 'serif'
    )
    g <-
      g + scale_color_manual(values = c('blue', '#00A600', '#FFFF00', 'red'))
    g <- g + theme_bw(base_family = "serif")
    g <-
      g + theme(
        legend.direction = 'horizontal',
        legend.position = 'top',
        text = element_text(size = 14)
      )
    print(g)
    
    #	Group 5
    g <- ggbiplot(
      dat.pca5,
      groups = dg5$Curve,
      ellipse = TRUE,
      circle = TRUE,
      alpha = 0.1,
      family = 'serif'
    )
    g <-
      g + scale_color_manual(values = c('blue', '#00A600', '#FFFF00', 'red'))
    g <- g + theme_bw(base_family = "serif")
    g <-
      g + theme(
        legend.direction = 'horizontal',
        legend.position = 'top',
        text = element_text(size = 14)
      )
    print(g)
  
  }
    
    #Evolution plot
  {
    ###################################
    #	1. plot Number of seeds       #
    ###################################
    
    #function to plot by curve with all field maps together
    #transfor the number of fields in % to standardize between the 9 fields
    parcClist2 = vector('list', length(dat$Num))
    for (i in 1:length(dat$Num)) {
      nbF = length(distCentralF[[dat$Field[i]]]$IDField)
      temp_dat = parcClist[[i]]
      temp_dat$nbFc01 = (temp_dat$nbFc01 * 100) / nbF
      temp_dat$nbFc1 = (temp_dat$nbFc1 * 100) / nbF
      temp_dat$nbFc5 = (temp_dat$nbFc5 * 100) / nbF
      parcClist2[[i]] = temp_dat
      progress(100 * i / length(dat$Num))
    }
    #save(parcClist2,file='parcClist2.Rdata')
    
    #	Function to plot each curve
    load(paste(myWD, 'Willian/R/data/WDSeed.Rdata', sep = ''))
    ff5 = function(Curve, xlab = xlab, ylab = ylab) {
      sel = dat$Num[which(dat$Curve == Curve &
                            dat$MainLandUse == c('Conv', 'Org'))]
      myplot(
        x = 0,
        y = 0,
        pch = '',
        xlim = c(0, 49),
        ylim = c(0, 1245000),
        xlab = xlab,
        ylab = ylab,
        Title = Curve,
        aAdjy = -0.85,
        aAdjx = -0.9
      )
      temp_dat = data.frame(matrix(NA, ncol = 50, nrow = length(sel)))
      for (i in 1:length(sel)) {
        k = sel[i]
        temp_dat[i, ] = WDSeed[[k]]$allSeed
        lines(WDSeed[[k]]$allSeed,
              col = addTrans('grey', 10),
              lwd = 2)
      }
      temp_dat$MLU = rep(NA, length(temp_dat[, 1]))
      for (i in 1:length(sel)) {
        k = sel[i]
        temp_dat$MLU[i] = dat$MainLandUse[k]
      }
      
      temp_dat2 = data.frame(matrix(NA, ncol = 4, nrow = 50))
      temp_dat2[, 1] = seq(1, 50)
      for (j in 1:50) {
        temp_dat2[j, 2] = median(temp_dat[, j])
        temp_dat2[j, 3] = median(temp_dat[which(temp_dat$MLU == 1), j])
        temp_dat2[j, 4] = median(temp_dat[which(temp_dat$MLU == 2), j])
      }
      
      sel1 = dat$Num[which(dat$Curve == Curve & dat$MainLandUse == 'PM')]
      sel2 = dat$Num[which(dat$Curve == Curve &
                             dat$MLUprop == c(0.97, 0.5))]
      temp_dat3 = data.frame(matrix(NA, ncol = 50, nrow = (length(sel1) + length(sel2))))
      for (i in 1:length(sel1)) {
        k = sel1[i]
        temp_dat3[i, ] = WDSeed[[k]]$TDMLU
      }
      for (i in 1:length(sel2)) {
        k = sel2[i]
        I = i + length(sel1)
        temp_dat3[I, ] = WDSeed[[k]]$TDPM
      }
      
      temp_dat4 = data.frame(matrix(NA, ncol = 2, nrow = 50))
      temp_dat4[, 1] = seq(1, 50)
      for (j in 1:50) {
        temp_dat4[j, 2] = median(temp_dat3[, j])
      }
      
      lines(temp_dat2[, 1], temp_dat2[, 2], lwd = 2.5, lty = 5)
      lines(temp_dat2[, 1],
            temp_dat2[, 3],
            lwd = 2.5,
            lty = 5,
            col = '#8B2323')
      lines(temp_dat2[, 1],
            temp_dat2[, 4],
            lwd = 2.5,
            lty = 5,
            col = '#458B74')
      lines(temp_dat4[, 1],
            temp_dat4[, 2],
            lwd = 2.5,
            lty = 5,
            col = '#BCEE68')
      
      rm(temp_dat)
      rm(temp_dat2)
      rm(temp_dat3)
      rm(temp_dat4)
    }
    
    ff5(
      Curve = 'Weibull',
      xlab = '',
      ylab = ~ "Number of seed " (n / m ^ 2)
    )
    ff5(Curve = 'f2dt', xlab = '', ylab = '')
    ff5(
      Curve = 'Gaussian',
      xlab = 'Time (years)',
      ylab = ~ "Number of seed " (n / m ^ 2)
    )
    ff5(Curve = 'Exponential', xlab = 'Time (years)', ylab = '')
    
    legend(
      'topleft',
      legend = c('Conv', 'Org', 'PM', 'Median'),
      lty = 5,
      col = c('#8B2323', '#458B74', '#BCEE68', 'black'),
      cex = .90,
      bty = 'n'
    )
    
    ###################################
    #	2. Weed Diversity    		  #
    ###################################
    
    load('WDSeed.Rdata')
    ff4 = function(Curve, xlab = xlab, ylab = ylab) {
      sel = dat$Num[which(dat$Curve == Curve)]
      myplot(
        x = 0,
        y = 0,
        pch = '',
        xlim = c(0, 50),
        ylim = c(0, 1.05),
        xlab = xlab,
        ylab = ylab,
        Title = Curve,
        aAdjx = -0.9,
        aAdjy = -0.85
      )
      temp_dat = data.frame(matrix(NA, ncol = 50, nrow = length(sel)))
      for (i in 1:length(sel)) {
        k = sel[i]
        temp_dat[i, ] = WDSeed[[k]]$WD
        lines(WDSeed[[k]]$WD, col = addTrans('grey', 10), lwd = 2)
      }
      temp_dat$MLU = rep(NA, length(temp_dat[, 1]))
      for (i in 1:length(sel)) {
        k = sel[i]
        temp_dat$MLU[i] = dat$MainLandUse[k]
      }
      temp_dat2 = data.frame(matrix(NA, ncol = 5, nrow = 50))
      temp_dat2[, 1] = seq(1, 50)
      for (j in 1:50) {
        temp_dat2[j, 2] = median(temp_dat[, j])
        temp_dat2[j, 3] = median(temp_dat[which(temp_dat$MLU == 1), j])
        temp_dat2[j, 4] = median(temp_dat[which(temp_dat$MLU == 2), j])
        temp_dat2[j, 5] = median(temp_dat[which(temp_dat$MLU == 3), j])
        
      }
      lines(temp_dat2[, 1], temp_dat2[, 2], lwd = 2.5, lty = 5)
      lines(temp_dat2[, 1],
            temp_dat2[, 3],
            lwd = 2.5,
            lty = 5,
            col = 'brown4')
      lines(temp_dat2[, 1],
            temp_dat2[, 4],
            lwd = 2.5,
            lty = 5,
            col = "aquamarine4")
      lines(temp_dat2[, 1],
            temp_dat2[, 5],
            lwd = 2.5,
            lty = 5,
            col = "darkolivegreen2")
      
      rm(temp_dat)
      rm(temp_dat2)
    }
    
    ff4(Curve = 'Weibull', xlab = '', ylab = 'Weed diversity')
    ff4(Curve = 'f2dt', xlab = '', ylab = '')
    ff4(Curve = 'Gaussian', xlab = 'Time (years)', ylab = 'Weed diversity')
    ff4(Curve = 'Exponential', xlab = 'Time (years)', ylab = '')
    
    legend(
      'bottomright',
      legend = c('Conv', 'Org', 'PM', 'Median'),
      lty = 5,
      col = c('#8B2323', '#458B74', '#BCEE68', 'black'),
      cex = .90,
      bty = 'n'
    )
    
    ###################################
    #  3. Time to reach all landscape #
    ###################################
    
    load(paste(myWD, 'Willian/R/data/parcClist2.Rdata', sep = ""))
    ff2 = function(Curve,
                   xlab = xlab,
                   ylab = ylab,
                   col = col) {
      sel = dat$Num[which(dat$Curve == Curve)]
      myplot(
        x = 0,
        y = 0,
        pch = '',
        xlim = c(1, 51),
        ylim = c(0, 100),
        xlab = xlab,
        ylab = ylab,
        Title = Curve,
        aAdjx = -.9,
        aAdjy = -0.85,
        width = 3.5,
        height = 3.5,
      )
      temp_dat = data.frame(matrix(NA, ncol = 51, nrow = length(sel)))
      for (i in 1:length(sel)) {
        k = sel[i]
        temp_dat[i, ] = parcClist2[[k]]$nbFc1
        lines(parcClist2[[k]]$Time,
              parcClist2[[k]]$nbFc1,
              col = addTrans('grey', 5),
              lwd = 1.1)
      }
      temp_dat2 = data.frame(matrix(NA, ncol = 2, nrow = 51))
      temp_dat2[, 1] = seq(1, 51)
      for (j in 1:51) {
        temp_dat2[j, 2] = median(temp_dat[, j])
      }
      lines(temp_dat2[, 1],
            temp_dat2[, 2],
            lwd = 2.5,
            lty = 5,
            col = col)
      
    }
    
    ff2(
      Curve = 'Weibull',
      xlab = '',
      ylab = 'Fields (%)',
      col = 'red'
    )
    ff2(
      Curve = 'f2dt',
      xlab = '',
      ylab = '',
      col = '#00A600'
    )
    ff2(
      Curve = 'Gaussian',
      xlab = 'Time (years)',
      ylab = 'Fields (%)',
      col = 'yellow'
    )
    ff2(
      Curve = 'Exponential',
      xlab = 'Time (years)',
      ylab = '',
      col = 'blue'
    )
    
    legend(
      29.5,
      20,
      legend = 'Median',
      lty = 5,
      cex = .90,
      bty = 'n'
    )
    
    #Mini zoon graph
    EvFields = vector("list", 4)
    curves = c('Weibull', 'f2dt', 'Gaussian', 'Exponential')
    color = c('red', '#00A600', 'yellow', 'blue')
    for (c in 1:4) {
      C = curves[c]
      sel = dat$Num[which(dat$Curve == C)]
      
      temp_dat = data.frame(matrix(NA, ncol = 51, nrow = length(sel)))
      for (i in 1:length(sel)) {
        k = sel[i]
        temp_dat[i, ] = parcClist2[[k]]$nbFc1
      }
      temp_dat2 = data.frame(matrix(NA, ncol = 2, nrow = 51))
      temp_dat2[, 1] = seq(1, 51)
      for (j in 1:51) {
        temp_dat2[j, 2] = median(temp_dat[, j])
      }
      EvFields[[c]] = temp_dat2
    }
    # plot
    plot(
      0,
      pch = '',
      xlim = c(2, 15),
      ylim = c(0, 15),
      xaxt = 'n',
      yaxt = 'n'
    )
    myplot(
      x = 0,
      y = 0,
      pch = '',
      xlim = c(3, 51),
      ylim = c(0, 100),
      aAdjx = -.9,
      aAdjy = -0.85,
      width = 3.5,
      height = 3.5,
    )
    for (i in 1:4) {
      lines(
        EvFields[[i]][, 1],
        EvFields[[i]][, 2],
        lty = 5,
        lwd = 1.7,
        col = color[i]
      )
    }
  }
    
    #Boxplots
  {
    #separating data for boxplot
    #8 different groups
    # 1. Conv 100%
    # 2. Conv 97%
    # 3. Conv 50%
    # 4. Org 100%
    # 5. Org 97%
    # 6. Org 50%
    # 7. PM 50%
    # 8. All OS and all proportion
    
    g1 <- dat[which(dat$MainLandUse == 'Conv' & dat$MLUprop == 1), ]
    g2 <- dat[which(dat$MainLandUse == 'Conv' & dat$MLUprop == 0.97), ]
    g3 <- dat[which(dat$MainLandUse == 'Conv' & dat$MLUprop == 0.5), ]
    g4 <- dat[which(dat$MainLandUse == 'Org' & dat$MLUprop == 1), ]
    g5 <- dat[which(dat$MainLandUse == 'Org' & dat$MLUprop == 0.97), ]
    g6 <- dat[which(dat$MainLandUse == 'Org' & dat$MLUprop == 0.5), ]
    g7 <- dat[which(dat$MainLandUse == 'PM' & dat$MLUprop == 1), ]
    g8 <- dat
    
    #lablist for boxplot
    lablistx = c('Conv 100%',
                 'Conv 97%',
                 'Conv 50%',
                 'Org 100%',
                 'Org 97%',
                 'Org 50%',
                 'PM 50%')
    lablistx2 = c('Exponential', 'f2dt', 'Gaussian', 'Weibull')
    
    ###################################
    #
    #	1. plot Number of seeds
    #
    ###################################
    
    # 100%
    i = 15
    Ylim <- range(dat$TDMLU[which(dat$MainLandUse == c('Conv', 'Org') & dat$MLUprop == 1)])
    #group 1 - Conv 100%
    myboxplot(
      x = g1[, i],
      y = g1$Curve,
      Interaction = 1,
      Rotation = TRUE,
      Graus = 20,
      Adj = 0.35,
      xlab = '',
      ylab = 'Number of seeds/' ~ 'm' ^ '2' * '',
      ylim = Ylim,
      Title = 'Conventional 100%',
      colorList = 'brown4',
      NewWindow = FALSE
    )
    #group 4 - Org 100%
    myboxplot(
      x = g4[, i],
      y = g4$Curve,
      Interaction = TRUE,
      Rotation = TRUE,
      Graus = 20,
      Adj = 0.35,
      xlab = '',
      ylab = '',
      lablisty = rep(NA, 6),
      ylim = Ylim,
      Title = 'Organic 100%',
      colorList = "aquamarine4",
      mary = 0,
      NewWindow = FALSE,
    )
    #group 7 - PM 100%
    myboxplot(
      x = g7[, i],
      y = g7$Curve,
      Interaction = TRUE,
      Rotation = TRUE,
      Graus = 20,
      Adj = 0.35,
      xlab = '',
      ylab = '',
      ylim = c(0, 500),
      Title = 'Grassland 100%',
      colorList = "darkolivegreen2",
      ySide = 4,
      yAdj = -0.9,
      marz = 1,
      mary = 0,
      NewWindow = FALSE,
    )
    
    # Both OS
    
    atVect = c(1.1, 1.9, 3.1, 3.9, 5.1, 5.9, 7.1, 7.9)
    atTicks = c(1.5, 3.5, 5.5, 7.5)
    
    #97% Conv
    subDataC <- data.frame(g2$TDMLU, g2$Curve)
    subDataC$LU <- rep('Conv', length(subDataC$g2.Curve))
    names(subDataC) <- c('TD', 'Curve', 'LU')
    
    subDataPMc <- data.frame(g2$TDPM, g2$Curve)
    subDataPMc$LU <- rep('PM', length(subDataPMc$g2.Curve))
    names(subDataPMc) <- c('TD', 'Curve', 'LU')
    
    subDataC <- rbind(subDataC, subDataPMc)
    
    myboxplot(
      x = subDataC$TD,
      y = subDataC$LU,
      z = subDataC$Curve,
      Interaction = 2,
      atBox = atVect,
      atTicks = atTicks,
      lablistx = lablistx2,
      # Rotation = TRUE,
      # Grau = 20,
      #Adj = 0.35,
      ylim = c(0, 805000),
      Title = '97% Conventional / 3% Grassland',
      ylab = 'Number of seeds/' ~ 'm' ^ '2' * '',
      colorList = rep(c('brown4', 'darkolivegreen2'), 4),
      NewWindow = FALSE
    )
    abline(v = 2.5, lty = 3)
    abline(v = 4.5, lty = 3)
    abline(v = 6.5, lty = 3)
    
    #97% Org
    subDataO <- data.frame(g5$TDMLU, g5$Curve)
    subDataO$LU <- rep('Org', length(subDataO$g5.Curve))
    names(subDataO) <- c('TD', 'Curve', 'LU')
    
    subDataPMo <- data.frame(g5$TDPM, g5$Curve)
    subDataPMo$LU <- rep('PM', length(subDataPMo$g5.Curve))
    names(subDataPMo) <- c('TD', 'Curve', 'LU')
    
    subDataO <- rbind(subDataO, subDataPMo)
    
    myboxplot(
      x = subDataO$TD,
      y = subDataO$LU,
      z = subDataO$Curve,
      Interaction = 2,
      atBox = atVect,
      atTicks = atTicks,
      lablistx = lablistx2,
      # Rotation = TRUE,
      # Grau = 20,
      #Adj = 0.35,
      ylim = c(0, 1320000),
      Title = '97% Organic / 3% Grassland',
      ylab = '',
      colorList = rep(c('aquamarine4', 'darkolivegreen2'), 4)
    )
    abline(v = 2.5, lty = 3)
    abline(v = 4.5, lty = 3)
    abline(v = 6.5, lty = 3)
    
    
    #50% Conv
    subDataC <- data.frame(g3$TDMLU, g3$Curve)
    subDataC$LU <- rep('Conv', length(subDataC$g3.Curve))
    names(subDataC) <- c('TD', 'Curve', 'LU')
    
    subDataPMc <- data.frame(g3$TDPM, g3$Curve)
    subDataPMc$LU <- rep('PM', length(subDataPMc$g3.Curve))
    names(subDataPMc) <- c('TD', 'Curve', 'LU')
    
    subDataC <- rbind(subDataC, subDataPMc)
    
    myboxplot(
      x = subDataC$TD,
      y = subDataC$LU,
      z = subDataC$Curve,
      Interaction = 2,
      atBox = atVect,
      atTicks = atTicks,
      lablistx = lablistx2,
      # Rotation = TRUE,
      # Grau = 20,
      #Adj = 0.35,
      ylim = c(0, 805000),
      Title = '50% Conventional / 50% Grassland',
      ylab = 'Number of seeds/' ~ 'm' ^ '2' * '',
      colorList = rep(c('brown4', 'darkolivegreen2'), 4)
    )
    abline(v = 2.5, lty = 3)
    abline(v = 4.5, lty = 3)
    abline(v = 6.5, lty = 3)
    
    #50% Org
    subDataO <- data.frame(g6$TDMLU, g6$Curve)
    subDataO$LU <- rep('Org', length(subDataO$g6.Curve))
    names(subDataO) <- c('TD', 'Curve', 'LU')
    
    subDataPMo <- data.frame(g6$TDPM, g6$Curve)
    subDataPMo$LU <- rep('PM', length(subDataPMo$g6.Curve))
    names(subDataPMo) <- c('TD', 'Curve', 'LU')
    
    subDataO <- rbind(subDataO, subDataPMo)
    
    myboxplot(
      x = subDataO$TD,
      y = subDataO$LU,
      z = subDataO$Curve,
      Interaction = 2,
      atBox = atVect,
      atTicks = atTicks,
      lablistx = lablistx2,
      # Rotation = TRUE,
      # Grau = 20,
      #Adj = 0.35,
      ylim = c(0, 1320000),
      Title = '50% Organic / 50% Grassland',
      ylab = '',
      colorList = rep(c('aquamarine4', 'darkolivegreen2'), 4)
    )
    abline(v = 2.5, lty = 3)
    abline(v = 4.5, lty = 3)
    abline(v = 6.5, lty = 3)
    
    ###################################
    #
    #	1.a plot Number of seeds (new way)
    #
    ###################################
    
    g1 <- dat[which(dat$MainLandUse == 'Conv' & dat$MLUprop == 1), ]
    g2 <- dat[which(dat$MainLandUse == 'Conv' & dat$MLUprop == 0.97), ]
    g3 <- dat[which(dat$MainLandUse == 'Conv' & dat$MLUprop == 0.5), ]
    g4 <- dat[which(dat$MainLandUse == 'Org' & dat$MLUprop == 1), ]
    g5 <- dat[which(dat$MainLandUse == 'Org' & dat$MLUprop == 0.97), ]
    g6 <- dat[which(dat$MainLandUse == 'Org' & dat$MLUprop == 0.5), ]
    g7 <- dat[which(dat$MainLandUse == 'PM' & dat$MLUprop == 1), ]
    g8 <- dat
    
    # 100%
    i = 15 #Getting TDMLU (number of seed in the main land use)
    Ylim <- range(dat$TDMLU[which(dat$MainLandUse == c('Conv', 'Org') & dat$MLUprop == 1)])
    Ylim2 <- c(321000, 1240000)
    
    options(scipen = 999) #disabling scientific notation
    
    #group 1 - Conv 100%
    myboxplot(
      x = g1[, i],
      y = g1$Curve,
      Interaction = 1,
      Rotation = TRUE,
      Graus = 20,
      Adj = 0.35,
      xlab = '',
      ylab = 'Number of seeds/' ~ 'm' ^ '2' * '',
      ylim = Ylim2,
      Title = '100% Conventional',
      colorList = 'brown4',
      width = 3.2,
      height = 4.24,
      NewWindow = FALSE
    )
    #group 4 - Org 100%
    myboxplot(
      x = g4[, i],
      y = g4$Curve,
      Interaction = 1,
      Rotation = TRUE,
      Graus = 20,
      Adj = 0.35,
      xlab = '',
      ylab = '',
      ylim = Ylim2,
      lablisty = rep(NA, 6),
      Title = '100% Organic',
      colorList = "aquamarine4",
      NewWindow = FALSE
    )
    #group 7 - PM 100%
    myboxplot(
      x = g7[, i],
      y = g7$Curve,
      Interaction = TRUE,
      Rotation = TRUE,
      Graus = 20,
      Adj = 0.35,
      xlab = '',
      ylab = '',
      ylim = c(0, 500),
      Title = 'Grassland 100%',
      colorList = "darkolivegreen2",
      ySide = 4,
      yAdj = -0.92,
      marz = 2,
      mary = 0,
      width = 3.2,
      height = 4.24,
      NewWindow = FALSE,
    )
    
    # Both OS
    
    atVect = c(1.1, 1.9, 3.1, 3.9, 5.1, 5.9, 7.1, 7.9)
    atTicks = c(1.5, 3.5, 5.5, 7.5)
    
    #97% Conv/Org
    subDataC <- data.frame(g2$TDAF, g2$Curve)
    subDataC$LU <- rep('Conv', length(subDataC$g2.Curve))
    names(subDataC) <- c('TDAF', 'Curve', 'LU')
    
    subDataO <- data.frame(g5$TDAF, g5$Curve)
    subDataO$LU <- rep('Org', length(subDataO$g5.Curve))
    names(subDataO) <- c('TDAF', 'Curve', 'LU')
    
    subDataC <- rbind(subDataC, subDataO)
    
    myboxplot(
      x = subDataC$TDAF,
      y = subDataC$LU,
      z = subDataC$Curve,
      Interaction = 2,
      atBox = atVect,
      Rotation = TRUE,
      Graus = 20,
      Adj = 0.35,
      atTicks = atTicks,
      lablistx = lablistx2,
      ylim = Ylim2,
      # Rotation = TRUE,
      # Grau = 20,
      # Adj = 0.35,
      Title = '97% Conv or Org / 3% Grassland',
      ylab = 'Number of seeds/' ~ 'm' ^ '2' * '',
      colorList = rep(c('brown4', 'aquamarine4'), 4),
      NewWindow = FALSE
    )
    abline(v = 2.5, lty = 3)
    abline(v = 4.5, lty = 3)
    abline(v = 6.5, lty = 3)
    
    #50% Conv/Org
    subDataC <- data.frame(g3$TDMLU, g3$Curve)
    subDataC$LU <- rep('Conv', length(subDataC$g3.Curve))
    names(subDataC) <- c('TDAF', 'Curve', 'LU')
    
    subDataO <- data.frame(g6$TDMLU, g6$Curve)
    subDataO$LU <- rep('Org', length(subDataO$g6.Curve))
    names(subDataO) <- c('TDAF', 'Curve', 'LU')
    
    subDataC <- rbind(subDataC, subDataO)
    
    myboxplot(
      x = subDataC$TDAF,
      y = subDataC$LU,
      z = subDataC$Curve,
      Interaction = 2,
      atBox = atVect,
      Rotation = TRUE,
      Graus = 20,
      Adj = 0.35,
      atTicks = atTicks,
      lablistx = lablistx2,
      lablisty = rep(NA, 5),
      # Rotation = TRUE,
      # Grau = 20,
      #Adj = 0.35,
      ylim = Ylim2,
      Title = '50% Conv or Org / 50% Grassland',
      ylab = '',
      colorList = rep(c('brown4', 'aquamarine4'), 4),
      NewWindow = FALSE
    )
    abline(v = 2.5, lty = 3)
    abline(v = 4.5, lty = 3)
    abline(v = 6.5, lty = 3)
    
  }
  
    
    ###################################
    #
    #	2. Weed Diversity
    #
    ###################################
    
    # 100%
    
    i = 13
    Ylim = c(0.58, 0.75)
    #group 1 - Conv 100%
    myboxplot(
      x = g1[, i],
      y = g1$Curve,
      Interaction = 1,
      Rotation = TRUE,
      Graus = 20,
      Adj = 0.35,
      xlab = '',
      ylab = 'Weed diversity',
      ylim = Ylim,
      Title = 'Conventional 100%',
      colorList = 'brown4',
      width = 3.2,
      height = 4.24,
    )
    #group 4 - Org 100%
    myboxplot(
      x = g4[, i],
      y = g4$Curve,
      Interaction = TRUE,
      Rotation = TRUE,
      Graus = 20,
      Adj = 0.35,
      xlab = '',
      ylab = '',
      lablisty = rep(NA, 6),
      ylim = Ylim,
      Title = 'Organic 100%',
      colorList = "aquamarine4",
      mary = 0,
      
    )
    #group 7 - PM 100%
    myboxplot(
      x = g7[, i],
      y = g7$Curve,
      Interaction = TRUE,
      Rotation = TRUE,
      Graus = 20,
      Adj = 0.35,
      xlab = '',
      ylab = '',
      ylim = Ylim,
      Title = 'Grassland 100%',
      colorList = "darkolivegreen2",
      ySide = 4,
      yAdj = -0.9,
      marz = 1,
      mary = 0,
    )
    
    # Both OS
    
    atVect = c(1.1, 1.9, 3.1, 3.9, 5.1, 5.9, 7.1, 7.9)
    atTicks = c(1.5, 3.5, 5.5, 7.5)
    
    #97% Conv/Org
    subDataC <- data.frame(g2$WD, g2$Curve)
    subDataC$LU <- rep('Conv', length(subDataC$g2.Curve))
    names(subDataC) <- c('WD', 'Curve', 'LU')
    
    subDataO <- data.frame(g5$WD, g5$Curve)
    subDataO$LU <- rep('Org', length(subDataO$g5.Curve))
    names(subDataO) <- c('WD', 'Curve', 'LU')
    
    subDataC <- rbind(subDataC, subDataO)
    
    myboxplot(
      x = subDataC$WD,
      y = subDataC$LU,
      z = subDataC$Curve,
      Interaction = 2,
      atBox = atVect,
      atTicks = atTicks,
      lablistx = lablistx2,
      # Rotation = TRUE,
      # Grau = 20,
      #Adj = 0.35,
      ylim = c(0.5, 1),
      Title = '97% Conv or Org / 3% Grassland',
      ylab = 'Weed diversity',
      colorList = rep(c('brown4', 'aquamarine4'), 4)
    )
    abline(v = 2.5, lty = 3)
    abline(v = 4.5, lty = 3)
    abline(v = 6.5, lty = 3)
    
    
    #50% Conv/Org
    subDataC <- data.frame(g3$WD, g3$Curve)
    subDataC$LU <- rep('Conv', length(subDataC$g3.Curve))
    names(subDataC) <- c('WD', 'Curve', 'LU')
    
    subDataO <- data.frame(g6$WD, g6$Curve)
    subDataO$LU <- rep('Org', length(subDataO$g6.Curve))
    names(subDataO) <- c('WD', 'Curve', 'LU')
    
    subDataC <- rbind(subDataC, subDataO)
    
    myboxplot(
      x = subDataC$WD,
      y = subDataC$LU,
      z = subDataC$Curve,
      Interaction = 2,
      atBox = atVect,
      atTicks = atTicks,
      lablistx = lablistx2,
      # Rotation = TRUE,
      # Grau = 20,
      #Adj = 0.35,
      ylim = c(0.5, 1),
      Title = '50% Conv or Org / 50% Grassland',
      ylab = 'Weed diversity',
      colorList = rep(c('brown4', 'aquamarine4'), 4)
    )
    abline(v = 2.5, lty = 3)
    abline(v = 4.5, lty = 3)
    abline(v = 6.5, lty = 3)
    
    
    ###################################
    #
    #	3. Propagation speed
    #
    ###################################
    
    # 100%
    i = 32 #Getting v01 (speed)
    Ylim = c(50, 400)
    #group 1 - Conv 100%
    myboxplot(
      x = g1[, i],
      y = g1$Curve,
      Interaction = 1,
      Rotation = TRUE,
      Graus = 20,
      Adj = 0.35,
      xlab = '',
      ylab = 'Propagation speed (m/year)',
      ylim = Ylim,
      Title = '100% Conventional',
      colorList = 'brown4',
      width = 3.2,
      height = 4.24,
      NewWindow = FALSE
    )
    #group 4 - Org 100%
    myboxplot(
      x = g4[, i],
      y = g4$Curve,
      Interaction = 1,
      Rotation = TRUE,
      Graus = 20,
      Adj = 0.35,
      xlab = '',
      ylab = '',
      ylim = Ylim,
      lablisty = rep(NA, 6),
      Title = '100% Organic',
      colorList = "aquamarine4",
      NewWindow = FALSE
    )
    #group 7 - PM 100%
    myboxplot(
      x = g7[, i],
      y = g7$Curve,
      Interaction = 1,
      Rotation = TRUE,
      Graus = 20,
      Adj = 0.35,
      xlab = '',
      ylab = '',
      ylim = Ylim,
      lablisty = rep(NA, 6),
      Title = '100% Grassland',
      colorList = "darkolivegreen2",
      yAdj = -0.9,
      NewWindow = FALSE
    )
    
    # Both OS
    
    atVect = c(1.1, 1.9, 3.1, 3.9, 5.1, 5.9, 7.1, 7.9)
    atTicks = c(1.5, 3.5, 5.5, 7.5)
    
    #97% Conv/Org
    subDataC <- data.frame(g2$v01, g2$Curve)
    subDataC$LU <- rep('Conv', length(subDataC$g2.Curve))
    names(subDataC) <- c('v01', 'Curve', 'LU')
    
    subDataO <- data.frame(g5$v01, g5$Curve)
    subDataO$LU <- rep('Org', length(subDataO$g5.Curve))
    names(subDataO) <- c('v01', 'Curve', 'LU')
    
    subDataC <- rbind(subDataC, subDataO)
    
    myboxplot(
      x = subDataC$v01,
      y = subDataC$LU,
      z = subDataC$Curve,
      Interaction = 2,
      atBox = atVect,
      Rotation = TRUE,
      Graus = 20,
      Adj = 0.35,
      atTicks = atTicks,
      lablistx = lablistx2,
      # Rotation = TRUE,
      # Grau = 20,
      #Adj = 0.35,
      ylim = c(50, 380),
      Title = '97% Conv or Org / 3% Grassland',
      ylab = 'Propagation speed (m/year)',
      colorList = rep(c('brown4', 'aquamarine4'), 4),
      NewWindow = FALSE
    )
    abline(v = 2.5, lty = 3)
    abline(v = 4.5, lty = 3)
    abline(v = 6.5, lty = 3)
    
    #50% Conv/Org
    subDataC <- data.frame(g3$v01, g3$Curve)
    subDataC$LU <- rep('Conv', length(subDataC$g3.Curve))
    names(subDataC) <- c('v01', 'Curve', 'LU')
    
    subDataO <- data.frame(g6$v01, g6$Curve)
    subDataO$LU <- rep('Org', length(subDataO$g6.Curve))
    names(subDataO) <- c('v01', 'Curve', 'LU')
    
    subDataC <- rbind(subDataC, subDataO)
    
    myboxplot(
      x = subDataC$v01,
      y = subDataC$LU,
      z = subDataC$Curve,
      Interaction = 2,
      atBox = atVect,
      Rotation = TRUE,
      Graus = 20,
      Adj = 0.35,
      atTicks = atTicks,
      lablistx = lablistx2,
      # Rotation = TRUE,
      # Grau = 20,
      #Adj = 0.35,
      ylim = Ylim,
      Title = '50% Conv or Org / 50% Grassland',
      ylab = 'Propagation speed (m/year)',
      colorList = rep(c('brown4', 'aquamarine4'), 4),
      NewWindow = FALSE
    )
    abline(v = 2.5, lty = 3)
    abline(v = 4.5, lty = 3)
    abline(v = 6.5, lty = 3)
    
  }
  
    #Landscape inference
  { 
    # Weed diversity in 50% MLUprop (exemple to the report)
    FMinf <- read.table('D:/Willian/R/FMinf.txt', head = T)
    datFM <- dat
    datFM$FieldMap <- rep(FMinf$nbF, each = 70)
    
    # Getting out the PM data
    dat.SPM <- datFM
    dat.SPM <- dat.SPM[which(dat.SPM$MainLandUse != 'PM'), ]
    dat.SPMc <- dat.SPM[which(dat.SPM$MainLandUse == 'Conv'), ]
    dat.SPMo <- dat.SPM[which(dat.SPM$MainLandUse == 'Org'), ]
    dat.PM <- datFM[which(datFM$MainLandUse == 'PM'), ]
    dat.PM <- dat.PM[which(dat.PM$MLUprop == 1), ]
    
    # Getting just the PM data
    datPM <- dat
    datPM100 <- datPM[which(dat$MainLandUse == 'PM'), c(2, 7, 9, 13)]
    datPMother <- datPM[which(dat$MLUprop == c(0.97, 0.5)), c(2, 7, 9, 13)]
    names(datPMother) <- c('Curve', 'FieldMap', 'MLUprop', 'WD')
    
    datPMt <- rbind(datPM100, datPMother)
    
    #Plot nombre de graine pour landscape MLUprop
    
    atVect1 = seq(0.8, 8.8, 1)
    atVect2 = seq(10.2, 18.2, 1)
    atTicks = c(4.8, 14.2)
    lablistx3 = c('Conventional', 'Organic')
    
    options(scipen = 999) #force R do not use scientific notation
    
    # For Conv and Org OS
    myboxplot(
      x = dat.SPMc$WD[which(dat.SPMc$MLUprop == 0.5)],
      y = dat.SPMc$FieldMap[which(dat.SPMc$MLUprop == 0.5)],
      Interaction = 1,
      atBox = atVect1,
      atTicks = atTicks,
      lablistx = lablistx3,
      # Rotation = TRUE,
      # Grau = 20,
      #Adj = 0.35,
      xlim = c(1, 18.1),
      ylim = c(0.47, 1),
      Title = '50% of Conv/Org land use and 50% of Grassland',
      ylab = 'Weed diversity',
      colorList = 'brown4',
      width = 8.4,
    )
    abline(v = 9.5, lty = 3)
    boxplot(
      dat.SPMo$WD[which(dat.SPMo$MLUprop == 0.5)] ~ dat.SPMo$FieldMap[which(dat.SPMo$MLUprop == 0.5)],
      at = atVect2,
      add = TRUE,
      xaxt = 'n',
      yaxt = 'n',
      col = "aquamarine4",
      cex = 0.8
    )
    
    #Figure not used
    
    # FieldMap information
    FMinf <- read.table('D:/Willian/R/FMinf.txt', head = T)
    datFM <- dat
    datFM$FieldMap <- rep(FMinf$nbF, each = 70)
    
    # Getting out the PM data
    dat.SPM <- datFM
    dat.SPM <- dat.SPM[which(dat.SPM$MainLandUse != 'PM'), ]
    dat.SPMc <- dat.SPM[which(dat.SPM$MainLandUse == 'Conv'), ]
    dat.SPMo <- dat.SPM[which(dat.SPM$MainLandUse == 'Org'), ]
    dat.PM <- datFM[which(datFM$MainLandUse == 'PM'), ]
    dat.PM <- dat.PM[which(dat.PM$MLUprop == 1), ]
    
    # Getting just the PM data
    datPM <- dat
    datPM100 <- datPM[which(dat$MainLandUse == 'PM'), c(2, 7, 9, 15)]
    datPMother <- datPM[which(dat$MLUprop == c(0.97, 0.5)), c(2, 7, 9, 16)]
    names(datPMother) <- c('Curve', 'FieldMap', 'MLUprop', 'TDMLU')
    
    datPMt <- rbind(datPM100, datPMother)
    
    #Plot nombre de graine pour landscape MLUprop
    
    atVect = c(seq(0.8, 8.8, 1), seq(10.2, 18.2, 1), seq(19.6, 27.6, 1))
    atTicks = c(4.8, 14.2, 23.6)
    lablistx3 = c('50% of Main land use', '97% of Main land use', '100% of Main land use')
    
    options(scipen = 999) #force R do not use scientific notation
    
    # For Conv and Org OS
    myboxplot(
      x = dat.SPMc$TDMLU,
      y = dat.SPMc$FieldMap,
      z = dat.SPMc$MLUprop,
      Interaction = 2,
      atBox = atVect,
      atTicks = atTicks,
      lablistx = lablistx3,
      # Rotation = TRUE,
      # Grau = 20,
      #Adj = 0.35,
      xlim = c(1, 27.5),
      ylim = c(0, 1220000),
      Title = 'Number of seeds by 9 different landscapes',
      ylab = 'Number of seeds/' ~ 'm' ^ '2' * '',
      colorList = 'brown4',
      width = 8.4,
    )
    abline(v = 9.5, lty = 3)
    abline(v = 18.9, lty = 3)
    boxplot(
      dat.SPMo$TDMLU ~ dat.SPMo$FieldMap + dat.SPMo$MLUprop,
      at = atVect,
      add = TRUE,
      xaxt = 'n',
      yaxt = 'n',
      col = "aquamarine4",
      cex = 0.8
    )
    
    
    bx <-
      boxplot(
        datPMt$TDMLU[which(datPMt$MLUprop == c(0.97, 0.5))] ~ datPMt$FieldMap[which(datPMt$MLUprop == c(0.97, 0.5))] +
          datPMt$MLUprop[which(datPMt$MLUprop == c(0.97, 0.5))],
        at = atVect,
        add = TRUE,
        xaxt = 'n',
        yaxt = 'n',
        col = "darkolivegreen2",
        cex = 0.8
      )
    
    
    # For PM OS (manually)
    myboxplot(
      x = dat.PM$TDMLU,
      y = dat.PM$FieldMap,
      z = dat.PM$MLUprop,
      Interaction = 2,
      atBox = atVect,
      atTicks = atTicks,
      lablistx = lablistx3,
      yAdj = -.9,
      xlim = c(1, 27.5),
      ylim = c(-2, 250),
      colorList = "darkolivegreen2",
      width = 8.875,
      height = 1.86,
      marz = 2,
      marx = 0,
      ySide = 4,
      NewWindow = FALSE,
    )
    abline(h = 230, lty = 3)
    
    #=============================================================================================
    #PLot by lanscape and curve
    
    atVect = c(seq(0.8, 8.8, 1),
               seq(10.2, 18.2, 1),
               seq(19.6, 27.6, 1),
               seq(29, 37, 1))
    atTicks = c(4.8, 14.2, 23.6, 33)
    lablistx3 = c('Exponential', 'f2dt', 'Gaussian', 'Weibull')
    
    options(scipen = 999) #force R do not use scientific notation
    
    # For Conv and Org OS
    myboxplot(
      x = dat.SPMc$TDMLU,
      y = dat.SPMc$FieldMap,
      z = dat.SPMc$Curve,
      Interaction = 2,
      atBox = atVect,
      atTicks = atTicks,
      lablistx = lablistx3,
      # Rotation = TRUE,
      # Grau = 20,
      #Adj = 0.35,
      xlim = c(1, 36.5),
      ylim = c(0, 1220000),
      Title = 'Number of seeds by 9 different landscapes',
      ylab = 'Number of seeds/' ~ 'm' ^ '2' * '',
      colorList = 'brown4',
      width = 8.55,
    )
    abline(v = 9.5, lty = 3)
    abline(v = 18.9, lty = 3)
    abline(v = 28.3, lty = 3)
    
    boxplot(
      dat.SPMo$TDMLU ~ dat.SPMo$FieldMap + dat.SPMo$Curve,
      at = atVect,
      add = TRUE,
      xaxt = 'n',
      yaxt = 'n',
      col = "aquamarine4",
      cex = 0.8
    )
    
    boxplot(
      datPMt$TDMLU ~ datPMt$FieldMap + datPMt$Curve,
      at = atVect,
      add = TRUE,
      xaxt = 'n',
      yaxt = 'n',
      col = "darkolivegreen2",
      cex = 0.8
    )
    
    # For PM OS (manually)
    myboxplot(
      x = dat.PM$TDMLU,
      y = dat.PM$FieldMap,
      z = dat.PM$Curve,
      Interaction = 2,
      atBox = atVect,
      atTicks = atTicks,
      lablistx = lablistx3,
      yAdj = -.9,
      xlim = c(1, 27.5),
      ylim = c(-2, 250),
      colorList = "darkolivegreen2",
      width = 8.875,
      height = 1.86,
      marz = 2,
      marx = 0,
      ySide = 4,
      NewWindow = FALSE,
    )
    abline(h = 130, lty = 3)
    
    
    ##################
    #				 #
    #     Boxe 1     #
    #				 #
    ##################
    
    #	Box 1a. relation between number of seed in the all landscape and in the Main Land Use
    
    # creating vector with color
    
    #By dispersal curve
    CurveColour <-
      ifelse(dat$Curve == 'Exponential',
             'blue',
             ifelse(
               dat$Curve == 'f2dt',
               '#00A600',
               ifelse(dat$Curve == 'Gaussian', 'yellow', 'red')
             ))
    
    #By Main Land Use
    MLUColour <-
      ifelse(
        dat$MainLandUse == 'Conv',
        '#8B2323',
        ifelse(dat$MainLandUse == 'Org', '#458B74', '#BCEE68')
      )
    
    #By Main Land Use Proportion
    MLUpropColour <-
      ifelse(dat$MLUprop == 1,
             'red',
             ifelse(dat$MLUprop == 0.97, 'green', 'blue'))
    
    # plot
    myplot(
      x = sqrt(dat$TDAF),
      y = sqrt(dat$TDMLU),
      Col = addTrans(MLUColour, 10),
      aAdjx = -.93,
      aAdjy = -.9,
      xlab = 'NS in the landscape',
      ylab = 'NS in the main land use'
    )
    
    # legend
    reg <- lm(sqrt(dat$TDMLU) ~ sqrt(dat$TDAF))
    abline(reg)
    r2 <- round(summary(reg)$adj.r.squared, 2)
    legend('bottomright', legend = paste('R2 = ', r2), bty = 'n')
    legend('topleft', legend =)
  }}}

#Annexes
{
  #Annexe 4.
  
  setwd('D:/Willian/R')
  dat <- read.table(
    'dispType.txt',
    head = T,
    dec = ',',
    na.strings = 'NA'
  )
  names(dat)
  
  #plot meand distance by seed mass and plant hight
  myplot(
    x = dat$m,
    y = dat$mean,
    ylab = 'Mean distance (m)',
    aAdjx = -.9,
  )
  reg <- lm(dat$mean ~ dat$m)
  r2 <- round(summary(reg)$adj.r.squared, 2)
  f <- summary(reg)$fstatistic
  p <- round(pf(f[1], f[2], f[3], lower.tail = F), 2)
  attributes(p) <- NULL
  
  legend('topright',
         legend = paste('R2 = ', r2, '\np = ', p),
         bty = 'n')
  
  myplot(x = dat$h,
         y = dat$mean,
         aAdjx = -.9,)
  reg <- lm(dat$mean ~ dat$h)
  r2 <- round(summary(reg)$adj.r.squared, 2)
  f <- summary(reg)$fstatistic
  p <- round(pf(f[1], f[2], f[3], lower.tail = F), 2)
  attributes(p) <- NULL
  
  legend('topright',
         legend = paste('R2 = ', r2, '\np = ', p),
         bty = 'n')
  
  #plot max distance by seed mass and plant hight
  myplot(
    x = dat$m,
    y = dat$max,
    xlab = 'Seed mass (g)',
    ylab = 'Maximum distance (m)',
    aAdjx = -.9,
  )
  reg <- lm(dat$max ~ dat$m)
  r2 <- round(summary(reg)$adj.r.squared, 2)
  f <- summary(reg)$fstatistic
  p <- round(pf(f[1], f[2], f[3], lower.tail = F), 2)
  attributes(p) <- NULL
  legend('topright',
         legend = paste('R2 = ', r2, '\np = ', p),
         bty = 'n')
  
  myplot(
    x = dat$h,
    y = dat$max,
    xlab = 'Plant height (m)',
    aAdjx = -.9,
  )
  reg <- lm(dat$max ~ dat$h)
  r2 <- round(summary(reg)$adj.r.squared, 2)
  f <- summary(reg)$fstatistic
  p <- round(pf(f[1], f[2], f[3], lower.tail = F), 2)
  attributes(p) <- NULL
  
  legend('topright',
         legend = paste('R2 = ', r2, '\np = ', p),
         bty = 'n')
  
  
  
  #===============================================================================
  #===============================================================================
  
  #separating data for boxplot
  #8 different groups
  # 1. Conv 100%
  # 2. Conv 97%
  # 3. Conv 50%
  # 4. Org 100%
  # 5. Org 97%
  # 6. Org 50%
  # 7. PM 50%
  # 8. All OS and all proportion
  
  g1 <- dat[which(dat$MainLandUse == 'Conv' & dat$MLUprop == 1), ]
  g2 <- dat[which(dat$MainLandUse == 'Conv' & dat$MLUprop == 0.97), ]
  g3 <- dat[which(dat$MainLandUse == 'Conv' & dat$MLUprop == 0.5), ]
  g4 <- dat[which(dat$MainLandUse == 'Org' & dat$MLUprop == 1), ]
  g5 <- dat[which(dat$MainLandUse == 'Org' & dat$MLUprop == 0.97), ]
  g6 <- dat[which(dat$MainLandUse == 'Org' & dat$MLUprop == 0.5), ]
  g7 <- dat[which(dat$MainLandUse == 'PM' & dat$MLUprop == 1), ]
  g8 <- dat
  
  #lablist for boxplot
  lablistx = c('Conv 100%',
               'Conv 97%',
               'Conv 50%',
               'Org 100%',
               'Org 97%',
               'Org 50%',
               'PM 50%')
  
  #========================================================================================
  
  #Next 4 graphics represent all Curves together
  
  #Number of seed - All landscape
  i = 14
  myboxplot(
    g1[, i],
    g2[, i],
    g3[, i],
    g4[, i],
    g5[, i],
    g6[, i],
    g7[, i],
    lablistx = lablistx,
    Rotation = TRUE,
    Graus = 45,
    Adj = 0.8,
    ylab = 'Number of seeds',
    Title = 'All landscape',
    colorList = c(rep('brown4', 3), rep("aquamarine4", 3), "darkolivegreen2"),
    width = 7,
  )
  
  #Number of seed - Main Land Use
  i = 15
  myboxplot(
    c(g1[, i], g2[, i]),
    #g3[,i],g4[,i],g5[,i],g6[,i],g7[,i],
    at = c(1, 2),
    lablistx = '',
    Rotation = TRUE,
    Graus = 45,
    Adj = 0.8,
    ylab = 'Number of seeds',
    Title = 'Main Land Use',
    colorList = c(rep('brown4', 3), rep("aquamarine4", 3), "darkolivegreen2"),
    width = 7,
  )
  
  #Weed diversity
  i = 13
  myboxplot(
    g1[, i],
    g2[, i],
    g3[, i],
    g4[, i],
    g5[, i],
    g6[, i],
    g7[, i],
    lablistx = '',
    Rotation = TRUE,
    Graus = 45,
    Adj = 0.8,
    ylab = 'Weed diversity',
    Title = 'Weed diversity',
    colorList = c(rep('brown4', 3), rep("aquamarine4", 3), "darkolivegreen2"),
    width = 7,
  )
  
  #time to reach all lanscape
  i = 29
  myboxplot(
    g1[, i],
    g2[, i],
    g3[, i],
    g4[, i],
    g5[, i],
    g6[, i],
    g7[, i],
    lablistx = lablistx,
    Rotation = TRUE,
    Graus = 45,
    Adj = 1,
    ylab = 'Time (year)',
    Title = 'Time to reach all landscape',
    colorList = c(rep('brown4', 3), rep("aquamarine4", 3), "darkolivegreen2"),
    width = 7,
  )
  
  #========================================================================================
  
  #Next graphics represent Curves separated
  
  #########################################
  #
  #Number of seed - All landscape
  #
  #########################################
  
  i = 14
  #group 1 - Conv 100%
  myboxplot(
    x = g1[, i],
    y = g1$Curve,
    Interaction = TRUE,
    Rotation = TRUE,
    Graus = 45,
    Adj = 1,
    lablistx = '',
    xlab = '',
    ylab = 'Number of seeds',
    Title = 'All landscape - Conv 100%',
    colorList = 'brown4',
    marx = 0,
  )
  #group 4 - Org 100%
  myboxplot(
    x = g4[, i],
    y = g4$Curve,
    Interaction = TRUE,
    Rotation = TRUE,
    Graus = 45,
    Adj = 1,
    lablistx = '',
    xlab = '',
    ylab = '',
    Title = 'All landscape - Org 100%',
    colorList = "aquamarine4",
    marx = 0,
  )
  #group 2 - Conv 97%
  myboxplot(
    x = g2[, i],
    y = g2$Curve,
    Interaction = TRUE,
    Rotation = TRUE,
    Graus = 45,
    Adj = 1,
    lablistx = '',
    ylab = 'Number of seeds',
    xlab = '',
    Title = 'Conv 97%',
    colorList = "brown4",
    marx = 0,
  )
  #group 5 - Org 97%
  myboxplot(
    x = g5[, i],
    y = g5$Curve,
    Interaction = TRUE,
    Rotation = TRUE,
    Graus = 45,
    Adj = 1,
    lablistx = '',
    xlab = '',
    ylab = '',
    Title = 'Org 97%',
    colorList = "aquamarine4",
    marx = 0,
  )
  #group 3 - Conv 50%
  myboxplot(
    x = g3[, i],
    y = g3$Curve,
    Interaction = TRUE,
    Rotation = TRUE,
    Graus = 45,
    Adj = 1,
    lablistx = '',
    xlab = '',
    ylab = 'Number of seeds',
    Title = 'Conv 50%',
    colorList = "brown4",
    marx = 0,
  )
  #group 6 - Org 50%
  myboxplot(
    x = g6[, i],
    y = g6$Curve,
    Interaction = TRUE,
    Rotation = TRUE,
    Graus = 45,
    Adj = 1,
    lablistx = '',
    xlab = '',
    ylab = '',
    Title = 'Org 50%',
    colorList = "aquamarine4",
    marx = 0,
  )
  #group 7 - PM 100%
  myboxplot(
    x = g7[, i],
    y = g7$Curve,
    Interaction = TRUE,
    Rotation = TRUE,
    Graus = 45,
    Adj = 1,
    xlab = '',
    ylab = 'Number of seeds',
    Title = 'PM 100%',
    colorList = "darkolivegreen2",
  )
  #group 8 - All OS and all proportion
  myboxplot(
    x = g8[, i],
    y = g8$Curve,
    Interaction = TRUE,
    Rotation = TRUE,
    Graus = 45,
    Adj = 1,
    xlab = '',
    ylab = '',
    Title = 'All OS and all proportion',
  )
  
  #########################################
  #
  #Number of seed - Main Land Use
  #
  #########################################
  
  i = 15
  #group 1 - Conv 100%
  myboxplot(
    x = g1[, i],
    y = g1$Curve,
    Interaction = TRUE,
    Rotation = TRUE,
    Graus = 45,
    Adj = 1,
    lablistx = '',
    xlab = '',
    ylab = 'Number of seeds',
    Title = 'Main Land Use - Conv 100%',
    colorList = 'brown4',
    marx = 0,
  )
  #group 4 - Org 100%
  myboxplot(
    x = g4[, i],
    y = g4$Curve,
    Interaction = TRUE,
    Rotation = TRUE,
    Graus = 45,
    Adj = 1,
    lablistx = '',
    xlab = '',
    ylab = '',
    Title = 'Main Land Use - Org 100%',
    colorList = "aquamarine4",
    marx = 0,
  )
  #group 2 - Conv 97%
  myboxplot(
    x = g2[, i],
    y = g2$Curve,
    Interaction = TRUE,
    Rotation = TRUE,
    Graus = 45,
    Adj = 1,
    lablistx = '',
    xlab = '',
    ylab = 'Number of seeds',
    Title = 'Conv 97%',
    colorList = "brown4",
    marx = 0,
  )
  #group 5 - Org 97%
  myboxplot(
    x = g5[, i],
    y = g5$Curve,
    Interaction = TRUE,
    Rotation = TRUE,
    Graus = 45,
    Adj = 1,
    lablistx = '',
    xlab = '',
    ylab = '',
    Title = 'Org 97%',
    colorList = "aquamarine4",
    marx = 0,
  )
  #group 3 - Conv 50%
  myboxplot(
    x = g3[, i],
    y = g3$Curve,
    Interaction = TRUE,
    Rotation = TRUE,
    Graus = 45,
    Adj = 1,
    lablistx = '',
    xlab = '',
    ylab = 'Number of seeds',
    Title = 'Conv 50%',
    colorList = "brown4",
    marx = 0,
  )
  #group 6 - Org 50%
  myboxplot(
    x = g6[, i],
    y = g6$Curve,
    Interaction = TRUE,
    Rotation = TRUE,
    Graus = 45,
    Adj = 1,
    lablistx = '',
    xlab = '',
    ylab = '',
    Title = 'Org 50%',
    colorList = "aquamarine4",
    marx = 0,
  )
  #group 7 - PM 100%
  myboxplot(
    x = g7[, i],
    y = g7$Curve,
    Interaction = TRUE,
    Rotation = TRUE,
    Graus = 45,
    Adj = 1,
    xlab = '',
    ylab = 'Number of seeds',
    Title = 'PM 100%',
    colorList = "darkolivegreen2",
  )
  #group 8 - All OS and all proportion
  myboxplot(
    x = g8[, i],
    y = g8$Curve,
    Interaction = TRUE,
    Rotation = TRUE,
    Graus = 45,
    Adj = 1,
    xlab = '',
    ylab = '',
    Title = 'All OS and all proportion',
  )
  
  
  #########################################
  #
  #Weed diversity
  #
  #########################################
  
  i = 13
  #group 1 - Conv 100%
  myboxplot(
    x = g1[, i],
    y = g1$Curve,
    Interaction = TRUE,
    Rotation = TRUE,
    Graus = 45,
    Adj = 1,
    lablistx = '',
    xlab = '',
    ylim = c(0.55, 0.8),
    ylab = 'Weed diversity ',
    Title = 'Weed diversity  - Conv 100%',
    colorList = 'brown4',
    marx = 0,
  )
  #group 4 - Org 100%
  myboxplot(
    x = g4[, i],
    y = g4$Curve,
    Interaction = TRUE,
    Rotation = TRUE,
    Graus = 45,
    Adj = 1,
    lablistx = '',
    xlab = '',
    ylim = c(0.55, 0.8),
    ylab = '',
    Title = 'Weed diversity  - Org 100%',
    colorList = "aquamarine4",
    marx = 0,
  )
  #group 2 - Conv 97%
  myboxplot(
    x = g2[, i],
    y = g2$Curve,
    Interaction = TRUE,
    Rotation = TRUE,
    Graus = 45,
    Adj = 1,
    lablistx = '',
    xlab = '',
    ylim = c(0.55, 0.8),
    ylab = 'Weed diversity',
    Title = 'Conv 97%',
    colorList = "brown4",
    marx = 0,
  )
  #group 5 - Org 97%
  myboxplot(
    x = g5[, i],
    y = g5$Curve,
    Interaction = TRUE,
    Rotation = TRUE,
    Graus = 45,
    Adj = 1,
    lablistx = '',
    xlab = '',
    ylim = c(0.55, 0.8),
    ylab = '',
    Title = 'Org 97%',
    colorList = "aquamarine4",
    marx = 0,
  )
  #group 3 - Conv 50%
  myboxplot(
    x = g3[, i],
    y = g3$Curve,
    Interaction = TRUE,
    Rotation = TRUE,
    Graus = 45,
    Adj = 1,
    lablistx = '',
    xlab = '',
    ylim = c(0.5, 1),
    ylab = 'Weed diversity',
    Title = 'Conv 50%',
    colorList = "brown4",
    marx = 0,
  )
  #group 6 - Org 50%
  myboxplot(
    x = g6[, i],
    y = g6$Curve,
    Interaction = TRUE,
    Rotation = TRUE,
    Graus = 45,
    Adj = 1,
    lablistx = '',
    xlab = '',
    ylim = c(0.5, 1),
    ylab = '',
    Title = 'Org 50%',
    colorList = "aquamarine4",
    marx = 0,
  )
  #group 7 - PM 100%
  myboxplot(
    x = g7[, i],
    y = g7$Curve,
    Interaction = TRUE,
    Rotation = TRUE,
    Graus = 45,
    Adj = 1,
    xlab = '',
    ylim = c(0.65, 0.7),
    ylab = 'Weed diversity',
    Title = 'PM 100%',
    colorList = "darkolivegreen2",
  )
  #group 8 - All OS and all proportion
  myboxplot(
    x = g8[, i],
    y = g8$Curve,
    Interaction = TRUE,
    Rotation = TRUE,
    Graus = 45,
    Adj = 1,
    xlab = '',
    ylab = '',
    Title = 'All OS and all proportion',
  )
  
  boxplot(dat$WD ~ dat$Curve * dat$MainLandUse)
  Anova(lm(dat$WD ~ dat$Curve + dat$MainLandUse + dat$MLUprop), type = "III")
  
  
  #########################################
  #
  #Time to reach all the landscape
  #
  #########################################
  
  i = 29
  #group 1 - Conv 100%
  myboxplot(
    x = g1[, i],
    y = g1$Curve,
    Interaction = TRUE,
    Rotation = TRUE,
    Graus = 45,
    Adj = 1,
    lablistx = '',
    ylim = c(12, 16),
    xlab = '',
    ylab = 'Time (years)',
    Title = 'Time to reach all the landscape',
    colorList = 'brown4',
    marx = 0,
  )
  #group 4 - Org 100%
  myboxplot(
    x = g4[, i],
    y = g4$Curve,
    Interaction = TRUE,
    Rotation = TRUE,
    Graus = 45,
    Adj = 1,
    lablistx = '',
    ylim = c(12, 16),
    xlab = '',
    ylab = '',
    Title = 'Time to reach all the landscape',
    colorList = "aquamarine4",
    marx = 0,
  )
  #group 2 - Conv 97%
  myboxplot(
    x = g2[, i],
    y = g2$Curve,
    Interaction = TRUE,
    Rotation = TRUE,
    Graus = 45,
    Adj = 1,
    lablistx = '',
    ylim = c(12, 16),
    ylab = 'Time (years)',
    xlab = '',
    Title = 'Conv 97%',
    colorList = "brown4",
    marx = 0,
  )
  #group 5 - Org 97%
  myboxplot(
    x = g5[, i],
    y = g5$Curve,
    Interaction = TRUE,
    Rotation = TRUE,
    Graus = 45,
    Adj = 1,
    lablistx = '',
    ylim = c(12, 16),
    xlab = '',
    ylab = '',
    Title = 'Org 97%',
    colorList = "aquamarine4",
    marx = 0,
  )
  #group 3 - Conv 50%
  myboxplot(
    x = g3[, i],
    y = g3$Curve,
    Interaction = TRUE,
    Rotation = TRUE,
    Graus = 45,
    Adj = 1,
    lablistx = '',
    xlab = '',
    ylab = 'Time (years)',
    Title = 'Conv 50%',
    colorList = "brown4",
    marx = 0,
  )
  #group 6 - Org 50%
  myboxplot(
    x = g6[, i],
    y = g6$Curve,
    Interaction = TRUE,
    Rotation = TRUE,
    Graus = 45,
    Adj = 1,
    lablistx = '',
    xlab = '',
    ylab = '',
    Title = 'Org 50%',
    colorList = "aquamarine4",
    marx = 0,
  )
  #group 7 - PM 100%
  myboxplot(
    x = g7[, i],
    y = g7$Curve,
    Interaction = TRUE,
    Rotation = TRUE,
    Graus = 45,
    Adj = 1,
    ylim = c(10, 51),
    xlab = '',
    ylab = 'Time (years)',
    Title = 'PM 100%',
    colorList = "darkolivegreen2",
  )
  #group 8 - All OS and all proportion
  myboxplot(
    x = g8[, i],
    y = g8$Curve,
    Interaction = TRUE,
    Rotation = TRUE,
    Graus = 45,
    Adj = 1,
    ylim = c(10, 51),
    xlab = '',
    ylab = '',
    Title = 'All OS and all proportion',
  )
  
  
  #======================================================================================
  
  # graphs with the evolution of the variables over the 50 years simulation time
  
  #function to plot by curve and by field map
  ff = function(fmap, Curve) {
    sel = dat$Num[which(dat$Curve == Curve & dat$FieldMap == fmap)]
    plot(
      0,
      pch = '',
      xlim = c(1, 51),
      ylim = c(0, length(distCentralF[[fmap]]$IDField)),
      xlab = 'Time (years)',
      ylab = paste('??? of fields (FM = ', fmap, ')', sep = ''),
      main = Curve
    )
    temp_dat = data.frame(matrix(NA, ncol = 51, nrow = length(sel)))
    for (i in 1:length(sel)) {
      k = sel[i]
      temp_dat[i, ] = parcClist[[k]]$nbFc1
      lines(parcClist[[k]]$Time,
            parcClist[[k]]$nbFc1,
            col = addTrans('grey', 10),
            lwd = 2)
    }
    temp_dat2 = data.frame(matrix(NA, ncol = 2, nrow = 51))
    temp_dat2[, 1] = seq(1, 51)
    for (j in 1:51) {
      temp_dat2[j, 2] = median(temp_dat[, j])
    }
    lines(temp_dat2[, 1], temp_dat2[, 2], lwd = 2.5, lty = 5)
  }
  par(
    mfrow = c(3, 4),
    cex.axis = 0.9,
    oma = c(0, 0, 0, 0) + 0.1,
    mar = c(3.8, 3.8, 1, 1)
  )
  ff(fmap = 2, Curve = 'f2dt')
  
  #plotting all curves and field maps
  Curves = c('Weibull', 'f2dt', 'Gaussian', 'Exponential')
  FM = c(1:9)
  par(
    mfrow = c(3, 4),
    cex.axis = 0.9,
    oma = c(0, 0.5, 0, 0) + 0.1,
    mar = c(3.8, 3.8, 1.5, 1) + 0.1
  )
  for (i in 9) {
    fmap = FM[i]
    for (j in 1:4) {
      Curve = Curves[j]
      ff(fmap = fmap, Curve = Curve)
    }
  }
  
  #function to plot by field map with all curves together (different colors)
  ff3 = function(fmap) {
    sel = dat[which(dat$FieldMap == fmap), ]
    plot(
      0,
      pch = '',
      xlim = c(1, 51),
      ylim = c(0, length(distCentralF[[fmap]]$IDField)),
      xlab = 'Time (years)',
      ylab = '??? of fields',
      main = paste('Field Map', fmap)
    )
    
    sel1 = sel$Num[which(sel$Curve == 'Weibull')]
    temp_dat = data.frame(matrix(NA, ncol = 51, nrow = length(sel1)))
    for (i in 1:length(sel1)) {
      k = sel1[i]
      temp_dat[i, ] = parcClist[[k]]$nbFc1
      lines(parcClist[[k]]$Time,
            parcClist[[k]]$nbFc1,
            col = addTrans('green', 5),
            lwd = 1.8)
    }
    
    sel2 = sel$Num[which(sel$Curve == 'f2dt')]
    temp_dat2 = data.frame(matrix(NA, ncol = 51, nrow = length(sel2)))
    for (k in 1:length(sel2)) {
      k = sel2[k]
      temp_dat2[k, ] = parcClist[[k]]$nbFc1
      lines(parcClist[[k]]$Time,
            parcClist[[k]]$nbFc1,
            col = addTrans('red', 5),
            lwd = 1.8)
    }
    
    sel3 = sel$Num[which(sel$Curve == 'Gaussian')]
    temp_dat3 = data.frame(matrix(NA, ncol = 51, nrow = length(sel3)))
    for (l in 1:length(sel3)) {
      k = sel3[l]
      temp_dat3[l, ] = parcClist[[k]]$nbFc1
      lines(parcClist[[k]]$Time,
            parcClist[[k]]$nbFc1,
            col = addTrans('blue', 5),
            lwd = 1.8)
    }
    
    sel4 = sel$Num[which(sel$Curve == 'Exponential')]
    temp_dat4 = data.frame(matrix(NA, ncol = 51, nrow = length(sel4)))
    for (l in 1:length(sel4)) {
      k = sel4[l]
      temp_dat4[l, ] = parcClist[[k]]$nbFc1
      lines(parcClist[[k]]$Time,
            parcClist[[k]]$nbFc1,
            col = addTrans('yellow', 5),
            lwd = 1.8)
    }
    
    temp_dat5 = data.frame(matrix(NA, ncol = 5, nrow = 51))
    temp_dat5[, 1] = seq(1, 51)
    for (j in 1:51) {
      temp_dat5[j, 2] = median(temp_dat[, j])
      temp_dat5[j, 3] = median(temp_dat2[, j])
      temp_dat5[j, 4] = median(temp_dat3[, j])
      temp_dat5[j, 5] = median(temp_dat4[, j])
      
    }
    lines(temp_dat5[, 1],
          temp_dat5[, 2],
          lwd = 2.5,
          lty = 5,
          col = 'green')
    lines(temp_dat5[, 1],
          temp_dat5[, 3],
          lwd = 2.5,
          lty = 5,
          col = 'red')
    lines(temp_dat5[, 1],
          temp_dat5[, 4],
          lwd = 2.5,
          lty = 5,
          col = 'blue')
    lines(temp_dat5[, 1],
          temp_dat5[, 5],
          lwd = 2.5,
          lty = 5,
          col = 'yellow')
    
  }
  
  ff3(fmap = 1)
  
  #plotting all fields
  FM = c(1:9)
  par(
    mfrow = c(3, 3),
    cex.axis = 0.9,
    oma = c(0, 0.5, 0, 0) + 0.1,
    mar = c(3.8, 3.8, 1.5, 1) + 0.1
  )
  for (i in 1:9) {
    fmap = FM[i]
    ff3(fmap = fmap)
  }
  
  #function to plot Number of Seed (all seed) by Curve type
  ff6 = function(Curve, WeedType) {
    sel = dat$Num[which(dat$Curve == Curve &
                          dat$MainLandUse == c('Conv', 'Org'))]
    if (WeedType == 'WinterSmallS') {
      lim = 959000
      tt = 2
    }
    if (WeedType == 'WinterLargeS') {
      lim = 3800
      tt = 3
    }
    if (WeedType == 'SpringSmallS') {
      lim = 392000
      tt = 4
    }
    if (WeedType == 'SpringLargeS') {
      lim = 2900
      tt = 5
    }
    plot(
      0,
      pch = '',
      xlim = c(0, 50),
      ylim = c(0, lim),
      xlab = 'Time (years)',
      ylab = ~ "Number of seed " (n / m ^ 2),
      main = paste(Curve, '-', WeedType)
    )
    temp_dat = data.frame(matrix(NA, ncol = 50, nrow = length(sel)))
    for (i in 1:length(sel)) {
      k = sel[i]
      temp_dat[i, ] = WDSeed[[k]][, tt]
      lines(WDSeed[[k]][, tt], col = addTrans('grey', 10), lwd = 2)
    }
    temp_dat$MLU = rep(NA, length(temp_dat[, 1]))
    for (i in 1:length(sel)) {
      k = sel[i]
      temp_dat$MLU[i] = dat$MainLandUse[k]
    }
    temp_dat2 = data.frame(matrix(NA, ncol = 2, nrow = 50))
    temp_dat2[, 1] = seq(1, 50)
    for (j in 1:50) {
      temp_dat2[j, 2] = median(temp_dat[, j])
      temp_dat2[j, 3] = median(temp_dat[which(temp_dat$MLU == 1), j])
      temp_dat2[j, 4] = median(temp_dat[which(temp_dat$MLU == 2), j])
    }
    lines(temp_dat2[, 1], temp_dat2[, 2], lwd = 2.5, lty = 5)
    lines(temp_dat2[, 1],
          temp_dat2[, 3],
          lwd = 2.5,
          lty = 5,
          col = 'brown4')
    lines(temp_dat2[, 1],
          temp_dat2[, 4],
          lwd = 2.5,
          lty = 5,
          col = "aquamarine4")
    
    
    
    rm(temp_dat)
    rm(temp_dat2)
  }
  
  par(
    mfrow = c(3, 4),
    cex.axis = 0.9,
    oma = c(0, 0, 0, 0) + 0.1,
    mar = c(3.8, 3.9, 1, 1)
  )
  ff6(Curve = 'f2dt', WeedType = 'SpringLargeS')
  
  #plotting all fields together by curve
  Curves = c('Weibull', 'f2dt', 'Gaussian', 'Exponential')
  WeedTypes = c('WinterSmallS',
                'WinterLargeS',
                'SpringSmallS',
                'SpringLargeS')
  par(
    mfrow = c(4, 4),
    cex.axis = 0.9,
    oma = c(0, 0.5, 0, 0) + 0.1,
    mar = c(3.8, 3.9, 1.5, 1) + 0.1
  )
  for (j in 1:4) {
    WeedType = WeedTypes[j]
    for (i in 1:4) {
      Curve = Curves[i]
      ff6(Curve = Curve, WeedType = WeedType)
    }
  }
  
  
}




