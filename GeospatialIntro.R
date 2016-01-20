library(maptools)
library(raster)
library(rgdal)
library(sp)

#################
# 1. COORDINATES

# Get points
plotdat <- matrix(NA,nrow=0,ncol=57) # set up an empty matrix
states <- state.abb[which(!state.abb%in%c("HI"))] # exclude Hawaii
# could also download data by hand, 
#  but pointing and clicking is the new smoking
prg <- txtProgressBar(min = 0, max = length(states), char = "+",style = 3) # progress bar
for(s in 1:length(states)){
  st <- states[s]
  url <- paste0("http://apps.fs.fed.us/fiadb-downloads/",st,"_PLOT.csv")
  tmp <- read.csv(url)
  plotdat <- rbind(plotdat,tmp)
  setTxtProgressBar(prg, s)  
}
close(prg)

# only forested plots with coordinates
forestdat <- plotdat[
  which(
    plotdat$PLOT_STATUS_CD==1& # forested plot
    !is.na(plotdat$LON)& # has coordinates
    !duplicated(plotdat[,c('LAT','LON')]) # not duplicated coordinates
  ),] 


# need to define projected coordinates, longlat, datum & ellps WGS84
# Go to spatialreference.org
coo <- SpatialPoints(forestdat[,c("LON","LAT")], CRS("+init=epsg:4326"))
coo

par(mar=c(0,0,0,0))
plot(coo,pch=".",col="#00000050")

#################
# 2. SHAPEFILES

# Get shapefiles
download.file("http://dds.cr.usgs.gov/pub/data/nationalatlas/statesp010g.shp_nt00938.tar.gz",
              destfile="states.tar.gz")
untar("states.tar.gz", exdir="states")
usa.states <- readOGR(dsn="states", layer="statesp010g")
usa.states

corners <- usa.states[which(usa.states$NAME%in%c("Arizona","New Mexico","Colorado","Utah")),]

# what projection is it in?
corners
projection(corners)

# transform to match our other data
cornerst <- spTransform(corners, CRS("+init=epsg:4326"))
cornerst

cornerso <- unionSpatialPolygons(cornerst,IDs=rep(1,nrow(cornerst)))

lines(cornerst,col="deeppink",lwd=2)
lines(cornerso,lwd=2,lty=2)


#################
# 3. RASTERS

# Get rasters
# Data from:
# Dobrowski, S.Z., J. Abatzoglou, A.K. Swanson, J.A. Greenberg, A.R. Mynsberge, Z.A. Holden, M.K. Schwartz (2013) The climate velocity of the contiguous United States during the 20th century. Global Change Biology 19: 241-251. (http://adaptwest.databasin.org/pages/adaptwest-waterbalance)

download.file("http://www.cacpd.org.s3.amazonaws.com/water_bal_conus/AET_def_annual+sums/def_2000_2009.zip",
              destfile="def_2000_2009.zip")
unzip("def_2000_2009.zip", exdir="deficit")

ff <- list.files("deficit", full.names=TRUE)
def <- brick(ff) # why brick? single pointer file, smaller size, faster calculations
defa <- aggregate(def, 12) # to 0.1 degrees, or 12x the original resolution

# Do a calculation
mndef <- stackApply(defa, rep(1,nlayers(def)), fun="mean")
# reproject to match forest plots
defpr <- projectRaster(mndef, crs=projection(coo))

# How long does it take to plot?
system.time(plot(mndef)) # slower
system.time(image(mndef)) # less info
par(mar=c(0,0,2,0))
plot(defpr, main="Climatic water deficit (PET - AET)", col=terrain.colors(255))

########### IMPORTANT
# removeTmpFiles(h=0)


#################
# 4. DO STUFF

# a. Crop and mask raster
defcrop <- crop(defpr,extent(cornerso))
defmask <- mask(defcrop,cornerso)

# b. Rasterize points
cooras <- rasterize(coo,defpr,
                    fun=function(x,...) if(length(x)>0) 1, na.rm=T)
cooras[is.na(cooras)] <- 0
cooras[is.na(values(defpr))] <- NA

# c. Extract values at points
defpoint <- extract(defpr, coo)
dev.off()
hist(defpoint, freq=FALSE, col="#FF000050")
hist(values(defpr),add=TRUE, freq=FALSE, col="#0000FF50")

# d. Exclude points that are outside of four corners polygon
wp <- over(coo, cornerso)
coocor <- coo[which(wp==1),]
plot(coo,pch=".",col="#00000010")
points(coocor,col="#001A57",pch=".")

# e. Some statistics
forestmat <- data.frame(deficit=values(defpr), forest=values(cooras))
fmod <- glm(forest~deficit, data=forestmat, family=binomial())
summary(fmod)

par(mar=c(4,4,0.4,0.4))
boxplot(deficit~forest, data=forestmat, at=c(0,1),
        horizontal=T,las=1,staplewex=0,lty=1,outline=F,frame=F,
        ylab="Forest?",xlab="Climatic water deficit (PET - AET)")
defpre <- seq(1,2500)
forpre <- predict(fmod,data.frame(deficit=defpre),type="response")
lines(forpre~defpre,lwd=3)
forval <- -coef(fmod)[1]/coef(fmod)[2] # 50% probability
abline(v=forval, lwd=5, col="#001A57")


# f. contours

# transformed to US National Atlas Equal Area
cooplot <- spTransform(coo,CRS("+init=epsg:2163"))
defplot <- projectRaster(defpr,crs="+init=epsg:2163")

plot(defplot, col=terrain.colors(255),main="Deficit + Plots + Prediction")
points(cooplot,pch=".",col="#00000015")
contour(defplot,nlevels=1,labels="Pr=0.5",levels=forval,add=T,lwd=3,col="#001A57")


#################
# 5. LEAFLET
# https://rstudio.github.io/leaflet/
library(leaflet)
defpro2 <- projectRaster(mndef, crs="+init=epsg:3857")
m <- leaflet() %>% addTiles() # PIPES
m %>% addRasterImage(defpro2,opacity=0.5)
leaflet(cornerso) %>% addTiles() %>% addPolygons()

