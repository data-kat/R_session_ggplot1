

#*************************************************************************
#*************************************************************************
#               ** GGPLOT SESSION 3 (...... 2019) **            ####
#*************************************************************************
#*************************************************************************




# If you are not working in a version control R Studio Project,
#  You will need to use setwd() to specify your working directory
#   Note: you will need to use forward slashes instead of backslashes (Wibdows default),
#   and remove the "#" at teh beginning of the line
#setwd("C:\Users\OneDrive - scion\Documents\1 - Workspace\R_sessions\R_session_ggplot1")

# If you are working inside a project, jsut specify the subfolder you'll be working from
setwd("./Session3")


if(!require(readxl)){install.packages("readxl")}
if(!require(maptools)){install.packages("maptools")}
if(!require(ggmap)){install.packages("ggmap")} # part of tidyverse
if(!require(ggplot2)){install.packages("ggplot2")} # part of tidyverse
if(!require(gstat)){install.packages("gstat")}
if(!require(sp)){install.packages("sp")}

library(readxl)
library(maptools)
library(ggmap)
library(ggplot2)
library(gstat)
library(sp)


# # install.packages("devtools")
# library(devtools)
# devtools::install_github("dkahle/ggmap", ref = "tidyup")
# library(ggmap)
# chicago <- get_stamenmap(bbox = c(left = -88.0225, bottom = 41.5949, 
#                                   right = -87.2713, top = 42.0677), 
#                          zoom = 11)
# 
# ggmap(chicago)



library(OpenStreetMap)

map = openmap(upperLeft = c(lon = 171.5555, lat = -43.39838),
              lowerRight = c(lon = 171.5748, lat = -43), zoom = 12)


map = openmap(upperLeft = c(lon = 80, lat = 60),
              lowerRight = c(lon = 90, lat = 50), zoom = 12)



map <- openmap(c(-43.39838,171.5555),
               c(-43.41561,171.5748),
               zoom = 15, type="bing")


ggmap(map)

autoplot(map)

plot(map)

autoplot(map)



# Reproject to lat long:
map_latlon <- openproj(map, projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

autoplot(map_latlon) + geom_point(aes(x = LONG, y = LAT, col = TRANSECT), data = dat1, alpha = .5)




# Set a desired ggplot theme:
theme_set(theme_classic())




# Talk about metadata
# Talk about reading data in from .xlsx vs csv files



#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
######################      HEADING 1     ####################
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


# You can read data in form .xlsx Excel files with the help of readxl package
#   Reading Sheet #1, and ignoring the first 3 rows of the dataframe
#   (that's where my metadata is stored)
dat1 <- read_excel("Rakaia_gorse_with_meta.xlsx", 1, skip=1)



gorse=getKMLcoordinates("Burn_envelope.kml", 
                        ignoreAltitude = TRUE)

gorse1.sr = SpatialPolygons(list(Polygons(list(Polygon(gorse[1])), "x")))
gorse2.sr = SpatialPolygons(list(Polygons(list(Polygon(gorse[5])), "x")))

plot(gorse1.sr)
plot(gorse2.sr)



points(dat1$LAT~dat1$LONG, pch=21, bg="green")




map <- get_stamenmap(location = c(lon = 171.566, lat = -43.408), zoom = 12, maptype = "toner")

graphics.off()
ggmap(map) + geom_point(aes(x = LONG, y = LAT, col = TRANSECT), data = dat1, alpha = .5)

ggmap(map) + geom_point(aes(x = LONG, y = LAT, col = PT.HT), data = dat1, alpha = .5)


ggmap(map) + geom_point(aes(x = LONG, y = LAT, col = PT.HT), data = dat1, alpha = .5) +
      scale_color_gradient(low="yellow",high="red",guide="legend")+ 
  geom_path(data=gorse1.sr, aes(long, lat), colour = "grey") + 
  geom_path(data=gorse2.sr, aes(long, lat), colour = "grey")


max(dat1$PT.HT, na.rm=T)


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#      INTERPOLATING VALUES ON THE map
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


# duplicate dat1 to make sure we don't make any changes to dat1:
dat2=dat1

# define x & y as longitude and latitude:
dat2$x=dat2$LONG 
dat2$y=dat2$LAT

# Set spatial coordinates to create a Spatial object:
coordinates(dat2) = ~x + y


# Plot the results:
plot(dat2)

min(dat2$x)
max(dat2$x)
min(dat2$y)
max(dat2$y)

points(c(min(dat2$x), max(dat2$x), min(dat2$x), max(dat2$x)), c(min(dat2$y), min(dat2$y), max(dat2$y), max(dat2$y)), pch=21, bg="red")

x.range <- as.numeric(c(min(dat2$x)-0.001, max(dat2$x)+0.001))  # min/max longitude of the interpolation area
y.range <- as.numeric(c(min(dat2$y)-0.001, max(dat2$y)+0.001))  # min/max latitude of the interpolation area


#Create a data frame from all combinations of the supplied vectors or factors.
#   See the description of the return value for precise details of the way this is done.
#   Set spatial coordinates to create a Spatial object. Assign gridded structure:

grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.00005), y = seq(from = y.range[1], 
              to = y.range[2], by = 0.00005))  # expand points to grid
coordinates(grd) <- ~x + y
gridded(grd) <- TRUE


# Plot the weather station locations and interpolation grid:
  
plot(grd, cex = 1.5, col = "grey")
points(dat2, pch = 1, col = "red", cex = 1)


# Interpolate surface and fix the output - for PT.HT:
idw.ht <- idw(formula = PT.HT ~ 1, locations = dat2, 
             newdata = grd)  # apply idw model for the data
idw.ht = as.data.frame(idw.ht)  # output is defined as a data table
names(idw.ht)[1:3] <- c("long", "lat", "var1.pred")  # give names to the modelled variables

# Interpolate surface and fix the output - for GORSE.HT:
# substiture NAs with 0's:
dat2$GORSE.HT[is.na(dat2$GORSE.HT)] <- 0

idw.ght <- idw(formula = GORSE.HT ~ 1, locations = dat2, 
              newdata = grd)  # apply idw model for the data
idw.ght = as.data.frame(idw.ght)  # output is defined as a data table
names(idw.ght)[1:3] <- c("long", "lat", "var1.pred")  # give names to the modelled variables

# Interpolate surface and fix the output - for GORSE.PERC:
# substiture NAs with 0's:
dat2$GORSE.PERC[is.na(dat2$GORSE.PERC)] <- 0

idw.gperc <- idw(formula = GORSE.PERC ~ 1, locations = dat2, 
               newdata = grd)  # apply idw model for the data
idw.gperc = as.data.frame(idw.gperc)  # output is defined as a data table
names(idw.gperc)[1:3] <- c("long", "lat", "var1.pred")  # give names to the modelled variables

# Interpolate surface and fix the output - for GORSE.HP:
# substiture NAs with 0's:
dat2$GORSE.HP[is.na(dat2$GORSE.HP)] <- 0

idw.ghp <- idw(formula = GORSE.HP ~ 1, locations = dat2, 
                 newdata = grd)  # apply idw model for the data
idw.ghp = as.data.frame(idw.ghp)  # output is defined as a data table
names(idw.ghp)[1:3] <- c("long", "lat", "var1.pred")  # give names to the modelled variables


# Interpolate surface and fix the output - for walk grade (WK.GRADE):
idw.wlk <- idw(formula = WK.GRADE ~ 1, locations = dat2, 
                 newdata = grd)  # apply idw model for the data
idw.wlk = as.data.frame(idw.wlk)  # output is defined as a data table
names(idw.wlk)[1:3] <- c("long", "lat", "var1.pred")  # give names to the modelled variables

# Interpolate surface and fix the output - for perc. cover within 10m radius (WK.PERC):
idw.wperc <- idw(formula = WK.PERC ~ 1, locations = dat2, 
               newdata = grd)  # apply idw model for the data
idw.wperc = as.data.frame(idw.wperc)  # output is defined as a data table
names(idw.wperc)[1:3] <- c("long", "lat", "var1.pred")  # give names to the modelled variables



# Point height graph:
ggplot() + geom_tile(data = idw.ht, alpha = 0.8, aes(x = long, y = lat, 
        fill = round(var1.pred, 0))) + scale_fill_gradientn(colours = terrain.colors(10), breaks=seq(0,200, 25)) + 
  geom_path(data=gorse1.sr, aes(long, lat), colour = "grey35") + 
  geom_path(data=gorse2.sr, aes(long, lat), colour = "grey35") + 
  geom_point(data = dat1, aes(x = LONG, y = LAT), shape = 21, 
             colour = "red") + labs(fill = "Height (cm)", title = "Vegetation height at sampling point") +
  xlim(min(dat2$x)-0.001, max(dat2$x)+0.001) +
  ylim(min(dat2$y)-0.001, max(dat2$y)+0.001)


# Gorse height graph:
ggplot() + geom_tile(data = idw.ght, alpha = 0.8, aes(x = long, y = lat, 
      fill = round(var1.pred, 0))) + scale_fill_gradientn(colours = terrain.colors(10), breaks=seq(0,200, 25)) + 
  geom_path(data=gorse1.sr, aes(long, lat), colour = "grey35") + 
  geom_path(data=gorse2.sr, aes(long, lat), colour = "grey35") + 
  geom_point(data = dat1, aes(x = LONG, y = LAT), shape = 21, 
             colour = "red") + labs(fill = "Median\ngorse ht (cm)", title = "Median gorse height within 1m radius") +
  xlim(min(dat2$x)-0.001, max(dat2$x)+0.001) +
  ylim(min(dat2$y)-0.001, max(dat2$y)+0.001)




# Gorse cover graph:
ggplot() + geom_tile(data = idw.gperc, alpha = 0.8, aes(x = long, y = lat, 
      fill = var1.pred)) + scale_fill_gradientn(colours = terrain.colors(10)) + 
  geom_path(data=gorse1.sr, aes(long, lat), colour = "grey35") + 
  geom_path(data=gorse2.sr, aes(long, lat), colour = "grey35") + 
  geom_point(data = dat1, aes(x = LONG, y = LAT), shape = 21, 
             colour = "red") + labs(fill = "Gorse cover\n(%)", title = "Gorse cover within 1m radius") +
  xlim(min(dat2$x)-0.001, max(dat2$x)+0.001) +
  ylim(min(dat2$y)-0.001, max(dat2$y)+0.001)



# Gorse height*perc.cover graph:
ggplot() + geom_tile(data = idw.ghp, alpha = 0.8, aes(x = long, y = lat, 
                    fill = var1.pred)) + scale_fill_gradientn(colours = topo.colors(10)) + 
  geom_path(data=gorse1.sr, aes(long, lat), colour = "grey35") + 
  geom_path(data=gorse2.sr, aes(long, lat), colour = "grey35") + 
  geom_point(data = dat1, aes(x = LONG, y = LAT), shape = 21, 
             colour = "red") + labs(fill = "Gorse\nheight ? cover\n(cm%)", title = "Gorse height ? cover within 1m radius") +
  xlim(min(dat2$x)-0.001, max(dat2$x)+0.001) +
  ylim(min(dat2$y)-0.001, max(dat2$y)+0.001)




# Walk grade graph:
ggplot() + geom_tile(data = idw.wlk, alpha = 0.8, aes(x = long, y = lat, 
          fill = var1.pred, 0)) + scale_fill_gradientn(colours = terrain.colors(10)) + 
  geom_path(data=gorse1.sr, aes(long, lat), colour = "grey35") + 
  geom_path(data=gorse2.sr, aes(long, lat), colour = "grey35") + 
  geom_point(data = dat1, aes(x = LONG, y = LAT), shape = 21, 
             colour = "red") + labs(fill = "Walk grade", title = "Walk grade within 10m radius") +
  xlim(min(dat2$x)-0.001, max(dat2$x)+0.001) +
  ylim(min(dat2$y)-0.001, max(dat2$y)+0.001)


# Walk percentage graph:
ggplot() + geom_tile(data = idw.wperc, alpha = 0.8, aes(x = long, y = lat, 
        fill = var1.pred, 0)) + scale_fill_gradientn(colours = terrain.colors(10)) + 
  geom_path(data=gorse1.sr, aes(long, lat), colour = "grey35") + 
  geom_path(data=gorse2.sr, aes(long, lat), colour = "grey35") + 
  geom_point(data = dat1, aes(x = LONG, y = LAT), shape = 21, 
             colour = "red") + labs(fill = "Cover within\n10m radius", title = "Cover within 10m radius") +
  xlim(min(dat2$x)-0.001, max(dat2$x)+0.001) +
  ylim(min(dat2$y)-0.001, max(dat2$y)+0.001)







#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#               PLAYING WITH DIFFERENT INTERPOLATIONS
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


# Gorse cover graph:

# Interpolate surface and fix the output - for GORSE.PERC:
# substiture NAs with 0's:
dat2$GORSE.PERC[is.na(dat2$GORSE.PERC)] <- 0

idw.gperc <- idw(formula = log(GORSE.PERC) ~ 1, locations = dat2, 
                 newdata = grd)  # apply idw model for the data
idw.gperc = as.data.frame(idw.gperc)  # output is defined as a data table
names(idw.gperc)[1:3] <- c("long", "lat", "var1.pred")  # give names to the modelled variables

ggplot() + geom_tile(data = idw.gperc, alpha = 0.8, aes(x = long, y = lat, 
                                                        fill = var1.pred)) + scale_fill_gradientn(colours = terrain.colors(10)) + 
  geom_path(data=gorse1.sr, aes(long, lat), colour = "grey35") + 
  geom_path(data=gorse2.sr, aes(long, lat), colour = "grey35") + 
  geom_point(data = dat1, aes(x = LONG, y = LAT), shape = 21, 
             colour = "red") + labs(fill = "Gorse cover", title = "Gorse cover at Rakaia Gorge") +
  xlim(min(dat2$x)-0.001, max(dat2$x)+0.001) +
  ylim(min(dat2$y)-0.001, max(dat2$y)+0.001)




dw.gperc <- idw(formula = GORSE.PERC/10 ~ 1, locations = dat2, 
                newdata = grd)  # apply idw model for the data

idw(dat2, grd, method = "Shepard", p = 2, R = 2, N = 15)






#---------------------------------
#       EXPERIMENTING WITH EXPORTING AS kml FILE


xy <- matrix(rnorm(400),20,20)
image(xy)



require(rgdal)
writeOGR(idw.ht, getwd(), 'ARS', 'ESRI Shapefile')


# https://stackoverflow.com/questions/7813141/how-to-create-a-kml-file-using-r

library("sp")
library("rgdal")
data(meuse)
coordinates(meuse) <- c("x", "y")
proj4string(meuse) <- CRS("+init=epsg:28992")
meuse_ll <- spTransform(meuse, CRS("+proj=longlat +datum=WGS84"))
writeOGR(meuse_ll["zinc"], "meuse.kml", layer="zinc", driver="KML") 

class(idw.ht)


idw.ht2=idw.ht
coordinates(idw.ht2) <- c("long", "lat")

proj4string(idw.ht2) <- CRS("+init=epsg:28992")
idw.ht2_ll <- spTransform(idw.ht2, CRS("+proj=longlat +datum=WGS84"))
writeOGR(idw.ht2_ll["var1.pred"], "idw.ht2.kml", layer="var1.pred", driver="KML") 


# look into: https://stackoverflow.com/questions/50575765/creating-a-georeferenced-png-file-from-leaflet-or-ggmap-in-r
#   http://r-sig-geo.2731867.n2.nabble.com/Save-plot-as-Geotiff-td7591800.html
#   https://www.researchgate.net/post/How_do_I_save_an_Rstudio_interpolated_map_into_Geotiff_format



# Gorse height graph:
p <- ggplot() + geom_tile(data = idw.ght, alpha = 0.8, aes(x = long, y = lat, 
                                                      fill = round(var1.pred, 0))) + scale_fill_gradientn(colours = terrain.colors(10), breaks=seq(0,200, 25)) + 
  geom_path(data=gorse1.sr, aes(long, lat), colour = "grey35") + 
  geom_path(data=gorse2.sr, aes(long, lat), colour = "grey35") + 
  geom_point(data = dat1, aes(x = LONG, y = LAT), shape = 21, 
             colour = "red") + labs(fill = "Median\ngorse ht (cm)", title = "Median gorse height within 1m radius") +
  xlim(min(dat2$x)-0.001, max(dat2$x)+0.001) +
  ylim(min(dat2$y)-0.001, max(dat2$y)+0.001)


ggsave("ggsave gorse ht.png", plot=p, device = png, width = 900, height = 900, limitsize = FALSE)



# --------------------------------------

# Code taken from: https://www.researchgate.net/post/How_do_I_save_an_Rstudio_interpolated_map_into_Geotiff_format

##Define spatial projection
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
proj4string(idw.ht2) <- crs.geo
##Export IDW output to GeoTIFF file
outfilename <- tempfile(pattern="file", tmpdir = tempdir())
writeGDAL(idw.ht2, outfilename, drivername = "GTiff")
file.rename (outfilename, "idw.tif")




#----------------------------------
library(raster)

r <- raster()
values(r) <- sample(x = 1:10,size = ncell(r),replace = T)

writeRaster(r,'test.tif',options=c('TFW=YES'))

#-----------------------------------------------

# from: https://stackoverflow.com/questions/13910161/how-to-get-geotiff-image

require(gstat)
data(meuse)
coordinates(meuse) = ~x+y
data(meuse.grid)
gridded(meuse.grid) = ~x+y
m <- vgm(.59, "Sph", 874, .04)
# ordinary kriging:
x <- krige(log(zinc)~1, meuse, meuse.grid, model = m)
spplot(x["var1.pred"], main = "ordinary kriging predictions")


require(raster)
r = raster(x["var1.pred"])
plot(r)
writeRaster(r,"r.tiff","GTiff")

#-----------------------------------------------
# from: https://www.rdocumentation.org/packages/plotKML/versions/0.5-8/topics/kml_layer.Raster

#install.packages("plotKML")
library(plotKML)

data(eberg_grid)
library(sp)
coordinates(eberg_grid) <- ~x+y
gridded(eberg_grid) <- TRUE
proj4string(eberg_grid) <- CRS("+init=epsg:31467")
data(SAGA_pal)
library(raster)
r <- raster(eberg_grid["TWISRT6"])

kml_layer.Raster(r, "name.png", kml.out <- get("kml.out", envir=plotKML.fileIO))
                 
  
kml_layer.Raster(r, subfolder.name = paste(class(obj)), plot.legend = TRUE, 
                 metadata = NULL, raster_name, 
                 png.width = ncol(obj), png.height = nrow(obj), 
                 min.png.width = 800, TimeSpan.begin, TimeSpan.end,
                 layer.name, png.type = "cairo-png")               
                 
                 
                 
kml(r, colour_scale = terrain.colors(10), colour = "TWISRT6")


# Try this with my data:

idw.ht2=idw.ht
coordinates(idw.ht2) <- ~long+lat
gridded(idw.ht2) <- TRUE
proj4string(idw.ht2) <- CRS("+proj=longlat")
data(SAGA_pal)
library(raster)
test <- raster(idw.ht2["var1.pred"])

plot(test)

kml(test, colour_scale = terrain.colors(10), colour = "var1.pred")

                 
writeOGR(idw.ht2["var1.pred"], "test OGR.kml", layer="var1.pred", driver="KML")                  
                 
                 
                 

writeRaster(test, "outputFilename", format = "GTiff")

writeRaster(test, "Geotiff", format = "GTiff")

View(test)















#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
######################      HEADING 1     ####################
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$



dat1 = read.csv("Kansas_prairie_allrecords.csv")


dat1$YEAR_SHORT = substr(dat1$PLOTYEAR, nchar(as.character(dat1$PLOTYEAR)) - 2, nchar(as.character(dat1$PLOTYEAR)))
dat1$PLOT = substr(dat1$PLOTYEAR, 1, nchar(as.character(dat1$PLOTYEAR)) - 2)

dat1$YEAR = as.numeric(paste0(19, dat1$YEAR_SHORT))

dat1$GEN = gsub(" .*$", "", dat1$GENSP)
dat1$SP = gsub("^.* ", "", dat1$GENSP)


dat1$FIRST_LETTER = substr(dat1$GENSP, 1, 1)

# dat1$GENSP_SHORT = paste0(substr(dat1$GEN, 1, 6), "_", substr(dat1$SP, 1, 6))


unique(dat1$GENSP)
length(unique(dat1$GENSP))
# length(unique(dat1$GENSP_SHORT))


ggplot(data = dat1, aes(x = YEAR, y = AREA, color = GENSP)) +
    # geom_point() +
    geom_line() +
    theme(legend.position = "none")

ggplot(data = dat1, aes(x = YEAR, y = AREA, color = GENSP)) +
    # geom_point() +
    geom_line() +
    theme(legend.position = "none")



sum1 <- group_by(dat1, GENSP) %>%
    summarize(count = n(), AREA = mean(AREA, na.rm = T))

ggplot(sum1, aes(x = GENSP, y = AREA)) +
    geom_point()

ggplot(dat1[dat1$GENSP == "Agropyron smithii", ], aes(x = YEAR, y = AREA)) +
    geom_col()

ggplot(dat1[dat1$GENSP == "Townsendia exscapa", ], aes(x = YEAR, y = AREA)) +
    geom_col()

ggplot(dat1[dat1$GENSP == c("Agropyron smithii", "Townsendia exscapa", "Plantago rhodosperma", "Lomatium macrocarpum",
                         "Lepidium densiflorum", "Euphorbia dentata", "Solidago mollis"), ],
       aes(x = YEAR, y = AREA, fill = GENSP)) +
    geom_col()

myplot = ggplot(dat1, aes(x = YEAR, y = AREA, fill = GENSP)) +
    geom_col() +
    theme(legend.position = "bottom")


# library(grid)
# library(gridExtra)

## Function to extract legend
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}




legend <- g_legend(myplot) 

grid.draw(legend)

grid.arrange(myplot + theme(legend.position = "none"), legend)
grid.arrange(myplot + theme(legend.position = "none"), legend, heights = c(8, 2))


ggsave("species_bars3.png", grid.arrange(myplot + theme(legend.position = "none"), legend),
       height = 11, width = 17)




ggplot(dat1, aes(x = YEAR, y = AREA, fill = FIRST_LETTER)) +
    geom_col() +
    theme(legend.position = "right")


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

ggplot(data = dat1[dat1$GENSP == "Bouteloua gracilis", ], aes(x = YEAR, y = AREA)) +
    geom_col()


# ggplot(data = dat1, aes(x = X, y = Y, color = GENSP)) +
#     geom_point(alpha = 0.01) +
#     theme(legend.position = "none")

ggplot(data = dat1[dat1$YEAR == 1950, ], aes(x = X, y = Y, color = GENSP, size = AREA)) +
    geom_point(alpha = 0.5) +
    theme(legend.position = "none")

library(gganimate)

ggplot(data = dat1[dat1$YEAR %in% c(1950, 1951, 1952), ], aes(x = X, y = Y, color = GENSP, size = AREA)) +
    geom_point(alpha = 0.5) +
    theme(legend.position = "none") +
    transition_time(YEAR) +
    labs(title = "Year: {frame_time}")


ggplot(data = dat1[dat1$YEAR == "19630", ], aes(x = X, y = Y, color = GENSP, size = AREA)) +
    geom_point(alpha = 0.5) +
    theme(legend.position = "none") +
    transition_time(YEAR) +
    labs(title = "Year: {frame_time}")

ggplot(data = dat1[dat1$YEAR %in% c(1950, 1951, 1952) & dat1$PLOT == "e1q1-1", ],
       aes(x = X, y = Y, color = GENSP, size = AREA)) +
    geom_point(alpha = 0.8) +
    theme(legend.position = "none") +
    transition_time(YEAR) +
    labs(title = "Year: {frame_time}")


dat1$YEAR_ID = paste0(dat1$YEAR, "_", dat1$ID)

ggplot(data = dat1[dat1$YEAR == 1950 & dat1$PLOT == "e1q1-1", ],
       aes(x = X, y = Y, color = GENSP, size = AREA)) +
    geom_point(alpha = 0.8) +
    theme(legend.position = "none") +
    transition_time(ID) +
    labs(title = "Year: {frame_time}")

ggplot(data = dat1[dat1$YEAR == 1950 & dat1$PLOT == "e1q1-1", ],
       aes(x = X, y = Y, color = GENSP, size = AREA)) +
    geom_point(alpha = 0.8) +
    theme(legend.position = "none") +
    transition_reveal(ID)



#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# Box plot

dat1$BLOCK = gsub("-.*$", "", dat1$PLOT)

y1950 = dat1[dat1$YEAR == 1950, ]

sum2 <- group_by(y1950, PLOT, BLOCK) %>%
    summarize(NUMSP = length(unique(GENSP)), NUMPLANTS = n(), AREA = mean(AREA, na.rm = T))


ggplot(sum2, aes(x = BLOCK, y = NUMSP)) +
    geom_boxplot()


ggplot(data = dat1[dat1$YEAR == 1950 & dat1$PLOT == "e1q1-1", ],
       aes(x = ID, y = , color = GENSP, size = AREA)) +
    geom_point(alpha = 0.8)







#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# saving plots

# fancy styling in x labels

# Geom_tile
ggplot(wx_small, aes(x = WD, y = TEMP)) +
    geom_tile()

# geographical data

