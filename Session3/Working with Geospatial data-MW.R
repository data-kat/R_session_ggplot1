#FROM GITHUB
# install.packages("devtools")
devtools::install_github("cwickham/geospatial")

if(!require(geospatial)){install.packages("geospatial")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(ggmap)){install.packages("ggmap")}
if(!require(OpenStreetMap)){install.packages("OpenStreetMap")}

library(geospatial)
library(ggplot2)
library(ggmap)
library(OpenStreetMap)



# If you are not working in a version control R Studio Project,
#  You will need to use setwd() to specify your working directory
#   Note: you will need to use forward slashes instead of backslashes (Windows default),
#   and remove the "#" at the beginning of the line
#setwd("C:\Users\OneDrive - scion\Documents\1 - Workspace\R_sessions\R_session_ggplot1")

# If you are working inside a project, just specify the subfolder you'll be working from
setwd("./Session3")




#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#######                        SAVING IMAGES                      #############
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


ggsave()


png()
ggplot()
graphics.off()


pdf()
ggplot()
graphics.off()






#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#######                   WORKING WITH GEOSPATIAL DATA            #############
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


# Read the data in
# Note: I placed metadata into the first row, so we will be skipping one row from the top:
fuels = read.csv("Rakaia_fuels.csv", skip = 1)


# Can simply plot points with ggplot:
ggplot(fuels, aes(x = LONG, y = LAT)) + 
  geom_point()

# Lacking spatial context... Need a background map for that



map = openmap(upperLeft = c(lon = 80, lat = 60),
              lowerRight = c(lon = 90, lat = 50), zoom = 12)



map <- openmap(c(-43.39838,171.5555),
               c(-43.41561,171.5748),
               zoom = 15, type="bing")

# View map using autoplot (part of ggplot):
autoplot(map)


# Can add our sampling points to the map:
autoplot(map) + geom_point(data=fuels, aes(x=LONG, y=LAT))


# Reproject to lat long:
map_latlon <- openproj(map, projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

autoplot(map_latlon) + geom_point(aes(x = LONG, y = LAT, col = TRANSECT), data = fuels, alpha = .5)


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# ggmap method:
height <- max(fuels$LAT) - min(fuels$LAT)
width <- max(fuels$LONG) - min(fuels$LONG)
sac_borders <- c(bottom  = min(fuels$LAT)  - 0.1 * height, 
                 top     = max(fuels$LAT)  + 0.1 * height,
                 left    = min(fuels$LONG) - 0.1 * width,
                 right   = max(fuels$LONG) + 0.1 * width)

stamen_map <- get_stamenmap(sac_borders, zoom = 13, scale = 1, maptype = "toner-lite")

ggmap(stamen_map)

# You can add layers of data to a ggmap() call (e.g. + geom_point()).
# NOTE: ggmap() sets the map as the default dataset and also sets the default aesthetic mappings
#   if you want to add a layer from something other than the map, you need to explicitly 
#   specify both the mapping and data arguments to the geom.


# This:
ggplot(fuels, aes(x = LONG, y = LAT)) + 
  geom_point()

# is the same as this:
ggplot() + 
  geom_point(data = fuels, aes(x = LONG, y = LAT))

# Now, replace ggplot() with ggmap(stamen_map):
ggmap(stamen_map) + 
  geom_point(data = fuels, aes(x = LONG, y = LAT))


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


# Add additional properties through color and size:

autoplot(map_latlon) +
  geom_point(aes(x = LONG, y = LAT, col = GORSE.HT, size = GORSE.PERC), data = fuels, alpha = .5)



#syntax for looking at different type of OSM and Stamen maps
?get_map

corvallis <- c(lon = -123.2620, lat = 44.5646)

# Add a maptype argument to get a satellite map
corvallis_map_bw <- get_stamenmap(sac_borders, zoom = 13, maptype = "toner", source = "stamen") #Also very good
corvallis_map1 <- get_stamenmap(sac_borders, zoom = 12, maptype = "terrain") #best so far
corvallis_map2 <- get_stamenmap(sac_borders, zoom = 13, maptype = "terrain-background")

ggmap(corvallis_map_bw)
ggmap(corvallis_map1)
ggmap(corvallis_map2)


# Edit to get display satellite map
ggmap(corvallis_map1) +
  geom_point(aes(x = LONG, y = LAT, col = GORSE.HT, size = GORSE.PERC), data = fuels, alpha = .5)

# # Leveraging ggplot2's strengths
# # You've seen you can add layers to a ggmap() plot by adding geom_***() layers and specifying the data and mapping explicitly, 
# but this approach has two big downsides: further layers also need to specify the data and mappings, and facetting won't work at all.
# # 
# # Luckily ggmap() provides a way around these downsides: the base_layer argument. You can pass base_layer a normal ggplot() call 
# that specifies the default data and mappings for all layers.
# # 
# # For example, the initial plot:
# # 
# # ggmap(corvallis_map) +
# #   geom_point(data = sales, aes(lon, lat))
# # could have instead been:
# # 
# # ggmap(corvallis_map, 
# #     base_layer = ggplot(sales, aes(lon, lat))) +
# #   geom_point()
# # By moving aes(x, y) and data from the initial geom_point() function to the ggplot() call within the ggmap() call, you can add
# facets, or extra layers, the usual ggplot2 way.
# # 
# # Let's try it out.


# Use base_layer argument to ggmap() to specify data and x, y mappings
ggmap(corvallis_map1, base_layer = ggplot(fuels, aes(LONG, LAT))) +
  geom_point(aes(color = PT.HT))


# This won't work with autoplot, so you'll need to specify info for each individual geom



# Use base_layer argument to ggmap() and add facet_wrap()

ggmap(corvallis_map1, base_layer = ggplot(fuels, aes(LONG, LAT))) +
  geom_point(aes(color = PT.HT))

ggmap(corvallis_map1, base_layer = ggplot(fuels, aes(LONG, LAT))) +
  geom_point(aes(color = PT.HT)) +
  facet_wrap(~ PT.SP)


# A quick alternative
# 
# ggmap also provides a quick alternative to ggmap(). Like qplot() in ggplot2, qmplot() is less flexible than a
# full specification, but often involves significantly less typing. qmplot() replaces both steps -- downloading 
# the map and displaying the map -- and its syntax is a blend between qplot(), get_map(), and ggmap().
# 
# Let's take a look at the qmplot() version of the faceted plot from the previous exercise:
# 
qmplot(LONG, LAT, data = fuels, geom = "point", color = PT.HT) +
  facet_wrap(~ PT.SP)

# Notice we didn't specify a map, since qmplot() will grab one on its own. Otherwise the qmplot() call looks a lot 
# like the corresponding qplot() call: use points to display the sales data, mapping lon to the x-axis, lat to the y-axis, 
# and class to color. qmplot() also sets the default dataset and mapping (without the need for base_layer) so you can add 
# facets without any extra work.



#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
########                     KML              ##############
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


library(maptools)
gorse=getKMLcoordinates("Burn_envelope.kml", 
                        ignoreAltitude = TRUE)

gorse1.sr = SpatialPolygons(list(Polygons(list(Polygon(gorse[1])), "x")))
gorse2.sr = SpatialPolygons(list(Polygons(list(Polygon(gorse[5])), "x")))


plot(gorse1.sr)
plot(gorse2.sr)


autoplot(map_latlon) + geom_point(aes(x = LONG, y = LAT, col = PT.HT), data = fuels, alpha = .5) +
      scale_color_gradient(low="yellow",high="red",guide="legend") + 
  geom_path(data=gorse1.sr, aes(long, lat), colour = "grey") + 
  geom_path(data=gorse2.sr, aes(long, lat), colour = "grey")



#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#######             GENERATE RANDOM POINTS           #############
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$










#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#      INTERPOLATING VALUES ON THE map
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


# duplicate fuels to make sure we don't make any changes to fuels:
fuels2=fuels

# define x & y as longitude and latitude:
fuels2$x=fuels2$LONG 
fuels2$y=fuels2$LAT

# Set spatial coordinates to create a Spatial object:
coordinates(fuels2) = ~x + y


# Plot the results:
plot(fuels2)

# Plot the minimum/maximum x and y coordinates:
points(c(min(fuels2$x), max(fuels2$x), min(fuels2$x), max(fuels2$x)), c(min(fuels2$y), min(fuels2$y), max(fuels2$y), max(fuels2$y)), pch=21, bg="red")

# Define x and y range for interpolation area by expanding the bounding box a little:
x.range <- as.numeric(c(min(fuels2$x)-0.001, max(fuels2$x)+0.001))  # min/max longitude of the interpolation area
y.range <- as.numeric(c(min(fuels2$y)-0.001, max(fuels2$y)+0.001))  # min/max latitude of the interpolation area


#Create a data frame from all combinations of the supplied vectors or factors.
#   See the description of the return value for precise details of the way this is done.
#   Set spatial coordinates to create a Spatial object. Assign gridded structure:

grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.00005), y = seq(from = y.range[1], 
              to = y.range[2], by = 0.00005))  # expand points to grid
coordinates(grd) <- ~x + y
gridded(grd) <- TRUE


# Plot sampling point locations and interpolation grid:

plot(grd, cex = 1.5, col = "grey")
points(fuels2, pch = 1, col = "red", cex = 1)


# Interpolate surface and fix the output - for PT.HT:
library(gstat)
idw.ht <- idw(formula = PT.HT ~ 1, locations = fuels2, 
             newdata = grd)  # apply idw model for the data
idw.ht = as.data.frame(idw.ht)  # output is defined as a data table
names(idw.ht)[1:3] <- c("long", "lat", "var1.pred")  # give names to the modelled variables



# Point height graph:
ggplot() + geom_tile(data = idw.ht, alpha = 0.8, aes(x = long, y = lat, 
        fill = round(var1.pred, 0))) + scale_fill_gradientn(colours = terrain.colors(10), breaks=seq(0,200, 25)) + 
  geom_path(data=gorse1.sr, aes(long, lat), colour = "grey35") + 
  geom_path(data=gorse2.sr, aes(long, lat), colour = "grey35") + 
  geom_point(data = fuels, aes(x = LONG, y = LAT), shape = 21, 
             colour = "red") + labs(fill = "Height (cm)", title = "Vegetation height at sampling point") +
  xlim(min(fuels2$x)-0.001, max(fuels2$x)+0.001) +
  ylim(min(fuels2$y)-0.001, max(fuels2$y)+0.001)





#$$$$$$$$$$$$$$$$$$$$$$$
# EXPORTING A KML
#$$$$$$$$$$$$$$$$$$$$$


# library("sp")
# library("rgdal")
# 
# # This works, but creates a zillion points, which GoogleEarth struggles with:
# idw.ht.test = idw.ht
# coordinates(idw.ht.test) <- c("long", "lat") # specify which columns contain spatial coords
# proj4string(idw.ht.test) <- CRS("+init=epsg:4326") # specify the projection of the raster
# writeOGR(idw.ht.test["var1.pred"], "idw_ht_test.kml", layer="ht", driver="KML") 


#$$$$$$$$$$



# Save as geotiff - this works for google earth (even though the image doesn't show up when opening tif as image)
idw.ht2=idw.ht
coordinates(idw.ht2) <- ~long+lat
gridded(idw.ht2) <- TRUE
proj4string(idw.ht2) <- CRS("+init=epsg:4326") # specify the projection of the raster
data(SAGA_pal)
library(raster)
test <- raster(idw.ht2["var1.pred"])

plot(test)

writeRaster(test, "writeRaster_output", format = "GTiff")



#$$
plot(test)
plot(gorse1.sr,add=TRUE)

rr <- mask(test, gorse1.sr)
plot(rr);plot(gorse1.sr, add=TRUE)

writeRaster(rr, "writeRaster_mask", format = "GTiff")




#$$
# Save as geotiff - this works for google earth

idw.ht.test3 = idw(formula = PT.HT ~ 1, locations = fuels2, newdata = grd)  # apply idw model for the data

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
proj4string(idw.ht.test3) <- crs.geo
##Export IDW output to GeoTIFF file
outfilename <- tempfile(pattern="file", tmpdir = tempdir())
writeGDAL(idw.ht.test3, outfilename, drivername = "GTiff")
file.rename (outfilename, "idw.tif")













