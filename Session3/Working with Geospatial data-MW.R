

if(!require(geospatial)){install.packages("geospatial")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(ggmap)){install.packages("ggmap")}
if(!require(OpenStreetMap)){install.packages("OpenStreetMap")}
if(!require(sp)){install.packages("sp")}


library(geospatial)
library(ggplot2)
library(ggmap)
library(OpenStreetMap)
library(sp)


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
sales = read.csv("sales.csv")


# Can simply plot points with ggplot:
ggplot(sales, aes(x = lon, y = lat)) + 
  geom_point()

# Lacking spatial context... Need a background map for that



# Before downloading the map, let's figure out the extent of our region of interest:
height <- max(sales$lat) - min(sales$lat)
width <- max(sales$lon) - min(sales$lon)

bounding_box <- c(bottom  = min(sales$lat)  - 0.1 * height,
                 top = max(sales$lat)  + 0.1 * height,
                 left = min(sales$lon) - 0.1 * width,
                 right = max(sales$lon) + 0.1 * width)


# bounding_box is a named vecotor, and each element can be accessed via square brackets:
bounding_box["bottom"]
bounding_box["left"]



#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#######         OPENSTREETMAP PACKAGE - SATELLITE IMAGERY          #############
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


# openmap requires upper-left and lower-right coordinates
osm_map <- openmap(c(bounding_box["top"], bounding_box["left"]),
               c(bounding_box["bottom"], bounding_box["right"]),
               zoom = 12, type="bing")


# View map using autoplot (part of ggplot):
autoplot(osm_map)


# Autoplot(map) is a self-sufficient ggplot object, and we can add geoms to it as usual.
#   Let's add our sampling points to the map:
autoplot(osm_map) + geom_point(data=sales, aes(x=lon, y=lat))


# This didn't work... Whats' wrong? Let's take a look at our map again:
autoplot(osm_map)

# Now, examine lat and lon columns we are trying to plot:
sales[1:5, c("lat", "lon")]

# Looks like the downloaded map is in NZTM grid coordinates and our data is in lat/long.
#   We can reproject our map to lat and long:
osm_map_latlon <- openproj(osm_map, projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")


autoplot(osm_map_latlon) +
  geom_point(aes(x = lon, y = lat), data = sales)


# We can get creative with ggplot:

autoplot(osm_map_latlon) +
  geom_point(aes(x = lon, y = lat, color = year_built), data = sales, alpha = .5) +
  scale_color_gradientn(colors = topo.colors(10))


# Map color to price / finished_squarefeet
autoplot(osm_map_latlon) +
  geom_point(aes(lon, lat, color = price / finished_squarefeet), data = sales)


# This is good for simple stuff, but with autoplot we don't have a way to pass a data and aes()
#   arguments for all geoms at once, and we would have to repeat them for every geom.
#   Also, with this method we cannot use facet_wrap


# ggmap package gives us this functionality, but it can't access nice satellite imagery without
#   signing up with google


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#######         GGMAP PACKAGE - SATELLITE IMAGERY          #############
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


# let's experiment with the ggmap package now. It doesn't have an opensource satellite map,
#   but has an intuitive interface

#syntax for looking at different types of ggmap maps
?get_map

# Check out some ggmap maps:
ggmap_tonerlite_map <- get_stamenmap(bounding_box, zoom = 12, scale = 1, maptype = "toner-lite")
ggmap(ggmap_tonerlite_map)

ggmap_toner_map <- get_stamenmap(bounding_box, zoom = 12, scale = 1, maptype = "toner")
ggmap(ggmap_toner_map)

ggmap_ter_back_map <- get_stamenmap(bounding_box, zoom = 13, maptype = "terrain-background")
ggmap(ggmap_ter_back_map)

ggmap_ter_map <- get_stamenmap(bounding_box, zoom = 13, maptype = "terrain")
ggmap(ggmap_ter_map)



# Just like autoplot(), ggmap() is a standalone ggplot object, and we can add points to it:

ggmap(ggmap_ter_map) + 
  geom_point(data = sales, aes(x = lon, y = lat))


# But a neat functionality of ggmap is that it allows us to specify data and aes() values
#   to be applied to every geom in the plot. This reduces repetition and
#   allows us to use facet wrap

# Use base_layer argument to ggmap() to specify data and aes() mappings
ggmap(ggmap_ter_map, base_layer = ggplot(sales, aes(lon, lat))) +
  geom_point(aes(color = year_built)) +
  facet_wrap(~class)





# A quick alternative
# 
# ggmap also provides a quick alternative to ggmap().
#   It's less flexible but allows to download and display a map in one go
# 
# Let's take a look at the qmplot() version of the faceted plot from the previous exercise:
# 
qmplot(lon, lat, data = sales, geom = "point", color = year_built) +
  facet_wrap(~ class)

# Notice we didn't specify a map, since qmplot() will grab one on its own. Otherwise the qmplot() call looks a lot 
# like the corresponding qplot() call: use points to display the sales data, mapping lon to the x-axis, lat to the y-axis, 
# and class to color. qmplot() also sets the default dataset and mapping (without the need for base_layer) so you can add 
# facets without any extra work.




#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#######             DRAWING POLYGONS           #############
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


ward_sales = read.csv("ward_sales.csv")

ward_sales$ward = as.numeric(ward_sales$ward)


# Drawing polygons
# 
# A choropleth map describes a map where polygons are colored according to some variable. In the ward_sales data frame,
# you have information on the house sales summarised to the ward level. Your goal is to create a map where each ward is 
# colored by one of your summaries: the number of sales or the average sales price.
# 
# In the data frame, each row describes one point on the boundary of a ward. The lon and lat variables describe its location 
# and ward describes which ward it belongs to, but what are group and order?
#   
#   Remember the two tricky things about polygons? An area may be described by more than one polygon and order matters. group 
# is an identifier for a single polygon, but a ward may be composed of more than one polygon, so you would see more than one 
# value of group for such a ward. order describes the order in which the points should be drawn to create the correct shapes.
# 
# In ggplot2, polygons are drawn with geom_polygon(). Each row of your data is one point on the boundary and points are joined
# up in the order in which they appear in the data frame. You specify which variables describe position using the x and y 
# aesthetics and which points belong to a single polygon using the group aesthetic.
# 
# This is a little tricky, so before you make your desired plot, let's explore this a little more.


# Add a point layer with color mapped to ward
ggplot(ward_sales, aes(lon, lat)) +
  geom_point(aes(color = ward))


# Add a point layer with color mapped to group
ggplot(ward_sales, aes(lon, lat)) +
  geom_point(aes(color = group))


# Add a path layer with group mapped to group
ggplot(ward_sales, aes(lon, lat)) +
  geom_path(aes(group = group))

# Add a polygon layer with fill mapped to ward, and group to group
ggplot(ward_sales, aes(lon, lat)) +
  geom_polygon(aes(group = group, fill = ward))

# Choropleth map
# 
# Now that you understand drawing polygons, let's get your polygons on a map. Remember, you replace your ggplot() 
# call with a ggmap() call and the original ggplot() call moves to the base_layer() argument, then you add your 
# polygon layer as usual:
# 
# ggmap(corvallis_map_bw,
#       base_layer = ggplot(ward_sales,
#                           aes(lon, lat))) +
#   geom_polygon(aes(group = group, fill = ward))
# Try it out in the console now!
# 
# Uh oh, things don't look right. Wards 1, 3 and 8 look jaggardy and wrong. What's happened? Part of the ward 
# boundaries are beyond the map boundary. Due to the default settings in ggmap(), any data off the map is dropped before 
# plotting, so some polygon boundaries are dropped and when the remaining points are joined up you get the wrong shapes.
# 
# Don't worry, there is a solution: ggmap() provides some arguments to control this behaviour. Arguments extent = "normal" 
# along with maprange = FALSE force the plot to use the data range rather than the map range to define the plotting boundaries.


# Fix the polygon cropping
ggmap(ggmap_ter_map, 
      base_layer = ggplot(ward_sales, aes(lon, lat)),
      extent = "normal", maprange = FALSE) +
  geom_polygon(aes(group = group, fill = ward))

# Repeat, but map fill to num_sales
ggmap(ggmap_ter_map, 
      base_layer = ggplot(ward_sales, aes(lon, lat)),
      extent = "normal", maprange = FALSE) +
  geom_polygon(aes(group = group, fill = num_sales))

# Repeat again, but map fill to avg_price
ggmap(ggmap_ter_map, 
      base_layer = ggplot(ward_sales, aes(lon, lat)),
      extent = "normal", maprange = FALSE) +
  geom_polygon(aes(group = group, fill = avg_price), alpha = 0.8)





#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
########                     KML              ##############
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


library(maptools)
gorse=getKMLcoordinates("Burn_envelope.kml", 
                        ignoreAltitude = TRUE)

gorse1.sr = SpatialPolygons(list(Polygons(list(Polygon(gorse[1])), "x")))
gorse2.sr = SpatialPolygons(list(Polygons(list(Polygon(gorse[5])), "x")))

gorse1.sr@bbox
gorse2.sr@bbox

# openmap requires upper-left and lower-right coordinates
rakaia_map <- openmap(c(-43.39838, 171.55549),
               c(-43.41561, 171.57479),
               zoom = 14, type="bing")


# Reproject the map to lat and long:
rakaia_map_latlon <- openproj(rakaia_map, projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")



autoplot(rakaia_map_latlon) + 
  geom_path(data=gorse1.sr, aes(long, lat), colour = "blue") + 
  geom_polygon(data=gorse2.sr, aes(long, lat), fill = "beige") + 
  geom_point(data=gorse2.sr, aes(long, lat), size = 1, shape = 3, color = "deeppink")





#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#######             GENERATE RANDOM POINTS           #############
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$








#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#      INTERPOLATING VALUES ON THE MAP
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


# Load in some fuel sampling data. First row contains metadata, so we will skip it
fuels = read.csv("Rakaia_fuels.csv", skip = 1)



autoplot(map_latlon) + geom_point(aes(x = LONG, y = LAT, col = PT.HT), data = fuels, alpha = .5) +
      scale_color_gradient(low="yellow",high="red",guide="legend") + 
  geom_path(data=gorse1.sr, aes(long, lat), colour = "grey") + 
  geom_path(data=gorse2.sr, aes(long, lat), colour = "grey")



# duplicate fuels to make sure we don't make any changes to fuels:
fuels2=fuels

# define x & y as longitude and latitude:
fuels2$x=fuels2$LONG 
fuels2$y=fuels2$LAT

# Set spatial coordinates to create a Spatial object:
coordinates(fuels2) = ~x + y



# Plot the results using base R:
plot(fuels2)

# Add some points to the above plot - minimum/maximum x and y coordinates:
points(c(min(fuels2$x), max(fuels2$x), min(fuels2$x), max(fuels2$x)),
       c(min(fuels2$y), min(fuels2$y), max(fuels2$y), max(fuels2$y)),
       pch=21, bg="red")

# Define x and y range for interpolation area by expanding the bounding box a little:
x.range <- as.numeric(c(min(fuels2$x)-0.001, max(fuels2$x)+0.001))  # min/max longitude of the interpolation area
y.range <- as.numeric(c(min(fuels2$y)-0.001, max(fuels2$y)+0.001))  # min/max latitude of the interpolation area


# Create a data frame from all combinations of the x and y ranges.
#   Set spatial coordinates to create a Spatial object. Assign gridded structure.
# Note: by = ... specifies the size of the resulting pixels (smal value = high resolution)

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













