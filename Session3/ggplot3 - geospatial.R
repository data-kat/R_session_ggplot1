

# NOTE: you will need an internet connection to run some parts of this script


if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(geospatial)){install.packages("geospatial")}
if(!require(ggmap)){install.packages("ggmap")}
if(!require(OpenStreetMap)){install.packages("OpenStreetMap")}
if(!require(sp)){install.packages("sp")}

library(ggplot2)
library(geospatial)
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



theme_set(theme_classic())



#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#######                        SAVING IMAGES                      #############
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


# Read some geospatial data in
sales = read.csv("sales.csv")


# Plot some points with ggplot:
plain_plot = ggplot(sales, aes(x = lon, y = lat)) + 
  geom_point()
plain_plot

colour_plot = ggplot(sales, aes(x = lon, y = lat)) + 
  geom_point(aes(color = year_built))
colour_plot


# Create a folder to save our plots into:
dir.create("R_Plots")


####----------------------     _GRAPHICAL INTERFACE      ----------------------####

# Could use the "Export" button in the Plots section to the right

# Coudl also pop-out a window with your graph, and use the right-click menu:

windows(6, 5)
colour_plot

# Coudl re-write this as a one-liner to avoid the annouyance of your R window loosing focus
#   (becoming inactive) as the new window pops up:
windows(6, 5); colour_plot # Note the semicolon! This is efefctively two lines of code


# Try copying the plot as metafile, pasting it into powerpoint, right-clicking on the plot
#   and pressing "Group" -> "Ungroup"


# NOTE: use graphics.off() command to clear all R-generated graphics - removes the need to
#   manually close all your numerous pop-up windows. try it :)

graphics.off()



####----------------------     _GGSAVE      ----------------------####

# Easiest way to save a plot is to use ggsave:
ggsave("R_Plots/Colour_plot-ggsave.png")

# If we don't specify which plot we'd like to save, ggsave will save the latest one displayed.

# Also, we can specifically specify which plot to save:
ggsave("R_Plots/Plain_plot-ggsave.png", plain_plot)


# Can also use other file formats:  "eps", "ps", "tex" (pictex), "pdf", "jpeg", "tiff", "png",
#   "bmp", "svg" or "wmf" (windows only)


# ggsave is very convenient, but cannot save multiple pages into one document.
#   we can use standalone graphics devices for that



####----------------------     _PNG(), PDF(), ETC.      ----------------------####


png("R_Plots/Plain_plot-png.png")
plain_plot
graphics.off()


pdf("R_Plots/Both_plots-pdf.pdf")
plain_plot
colour_plot
graphics.off()


####----------------------     _ADJUSTING SETTINGS      ----------------------####

# take a look at the ggsave help menu by typing "?ggsave" in teh console below
# you will likely change width, height and dpi most often

# Before saving the plot, try experimenting with the desired size using the windows()
#   command to make it exactly how you like it.

graphics.off()
windows(6, 3); colour_plot

ggsave("R_Plots/Colour_plot-ggsave.png", colour_plot, width = 6, height = 3)


# NOTE: all plot-generating/saving devices think in inches unless specified otherwise.
#   An A4 page is 8.5 in x 11 in, so with 0.5 inch margins, plots should be at most
#   7.5 in x 10 in. If they are larger, you'll just have to scale them once pasted


# The beauty of aujtomatically saving images to a folder is that you can automatically
#   paste them into reports using LaTeX or Knitr!





#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#######                   WORKING WITH GEOSPATIAL DATA            #############
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


# Read the data in
sales = read.csv("sales.csv")


# Can simply plot points with ggplot (again):
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
#   signing up with google... boo.



#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#######              GGMAP PACKAGE - SATELLITE IMAGERY            #############
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

# Notice we didn't specify a map, since qmplot() will grab one on its own. Otherwise the
#   qmplot() call looks a lot like the corresponding qplot() call: use points to display
#   the sales data, mapping lon to the x-axis, lat to the y-axis, and class to color.
#   qmplot() also sets the default dataset and mapping (without the need for base_layer)
#   so you can add facets without any extra work.




#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#######                      DRAWING POLYGONS                     #############
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


ward_sales = read.csv("ward_sales.csv")

ward_sales$ward = as.numeric(ward_sales$ward)


####----------------------     _DRAWING POLYGONS      ----------------------####
# 
# A choropleth map describes a map where polygons are colored according to some variable.
#   In the ward_sales data frame, you have information on the house sales summarised to the
#   ward level.
# Goal: create a map with each ward colored by number of sales or average sales price.
# 
# Note the group and order columns in the dataframe.
# An area can be described by more than one polygon, and order of points matters.
#   Group = identifier for a single polygon (ward may be composed of more than one polygon)
#   Order = order in which the points should be drawn to create the correct shapes.
# 
# In ggplot2, polygons are drawn with geom_polygon(). Each row of your data is one point on
#   the boundary and points are joined up in the order in which they appear in the data frame.
#   You specify which variables describe position using the x and y aesthetics and which
#   points belong to a single polygon using the group aesthetic.
# 
# This is a little tricky, so before you make your desired plot, let's explore this a
#   little more.


# Add a point layer with color mapped to ward
ggplot(ward_sales, aes(lon, lat)) +
  geom_point(aes(color = ward))


# Add a point layer with color mapped to group
ggplot(ward_sales, aes(lon, lat)) +
  geom_point(aes(color = group))


# Add a path layer with group mapped to group
ggplot(ward_sales, aes(lon, lat)) +
  geom_path(aes(group = group, colour = group))

# Add a polygon layer with fill mapped to ward, and group to group
ggplot(ward_sales, aes(lon, lat)) +
  geom_polygon(aes(group = group, fill = ward))


####----------------------     _CHOROPLETH MAP      ----------------------####

# Let's now get the polygons on a map:

ggmap(ggmap_ter_map,
      base_layer = ggplot(ward_sales,
                          aes(lon, lat))) +
  geom_polygon(aes(group = group, fill = ward))

# Uh oh, things don't look right. Wards 1, 3 and 8 look jaggardy and wrong.
#   Part of the ward boundaries are beyond the map boundary. Due to the default settings
#   in ggmap(), any data off the map is dropped beforeplotting, so some polygon boundaries
#   are dropped and when the remaining points are joined up you get the wrong shapes.
# 
# ggmap() provides some arguments to control this behaviour. Arguments extent = "normal" 
#   along with maprange = FALSE force the plot to use the data range rather than the map
#   range to define the plotting boundaries.


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




#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#######                 GETTING POLYGONS FROM KML                 #############
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


if(!require(maptools)){install.packages("maptools")}
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
  geom_polygon(data=gorse2.sr, aes(long, lat), fill = "green", alpha = 0.4) + 
  geom_point(data=gorse2.sr, aes(long, lat), shape = 3, color = "deeppink")





#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#######                      GENERATE RANDOM POINTS               #############
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


####--------------     _WITHIN USER-DEFINED BOUNDARIES      ---------------####

# library(maptools)
# library(sp)

# punch in the coordinates of your polygon below, first x-coordinate, then y-coordinate:
topleft=c(-5,5)
topright=c(5,5)
bottomright=c(5,-5)
bottomleft=c(-5,-5)

# Input the above coordinates into a 2-column matrix:
xym=rbind(topleft, topright, bottomright, bottomleft)

# Create a Polygon out of your coordinates, wrap that into a Polygons object,
#   then wrap that into a SpatialPolygons object:
p = Polygon(xym)
ps = Polygons(list(p),1)
sps = SpatialPolygons(list(ps))

# Generate random points:
sample1 = spsample(sps, n = 5, "stratified")
# NOTE: n = ... is the APPROXIMATE number of random points R will generate
#   (might need to re-run the code a few times to get the exact number you are looking for)
sample1

# Check how many points were generated:
length(sample1)


# if you need an EXACT number of points generated, could use a "while" loop:

sample1 = spsample(sps, n = 5, "stratified")

while(length(sample1) != 5){
    sample1 = spsample(sps, n = 5, "stratified")
}
# Translation: while the number of objects in sample1 is not equal to 5, recalculate sample1


# Check to see what your polygon and points look like:
ggplot() +
  geom_point(data = sps, aes(x = long, y = lat), shape = 15, color = "blue") +
  geom_point(data = sample1, aes(x = long, y = lat), color = "red")

# The generated poitns aren't plotted... Need to change "sample1" to a dataframe
#   and figure out the column names to use in the aes() command

ggplot() +
  geom_point(data = sps, aes(x = long, y = lat), shape = 15, color = "blue", size = 3) +
  geom_point(data = data.frame(sample1), aes(x = x1, y = x2), shape = 8)


# Foolproof way of quickly visualizing the same thing with base R graphics:
windows(6, 5)
plot(sps)
points(sample1)

# I used the windows() command here just to allow for easy comparison between the two graphs


####-----------------     _WITHIN EXISTING POLYGONS      -----------------####

# library(maptools)

# Take a look at our polygon:
ggplot(gorse1.sr, aes(x = long, y = lat)) +
  geom_path()

# or:
plot(gorse1.sr)


sample2 <- spsample(gorse1.sr, n = 200, "stratified")
while(length(sample2) != 200){
    sample2 = spsample(sps, n = 200, "stratified")
}


ggplot() +
  geom_path(data = gorse1.sr, aes(x = long, y = lat)) +
  geom_point(data = as.data.frame(sample2), aes(x = x1, y = x2))

# or:
windows(6, 5)
plot(gorse1.sr)
points(sample2)
# I used the windows() command here just to allow for easy comparison between the two graphs


# if you'd like to save this as a csv file:
write.csv(sample2, "write_Sample2.csv", row.names=FALSE)






#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#######                 INTERPOLATING VALUES ON THE MAP           #############
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


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
ggplot() +
  geom_tile(data = idw.ht, aes(x = long, y = lat, fill = round(var1.pred, 0))) +
  scale_fill_gradientn(colours = terrain.colors(10), breaks=seq(0,200, 25)) + 
  geom_path(data=gorse1.sr, aes(long, lat), colour = "grey35") + 
  geom_path(data=gorse2.sr, aes(long, lat), colour = "grey35") + 
  geom_point(data = fuels, aes(x = LONG, y = LAT), shape = 21) +
  labs(fill = "Height (cm)", title = "Vegetation height at sampling point") +
  xlim(min(fuels2$x)-0.001, max(fuels2$x)+0.001) +
  ylim(min(fuels2$y)-0.001, max(fuels2$y)+0.001)





#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#######                        EXPORTING A KML                    #############
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

library(raster)

# Save as geotiff - this works for google earth (even though the image doesn't show up when
#   opening tif as image)
idw.ht2=idw.ht
coordinates(idw.ht2) <- ~long+lat
gridded(idw.ht2) <- TRUE
proj4string(idw.ht2) <- CRS("+init=epsg:4326") # specify the projection of the raster
test <- raster(idw.ht2["var1.pred"])

plot(test)

writeRaster(test, "R_Plots/writeRaster_output", format = "GTiff")



# Clip the raster to the extent of a polygon
plot(test)
plot(gorse1.sr,add=TRUE)

rr <- mask(test, gorse1.sr)
plot(rr);plot(gorse1.sr, add=TRUE)

writeRaster(rr, "writeRaster_mask", format = "GTiff")




# Save as geotiff - this works for google earth

idw.ht3 = idw(formula = PT.HT ~ 1, locations = fuels2, newdata = grd) # apply idw model for the data

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
proj4string(idw.ht3) <- crs.geo
##Export IDW output to GeoTIFF file
outfilename <- tempfile(pattern="file", tmpdir = tempdir())
writeGDAL(idw.ht3, outfilename, drivername = "GTiff")
file.rename (outfilename, "idw.tif")













