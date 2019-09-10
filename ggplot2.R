
#*************************************************************************
#*************************************************************************
#               ** GGPLOT SESSION 2 (11 SEPTEMBER 2019) **            ####
#*************************************************************************
#*************************************************************************




# If you are not working in a version control R Studio Project,
#  You will need to use setwd() to specify your working directory
#   Note: you will need to use forward slashes instead of backslashes (Wibdows default),
#   and remove the "#" at teh beginning of the line
#setwd("C:\Users\OneDrive - scion\Documents\1 - Workspace\R_sessions\R_session_ggplot1")


if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(reshape2)){install.packages("reshape2")}
if(!require(dplyr)){install.packages("dplyr")} # part of tidyverse
if(!require(magrittr)){install.packages("magrittr")} # part of tidyverse
if(!require(gridExtra)){install.packages("gridExtra")}
if(!require(grid)){install.packages("grid")}

library(ggplot2)
library(reshape2)
library(dplyr) # part of tidyverse
library(magrittr) # part of tidyverse
library(gridExtra)
library(grid)


# Set a desired ggplot theme:
theme_set(theme_classic())




## Function to extract legend
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}





#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
######################      SUBSETTING OVERVIEW     ####################
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

# Subsetting or slicing vectors and dataframes means retrieving only the parts
# that are of interest. It's easy to do using square barckets


####--------------   _SUBSETTING VECTORS  ------------####

# Vectors are one-dimensional, and take only ONE index or ONE list of indeces for subsetting

# Basic syntax for vectors:
#   myvector[indices_to_keep]. Let's try a few examples.

#Create a vector:
myvector = c("a", "b", "c", "d", "a", "c")

# Select the first item:
myvector[1]

# Select items 3 to 4 (inclusive):
myvector[3:4]

# Select items 2 and 4 (note that the list of indices has to be in a vector of their own to work):
myvector[c(2, 4)]

# Now lets try subsetting based on value. Select all items that are equal to "a":
myvector[myvector == "a"]

# Next, select all items that are equal to "b" or "c":
myvector[myvector == "b" | myvector == "c"] # method 1
myvector[myvector %in% c("b", "c")] # method 2


####--------------   _SUBSETTING DATAFRAMES  ------------####

# Dataframes are two-dimensional, and need TWO indeces or TWO lists of indeces for subsetting

# Basic syntax for dataframes:
#   mydataset[indices_of_rows_to_keep, indices_of_columns_to_keep].

# Create a dataframe:
mydf = data.frame(FRUIT = c("apple", "orange", "pear", "cherry", "eggplant"),
                  FRUIT_TYPE = c("pome", "berry", "pome", "drupe", "berry"),
                  COST = c(2, 3, 3, 15, 8))

# Select the value in the first row and first column:
mydf[1, 1]

# Select all values in the first row:
mydf[1, ]

# Select all values in the first column:
mydf[ , 1]

# Select all rows where FRUIT_TYPE is a berry:
mydf[mydf$FRUIT_TYPE == "berry", ] # don't forget the comma!

# Select only column FRUIT and COST:
mydf[, c("FRUIT", "COST")]

# List fruits that cost less than $5:
mydf[mydf$COST < 5, "FRUIT"]

# Select rows where cost is either 2 or 8 dollars:
mydf[mydf$COST %in% c(2, 8), ]


# Challenge1:
# Create a smaller dataframe that only contains rows where FRUIT_TYPE is berry or drupe






# Challenge2:
# Get the cost of the eggplant








#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#####################   _IMPORTING AND PREPPING REAL DATA   ############
#$$$$$$$$$$$$$$$$$$$$    (same as in the last session)      $$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


# Read in the weather data file
wx_all = read.csv("Athol_wx.csv", skip = 1)

# Check structure of the DATETIME column:
str(wx_all$DATETIME)


# Create a NEWDATETIME column, which is the same as DATETIME,
#   but in proper datetime format
wx_all$NEWDATETIME = as.POSIXct(wx_all$DATETIME, format = "%d/%m/%Y %H:%M")

# Ensure that the NEWDATETIME has the same values as DATETIME.
#   Then delete the DATETIME COLUMN:
wx_all = within(wx_all, rm(DATETIME))

# Rename NEWDATETIME to DATETIME:
names(wx_all)[names(wx_all) == "NEWDATETIME"] <- "DATETIME"

# Now that DATETIME is in POSIXct format, can use "<" and ">" and plot it more easily.

# Subset the dataframe to only retain 2018 weather:
wx_jan18 = wx_all[wx_all$DATETIME >= as.POSIXct("2018-01-01 00:00:00") &
                    wx_all$DATETIME < as.POSIXct("2018-02-01 00:00:00"), ]


# Add some new columns to the wx_all dataset to be used later

wx_all$YEAR = substr(wx_all$DATETIME, 1, 4) # extract from 1st to 4th character
wx_all$MONTH = substr(wx_all$DATETIME, 6, 7) # extract from 6th to 7th character
wx_all$DAY = substr(wx_all$DATETIME, 9, 10) # extract from 9th to 10th character


# Convert month number to month abbreviation
wx_all$MONTH_ABB = factor(month.abb[as.numeric(wx_all$MONTH)], levels = month.abb)

# Create a new date column hwere you substitute the year with "0000"
#   The result is still in datetime format, but year has no effect
#   -> can plot multiple years together
wx_all$MONTH_DAY = as.POSIXct(gsub("\\d{4}", "0000", wx_all$DATETIME))



#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
######################  CREATE MULTIPLE SUBPLOTS ####################
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


####-----------------   _COMBINE PLOTS WITH GRID.ARRANGE  -----------------####

# We want to create a plot like the one below for the first 4 years
ggplot(data = wx_all[wx_all$YEAR == "2013", ], aes(x = WS, color = TEMP)) +
    geom_point(aes(y = WD)) +
    labs(title = "2013")


# First, figure out common y-axis limits and color scale limits

value_ranges0 = summarize(group_by(wx_all[wx_all$YEAR %in% c(2013, 2014, 2015, 2016), ], YEAR),
          WS_MIN = min(WS, na.rm = T), WS_MAX = max(WS, na.rm = T),
          WD_MIN = min(WD, na.rm = T), WD_MAX = max(WD, na.rm = T),
          TEMP_MIN = min(TEMP, na.rm = T), TEMP_MAX = max(TEMP, na.rm = T))

value_ranges = summarize(group_by(value_ranges0),
          WS_MIN = min(WS_MIN, na.rm = T), WS_MAX = max(WS_MAX, na.rm = T),
          WD_MIN = min(WD_MIN, na.rm = T), WD_MAX = max(WD_MAX, na.rm = T),
          TEMP_MIN = min(TEMP_MIN, na.rm = T), TEMP_MAX = max(TEMP_MAX, na.rm = T))


# Get a list of all unique years in the YEAR column
unique(wx_all$YEAR)
# There are 7 different years, but here we will onyl plot the first 4

# Create each individual plot and save as an object:
p1 = ggplot(data = wx_all[wx_all$YEAR == "2013", ], aes(x = WS, color = TEMP)) +
    geom_point(aes(y = WD)) +
    labs(title = "2013") +
    lims(x = c(value_ranges$WS_MIN, value_ranges$WS_MAX), y = c(value_ranges$WD_MIN, value_ranges$WD_MAX),
         color = c(value_ranges$TEMP_MIN, value_ranges$TEMP_MAX))

p2 = ggplot(data = wx_all[wx_all$YEAR == "2014", ], aes(x = WS, color = TEMP)) +
    geom_point(aes(y = WD)) +
    labs(title = "2014") +
    lims(x = c(value_ranges$WS_MIN, value_ranges$WS_MAX), y = c(value_ranges$WD_MIN, value_ranges$WD_MAX), color = c(value_ranges$TEMP_MIN, value_ranges$TEMP_MAX))

p3 = ggplot(data = wx_all[wx_all$YEAR == "2015", ], aes(x = WS, color = TEMP)) +
    geom_point(aes(y = WD)) +
    labs(title = "2015") +
    lims(x = c(value_ranges$WS_MIN, value_ranges$WS_MAX), y = c(value_ranges$WD_MIN, value_ranges$WD_MAX), color = c(value_ranges$TEMP_MIN, value_ranges$TEMP_MAX))

p4 = ggplot(data = wx_all[wx_all$YEAR == "2016", ], aes(x = WS, color = TEMP)) +
    geom_point(aes(y = WD)) +
    labs(title = "2016") +
    lims(x = c(value_ranges$WS_MIN, value_ranges$WS_MAX), y = c(value_ranges$WD_MIN, value_ranges$WD_MAX), color = c(value_ranges$TEMP_MIN, value_ranges$TEMP_MAX))


# Join the plots together:
grid.arrange(p1, p2, p3, p4) # grid.arrange() requires "gridExtra" package



# Remove individual axis labels using: theme(axis.title = element_blank()
#   and add a global axis labels using bottom = textGrob() and left = textGrob()
# Note: textGrob() requires "grid" package
grid.arrange(arrangeGrob(p1 + theme(axis.title = element_blank()), 
                         p2 + theme(axis.title = element_blank()),
                         p3 + theme(axis.title = element_blank()),
                         p4 + theme(axis.title = element_blank()), 
                         nrow = 2,
                         bottom = textGrob("Wind speed (m/s)", vjust = 0.5,
                                           gp = gpar(fontsize = 12)),
                         left = textGrob("Wind direction (°)", rot = 90, vjust = 0.5,
                                           gp = gpar(fontsize = 12))), 
             widths=unit.c(unit(1, "npc")), nrow=1)


# The legends are repetitive.
# Need to extract the legends from the first plot, remove all legends,
#   and then paste the extracted legend on the right side




mylegend = g_legend(p1) # NOTE: g_legend is a custom function at the top of this script!

# Remove legends from individual plots using: legend.position = "none",
#   and add our extracted legend on the right using: right = mylegend
grid.arrange(arrangeGrob(p1 + theme(axis.title = element_blank(), legend.position = "none"), 
                         p2 + theme(axis.title = element_blank(), legend.position = "none"),
                         p3 + theme(axis.title = element_blank(), legend.position = "none"),
                         p4 + theme(axis.title = element_blank(), legend.position = "none"), 
                         nrow = 2,
                         bottom = textGrob("Wind speed (m/s)", vjust = 0.5,
                                           gp = gpar(fontsize = 12)),
                         left = textGrob("Wind direction (°)", rot = 90, vjust = 0.5,
                                           gp = gpar(fontsize = 12)),
                         right = mylegend), 
             widths=unit.c(unit(1, "npc")), nrow=1)




# Summary: using this method can get quite cumbersome, but it is also very flexible.
#   It allows us to join any plots together, related or not
#   It also preserves x and y axes of each individual plot




####-----------------   _MAKE SUBPLOTS WITH FACET_WRAP  -----------------####

ggplot(data = wx_all, aes(x = WS, color = TEMP)) +
    geom_point(aes(y = WD)) +
    facet_wrap(~YEAR) +
    labs(x = "Wind speed (m/s)", y = "Wind direction (°)")

# The x and y axis are made equal across plots by default, and all years are plotted by default

# Challenge1:
# Try inserting the following into facet_wrap(): scales = "free"
# Also, try specifying the number of rows or the number of columns inside facet_wrap()
#   with ncol = ... or nrow = ...





# Challenge:
# Subset the dataset to only plot years 2013, 2014, 2015 and 2016
# Hint: use square brackets and %in%



# Solution:
wx_4years = wx_all[wx_all$YEAR %in% c(2013, 2014, 2015, 2016), ]

ggplot(data = wx_4years, aes(x = WS, color = TEMP)) +
    geom_point(aes(y = WD)) +
    facet_wrap(~YEAR) +
    labs(x = "Wind speed (m/s)", y = "Wind direction (°)")


# The titles of each individual facet are referred to as "strip.text", which can be customized:
ggplot(data = wx_4years, aes(x = WS, color = TEMP)) +
    geom_point(aes(y = WD)) +
    facet_wrap(~YEAR) +
    labs(x = "Wind speed (m/s)", y = "Wind direction (°)") +
    theme(strip.background = element_blank(), strip.text = element_text(hjust = 0))


# Summary: facet_wrap requires very little coding, but is not nearly as flexible.
#   The x and y axes are only shown once. Several packages allow customization (eg package "lemon")

if(!require(lemon))install.packages("lemon")
library(lemon)
ggplot(data = wx_4years, aes(x = WS, color = TEMP)) +
    geom_point(aes(y = WD)) +
    facet_rep_wrap(~ YEAR, repeat.tick.labels = 'all') + # note that facet_rep_wrap is NOT a native ggplot command
    labs(x = "Wind speed (m/s)", y = "Wind direction (°)") +
    theme(strip.background = element_blank(), strip.text = element_text(hjust = 0))



# Challenge:
# Try making your own faceted plot here for years 2017 and 2018 only.
#   It can be very similar to the ones above, or can use different geometry (eg geom_line)
#   And different variable (eg TEMP and RH colored by MONTH_ABB)
# Hint: use square brackets to subset the wx_all dataset








# Solution:

wx_2years = wx_all[wx_all$YEAR %in% c(2017, 2018), ]

ggplot(data = wx_2years, aes(x = TEMP, color = MONTH_ABB)) +
    geom_point(aes(y = RH)) +
    facet_wrap(~YEAR, ncol = 1) +
    labs(x = "Wind speed (m/s)", y = "Wind direction (°)")







#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
######################    COLORS AND COLOR SCALES   ####################
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


# REVISITING CONTINUOUS COLOR SCALE FROM THE SECTION ABOVE

ggplot(wx_all, aes(x = WS, y = WD, color = TEMP)) +
    geom_point()


# Want to change the color scheme?

# Can use scale_color_gradient to specify low and high colors:
ggplot(wx_all, aes(x = WS, y = WD, color = TEMP)) +
    geom_point() +
    scale_color_gradient(low = "blue", high = "green3")


# Or, use scale_color_gradient2 (note the 2!) to specify low, mid and high colors:
ggplot(wx_all, aes(x = WS, y = WD, color = TEMP)) +
    geom_point() +
    scale_color_gradient2(low = "black", mid = "blue3", high = "magenta", midpoint = 15)


# Another option is to use pre-built color palettes
ggplot(wx_all, aes(x = WS, y = WD, color = TEMP)) +
    geom_point() +
    scale_color_gradientn(colours = heat.colors(10))
# The other built-in palettes that come with R by default are rainbow() and topo.colors()

# Challenge:
# Try the rainbow() and topo.colors() palettes here, and also experiment with changing the number
#   in the brackets, which indicates the number of colors generated





# rainbow() function can takes additional argument such as start and end:
ggplot(wx_all, aes(x = WS, y = WD, color = TEMP)) +
    geom_point() +
    scale_color_gradientn(colours = rainbow(10, start = 1/6, end = 5/6))

# Can reverse any color scale (or any vector for that matter) using rev() function:
ggplot(wx_all, aes(x = WS, y = WD, color = TEMP)) +
    geom_point() +
    scale_color_gradientn(colours = rev(rainbow(10, start = 1/6, end = 5/6)))




# there are also great packages that provide more options (e.g. viridis)
if(!require(viridis))install.packages("viridis")
library(viridis)

ggplot(wx_all, aes(x = WS, y = WD, color = TEMP)) +
    geom_point() +
    scale_color_gradientn(colours = viridis(10))

# Challenge:
# Try using magma, plasma, inferno, cividis pallettes from the viridis package





#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
######################    BOXPLOTS AND BAR PLOTS   ####################
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


####-----------------   _BOXPLOTS  -----------------####

# Simple bar plot of temperature by month:
ggplot(wx_all, aes(x = MONTH_ABB, y = TEMP)) +
    geom_boxplot()


# Could add geom_jitter to see the actual distribution of poitn overlaid overtop
#   (not that useful in thsi case)
ggplot(wx_all, aes(x = MONTH_ABB, y = TEMP)) +
    geom_boxplot() +
    geom_jitter(alpha = 0.2, color = "blue")

# For smaller datasets, geom_dotplot might be useful:
wx_small = wx_all[wx_all$DAY == "01", ]

ggplot(wx_small, aes(x = MONTH_ABB, y = TEMP)) +
    geom_boxplot() +
    geom_dotplot(binaxis = "y", stackdir = "center", alpha = 0.5, fill = "blue", dotsize = 0.7)



# Can use stat_summary to find the maximum value in each category, and put the label there:
ggplot(wx_all, aes(x = MONTH_ABB, y = TEMP)) +
    geom_boxplot() +
    stat_summary(geom = 'text', label = c("Cu", "s", "t", "o", "m", "", "L",
                                          "a", "b", "e", "l", "s"),
                 fun.y = max, vjust = -0.4, color = "blue2")


####-----------------   _BAR PLOTS  -----------------####

# Lets try making a barplot using geom_col:
ggplot(wx_small, aes(x = YEAR, y = TEMP)) +
    geom_col()
# It shows the "total" temperature occuring in each year, Doesn't make much sense

# It's actually stacking every observation form every day on top of each other... To visualize:
ggplot(wx_small, aes(x = YEAR, y = TEMP, fill = MONTH_ABB)) +
    geom_col() 
  

# To avoid this behavior, and to calculate the average temperature instead,
#   can use position = "dodge":
ggplot(wx_all, aes(x = YEAR, y = TEMP)) +
    geom_col(position = "dodge")

# Adding fill factor here will actually place multiple months side-by-side within each year
ggplot(wx_all, aes(x = YEAR, y = TEMP, fill = MONTH_ABB)) +
    geom_col(position = "dodge")

# Lets try stacking the observations instead
ggplot(wx_all, aes(x = YEAR, y = TEMP, fill = MONTH_ABB)) +
    geom_col(position = "stack")


# In order to make a "typical" bar chart with error bars, use stat_summary:
ggplot(wx_small, aes(x = MONTH_ABB, y = TEMP)) +
  stat_summary(geom = "col", fun.y = mean, position = "dodge") +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge")


# Can try to use stat summary to nicely alighn text labels:
ggplot(wx_small, aes(x = MONTH_ABB, y = TEMP)) +
  stat_summary(geom = "col", fun.y = mean) +
  stat_summary(geom = "errorbar", fun.data = mean_se) +
  stat_summary(geom = "text", fun.y = max, color = "blue3",
               label = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l"))

# I'd liek to place the labels right above the error bars...
#   We can summarize the data by hand for that

sum_small = summarize(group_by(wx_small, MONTH_ABB), TEMP_MEAN = mean(TEMP, na.rm = T),
                  TEMP_SE = sd(TEMP, na.rm = T)/sqrt(n()))

# Lets try not using stat_summary at all this time, and see if we can make the same plot
#   But eith better label positioning
ggplot(sum_small, aes(x = MONTH_ABB, y = TEMP_MEAN)) +
  geom_col() +
  geom_errorbar(aes(ymin = TEMP_MEAN-TEMP_SE, ymax = TEMP_MEAN+TEMP_SE)) +
  geom_text(aes(y = TEMP_MEAN+TEMP_SE), color = "blue3",
               label = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l"), vjust = -0.5)
# Perfect




####-----------------   _WIND RSOE USING GEOM_COL -----------------####

# First, need to convert degrees to cardinal directions (we'll breask it up into 16 directions)

# Create a vector with the 16 letter directions in proper order:
mydir = c("N","NNE", "NE", "NEE", "E", "SEE", "SE", "SSE", "S",
                  "SSW", "SW", "SWW", "W", "NWW", "NW", "NNW", "N")

# Convert degrees to letter directions:
wx_small$WD_LET = mydir[round(wx_small$WD/(360/16)) + 1]

# Assign order to the letter directions:
wx_small$WD_LET = factor(wx_small$WD_LET, levels = unique(mydir))

# Now we are ready to plot!
ggplot(wx_small, aes(x = WD_LET, y = 1, fill = WS)) +
  geom_col()
# Hm, the colors are all over the place

# Can order the dataframe by WS_KH in increasing order before plotting it:
wx_small_ordered = wx_small[order(wx_small$WS, decreasing = F), ]

ggplot(wx_small_ordered, aes(x = WD_LET, y = 1, fill = WS)) +
  geom_col()
# Looks good! But where is the rose?

# Add coord_polar
ggplot(wx_small_ordered, aes(x = WD_LET, y = 1, fill = WS)) +
    geom_col() +
    coord_polar(start = -((22.5/2))/180 * pi)

# Prettify the plot a bit:
ggplot(wx_small_ordered, aes(x = WD_LET, y = 1, fill = WS)) +
    geom_col() +
    coord_polar(start = -((22.5/2))/180 * pi) +
    scale_fill_gradientn(colours = terrain.colors(10)) +
  labs(y = "Number of observations") +
    theme_light() +
    theme(axis.title.x = element_blank())



# For large datasets this would be inefficient, as each individual observation gets plotted separately.
#   To make the computer's task easier, can sumamrize the dataset first, and calculate the size of each
#   stacked bar addition

wx_sum_wind1 = summarize(group_by(wx_all[!is.na(wx_all$WD_LET), ], WD_LET, WS), COUNT = n())
wx_sum_wind1_ordered = wx_sum_wind1[order(wx_sum_wind1$WS, decreasing = F), ]


ggplot(wx_sum_wind1_ordered, aes(x = WD_LET, y = COUNT, fill = WS)) +
    geom_col() +
    coord_polar(start = -((22.5/2))/180 * pi) +
    scale_fill_gradientn(colours = terrain.colors(10)) +
    theme_light()

# Could also calculate relative frequency of occurence:
wx_sum_wind2 = mutate(group_by(wx_sum_wind1_ordered, WS), TOTAL = n(), FREQ = COUNT/TOTAL)

ggplot(wx_sum_wind2, aes(x = WD_LET, y = FREQ, fill = WS)) +
    geom_col() +
    coord_polar(start = -((22.5/2))/180 * pi) +
    scale_fill_gradientn(colours = terrain.colors(10)) +
    theme_light()

