


setwd("C:\Users\MelnikK\OneDrive - scion\Documents\1 - Workspace\R_sessions\R_session_ggplot1")


if(!require(ggplot2)){install.packages("ggplot2")}
library(ggplot2)

library(tidyverse)
library(reshape2)


# color gradients
# geom_tile for fun
# geographical data




# To navigate headers in this script, clip at the two little up/down arrows in
#   the bottom-left corner of this text editor window
# A heading gets generated when there's 4 or more "#" at the end of the line.
# If there's no text, the heading is blank. So to simply separate out sections,
#   I generally use one "#" and some other symbol to avoid getting a blank heading


# Talk about:
#   - keep lines short
#   - column names are in CAPS
#   - 

# After the first ggplot graph, talk about colors

# Talk about themes


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
######################  PRE-PROCESSING WEATHER DATA ####################
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

# Plotting weather data

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


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
######################  POINT AND LINE GRAPHS ####################
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

####--------------------   _GGPLOT BASICS  --------------------####

testdata = data.frame(my_x = c(1,2), my_y = c(20, 10))
testdata

# ggplot() takes from zero to 2 arguments in this order: data and mapping
ggplot(data = testdata, aes(x = my_x , y = my_y)) +
    geom_line()

# If you get the order wrong, ggplot will throw an error:
ggplot(aes(x = my_x , y = my_y), testdata) +
    geom_line()

# can remedy this by specifying the name of each argument:
ggplot(mapping = aes(x = my_x , y = my_y), data = testdata) +
    geom_line()

# You cannot omit the mapping/aes() argument, as it's needed for ggplot to know
#   where the points/line are to be drawn
ggplot(data = testdata) +
    geom_line()

# However, you can omit data argument, as long as you give the exact x an y coordinates
#   in the aes() call:
ggplot(mapping = aes(x = testdata$my_x, y = testdata$my_y)) +
    geom_line()

# You don't even need to refer to any dataframe at all,
#   as long as x and y gets the same number of entries:
ggplot(mapping = aes(x = c(1, 2), y = c(10, 20))) +
    geom_line()


# Data and mapping can be specified in ggplot() AND/OR geom_line (and other geoms).
# The order of these two arguments are reversed in the geoms, but ggplot will
#   remind you if you forget :)
# Of course you can specify the names of the arguments if there's a particular order
#   that you want to follow

ggplot() +
    geom_line(aes(x = my_x , y = my_y), testdata)

# Usually the easiest way is to specify things that do not change between multiple
#   geomm in ggplot() call, and changeable things in individual geom_...() calls



####--------------------   _LINE PLOTS  --------------------####


# Can create a basic plot of temperature over time:
ggplot(wx_jan18, aes(x = DATETIME, y = TEMP)) +
    geom_line()

# To plot multiple variables this way, would need to specify x and y
#   within the aesthetic separately for every line:
ggplot(wx_jan18) +
    geom_line(aes(x = DATETIME, y = TEMP), color = "green") +
    geom_line(aes(x = DATETIME, y = RH), color = "red")

# But how do we insert a legend? Include color in the aes() command!
#   Note: when inside aes(), you cannot specify the actual color of the line,
#         but only the NAME associated with that color in the legend
ggplot(wx_jan18) +
    geom_line(aes(x = DATETIME, y = TEMP, color = "green")) +
    geom_line(aes(x = DATETIME, y = RH, color = "red"))


# So it's better to use meaningful names:
ggplot(wx_jan18) +
    geom_line(aes(x = DATETIME, y = TEMP, color = "temp")) +
    geom_line(aes(x = DATETIME, y = RH, color = "RH"))

# Want to have control over actual colors of each variable?
#   -> use scale_color manual().
# Bonus: it also allows us to change legend lables if needed
ggplot(wx_jan18) +
    geom_line(aes(x = DATETIME, y = TEMP, color = "temp")) +
    geom_line(aes(x = DATETIME, y = RH, color = "RH")) +
    scale_color_manual(values = c("orange", "blue"), labels = c("RH (%)", "Temp (째C)"))
# Note1: to get the 째 symbol, hold down ALT and press 248 on the number pad of the keyboard

# Want to change the position of the legend?
# Basic options include "top", "bottom", "right", "left", "none"
ggplot(wx_jan18) +
    geom_line(aes(x = DATETIME, y = TEMP, color = "temp")) +
    geom_line(aes(x = DATETIME, y = RH, color = "RH")) +
    scale_color_manual(values = c("orange", "blue"), labels = c("RH (%)", "Temp (째C)")) +
    theme(legend.position = "bottom")

# Note1: change the name of the COLOR legend with:
#    + labs(color = "Variables")
# Notes2: remove the legend title with:
#   + theme(legend.title = element_blank())

# Try it here!

# labs() allows us to change x-axis and y-axis labels, title, subtitle,
#   and title of any legend you may have
ggplot(wx_jan18) +
    geom_line(aes(x = DATETIME, y = TEMP, color = "temp")) +
    geom_line(aes(x = DATETIME, y = RH, color = "RH")) +
    scale_color_manual(values = c("orange", "blue"), labels = c("RH (%)", "Temp (째C)")) +
    theme(legend.position = "bottom") +
    labs(color = "Variables: ", title = "My title", subtitle = "My subtitle", x = "Date", y = "Value")


# Can customize other stuff aside from color: line type, line width, transparency
ggplot(wx_jan18) +
    geom_line(aes(x = DATETIME, y = TEMP),
              color = "blue", linetype = 6, alpha = 0.5, lwd = 1.5)

####--------------------   _POINT PLOTS  --------------------####


######## DISCUSS DIFFERENT SHAPES AND COLOR VS FILL

# show help document with point hapes, line widths, colors, etc.

# While lines only had color, points can have color and/or fill:
ggplot() +
    geom_point(aes(x = 1, y = 1), shape = 1, size = 7, color = "blue", fill = "orange") +
    geom_point(aes(x = 2, y = 1), shape = 19, size = 7, color = "blue", fill = "orange") +
    geom_point(aes(x = 3, y = 1), shape = 21, size = 7, color = "blue", fill = "orange")


# By default geom_point uses shape = 19
ggplot(wx_jan18) +
    geom_point(aes(x = DATETIME, y = TEMP), color = "red")

# Experiment with different shapes. size and colors here


##### GEOM_SMOOTH FOR LM LINEAR TRENDLINE

# Adding a best-fit line to the scatterplot.
# Note that if aesthethics are recycled, it's more efficient to place all aes() info
#   into the main ggplot() call

# Default:
ggplot(wx_jan18, aes(x = DATETIME, y = TEMP)) +
    geom_point() +
    geom_smooth()

# Trend line:
ggplot(wx_jan18, aes(x = DATETIME, y = TEMP)) +
    geom_point() +
    geom_smooth(method = "lm", se = F)






#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
######################  PLOT MULTIPLE VARIABLES ####################
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

# If you want to have multiple of the same geom for multiple variables,
#   can do it the long way:

ggplot(wx_jan18, aes(x = DATETIME)) +
    geom_line(aes(y = TEMP, color = "Temp")) +
    geom_line(aes(y = RH, color = "RH")) +
    geom_line(aes(y = WD, color = "WD")) +
    geom_line(aes(y = WS, color = "WS")) +
    geom_line(aes(y = PRECIP, color = "PRECIP"))


# Or, can "melt" the dataset: reshape it from wide to long format, and group
#   multiple variables of interest into a "variable" column, and their values into
#   the "value" column:

wx_jan18_melt = melt(wx_jan18, id.vars = c("DATETIME"))

# This allows us to plot multiple variables all at once (more space-efficient)
ggplot(wx_jan18_melt, aes(x = DATETIME, y = value, color = variable)) +
    geom_line()



# Challenge:
# Draw RH with points, precip with columns, and the rest with lines
#   hint: it's easier using the non-melted data







# Solution:
# melted:
ggplot(wx_jan18_melt[wx_jan18_melt$variable %in% c("TEMP", "RH", "WS"), ],
       aes(x = DATETIME, y = value,
             color = variable, fill = variable)) +
    geom_line() +
    geom_col(data = wx_jan18_melt[wx_jan18_melt$variable %in% c("PRECIP"), ]) +
    geom_point(data = wx_jan18_melt[wx_jan18_melt$variable %in% c("WD"), ])

# non-melted data:
ggplot(data = wx_jan18, aes(x = DATETIME)) +
    geom_line(aes(y = TEMP, color = "Temp")) +
    geom_line(aes(y = RH, color = "RH")) +
    geom_line(aes(y = WS, color = "Wind speed")) +
    geom_point(aes(y = WD, color = "Wind dir")) +
    geom_col(aes(y = PRECIP, fill = "Precip")) +
    labs(fill = "") +
    guides(color = guide_legend(order = 1), fill = guide_legend(order = 2)) +
    theme(legend.spacing.y = unit(0, 'cm'))


# The legend doesn't look right... To fix it, need to do some creative things
#   - remove the fill legend by using show.legend = F in geom_col()
#   - create a dummy geom_point for "Precip" with a point size of -Inf (makes it invisible)
#   - overrride the linetype, shape and size of the legend element susing override.aes

ggplot(data = wx_jan18, aes(x = DATETIME)) +
    geom_line(aes(y = TEMP, color = "Temp")) +
    geom_line(aes(y = RH, color = "RH")) +
    geom_line(aes(y = WS, color = "Wind speed")) +
    geom_point(aes(y = WD, color = "Wind dir")) +
    geom_col(aes(y = PRECIP, fill = "Precip"), show.legend = F) +
    geom_point(aes(y = PRECIP, color = "Precip"), size = -Inf) +
    guides(color = guide_legend(override.aes = list(linetype = c(1, 1, 1, NA, 1),
                                                    shape = c(NA, NA, NA, 19, NA),
                                                    size = c(6, 0.7, 0.7, 1.6, 0.7))))


# Can also reorder legend elements using scale_color_discrete(breaks = ...)
#   but note that we'll need to redo the order in override.aes!
ggplot(data = wx_jan18, aes(x = DATETIME)) +
    geom_line(aes(y = TEMP, color = "Temp")) +
    geom_line(aes(y = RH, color = "RH")) +
    geom_line(aes(y = WS, color = "Wind speed")) +
    geom_point(aes(y = WD, color = "Wind dir")) +
    geom_col(aes(y = PRECIP, fill = "Precip"), show.legend = F) +
    geom_point(aes(y = PRECIP, color = "Precip"), size = -Inf) +
    guides(color = guide_legend(override.aes = list(linetype = c(1, 1, 1, NA, 1),
                                                    shape = c(NA, NA, NA, 19, NA),
                                                    size = c(0.7, 0.7, 0.7, 1.6, 6)))) +
    scale_color_discrete(breaks = c("Temp", "RH", "Wind speed", "Wind dir", "Precip"))
    

# Challenge:
#   - Add plot title, change x axis label, y axis label, legend label.
#   - Change color scheme
#   - Change legend position








# Challenge:
# Make the same plot, but for February 2018








# Subset the dataset to only plot selected variables

# Only plot temp, RH and wind speed:
ggplot(wx_jan18_melt[wx_jan18_melt$variable %in% c("TEMP", "RH", "WS"), ],
       aes(x = DATETIME, y = value, color = variable)) +
    geom_line()





#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
######################  CREATE MULTIPLE SUBPLOTS ####################
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$









wx_all$YEAR = substr(wx_all$DATETIME, 1, 4)

wx_all$MONTH = substr(wx_all$DATETIME, 6, 7)

wx_all$DAY = substr(wx_all$DATETIME, 9, 10)

wx_all$MONTH_DAY = as.POSIXct(gsub("\\d{4}", "0000", wx_all$DATETIME))



# Combine previously made plots using grid.arrange


# Figure out y-axis limits
min_temp = min(wx_all[wx_all$YEAR == "2013", ]$TEMP, wx_all[wx_all$YEAR == "2014", ]$TEMP,
    wx_all[wx_all$YEAR == "2015", ]$TEMP, wx_all[wx_all$YEAR == "2016", ]$TEMP)

max_temp = max(wx_all[wx_all$YEAR == "2013", ]$TEMP, wx_all[wx_all$YEAR == "2014", ]$TEMP,
    wx_all[wx_all$YEAR == "2015", ]$TEMP, wx_all[wx_all$YEAR == "2016", ]$TEMP)



unique(wx_all$YEAR)

p1 = ggplot(data = wx_all[wx_all$YEAR == "2013", ], aes(x = MONTH_DAY)) +
    geom_line(aes(y = TEMP)) +
    labs(title = "2013") +
    lims(y = c(min_temp, max_temp))

p2 = ggplot(data = wx_all[wx_all$YEAR == "2014", ], aes(x = MONTH_DAY)) +
    geom_line(aes(y = TEMP)) +
    labs(title = "2014") +
    lims(y = c(min_temp, max_temp))

p3 = ggplot(data = wx_all[wx_all$YEAR == "2015", ], aes(x = MONTH_DAY)) +
    geom_line(aes(y = TEMP)) +
    labs(title = "2015") +
    lims(y = c(min_temp, max_temp))

p4 = ggplot(data = wx_all[wx_all$YEAR == "2016", ], aes(x = MONTH_DAY)) +
    geom_line(aes(y = TEMP)) +
    labs(title = "2016") +
    lims(y = c(min_temp, max_temp))

library(gridExtra)

grid.arrange(p1, p2, p3, p4) # requires gridExtra package

library(grid)

grid.arrange(arrangeGrob(p1 + theme(axis.title = element_blank()), 
                         p2 + theme(axis.title = element_blank()),
                         p3 + theme(axis.title = element_blank()),
                         p4 + theme(axis.title = element_blank()), 
                         nrow = 2,
                         bottom = textGrob("Date", vjust = 0.5,
                                           gp = gpar(fontsize = 12)),
                         left = textGrob("Temperature (캜)", rot = 90, vjust = 0.5,
                                           gp = gpar(fontsize = 12))), 
    widths=unit.c(unit(1, "npc")), 
    nrow=1) # requires grid package






# Facet Wrap


ggplot(data = wx_all, aes(x = MONTH_DAY)) +
    geom_line(aes(y = TEMP)) +
    facet_wrap(~YEAR) +
    labs(x = "Date", y = "Temperature (캜)")
# The x and y axis are made equal across plots by default.
# Try inserting the following into facet_wrap(): scales = "free"




# Smiley
ggplot() +
    geom_point(aes(x = c(2, 3), y = c(3, 3)), size = 2) +
 geom_curve(aes(x = 1.5, y = 2, xend = 3.5, yend = 2)) +
 # geom_segment(aes(x = 2, y = 2, xend = 4, yend = 2, colour = "segment")) +
    lims(x = c(1, 4), y = c(1, 4)) +
    coord_fixed(ratio = 1) +
    geom_point(aes(x = 2.5, y = 2.25), shape = 25)












#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$







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
# Try plotting some basic geographical data


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# Line plots

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
# Wind roses


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# Subplots


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# saving plots

# fancy styling in x labels


#$$$$$$$$$$$$$$$$$
# ggplot themes


#$$$$$$$$$$$$$$$$$$$$
# Colors















