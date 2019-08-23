


setwd("C:\Users\MelnikK\OneDrive - scion\Documents\1 - Workspace\R_sessions\R_session_ggplot1")


if(!require(ggplot2)){install.packages("ggplot2")}
library(ggplot2)

library(tidyverse)
library(reshape2)


# Talk about:
#   - headings in teh script (show how to display)
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
wx_2018 = wx_all[wx_all$DATETIME >= as.POSIXct("2018-01-01 12:00:00") &
                wx_all$DATETIME <= as.POSIXct("2018-12-31 12:00:00"), ]


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
######################  POINT AND LINE GRAPHS ####################
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


####--------------------   _LINE PLOTS  --------------------####


# Can create a basic plot of temperature over time:
ggplot(wx_2018, aes(x = DATETIME, y = TEMP)) +
    geom_line()

# To plot mulptiple variables this way, would need to specify x and y
#   within the aesthetic separately for every line:
ggplot(wx_2018) +
    geom_line(aes(x = DATETIME, y = TEMP), color = "green") +
    geom_line(aes(x = DATETIME, y = RH), color = "red")

# But how do we insert a legend? Include color in the aes() command!
#   Note: when inside aes(), you cannot specify the actual color of the line,
#         but only the NAME associated with that color in the legend
ggplot(wx_2018) +
    geom_line(aes(x = DATETIME, y = TEMP, color = "green")) +
    geom_line(aes(x = DATETIME, y = RH, color = "red"))


# So it's better to use meaningful names:
ggplot(wx_2018) +
    geom_line(aes(x = DATETIME, y = TEMP, color = "temp")) +
    geom_line(aes(x = DATETIME, y = RH, color = "RH"))

# Want to have control over actual colors of each variable?
#   -> use scale_color manual().
# Bonus: it also allows us to change legend lables if needed
ggplot(wx_2018) +
    geom_line(aes(x = DATETIME, y = TEMP, color = "temp")) +
    geom_line(aes(x = DATETIME, y = RH, color = "RH")) +
    scale_color_manual(values = c("orange", "blue"), labels = c("RH (%)", "Temp (°C)"))
# Note1: to get the ° symbol, hold down ALT and press 248 on the number pad of the keyboard

# Want to change the position of the legend?
# Basic options include "top", "bottom", "right", "left", "none"
ggplot(wx_2018) +
    geom_line(aes(x = DATETIME, y = TEMP, color = "temp")) +
    geom_line(aes(x = DATETIME, y = RH, color = "RH")) +
    scale_color_manual(values = c("orange", "blue"), labels = c("RH (%)", "Temp (°C)")) +
    theme(legend.position = "bottom")

# Note1: change the name of the COLOR legend with:
#    + labs(color = "Variables")
# Notes2: remove the legend title with:
#   + theme(legend.title = element_blank())

# Try it here!

# labs() allows us to change x-axis and y-axis labels, title, subtitle,
#   and title of any legend you may have
ggplot(wx_2018) +
    geom_line(aes(x = DATETIME, y = TEMP, color = "temp")) +
    geom_line(aes(x = DATETIME, y = RH, color = "RH")) +
    scale_color_manual(values = c("orange", "blue"), labels = c("RH (%)", "Temp (°C)")) +
    theme(legend.position = "bottom") +
    labs(color = "Variables: ", title = "My title", subtitle = "My subtitle", x = "Date", y = "Value")


# Can customize other stuff aside from color: line type, line width, transparency




####--------------------   _POINT PLOTS  --------------------####

ggplot(wx_2018) +
    geom_point(aes(x = DATETIME, y = TEMP), color = "red")



######## DISCUSS DIFFERENT SHAPES AND COLOR VS FILL

ggplot() +
    geom_point(aes(x = 1, y = 1), shape = 1, size = 7, color = "blue", fill = "orange") +
    geom_point(aes(x = 2, y = 1), shape = 2, size = 7, color = "blue", fill = "orange") +
    geom_point(aes(x = 3, y = 1), shape = 3, size = 7, color = "blue", fill = "orange") +
    geom_point(aes(x = 4, y = 1), shape = 4, size = 7, color = "blue", fill = "orange") +
    geom_point(aes(x = 5, y = 1), shape = 5, size = 7, color = "blue", fill = "orange") +
    geom_point(aes(x = 6, y = 1), shape = 6, size = 7, color = "blue", fill = "orange") +
    geom_point(aes(x = 7, y = 1), shape = 7, size = 7, color = "blue", fill = "orange") +
    geom_point(aes(x = 8, y = 1), shape = 8, size = 7, color = "blue", fill = "orange") +
    geom_point(aes(x = 9, y = 1), shape = 9, size = 7, color = "blue", fill = "orange") +
    geom_point(aes(x = 10, y = 1), shape = 10, size = 7, color = "blue", fill = "orange") +
    geom_point(aes(x = 11, y = 1), shape = 11, size = 7, color = "blue", fill = "orange") +
    geom_point(aes(x = 12, y = 1), shape = 12, size = 7, color = "blue", fill = "orange") +
    geom_point(aes(x = 13, y = 1), shape = 13, size = 7, color = "blue", fill = "orange") +
    geom_point(aes(x = 14, y = 1), shape = 14, size = 7, color = "blue", fill = "orange") +
    geom_point(aes(x = 15, y = 1), shape = 15, size = 7, color = "blue", fill = "orange") +
    geom_point(aes(x = 16, y = 1), shape = 16, size = 7, color = "blue", fill = "orange") +
    geom_point(aes(x = 17, y = 1), shape = 17, size = 7, color = "blue", fill = "orange") +
    geom_point(aes(x = 18, y = 1), shape = 18, size = 7, color = "blue", fill = "orange") +
    geom_point(aes(x = 19, y = 1), shape = 19, size = 7, color = "blue", fill = "orange") +
    geom_point(aes(x = 20, y = 1), shape = 20, size = 7, color = "blue", fill = "orange") +
    geom_point(aes(x = 21, y = 1), shape = 21, size = 7, color = "blue", fill = "orange") +
    geom_point(aes(x = 22, y = 1), shape = 22, size = 7, color = "blue", fill = "orange") +
    geom_point(aes(x = 23, y = 1), shape = 23, size = 7, color = "blue", fill = "orange") +
    geom_point(aes(x = 24, y = 1), shape = 24, size = 7, color = "blue", fill = "orange") +
    geom_point(aes(x = 25, y = 1), shape = 25, size = 7, color = "blue", fill = "orange")





##### GEOM_SMOOTH FOR LM LINEAR TRENDLINE


# In order to avoid adding a geom_line() for every variable, can "melt"
#   the dataset:
wx_2018_melt = melt(wx_2018, id.vars = c("DATETIME"))


ggplot(wx_2018_melt, aes(x = DATETIME, y = value, color = variable)) +
    geom_line()


# Only plot temp, RH and precip:
ggplot(wx_2018_melt[wx_2018_melt$variable %in% c("TEMP", "RH", "PRECIP"), ],
       aes(x = DATETIME, y = value, color = variable)) +
    geom_line()


# dispaly precipitation as bars:
wx_plot1 = ggplot(wx_2018_melt[wx_2018_melt$variable %in% c("TEMP", "RH"), ], aes(x = DATETIME, y = value,
             color = variable, fill = variable)) +
    geom_line() +
    geom_col(data = wx_2018_melt[wx_2018_melt$variable %in% c("PRECIP"), ])
wx_plot1

# Customize labels:
wx_plot2 = wx_plot1 +
    labs(title = "Weather plot", x = "Date",
         y = "Daily precipitation (mm)\nRelative humidity (%)\nAir temperature (?C)")
wx_plot2

# Customize colors and 
wx_plot3 = wx_plot2 +
    scale_fill_manual(values = c("red", "green", "blue"),
                      labels = c("Precip", "RH", "Temp")) +
    scale_color_manual(values = c("red", "green", "blue"),
                       labels = c("Precip", "RH", "Temp"))
wx_plot3

# Place legend on the bottom:
wx_plot4 = wx_plot3 +
    theme(legend.position = "bottom", legend.title = element_blank())
wx_plot4

wx_plot4 +
    guides(colour = guide_legend(override.aes=list(shape = c(18, NA, 19),
                                                 linetype = c(0, 1, 1))))

wx_plot4 +
    guides(fill = guide_legend(override.aes = list(alpha = c(0, 1, 0.5),
                                                   width = 0.1)),
           linetype = guide_legend(override.aes = list(size = 2)))



ggplot(data = wx_2018_melt[wx_2018_melt$variable %in% c("PRECIP", "TEMP"), ],
       aes(x = DATETIME, y = value, fill = variable), ) +
    geom_col() +
    guides(fill = guide_legend(override.aes = list(alpha = c(0.2, 1))))


ggplot(wx_2018_melt[wx_2018_melt$variable %in% c("TEMP", "RH"), ], aes(x = DATETIME, y = value,
             color = variable, fill = variable)) +
    geom_line() +
    guides(linetype = guide_legend(override.aes = list(linetype = 4)))




# the final plot can also be called like this:
ggplot(wx_2018_melt[wx_2018_melt$variable %in% c("TEMP", "RH"), ], aes(x = DATETIME, y = value,
             color = variable, fill = variable)) +
    geom_line() +
    geom_col(data = wx_2018_melt[wx_2018_melt$variable %in% c("PRECIP"), ]) +
    labs(title = "Weather plot", x = "Date",
         y = "Daily precipitation (mm)\nRelative humidity (%)\nAir temperature (?C)") +
    scale_fill_manual(values= c("red", "green", "blue"),
                      labels = c("Precip", "RH", "Temp")) +
    scale_color_manual(values = c("red", "green", "blue"),
                       labels = c("Precip", "RH", "Temp")) +
    theme(legend.position = "bottom", legend.title = element_blank())

# Need to add some spaces to the legend labels to make them nicer





#$$$$$$$$$$$$$$$
# Can improve the legend by using
#   show.legend = FALSE in the geom_col and adding to the plot
#   guides(fill = guide_legend(override.aes = list(size = c(7, 1, 1))))

ggplot(wx_2018_melt[wx_2018_melt$variable %in% c("TEMP", "RH"), ], aes(x = DATETIME, y = value,
             color = variable, fill = variable)) +
    geom_line() +
    geom_col(data = wx_2018_melt[wx_2018_melt$variable %in% c("PRECIP"), ],
             show.legend = FALSE) +
    labs(title = "Weather plot", x = "Date",
         y = "Daily precipitation (mm)\nRelative humidity (%)\nAir temperature (?C)") +
    scale_color_manual(values = c("red", "green", "blue"),
                       labels = c(" Precip   ", " RH   ", " Temp   ")) +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    guides(color = guide_legend(override.aes = list(size = c(7, 1, 1))))





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















