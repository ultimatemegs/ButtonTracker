# Authors: 
# Megan Barnes  #add your name if you write some code

# If you can't see the data, i might have to approve something on Google Drive please request it so i can - It is a live dataset
# If you use the data, please tag @lilulovedog instagram, or LMK on twitter @ultimatemegs 

# Load required packages

library(googlesheets4)
library(tidyverse)
library(shiny)
library(waffle)

buttonTracking <- read_sheet("https://docs.google.com/spreadsheets/d/1XJpADBMuZrepK0e7dP7yd9oIIi1KLmdvMec9Xfo-ouQ/edit#gid=2013400418")
buttonTracking <- buttonTracking[,c(1:5,8)]
buttonTracking <- buttonTracking[which(!is.na(buttonTracking$Timestamp)),]
names(buttonTracking) <- c("Timestamp", "ActivationDate", "ActivationTime", 
                           "Button", "Use","ReportingPeriod")

# for practice, rm last activiation 22nd
buttonTracking <- buttonTracking[1:50,]

# Specific manual tidying - do after date conversion of col 3
buttonTracking[1,3] <- buttonTracking[1,1]

# Fixing various date related elements
# first time 
buttonTracking$ActivationTime <- format(buttonTracking$ActivationTime, format="%HH:%MM:%SS")

# now date col
# note that i had to put format in automatic and copy the date formatted" versions 
# into the rows i had entered manually to fix the spreadsheet

buttonTracking$ActivationDate <- format(buttonTracking$ActivationDate,format="%Y-%m-%d", tz = "UTC") %>%
  as.POSIXct(usetz=TRUE, tz= "Australia/Perth")


# Now merge
buttonTracking$DateTimeCombo <-  with(buttonTracking, ymd(ActivationDate) + hms(ActivationTime)) # this is lubridate
p <- buttonTracking$DateTimeCombo
p2 <- as.POSIXct(format(p),tz="UTC")
attr(p2,"tzone") <- "Australia/Perth"

###### Summarise data ######

# by button overall
buttonSummary_total <- buttonTracking %>%
  group_by(Button) %>%
  summarise(count_by_button = n())

# to generate button counts by day
buttonSummary_date <- buttonTracking %>%
  group_by(Button, ActivationDate) %>% 
  summarise(count_by_date = n())

# to generate average frequency by two week reporting period
        # cheat way is to manually add a reporting period ID column and summarise by that...
        # it should be relatively easy to do this computationally i sjust ahve to deal with dates which i hate
buttonSummary_period <- buttonTracking %>%
  group_by(Button, ReportingPeriod) %>% 
  summarise(count_by_period = n())


###### A BUNCH OF PLOTS ########


#Total 
buttonPlot_total <- ggplot(buttonSummary_total, aes(x =Button, y =  count_by_button, # change to date/time omce i combine them somehow... ?
                                                    fill = Button)) +
  geom_col() +
  #theme(legend.position = "top") +
  #theme_minimal()+
  labs(title = "Button activations by Reporting Period",
       x = "ReportingPeriod",
       y = "Activation Frequency") +
  scale_x_discrete()  
#geom_col(width = 0.5)

# By Reporting Period
buttonPlot_period_stackedbar <- ggplot(buttonSummary_period, aes(x = ReportingPeriod, y = count_by_period, # change to date/time omce i combine them somehow... ?
                                                  fill = Button)) +
  #geom_bar(position="stack", stat="identity") +
  geom_col() +
  #theme(legend.position = "top") +
  #theme_minimal()+
  labs(title = "Button activations by Reporting Period",
       x = "ReportingPeriod",
       y = "Activation Frequency") +
  scale_x_discrete()  
  #geom_col(width = 0.5)

buttonPlot_period_groupedbar <-  ggplot(buttonSummary_period) +
  geom_bar(aes(x = ReportingPeriod, y = count_by_period, fill = Button),
           stat = "identity", position = position_dodge2(width = 0.9, preserve = "single")) +  # there are other options for that .https://stackoverflow.com/questions/38101512/the-same-width-of-the-bars-in-geom-barposition-dodge

  labs(title = "Button activations by Reporting Period",
       y = "Activation Frequency") +
  scale_x_discrete("ReportingPeriod", labels=c("1","2"))
  
buttonPlot_period_facet <-  ggplot(buttonSummary_period) +
  geom_bar(aes(x = ReportingPeriod, y = count_by_period, fill = Button),
           stat = "identity", position = position_dodge2(width = 0.9, preserve = "single")) +  
  facet_grid(.~ReportingPeriod, switch="both") +
  ylim(0,max(buttonSummary_period$count_by_period+5)) + #if i can round to a multiple of 5 and add 1, that would be better 
  labs(title = "Button activations by Reporting Period",
       y = "Activation Frequency") +
  scale_x_discrete("ReportingPeriod", labels=c("1","2"))


### BY Date - not quite there yet
buttonPlot_date <- ggplot(buttonSummary_date, aes(x = ActivationDate, y= count_by_date,   # change to date/time omce i combine them somehow... ?
                                                  fill = Button)) +
  geom_col(stat = "identity") +
  scale_x_continuous() +
  labs(title = "Button activations by date",
       x = "Date",
       y = "Activation Frequency") + 
  geom_col(width = 1)

buttonPlot_date

# Basic line plot
buttonPlot_date_line <- ggplot(buttonSummary_date, aes(x = ActivationDate, y= count_by_date)) + 
                                geom_line(aes(color = Button), size = 1)

# 3 nice colours - need 7, or a goood package for divergent palettes
# + scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))

# Plot a subset of the data - just one button
ss <- subset(buttonSummary_date, Button = LoveYou)
ggplot(data = ss, aes(x = ActivationDate, y= count_by_date)) + 
  geom_line(color = "#FC4E07", size = 2)

buttonPlot_date_area <- ggplot(buttonSummary_date, aes(x = ActivationDate, y= count_by_date)) +
  geom_area(aes(color = Button, fill = Button), 
            alpha = 0.2, position = position_dodge(0.8)) #+
  # scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  # scale_fill_manual(values = c("#00AFBB", "#E7B800"))


# WAFFLE
waffledata <- as.vector(buttonSummary_total$count_by_button)
names(waffledata) <- as.character(buttonSummary_total$Button)

norm <- waffledata/length(waffledata)
norm <- waffledata

buttonSummary_total_waffle <- waffle(norm, rows=2, size=0.1, 
       colors=c("#c7d4b6", "#a3aabd", "#a0d0de", "#97b5cf", "#00AFBB", "#E7B800", "#FC4E07"), 
       title="Button Presses as a proportion of total presses", 
       xlab="One square == 1 button press")

# Print all the plots - if you make a new one, please put it here
pdf("allPlots.pdf")
#buttonPlot_total

jpeg("waffle.jpg")
buttonSummary_total_waffle

# buttonPlot_period_stackedbar
# buttonPlot_period_groupedbar
# buttonPlot_period_facet
# 
# buttonPlot_date_line
# ss
# buttonPlot_date_area
# 
# buttonSummary_total_waffle

dev.off()

# Make a markdown and add observations
# Line  Charts for daily activations? 
# Waffle Charts for each button as proportion of total using waffle package 
# CDF for total presses through time by button? - cum hist 
# Functionify the plots 
# more beautiful colours
# RShiny for display (new script required): https://shiny.rstudio.com/articles/plot-interaction.html

# add: 
    # Number of words per day summary sheet & visualise
    # Number per day, and plot as time/series
    # Daily average, and plot moving window?
    # Total number of words used
  


# Some more ideas... e.g. the ball one looks interesting , 
  # http://www.rebeccabarter.com/blog/2018-05-29_getting_fancy_ggplot2/

