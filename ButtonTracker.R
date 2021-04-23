#Load required packages

library(googlesheets4)
library(tidyverse)
library(shiny)
source("./CountNAsBill.R")

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

##### Summarise data #####

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

# plot data

buttonPlot_date <- ggplot(buttonSummary_date, aes(x = ActivationDate, # change to date/time omce i combine them somehow... ?
                                     fill = Button)) +
  #geom_bar(aes(fill = Button), position = position_stack(reverse = TRUE)) +
  geom_bar(stat = "bin") +
  #theme(legend.position = "top") +
  #theme_minimal()+
  labs(title = "Button activations by date",
       x = "Date",
       y = "Activation Frequency") + 
  geom_col(width = 1)

buttonPlot_date

# By Reporting Period
buttonPlot_period <- ggplot(buttonSummary_period, aes(x = ReportingPeriod, y = count_by_period, # change to date/time omce i combine them somehow... ?
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

#Total 
buttonSummary_total
ggplot(buttonSummary_period, aes(x = ReportingPeriod, y = count_by_period, # change to date/time omce i combine them somehow... ?
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


# Print all the plots 
buttonPlot_period
buttonPlot_date
buttonPlot_total

