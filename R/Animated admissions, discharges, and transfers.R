
# Environment -------------------------------------------------------------

pacman::p_load(stats, tools, devtools, purrr, httr, readxl, openxlsx,
               dplyr, writexl, tidyr, feather, ggplot2, ggthemes,
               hrbrthemes, gganimate, lubridate, scales, extrafont,
               animation, ggExtra,
               WorksheetFunctions, smutilities)
library(gganimate)
library(tidyverse)
library(lubridate)
library(scales)
library(readxl)
library(extrafont)
library(hrbrthemes)
library(animation)
library(ggExtra)

setoptions()
wd <- here::here()
ad <- here::here("analysis")
dd <- here::here("data")
gd <- here::here("graphics")
pd <- here::here("data", "Product Data")
fd <- here::here("feather")

ses.info <- sessioninfo::session_info()
time <- time_stamp()
# pryr::mem_used()

# Load Data ---------------------------------------------------------------

data <- import(fp(dd, "RedGreenGreyDots.xlsx"))

plot_data <- data %>% 
  mutate(Movement15 = lubridate::floor_date(MovementDateTime,"15 minutes")) %>% 
  group_by(IN_OUT, Movement_Type,Staging_Post,Movement15) %>% 
  mutate(counter = case_when (
    IN_OUT == 'IN' ~ 1,
    IN_OUT == 'OUT' ~ -1)) %>% 
  mutate(Movement_15_SEQNO =cumsum(counter)) %>% 
  ungroup() 

# Change "Tranfer In"  or "Transfer Out" to "Transfer"
plot_data$Movement_Type <- gsub("Transfer.*","Transfer",x=plot_data$Movement_Type)

# First we read the data in using the readxlsx function from the readxl package
#
# (Not shown) check the structure of the dataframe and make sure the data types
# all check out (always an especially good idea when importing from Excel)
#
# use dplyr’s mutate function to create a new variable to group all the
# movements into 15 minute intervals, which is a piece of cake with lubridate’s
# floordate function.
#
# Next we group the data by the IN_OUT, Movement_Type, Staging_Post and our new
# Movement15 variable

# We then create another new column, this time to create a counter field, with a
# value of 1 when the IN_OUT column = “IN” (so that these points appear above
# the x axis horizon) and -1 when the value is “OUT” (so the points display
# below the horizon)
#
# We create yet another variable, giving the cumulative sum of the counter
# field, so that we have a dots that stack upon each other at each 15 minute
# interval ( rather than just having one point representing the maximum /
# minimum values at each stage)
#
# The last thing to do is to tidy up the Movement_Type field - we don’t want 4
# movement types on our final plot, so we just change the values to “Transfer” -
# they are colour coded grey regardless of whether they are a transfer in or
# transfer out.


# Set limits for plotting
lims <- as.POSIXct(strptime(c("2014-09-03 00:00","2014-09-03 24:00")
                            , format = "%Y-%m-%d %H:%M")) 

ggplot(plot_data,aes(Movement15,Movement_15_SEQNO, colour=Movement_Type))+
  geom_point()+
  facet_grid(Staging_Post~.)+
  scale_x_datetime(date_labels="%H:%M",date_breaks = "3 hours",
                   limits = lims, timezone = "CET",expand = c(0,0))+
  theme_minimal()+
  theme(legend.position="bottom")+
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank())

ggplot(plot_data,aes(Movement15,Movement_15_SEQNO, colour=Movement_Type))+
  geom_jitter(width=0.10)+
  scale_colour_manual(values=c("#D7100D","#40B578","grey60"))+
  facet_grid(Staging_Post~.,switch = "y")+
  scale_x_datetime(date_labels="%H:%M",date_breaks = "3 hours",
                   limits = lims,
                   timezone = "CET",
                   expand = c(0,0))+
  ggtitle(label = "Anytown General Hospital | Wednesday 3rd September 2014 00:00 to 23:59\n",
          subtitle="A&E AND INPATIENT ARRIVALS, DEPARTURES AND TRANSFERS")+
  labs(x= NULL, y= NULL)+
  theme_ipsum(base_family = "Arial Narrow")+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  theme(axis.text.x=element_text(size=7)) +
  theme(axis.ticks.x=element_blank())+
  theme(legend.position="bottom")+
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank())+
  theme(strip.text.y = element_text(angle = 0))+
  guides(color=guide_legend("Movement Type"))


# Animation ---------------------------------------------------------------

animation::ani.options(interval = .2,ani.width = 900, ani.height = 550, ani.res = 300)

p <- ggplot(plot_data,aes(Movement15,Movement_15_SEQNO, colour=Movement_Type, frame = Movement15,cumulative = TRUE))+
  geom_jitter(width=0.10)+
  scale_colour_manual(values=c("#D7100D","#40B578","grey60"))+
  facet_grid(Staging_Post~.,switch = "both")+
  scale_x_datetime(date_labels="%H:%M",date_breaks = "3 hours",
                   limits = lims,
                   timezone = Sys.timezone(),
                   expand = c(0,0))+
  labs(x = NULL, 
       y=NULL,
       caption="@HighlandDataSci | johnmackintosh.com  Source: Neil Pettinger | @kurtstat | kurtosis.co.uk")+
  ggtitle(label = "Anytown General Hospital | Wednesday 3rd September 2014 00:00 to 23:59\n",
          subtitle="A&E AND INPATIENT ARRIVALS, DEPARTURES AND TRANSFERS")+
  theme_ipsum(base_size = 10,base_family = "Tahoma")+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  theme(axis.text.x=element_text(size=7)) +
  theme(axis.ticks.x=element_blank())+
  guides(color=guide_legend("Movement Type"))+
  theme(legend.position="bottom")+ 
  theme(strip.text.y = element_text(angle = 0))+
  ggExtra::removeGrid() 


gganimate(p,filename = "row_of_dots.gif")#,#interval = .2)

