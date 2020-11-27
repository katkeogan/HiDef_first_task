###########################################################
#### script for exploring overlapping aerial transects ####
####                  Seagreen 2 + 3                   ####
####              Kat Keogan - 25.11.2020              ####
###########################################################

rm(list = ls(all = TRUE))

# load libraries
library(raster)
library(rgdal)
library(ggplot2)
library(tidyr)
library(reshape2)

# set work directory
setwd("R:\\Projects\\HP00000 - Projects\\HP00104 - Seagreen Phase 2 and 3\\500 - Data Processing\\2019 - Month 10 - Survey 01\\Density Estimates\\Output\\D01\\")

# read in files: convert from shapefiles and convert to data frame at the same time
flight1 <- as.data.frame(raster::shapefile("Zone85_M10_S01_D01_19_Output\\Zone85_M10_S01_D01_19_Output-Day1-CentCount.shp"))
flight2 <- as.data.frame(raster::shapefile("Zone85_M10_S01_D02_19_Output\\Zone85_M10_S01_D02_19_Output-Day1-CentCount.shp"))

# convert to long format for data exploration and plotting
flight1_long <- melt(flight1, id.vars = c("transect", "coords.x1", "coords.x2"))
flight2_long <- melt(flight2, id.vars = c("transect", "coords.x1", "coords.x2"))

ggplot(data = flight1_long) +
  geom_point(aes(x = coords.x1, y = coords.x2, colour = transect)) +
  facet_wrap( ~ transect)



# merge datasets
flight1_long$Flight <- "F1"
flight2_long$Flight <- "F2"
flights <- rbind(flight1_long, flight2_long) # "flights" will be the main dataframe

# assign transect numbers to each transect
flights$transect <- NA # new column for transect numbers
flights$transect[which(flights$Broad_Cate == "Start of Transect")] <- seq(1,length(which(flights$Broad_Cate == "Start of Transect")),1) # assign a unique number to each "Start of Transect" row.
flights <- flights %>% tidyr::fill(transect) # fill the remaining rows with the corresponding transect number. Could only get the fill function to work using a pipe.
flights$transect <- as.factor(flights$transect)

# find overlapping transects
# will do this by looking for matching starting coordinates. 

# view data
# split data by "transect" but not by "flight" yet. 
ggplot(data = flights) +
  geom_point(aes(x = coords.x1, y = coords.x2, colour = Flight)) 
  facet_wrap( ~ transect) # I just wanted to facet wrap the transects because plotting them all together showed up some odd structure to the points.

# plot is weird. Looks like after the final transect the centroid of each grid is plotted, or something like that? I.e. "transects" 30 and 44!
# Time variable outputs all rows as "1899/12/30", so can't really use it for anything. Makes it hard to check when the weird points were generated..


