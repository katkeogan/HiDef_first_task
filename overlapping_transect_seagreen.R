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

# set work directory
setwd("R:\\Consultancy\\HC00002 - Resources\\Grants Training Spreadsheets\\Zone85_M10_S01_20 Seagreen Phase 2&3 Overlap_DrK\\Shapefiles\\")

# read in files: convert from shapefiles and convert to data frame at the same time
flight1 <- as.data.frame(raster::shapefile("HiDef_SG_Zone85_M10_S01_D01_19_Observations_20200116.shp"))
flight2 <- as.data.frame(raster::shapefile("HiDef_SG_Zone85_M10_S01_D02_19_Observations_20200116.shp"))

# merge datasets
flight1$Flight <- "F1"
flight2$Flight <- "F2"
flights <- rbind(flight1, flight2) # "flights" will be the main dataframe

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
  geom_point(aes(x = coords.x1, y = coords.x2, colour = transect)) +
  facet_wrap( ~ transect) # I just wanted to facet wrap the transects because plotting them all together showed up some odd structure to the points.

# plot is weird. Looks like after the final transect the centroid of each grid is plotted, or something like that? I.e. "transects" 30 and 44!
# Time variable outputs all rows as "1899/12/30", so can't really use it for anything. Makes it hard to check when the weird points were generated..


