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
library(dplyr)
library(sf)

# set work directory
# if not connected to VPN you get an error message
setwd("R:\\Projects\\HP00000 - Projects\\HP00104 - Seagreen Phase 2 and 3\\500 - Data Processing\\2019 - Month 10 - Survey 01\\Density Estimates\\Output\\D01\\")

# read in files: convert centcount file from shapefiles and convert to data frame at the same time
flight1 <- as.data.frame(raster::shapefile("Zone85_M10_S01_D01_19_Output\\Zone85_M10_S01_D01_19_Output-Day1-CentCount.shp"))
flight2 <- as.data.frame(raster::shapefile("Zone85_M10_S01_D02_19_Output\\Zone85_M10_S01_D02_19_Output-Day1-CentCount.shp"))

# read in files: convert transects file from shapefiles and convert to data frame at the same time
# need to take the area of each kernel from this. 
transect1 <- as.data.frame(raster::shapefile("Zone85_M10_S01_D01_19_Output\\Zone85_M10_S01_D01_19_Output-Day1-Transects.shp"))
transect2 <- as.data.frame(raster::shapefile("Zone85_M10_S01_D02_19_Output\\Zone85_M10_S01_D02_19_Output-Day1-Transects.shp"))

# filter the correct transect numbers
flight1_overlap <- dplyr::filter(flight1, transect %in% c(1, 3, 4, 5))
flight2_overlap <- dplyr::filter(flight2, transect %in% c(5, 6, 8, 9))

transect1_overlap <- transect1 %>% filter(transect %in% c(1, 3, 4, 5)) 
transect2_overlap <- transect2 %>% filter(transect %in% c(5, 6, 8, 9)) 

# turn every filtered dataframe into a spatial points data frame (otherwise you can't save a shapefile)
# flight 1
xy <- cbind(flight1_overlap$coords.x1,flight1_overlap$coords.x2)
flight1_overlap_sp <- SpatialPointsDataFrame(xy, flight1_overlap)
xy <- cbind(transect1_overlap$coords.x1, transect1_overlap$coords.x2)
transect1_overlap_sp <- SpatialPointsDataFrame(xy, transect1_overlap)

# flight 2
xy <- cbind(flight2_overlap$coords.x1,flight2_overlap$coords.x2)
flight2_overlap_sp <- SpatialPointsDataFrame(xy, flight2_overlap)
xy <- cbind(transect2_overlap$coords.x1, transect2_overlap$coords.x2)
transect2_overlap_sp <- SpatialPointsDataFrame(xy, transect2_overlap)

# save shapefiles for flights and transect lines
raster::shapefile(flight1_overlap_sp, filename = "C:\\Users\\keoghank\\Documents\\Test analyses\\Bootstrapping_test\\Shapefiles\\Test_day1\\Zone85_M10_S01_D01_19_Output-Day1-CentCount.shp", overwrite = TRUE)
raster::shapefile(transect1_overlap_sp, filename = "C:\\Users\\keoghank\\Documents\\Test analyses\\Bootstrapping_test\\Shapefiles\\Test_day1\\Zone85_M10_S01_D01_19_Output-Day1-Transect.shp", overwrite = TRUE)
raster::shapefile(flight2_overlap_sp, filename = "C:\\Users\\keoghank\\Documents\\Test analyses\\Bootstrapping_test\\Shapefiles\\Test_day1\\Zone85_M10_S01_D01_19_Output-Day2-CentCount.shp", overwrite = TRUE)
raster::shapefile(transect2_overlap_sp, filename = "C:\\Users\\keoghank\\Documents\\Test analyses\\Bootstrapping_test\\Shapefiles\\Test_day1\\Zone85_M10_S01_D01_19_Output-Day2-Transect.shp", overwrite = TRUE)

# Saving the shapefile has two issues, first of all Field names are abbreviated making things harder
# second of all the .prj file is missing 
testflight <- as.data.frame(raster::shapefile("C:\\Users\\keoghank\\Documents\\Test analyses\\Bootstrapping_test\\Shapefiles\\Test_day1\\Zone85_M10_S01_D01_19_Output-Day1-CentCount.shp"))



# Code below here figures out which transects overlap, but not automatically!

# convert to long format for data exploration and plotting
flight1_long <- melt(flight1, id.vars = c("transect", "coords.x1", "coords.x2"))
flight2_long <- melt(flight2, id.vars = c("transect", "coords.x1", "coords.x2"))

flight1_long$flight <- "F1"
flight2_long$flight <- "F2"

# Join two datasets together
flight1_long <- full_join(flight1_long, transect1)
flight2_long <- full_join(flight2_long, transect2)

# Find rows where transect starts and ends (2 for each)
# Will find the starting points that match between the two flights, and only use those transects in the bootstrapping
flight1_long$position_in_transect <- "middle" # create new column where we'll mark start and end of each transect
flight1_long$position_in_transect[match(unique(flight1_long$transect), flight1_long$transect)] <- "start"
#flight1_long$position_in_transect[match(unique(flight1_long$transect), flight1_long$transect)-1] <- "end"
#flight1_long$position_in_transect[dim(flight1_long)[1]] <- "end"

flight2_long$position_in_transect <- "middle" 
flight2_long$position_in_transect[match(unique(flight2_long$transect), flight2_long$transect)] <- "start"
#flight2_long$position_in_transect[match(unique(flight2_long$transect), flight2_long$transect)-1] <- "end"
#flight2_long$position_in_transect[dim(flight2_long)[1]] <- "end"


# merge datasets
flights <- rbind(flight1_long, flight2_long)
labels <- flights[which(flights$position_in_transect == "start"),]

# View datasets
ggplot(data = flights) +
  geom_point(aes(x = coords.x1, y = coords.x2, colour = flight)) +
  geom_text(data = labels, aes(x = coords.x1, y = coords.x2, label = transect, colour = flight), position = position_jitter())

# Can do this a better way than just eyeballing it, but for now, the transects that overlap are:
#F1 - 1, 3, 4, 5
#F2 - 5, 6, 8, 9
  
# assign transect numbers to each transect
#flights$transect <- NA # new column for transect numbers
#flights$transect[which(flights$Broad_Cate == "Start of Transect")] <- seq(1,length(which(flights$Broad_Cate == "Start of Transect")),1) # assign a unique number to each "Start of Transect" row.
#flights <- flights %>% tidyr::fill(transect) # fill the remaining rows with the corresponding transect number. Could only get the fill function to work using a pipe.
#flights$transect <- as.factor(flights$transect)


# view data
# split data by "transect" but not by "flight" yet. 
#ggplot(data = flights) +
#  geom_point(aes(x = coords.x1, y = coords.x2, colour = flight)) +
#  facet_wrap( ~ transect) # I just wanted to facet wrap the transects because plotting them all together showed up some odd structure to the points.
