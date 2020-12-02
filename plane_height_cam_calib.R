##########################################################
#####       Plane height and camera calibration      #####
### Kat Keogan - katharine.keogan@hidefsurveying.co.uk ###
##########################################################

# Read me ####
Instructions here
# To add a new comments column I've had to leave a space after comments ("Comments ") as there is already a column named "Comments". Just FYI @Grant
# Here tell the user which rows they'll need to choose their own file from/set the working directory etc. 


# Run from here:
rm(list = ls(all = TRUE)) # clears anything previously saved so you can start from scratch

# install packages
install.packages(c("readxl", "tibble", "stringr", "writexl"))

# packages to load
library(readxl) # this will read excel files
library(tibble) # to add columns
library(stringr)
library(writexl)


# Step 1 ####
# Read in data from all sources

this_survey <- "Survey Data/Zone85_M10_S01_D01_C1_19.xlsx"

dataframe <- readxl::read_excel(this_survey) 
calibration <- readxl::read_excel("GPS Search.xlsm", sheet = "Calibration")

# Step 2 ####
# Insert extra columns in main dataframe
dataframe <- tibble::add_column(dataframe, 
                        "Plane Height" = as.numeric(""),	
                        "Calibration" = as.numeric(""),	
                        "Frame 1" = "",	
                        "Frame 2" = "",	
                        "Frame 3" = "",	
                        "Frame 4" = "",	
                        "Frame 5" = "",	
                        "Frame 6" = "",	
                        "Frame 7" = "",	
                        "Frame 8" = "",	
                        "Reflection?" = "",	
                        "Comments " = "", .after = "Added Frame Number")

 
# Step 3 ####
# Import all the GPS files needed for this particular spreadsheet
GPS_list <- paste(levels(as.factor(dataframe$`Reel Name`)),"_gps.txt", sep = "") # list of GPS files used in this transect

# combine all the relevant .txt files into one dataframe, and insert a column with reel name to match with full dataset
flight_details <- Reduce(rbind, 
                         Map(cbind, lapply(paste("GPS data/",GPS_list, sep = ""), read.csv, header = FALSE), v8 = stringr::str_remove(GPS_list, "_gps.txt")))
                         

colnames(flight_details) <- c("Frame", 
                              "Lat", 
                              "Lon", 
                              "Plane Height", 
                              "Speed", 
                              "Date", 
                              "Time", 
                              "Reel Name") # adds column names


dataframe_fly <- dataframe[grepl("Flying", dataframe$Behaviour),] # filter by flying birds only


# Step 4 ####
# match the frame and reel name in main dataframe with the frame and reel name in the GPS dataframe. 
# Return Plane heights from GPS dataframe
dataframe_fly$`Plane Height` <- flight_details$`Plane Height`[
                                pmatch(paste(dataframe_fly$Frame, dataframe_fly$'Reel Name', sep = " "),
                                      paste(flight_details$Frame, flight_details$'Reel Name', sep = " "),
                                duplicates.ok = TRUE)] 
  

# Step 5 ####
# Populate the "calibration" column
heights <- pmatch((round(dataframe_fly$`Plane Height`/5)*5), calibration$'Aircraft\r\nHeight (m)', duplicates.ok = TRUE)
cameras <- paste("GSD C",dataframe_fly$Camera,"\r\n(cm)", sep = "")

for(i in 1:dim(dataframe_fly)[1]){
          dataframe_fly$Calibration[i] <-
              as.numeric(calibration[heights[[i]], cameras[[i]]])
                }


# replace rows in main dataframe with the smaller dataframe that only included flying birds
dataframe[dataframe$Behaviour %in% dataframe_fly$Behaviour,] <- dataframe_fly

finished_survey_path <- stringr::str_replace(this_survey, "Survey Data", "Output Survey")
writexl::write_xlsx(dataframe, finished_survey_path)
