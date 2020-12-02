##########################################################
#####       Plane height and camera calibration      #####
### Kat Keogan - katharine.keogan@hidefsurveying.co.uk ###
##########################################################

# Always run this line first
rm(list = ls(all = TRUE)) # clears anything previously saved so you can start from scratch. 

# If you haven't used this before, you'll need to install the following packages by running this line of code
# install packages
install.packages(c("readxl", "tibble", "stringr", "writexl"))

# load all of these packages
library(readxl) # this will read excel files
library(tibble) # to add columns
library(stringr) # alter character strings
library(writexl) # write excel files

# READ ME ####
# The only line of code you will need to alter is line 23 called "file name"
# if you get an error in writing the excel file at the end, make sure that there isn't already an excel file of that name in the folder

file_name <- "Zone85_M10_S01_D01_C1_19.xlsx" # paste name of excel file inside the quotations e.g. "Zone85_M10_S01_D01_C1_19.xlsx"
this_survey <- paste("Survey Data/",file_name,sep = "")


# Step 1 ####
# Read in data from all sources
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


# Step 6 ####
# replace rows in main dataframe with the smaller dataframe that only included flying birds
dataframe[dataframe$Behaviour %in% dataframe_fly$Behaviour,] <- dataframe_fly


# Step 7 ####
# write new excel file to output folder
finished_survey_path <- stringr::str_replace(this_survey, "Survey Data", "Output Survey")
writexl::write_xlsx(dataframe, finished_survey_path)
