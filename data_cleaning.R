# Ideally this would be replaced with creating a `.Rproject` file 
# used in conjunction with the `here` package 
# setwd('/some/location/on/your/computer')

# Created By: Raul Eulogio


# Purpose -----------------------------------------------------------------

# This script was created to clean xlsx file and return tidy csv files. 
# Serves as a run only once script, where loading csv files into working
# environment is recommended path since running/sourcing this script everytime 
# to load data would take a while. 

# Load Packages -----------------------------------------------------------

# If needed this would handle downloading all dependencies (except `here` package)
# install.packages("tidyverse")
library(magrittr)
library(here)
here::here()

# Other Dependencies (From tidyverse):
# dplyr
# purrr
# readxl
# lubridate
# tidyr

# Get file path
file_path <- here::here("hackathon_20180621.xlsx") 

# Load Data ---------------------------------------------------------------
read_sheets_max <- function(x, path_name){
  # Custom function which includes the set parameter `guess_max` to be 2000
  # Utilized due to columns being read in as logical data types incorrectly
  excel_sheet <- readxl::read_xlsx(x, path = path_name, guess_max = 2000)
}

# Load data into a list of dataframes
my_data <- file_path %>%
  readxl::excel_sheets() %>%
  purrr::set_names() %>%
  purrr::map(read_sheets_max, path = file_path)


# Remove White Space from Column Names ------------------------------------
remove_ws_col <- function(data_frame){
  # Custom function used to remove white space from column names
  names(data_frame) <- gsub(" ", "", names(data_frame))
  data_frame
}

# Map function across list of tibbles
my_data <- my_data %>%
  purrr::map(remove_ws_col)

# Lower Case Character Columns --------------------------------------------
lower_case_str <- function(data_frame){
  data_frame %>%
    # Lower caps everything if column is character type
    dplyr::mutate_if(is.character, tolower)  
}

# Map function across list of tibbles
my_data <- my_data %>%
  purrr::map(lower_case_str)

# Time Column Conversion --------------------------------------------------
time_col_conversion <- function(data_frame){
  # Custom function that searches columns which include "Time" (based on 
  # structure of data, all columns with time are time formatted). 
  my_vals <- colnames(data_frame) %>%
    stringr::str_detect("Time")
  my_names <- colnames(data_frame[my_vals])
  
  data_frame %>%
    # Convert to time as character 
    dplyr::mutate_if(names(.) %in% my_names, 
                     dplyr::funs(as.character(strftime(., format = "%H:%M:%S", tz = 'UTC'))))
}

# Map function across list of tibbles
my_data <- my_data %>% 
  purrr::map(~ time_col_conversion(.x))

# Date Column Conversion --------------------------------------------------
date_col_conversion <- function(data_frame){
  # Custom function that searches columns which include "Date" (based on 
  # structure of data, all columns with date are date formatted). 
  my_vals <- colnames(data_frame) %>%
    stringr::str_detect("Date")
  my_names <- colnames(data_frame[my_vals])
  
  data_frame %>%
    # Convert to date
    dplyr::mutate_if(names(.) %in% my_names, 
                     dplyr::funs(lubridate::as_date(.)))
}

# Map function across list of tibbles
my_data <- my_data %>% 
  purrr::map(~ date_col_conversion(.x))

# Manual Data Cleaning
# Chemistry ---------------------------------------------------------------
my_data[["Chemistry"]] <- my_data[["Chemistry"]] %>%
  # Remove character string representing NA
  dplyr::mutate(TeamNumber = replace(TeamNumber, TeamNumber == 'n/a', NA)) 

# Bacteria ----------------------------------------------------------------

# Data Cleaning 
my_data[["Bacteria"]] <- my_data[["Bacteria"]] %>%
  # Replace misspelling of word
  dplyr::mutate(ParameterCode = replace(ParameterCode, ParameterCode == "e. coil", "e. coli")) 


# Nutrients ---------------------------------------------------------------

# No manual cleaning required but if you wanted to combine date and time columns 
# Here's how you would do it:

# my_data[["Nutrients"]] %>%
  # tidyr::unite(col = "CollectionDatetime", c("SampleDate", "SampleTime"), sep = " ")
  # OR: dplyr::mutate(SampleCol = paste(SampleDate, SampleTime))

# Site List ---------------------------------------------------------------

# Data Cleaning 
my_data[["SiteList"]] <- my_data[["SiteList"]] %>%
  # Separate Column containing Lat and Long to two columns
  tidyr::separate(`GeographicCoordinates(DecimalDegrees)`, c("Longitude", "Latitude"), sep = ",")


# Output CSV Files --------------------------------------------------------

# Create array containing output file name and path
csv_names <- sprintf("data/%s.csv", names(my_data))

# Here we're checking if any of the previously created csv files exist in the
# data directory. If not then the for-loop will run and output csv files. 
if (any(!file.exists(csv_names))) {
  # for-loop to create new csv files with tidy data
  for (i in 1:length(my_data)){
    my_data[[i]] %>%
      readr::write_csv(path = csv_names[[i]])
  }
  print("Done")
} else {
  print("CSV files have already been created!")
  print("To load data into working environment load CSV files directly using read_csv/read.csv.")
}


