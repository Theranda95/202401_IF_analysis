## Load Packages --------------------------------------
install.packages("readxl")
install.packages("dplyr")
library(readxl)
library(dplyr)
library(ggbeeswarm) #needed for graphing 
library(ggplot2) #needed for graphing 
library(ggpubr)
library(purrr) #needed for function map 
library(stringr) #needed for extracting names from a string 

##-----------------------------------------------------
#This is draft 1 of my script to prepare data sheets for plotting intensity data for h3k4me3 and myog.  
#1. Step 1: Importing h3k4me3 and myog intensity spreadsheet
#2. Step 2: Extracting two columns from each of the spreadsheets 
#3. Step 3: Creating new columns with identifiers from each image (Condition, Genotype, Image)
#4. Step 4: Merging H3k4me3 and Myog intensity spreadsheets 
#6. Step 6: Merging all by row into one large spreadsheet
#------------------test------------------------------
setwd("/Users/therandajashari/Documents/experiments_2024/20240124_comb_seq_comparisons/240124_H3K4Me_bl/test")
getwd()
data <- read.csv("240201_wt26_bl_myoblasts_01_h3k4_Detailed.csv", skip = 3) 
head(data)
data <- data %>% rename(h3k4me3_intensity = Intensity.Sum, nuc_id = ID) %>% #renaming columns
  mutate(nuc_id = as.integer(nuc_id))
datax <- read.csv("240201_wt26_bl_myoblasts_01_myog_Detailed.csv", skip = 3) #importing the spreadsheets 
head(datax)
datax <- datax %>% rename(myog_intensity = Intensity.Sum, nuc_id = ID) %>% #renaming columns
  mutate(nuc_id = as.integer(nuc_id))

data1x <- left_join(datax, data, by = "nuc_id")
head(data1x)
datax1 <- left_join(data, datax, by = "nuc_id")
head(datax1)

cleaned_data <- select(data1x, -2:-6,-8,-10:-15) #need the dplyr library
head(cleaned_data)
#--------------------------Function-----------------------------------------
process_folder <- function(folder_path) {
  setwd(folder_path)
  file_list <- list.files(pattern = "*.csv")
  data_list <- list()  # Create a list to store data frames
  
  for (file in file_list) { 
    if (grepl("h3k4",file, ignore.case = TRUE)) {
      # Import spreadsheet without the first two rows
      data1 <- read.csv(file, skip = 3) %>%
        rename(h3k4me3_intensity = Intensity.Sum, nuc_id = ID) %>% #renaming columns
        mutate(nuc_id = as.integer(nuc_id)) %>%
      mutate(
        condition = str_extract(file_list[1], "(?<=_)([a-zA-Z]+)(?=_h3k4_Detailed)"),  # Extracts text between underscores before "_h3k4_Detailed"
        genotype = str_extract(file_list[1], "(?<=_)([a-zA-Z0-9]+)(?=_bl)"),            # Extracts text between underscores before "_bl"
        image = str_extract(file_list[1], "(?<=_)([0-9]+)(?=_h3k4_Detailed)")           # Extracts text between underscores before "_h3k4_Detailed"
      )
      data_list[["h3k4"]] <- data1  # Store data frame in the list
      
    } else if (grepl("myog", file, ignore.case = TRUE)) {
      data2 <- read.csv(file, skip = 3) %>% 
        rename(myog_intensity = Intensity.Sum, nuc_id = ID) #renaming columns for the myog sheets 
      data_list[["myog"]] <- data2  # Store data frame in the list
      }
  }
  
  # Combine the processed data frames
  final_data <- left_join(data_list[["myog"]], data_list[["h3k4"]], by = "nuc_id") %>%
    select(-2:-6,-8,-10:-15) %>% 
  return(list(final_data = final_data))
}

#calling the function
folder_path <- "/Users/therandajashari/Documents/experiments_2024/20240124_comb_seq_comparisons/240124_H3K4Me_bl/2402_spreadsheets/specifics"
result <- process_folder(folder_path)




