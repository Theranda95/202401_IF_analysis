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
getwd()
setwd("/Users/therandajashari/Documents/experiments_2024/20240124_comb_seq_comparisons/240124_H3K4Me_bl/2402_spreadsheets/test")

data1 <- read.csv("240208_ko1_bl_comb_myotubes_01_h3k4_Detailed.csv", skip = 3) 
head(data1)

data1 <- data1 %>% rename(h3k4me3_intensity = Intensity.Sum, nuc_id = ID) %>% #renaming columns
  mutate(nuc_id = as.integer(nuc_id))

data2 <- read.csv("240208_ko1_bl_comb_myotubes_01_myog_Detailed.csv", skip = 3) #importing the spreadsheets 
head(data2)
data2 <- data2 %>% rename(myog_intensity = Intensity.Sum, nuc_id = ID) %>% #renaming columns
  mutate(nuc_id = as.integer(nuc_id))

data3 <- read.csv("240208_ko1_bl_comb_myotubes_01_dapi_Detailed.csv", skip = 3) #importing the spreadsheets 
head(data3)
data3 <- data3 %>% rename(dapi_intensity = Intensity.Sum, nuc_id = ID) %>% #renaming columns
  mutate(nuc_id = as.integer(nuc_id))


data12 <- left_join(data1, data2, by = "nuc_id")
head(data12)
data123 <- left_join(data12, data3, by = "nuc_id")
head(data123)

cleaned_data <- select(data123, 1,7,9,16) #need the dplyr library
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
} #trying to process spreadsheets without divind them in folders 

process_folder <- function(folder_path) { 
  setwd(folder_path)
  folder_name <- basename(folder_path)
  file_list <- list.files(pattern = "*.csv")
  
  for (file in file_list) { 
    if (grepl("_h3k4_",file, ignore.case = TRUE)) {
      # Import spreadsheet and rename column 
      data1 <- read.csv(file, skip = 3) %>%
      rename(h3k4me3_int = Intensity.Sum, nuc_id = ID) %>% #renaming columns
        mutate(nuc_id = as.integer(nuc_id))
      
    } else if (grepl("_myog_", file, ignore.case = TRUE)) {
      data2 <- read.csv(file, skip = 3) %>%
        rename(myog_int = Intensity.Sum, nuc_id = ID)
    } else if (grepl("_dapi_", file, ignore.case = TRUE)) {
      data3 <- read.csv(file, skip = 3) %>%
        rename(dapi_int = Intensity.Sum, nuc_id = ID)
    }
  }
  # Combine the processed data frames
  final_data <- left_join(data2, data1, by = "nuc_id") %>%
    left_join(data3, by = "nuc_id") %>%
    select(1,7,9,16) %>%
    mutate(norm_h3k4me_int = h3k4me3_int / dapi_int) %>%
    mutate(norm_h3k4me_subtr_int = h3k4me3_int - dapi_int) %>%
    mutate(
      condition = str_extract(folder_name, "(?<=_)[^_]+"),  # Extracts text after the first underscore
      genotype = str_extract(folder_name, "^[^_]+"),          # Extracts text before the first underscore
      image = str_extract(folder_name, "(?<=_)[^_]+$")        # Extracts text after the last underscore
    )
  
  # Create a spreadsheet with average numbers for each of the columns from cleaned_data spreadsheet 
  #Need to fix the code
  return(list(final_data = final_data))
}

#calling the function
folder_path <- "/Users/therandajashari/Documents/experiments_2024/20240124_comb_seq_comparisons/240124_H3K4Me_bl/2402_spreadsheets/myotubes"

# Get a list of subfolders
subfolders <- list.dirs(folder_path, recursive = FALSE)

# Calling the function process_folder
result_list <- lapply(subfolders, process_folder)

# Combine final_data data frames from all subfolders
all_data_grouped <- bind_rows(result_list %>% map("final_data"))

# Filter the data so that all the values in myog_int column to be higher than 500 
filtered_data <- all_data_grouped %>%
  filter(myog_int > 300)

# Save the master spreadsheets
write.csv(all_data_grouped, file = "all_data_grouped.csv", row.names = FALSE)
write.csv(filtered_data, file = "all_data_filtered.csv", row.names = FALSE)

# Create and save average spreadsheets for each column - not really what this is doing 
##Need to check and understand it better! 
average_data <- filtered_data %>%
  group_by(condition, genotype, image) %>%
  summarise(across(ends_with("_int"), mean, na.rm = TRUE)) #for all columns that end with _int (in this case I have named them all the same)

#for (subfolder in subfolders) {
# folder_name <- basename(subfolder)
#  subfolder_average_data <- average_data %>%
#    filter(condition == str_extract(folder_name, "(?<=_)[^_]+"),
#           genotype == str_extract(folder_name, "^[^_]+"),
#           image == str_extract(folder_name, "(?<=_)[^_]+$"))
#  write.csv(subfolder_average_data, file = paste0(subfolder, "/average_data.csv"), row.names = FALSE)
#}

# Combine average data frames from all subfolders
all_average_data <- bind_rows(result_list %>% map("average_data"))

# Save the master average spreadsheet
write.csv(average_data, file = "all_average_data.csv", row.names = FALSE)
