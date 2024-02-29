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
#Data is first organized in different folders for each image (total of 26 folders) with each folder containing 
#specific values for Hp1a and Hoechst Sum Intensity - values for each nuclei. 
#1. Step 1: Importing hp1a and hoechst intensity spreadsheet
#2. Step 2: Extracting two columns from each of the spreadsheets(sum intensity column and nuclear id)
#3. Step 3: Merging Hp1a and Hoechst intensity spreadsheets 
#4. Step 4: Creating new columns with identifiers from each image Condition, Genotype, Image)
#6. Step 6: Merging all by row into one large spreadsheet (outside of the function)
#7. Step 7: Getting the averages for each image (outside of the function)
#--------------------------Function-----------------------------------------
process_folder <- function(folder_path) { 
  setwd(folder_path)
  folder_name <- basename(folder_path)
  file_list <- list.files(pattern = "*.csv")
  
  for (file in file_list) { 
    if (grepl("_hp1a_",file, ignore.case = TRUE)) {
      # Import spreadsheet and rename column 
      data1 <- read.csv(file, skip = 3) %>%
        rename(hp1a_int = Intensity.Sum, nuc_id = ID) %>% #renaming columns
        mutate(nuc_id = as.integer(nuc_id))
      
    } else if (grepl("_dapi_", file, ignore.case = TRUE)) {
      data2 <- read.csv(file, skip = 3) %>%
        rename(hoechst_int = Intensity.Sum, nuc_id = ID)
    }
  }
  # Combine the processed data frames
  final_data <- left_join(data2, data1, by = "nuc_id") %>%
    select(1,7,9) %>%
    mutate(norm_hp1a_int = hp1a_int / hoechst_int) %>%
    mutate(norm_hp1a_subtr_int = hp1a_int - hoechst_int) %>%
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
  folder_path <- "/Users/therandajashari/Documents/experiments_2024/20240124_comb_seq_comparisons/240124_hp1alpha/240225_spreadsheets/specifics/sum"
  
  # Get a list of subfolders
  subfolders <- list.dirs(folder_path, recursive = FALSE)
  
  # Calling the function process_folder
  result_list <- lapply(subfolders, process_folder)
  
  # Combine final_data data frames from all subfolders
  all_data_grouped <- bind_rows(result_list %>% map("final_data"))
  
  # Filter the data so that all the values in myog_int column to be higher than 500 
  filtered_data <- all_data_grouped %>%
    filter(hp1a_int > 300)
  
  # Save the master spreadsheets
  write.csv(all_data_grouped, file = "240228_all_sum_data.csv", row.names = FALSE)
  write.csv(filtered_data, file = "240228_all_sum_data_filtered.csv", row.names = FALSE)
  
  # Create and save average spreadsheets for each column - not really what this is doing 
  ##Need to check and understand it better! 
  average_data <- filtered_data %>%
    group_by(condition, genotype, image) %>%
    summarise(across(ends_with("_int"), mean, na.rm = TRUE)) #for all columns that end with _int (in this case I have named them all the same)
  
  # Save the master average spreadsheet
  write.csv(average_data, file = "240228_all_sum_average_data.csv", row.names = FALSE)
  