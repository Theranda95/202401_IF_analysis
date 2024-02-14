## Load Packages --------------------------------------
install.packages("readxl")
install.packages("dplyr")
library(readxl)
library(dplyr)
library(ggbeeswarm) #needed for graphing 
library(ggplot2) #needed for graphing 
library(ggpubr)
library(stringr) #needed for extracting names from a string 
##-----------------------------------------------------
#This is draft 1 of my script to prepare data sheets for plotting of fibrillarin data. 
#1. Step 1: Importing fib volume spreadsheet and getting total puncta volume per nucleus  
#2. Step 2: Import the other two spreadsheets, processing and merging them together
#3. Step 3: Creating the number of nuclei for each x number of fib puncta
#4. Step 4: Creating new columns with identifiers from each image (Condition, Genotype, Image)

#5. Step 5: Creating a function to do this for all of the spreadsheets 

#6. Step 6: Creating averages and saving into a new spreadsheet - another function 


#1.-----------------------------------------------------------------------------
setwd("/Users/therandajashari/Documents/experiments_2024/20240124_comb_seq_comparisons/240124_fibrillarin/240202_test/ko1_seq_04")
getwd()
#Import spreadsheet without the first two rows 
data <- read.csv("240130_ko1_gr_seq_myotubes_04_fib_vol_Detailed.csv", skip = 3) #skip 3 -> skips three first rows of the spreadsheet that we don't need!   
head(data) # fourth row becomes the column names by default. header = F would remove row 1 as column name (which is default in R)
str(data)

#resetting the data frame index (do I need to?)
#rownames(data1) <- NULL 
#rownames(data1)

#Change column names for column 1 and 5 
data <- data %>%
  rename(fib_vol = Nucleus.Volume, nuc_id = CellID)
#change nuc_id to integer values 
data <- data %>%
  mutate(nuc_id = as.integer(nuc_id))
str(data)
# Sum the values in fib_vol column based on unique values in nuc_id column 
data1 <- data %>%
  group_by(nuc_id) %>%
  summarise(fib_tot_vol = sum(as.numeric (fib_vol, na.rm = TRUE)))
# Divide column 2 by column 1 to get the fib puncta average size per nucleus. 

#2.-----------------------------------------------------------------------------
data2 <- read.csv("240130_ko1_gr_seq_myotubes_04_nuc_num_foci_Detailed.csv", skip = 3) #importing the spreadsheets 
head(data2)
#renaming column names to match the primary key for all three genotypes 
data2 <- data2 %>%
  rename(fib_count = Cell.Number.Of.Nuclei, nuc_id = ID)
data3 <- read.csv("240130_ko1_gr_seq_myotubes_04_vol_ratio_Detailed.csv", skip = 3)
head(data3)
data3 <- data3 %>%
  rename(fib_nuc_ratio = Cell.Nucleus.to.Cytoplasm.Volume.Ratio, nuc_id = ID)

#Joining data 2 to data 1 and creating a new spreadsheet data 12 
data12 <- left_join(data2, data1, by = "nuc_id")
#Joining data 3 to data 12 and creating a new spreadsheet data 123
data123 <- left_join(data12, data3, by = "nuc_id")

#Cleaning up the data frame: removing and adding columns 
head(data123)
#removing columns by their position
cleaned_data <- select(data123, -2:-4,-6,-9:-12) #need the dplyr library 
head(cleaned_data)
#adding the average puncta volume by dividing column 3 by column 1 
cleaned_data <- cleaned_data %>%
  mutate(fib_ave_vol = fib_tot_vol / fib_count)
head(cleaned_data)

#3.-----------------------------------------------------------------------------

#Add columns with other identifiers for this image 
#condition = myotube_seq, genotype = ko1, image = 4 in repeat 
cleaned_data <- cleaned_data %>%
  mutate(condition = "myotube_seq", genotype = "ko1", image = "4")

?mutate()

#4.-----------------------------------------------------------------------------

#make another spreadsheet that stores the number of nuclei with the same number
#of fib puncta 
count_puncta <- cleaned_data %>%
  count(fib_count, name = "count")
head(count_puncta) #fib_count = puncta number and count = nuclei # with x number of puncta


###----------Make the code above into a function -------------------------------

# Data is separated into different folders and each folder has three .csv files with common 
# identifiers fib_vol, vol_ratio and nuc_num_foci. Each folder has three identifiers: genotype, 
# condition and image. 

#Turning the above steps into a function 
#Nested function 


#####Function trial 1 
process_folder <- function(folder_path) {
  setwd(folder_path)
  folder_name <- basename(folder_path)
  file_list <- list.files(pattern = "*.csv")
  
    for (file in file_list) { 
      if (grepl("fib_vol",file, ignore.case = TRUE)) {
      # Import spreadsheet without the first two rows
      data <- read.csv(file, skip = 3)
      
      # Rename columns and perform necessary operations
      data <- data %>%
        rename(fib_vol = Nucleus.Volume, nuc_id = CellID) %>%
        mutate(nuc_id = as.integer(nuc_id))
      
      data1 <- data %>%
        group_by(nuc_id) %>%
        summarise(fib_tot_vol = sum(as.numeric(fib_vol, na.rm = TRUE)))
    } else if (grepl("nuc_num_foci", file, ignore.case = TRUE)) {
      data2 <- read.csv(file, skip = 3) %>%
        rename(fib_count = Cell.Number.Of.Nuclei, nuc_id = ID)
    } else if (grepl("vol_ratio", file, ignore.case = TRUE)) {
      data3 <- read.csv(file, skip = 3) %>%
        rename(fib_nuc_ratio = Cell.Nucleus.to.Cytoplasm.Volume.Ratio, nuc_id = ID)
    }
  }
  
  # Combine the processed data frames
  final_data <- left_join(data2, data1, by = "nuc_id") %>%
    left_join(data3, by = "nuc_id") %>%
    select(-2:-4, -6, -9:-12) %>%
    mutate(fib_ave_vol = fib_tot_vol / fib_count) %>%
    mutate(
      condition = str_extract(folder_name, "(?<=_)[^_]+"),  # Extracts text after the first underscore
      genotype = str_extract(folder_name, "^[^_]+"),          # Extracts text before the first underscore
      image = str_extract(folder_name, "(?<=_)[^_]+$")        # Extracts text after the last underscore
    )
  
  # Create a spreadsheet that stores the number of nuclei with the same number of fib puncta 
  count_puncta <- final_data %>%
    group_by(condition, genotype, image, fib_count) %>%  # Group by these variables
    count(name = "count")  # Include the folder name in the output
  
  #count_puncta <- final_data %>%
    #count(folder_name, fib_count, name = "count")  # Include the folder name in the output
  
  # Create a spreadsheet with average numbers for each of the columns from cleaned_data spreadsheet 
  #Need to fix the code
  return(list(final_data = final_data, count_puncta = count_puncta))
}

#####Function trial 2

#process_files <- function(folder_path, fib_vol_pattern, nuc_num_foci_pattern, vol_ratio_pattern) {
  # Read the first CSV file
  fib_vol_file <- list.files(folder_path, pattern = fib_vol_pattern, full.names = TRUE)
  data1 <- read.csv(fib_vol_file, skip = 3) %>%
    rename(fib_vol = Nucleus.Volume, nuc_id = CellID) %>%
    mutate(nuc_id = as.integer(ifelse(is.na(nuc_id), 0, nuc_id))) %>%  # Handling missing values
    group_by(nuc_id) %>%
    summarise(fib_tot_vol = sum(as.numeric(fib_vol, na.rm = TRUE)))
  
  # Read the second CSV file
  nuc_num_foci_file <- list.files(folder_path, pattern = nuc_num_foci_pattern, full.names = TRUE)
  data2 <- read.csv(nuc_num_foci_file, skip = 3) %>%
    rename(fib_count = Cell.Number.Of.Nuclei, nuc_id = ID)
  
  # Read the third CSV file
  volume_ratio_file <- list.files(folder_path, pattern = volume_ratio_pattern, full.names = TRUE)
  data3 <- read.csv(volume_ratio_file, skip = 3) %>%
    rename(fib_nuc_ratio = Cell.Nucleus.to.Cytoplasm.Volume.Ratio, nuc_id = ID)
  
  # Join data2 to data1
  data12 <- left_join(data2, data1, by = "nuc_id")
  
  # Join data3 to data12
  data123 <- left_join(data12, data3, by = "nuc_id")
  
  # Clean up the data frame
  cleaned_data <- data123 %>%
    select(-2:-4, -6, -9:-12) %>%
    mutate(fib_ave_vol = fib_tot_vol / fib_count)
  
  return(cleaned_data)
}

##Calling the function for a single folder 
#result_single_folder <- process_files(single_folder_path, "fib_vol", "cel_num_foci", "volume_ratio")

# Specify the main folder containing subfolders with CSV files
main_folder <- "/Users/therandajashari/Documents/experiments_2024/20240124_comb_seq_comparisons/240124_fibrillarin/2402_spreadsheets/240206_wt26_gr"

# Get a list of subfolders
subfolders <- list.dirs(main_folder, recursive = FALSE)

# Process each subfolder
# Calling the function process_folder
result_list <- lapply(subfolders, process_folder)

# Combine the results from all subfolders
final_results <- bind_rows(result_list)

final_ave_results <- bind_rows(lapply(result_list, function(result) result$ave_result))
final_count_puncta <- bind_rows(lapply(result_list, function(result) result$count_puncta))



