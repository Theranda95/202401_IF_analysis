## Load Packages --------------------------------------
install.packages("readxl")
install.packages("dplyr")
library(readxl)
library(dplyr)
library(ggbeeswarm) #needed for graphing 
library(ggplot2) #needed for graphing 
library(ggpubr)
##-----------------------------------------------------
#This is draft 1 of my script to prepare data sheets for plotting of fibrillarin data. 
#1. Step 1: Importing fib volume spreadsheet and getting total puncta volume per nucleus  
#2. Step 2: Import the other two spreadsheets, processing and merging them together
#3. Step 3: Creating the number of nuclei for each x number of fib puncta
#4. Step 4: Creating new columns with identifiers from each image (Condition, Genotype, Image)
#5. Step 5: Creating averages and saving into a new spreadsheet 
#6. Step 6: 
#7. Step 7: Creating a function to do this for all of the spreadsheets 

#1.-----------------------------------------------------------------------------
setwd("/Users/therandajashari/Documents/experiments_2024/20240124_comb_seq_comparisons/240124_fibrillarin/2402_spreadsheets/240202_test")
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
  summarise( fib_tot_vol = sum(as.numeric(fib_vol, na.rm = TRUE)))
# Divide column 2 by column 1 to get the fib puncta average size per nucleus. 

#2.-----------------------------------------------------------------------------
data2 <- read.csv("240130_ko1_gr_seq_myotubes_04_cel_num_foci_Detailed.csv", skip = 3) #importing the spreadsheets 
head(data2)
#renaming column names to match the primary key for all three genotypes 
data2 <- data2 %>%
  rename(fib_count = Cell.Number.Of.Nuclei, nuc_id = ID)
data3 <- read.csv("240130_ko1_gr_seq_myotubes_04_volume_ratio_Detailed.csv", skip = 3)
head(data3)
data3 <- data3 %>%
  rename(fib_nuc_ratio = Cell.Nucleus.to.Cytoplasm.Volume.Ratio, nuc_id = ID)

#Joining data 2 to data 1 and creating a new spreadsheet data 12 
?left_join()
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

#make another spreadsheet that stores the number of nuclei with the same number
#of fib puncta 
count_puncta <- cleaned_data %>%
  count(fib_count, name = "count")
head(count_puncta) #fib_count = puncta number and count = nuclei # with x number of puncta

#4.-----------------------------------------------------------------------------

#Add columns with other identifiers for this image 
#condition = myotube_seq, genotype = ko1, image = 4 in repeat 
cleaned_data <- cleaned_data %>%
  mutate(condition = "myotube_seq", genotype = "ko1", image = "4")

?mutate()
#5.-----------------------------------------------------------------------------

#Create a spreadsheet with average numbers for each of the columns
#from cleaned_data spreadsheet 

averages <- cleaned_data %>%
  summarise(
    avg_fib_tot_vol = mean(fib_tot_vol, na.rm = TRUE),
    avg_fib_ave_vol = mean(fib_ave_vol, na.rm = TRUE),
    avg_fib_nuc_ratio = mean(fib_nuc_ratio, na.rm = TRUE)
  )
ave_result <- cleaned_data %>%
  select(condition, genotype, image, averages)










