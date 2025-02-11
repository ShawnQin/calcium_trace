# Statistical comparison between AD and Ctr
library(ggplot2)
library(readxl)
library("viridis") 
library("ggpubr")

# load the data
AD_file_path <- "./data/Filter_AD_Paranode.xlsx"
# Get sheet names

# define the function to read the data
read_data <- function(AD_file_path, target_column){
  sheets_AD <- excel_sheets(AD_file_path)
  
  volume <- numeric(0)
  for (sheet in 2:length(sheets_AD)){
    data <- read_excel(AD_file_path,sheet)
    num_row = nrow(data)
    # column names
    column_index <- which(colnames(data) == target_column)
    if (length(column_index) == 0) {
      cat("Column not found.\n")
    } 
    else {
      volume <- c(volume, data[2:num_row,column_index+1])
    }
  }
  volume <- unlist(volume)
  return(volume)
}

### READ TWO DATASETS
target_column <- "Volume (px³)"


# 
ctr_file_path <- "./data/Fitler_Ctrl_Paranode.xlsx"
ctr_data <- read_excel(file_path, sheet = "Fig7G_area continue")