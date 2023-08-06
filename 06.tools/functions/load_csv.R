read_df_csv <- function(folder_name_n, file_name_n){
  
  file_name <- paste0(file_name_n, ".csv")
  
  file_path <- here::here('03.build', 
                          folder_name_n, 
                          'data', 
                          file_name)
  
  output_df <- read.csv(file_path, fileEncoding = "CP932")
  
  return(output_df)
}


save_df_csv <- function(input_df, folder_name_n, file_name_n){
  
  file_name <- paste0(file_name_n, ".csv")
  
  file_path <- here::here('03.build', 
                          folder_name_n, 
                          'data', 
                          file_name)
  
  output_df <- write.csv(input_df,
                         file = file_path, 
                         fileEncoding = "CP932",
                         row.names = FALSE)
}


read_df_xlsx <- function(folder_name_n, file_name_n) {
  
  file_name <- paste0(file_name_n, ".xlsx")
  
  file_path <- here::here('03.build', 
                          folder_name_n, 
                          'data', 
                          file_name)
  
  output_df <- readxl::read_xlsx(file_path)
  
  return(output_df)
}


save_df_xlsx <- function(input_df, folder_name_n, file_name_n){
  
  file_name <- paste0(file_name_n, ".xlsx")
  
  file_path <- here::here('03.build', 
                          folder_name_n, 
                          'data', 
                          file_name)
  
  openxlsx::write.xlsx(input_df, file = file_path)
  
}


library(readxl)
library(openxlsx)
library(dplyr)
library(Synth)
library(tidysynth)
library(ggplot2)
library(stringr)
library(stringi)
library(patchwork)
