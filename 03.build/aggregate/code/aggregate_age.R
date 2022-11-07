main <- function(){
  
 new_data <- read_data()
 
 age_data <- change_id(new_data)

 save_table(final_data) 
 
}


read_data <- function(){
  
  new_data <- read.csv(here::here('02.raw','age','age_data.csv'),
                       fileEncoding = "CP932")
  
  return(new_data)
}

five_func <- function(id){
  
  new_id <- id
  
  num <- nchar(new_id)
  
  new_id <- str_sub(new_id, start = 1, end = num -1)
  
  return(new_id)
}

change_id <- function(new_data){
  
  city_id_only <- new_data %>% 
    select(city_id)
  
  city_id_list <- city_id_only %>% 
    unlist() %>%
    as.character()
  
  new_id_list <- lapply(city_id_list, five_func) %>% 
    unlist()
  
  new_data <- new_data %>% 
    mutate(city_id = new_id_list)
  
  return(new_data)
  
}



save_table <- function(new_city_data){
  
  write.csv(age_data, file = here::here('03.build','aggregate','data','age.csv'),
            fileEncoding = "CP932", row.names = FALSE)
  
  
}

class(cc_data$city_id)



library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(rlang)
library(multiplex)
# install.packages("multiplex")
library(data.table)


