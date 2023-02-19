main <- function(){
  
  long_lists <- make_long_lists()
  short_lists <- make_short_lists()
  
  year_list <- seq(1995,2019)
  
  int_pop_data <- lapply(year_list, aggregate_pop, short_lists, long_lists) %>% 
    dplyr::bind_rows() 
  
  
  # new_data <- load_data()
  
  age_data <- change_id(new_data)
  add_data <- add_working(age_data)
 
  save_table(add_data) 
 
}



make_long_lists <- function(){
  
  long_lists <- c('city_id','prefecture','city_name','gender','total',
                  'r0_4','r5_9','r10_14','r15_19','r20_24','r25_29','r30_34',
                  'r35_39','r40_44','r45_49','r50_54','r55_59','r60_64','r65_69',
                  'r70_74','r75_79','r80_84','r85_89','r90_94','r95_99','r100_over')
  
  return(long_lists)
  
}


make_short_lists <- function(){
  
  short_lists <- c('city_id','prefecture','city_name','gender','total',
                   'r0_4','r5_9','r10_14','r15_19','r20_24','r25_29','r30_34',
                   'r35_39','r40_44','r45_49','r50_54','r55_59','r60_64','r65_69',
                   'r70_74','r75_79','r80_over')
  
  return(short_lists)
}

base_year <- 2015

aggregate_pop <- function(base_year, short_lists, long_lists){
  
  if(base_year <= 2014){
    file_n <- paste0(base_year, '.xls') 
    file_name <- here::here('02.raw', 'age', file_n) 
    new_data <-  readxl::read_xls(file_name) 
    
    colnames(new_data) <- short_lists
    
    num_only <- new_data %>% 
      dplyr::select(-prefecture, -city_name, -gender)
    
    cha_only <- new_data %>% 
      dplyr::select(prefecture, city_name, gender)
    
    num_only <- num_only %>% 
      apply(2, as.numeric) 
    
    new_data <- bind_cols(cha_only, num_only)
    
  } else {
    file_n <- paste0(base_year, '.xls') 
    file_name <- here::here('02.raw', 'age', file_n) 
    new_data <-  readxl::read_xls(file_name) 
    
    colnames(new_data) <- long_lists
    
    num_only <- new_data %>% 
      dplyr::select(-prefecture, -city_name, -gender)
    
    cha_only <- new_data %>% 
      dplyr::select(prefecture, city_name, gender)
    
    num_only <- num_only %>% 
      apply(2, as.numeric) 
    
    new_data <- bind_cols(cha_only, num_only)
    
    new_data <- new_data %>% 
      dplyr::mutate(r80_over = dplyr::select(.,c(r80_84,r85_89,r90_94,r95_99,r100_over)) %>% 
                      rowSums(na.rm = TRUE)) %>% 
      dplyr::select(-r80_84, -r85_89, -r90_94, -r95_99, -r100_over)
    
  }
  
  new_data <- new_data %>% 
    dplyr::mutate(year = base_year) %>% 
    dplyr::relocate(year, .after = city_id)
  
  return(new_data)
  
}





five_func <- function(id){
  
  new_id <- id
  
  num <- nchar(new_id)
  
  new_id <- str_sub(new_id, start = 1, end = num -1)
  
  return(new_id)
}

change_id <- function(int_pop_data){
  
  new_id <- int_pop_data %>% 
  
  output_data <- int_pop_data %>% 
    dplyr::mutate(city_id = stringr::str_sub(int_pop_data$city_id, 
                     start = 1,
                     end = length(int_pop_data$city_id) - 1))
  

  
  return(new_data)
  
}


add_working <- function(data){
  
  output_data <- data %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(city_id, year) %>% 
    dplyr::mutate(working = sum(r20_24,r25_29,
                              r30_34,r35_39,
                              r40_44,r45_49,
                              r50_54,r55_59,
                              r60_64))  
    
  
  return(output_data)
  
}


save_table <- function(data){
  
  write.csv(data, file = here::here('03.build','aggregate','data','age.csv'),
            fileEncoding = "CP932", row.names = FALSE)
  
}




library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(rlang)
