main <- function(){
  
  main_data <- load_main()
  pop_data <- load_pop()
  
  integrated_control <- integrate_id(main_data, pop_data)
  
  save_table(integrated_control)
  
}


load_main <- function(passenger_num){
  
  new_data <- readxl::read_xlsx(here::here('02.raw','continue.xlsx')) %>% 
    dplyr::filter(passenger %in% c("1","2","3","4")) 
  
  return(new_data)
  
}



load_pop <- function(){
  
  new_data <- read.csv(here::here('03.build','aggregate','data','pop_all.csv'),
                       fileEncoding = "CP932", colClasses = "character") %>% 
    dplyr::mutate(across(.cols = -c(city_id, region_name, city_name), ~ as.numeric(.x)))
  
  new_data$total <- as.numeric(new_data$total)
  
  return(new_data)
}


integrate_id <- function(main_data, pop_data){
  
  adjust_list <- distinct(main_data, city_id) %>% 
    na.omit()
  
  current_id <- dplyr::distinct(main_data, adjust_id) %>% 
    na.omit() %>% 
    unlist() %>% 
    as.character()
  
  old_new_number <- main_data %>% 
    dplyr::select(city_id, adjust_id) %>% 
    na.omit()
  
  current_city_name <- main_data %>% 
    dplyr::filter(city_id %in% current_id) %>% 
    dplyr::select(city_id, city_name)
  
  pop_old_data <- dplyr::left_join(adjust_list, pop_data, by = "city_id")
  
  pop_new_data <- dplyr::left_join(pop_old_data, old_new_number, by = "city_id") %>% 
    dplyr::relocate(adjust_id, .after = city_id) %>% 
    dplyr::group_by(adjust_id, year)
    
  
  new_data <- dplyr::summarise(pop_new_data, pop = sum(total),
                               male = sum(male),
                               female = sum(female),
                               natural_increase = sum(natural_increase),
                               social_increase = sum(social_increase))
  
  new_data <- new_data %>% 
    mutate(percentage = (social_increase/lag(pop))*100)
  
  final_data <- current_city_name %>% 
    left_join(new_data, by = c("city_id" = "adjust_id")) 
  
  final_data <- final_data %>% 
    dplyr::mutate(treatment_year = 0, .after = city_name)
  
  
  
  return(final_data)
}


save_table <- function(integrated_control){
  
  folder_name <- here::here('03.build','integrate','data','control_data')
  file_name <- paste0(folder_name,'.csv')
  
  write.csv(integrated_control, file = file_name, fileEncoding = "CP932",
            row.names = FALSE)
  
  
}

# save_table(integrated_control)


library(dplyr)
library(tidyr)
