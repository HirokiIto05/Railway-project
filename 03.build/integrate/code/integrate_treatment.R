main <- function(){
  
  main_data <- load_main()
  pop_data <- load_pop()
  
  # pop_data <- change_id(pop_data)
  
  integrated_treatment <- integrate_id(main_data, pop_data)
  
  
}


load_main <- function(){
  
  new_data <- readxl::read_xlsx(here::here('02.raw','adjust_local.xlsx'))
  
  return(new_data)
  
}

load_pop <- function(){
  
  new_data <- read.csv(here::here('03.build','aggregate','data','pop_all.csv'),
                       fileEncoding = "CP932") 
  
  
  new_data$city_id <- as.numeric(new_data$city_id)
  new_data$total <- as.numeric(new_data$total)
  
  return(new_data)
}

# five_func <- function(id){
#   
#   new_id <- id
#   
#   num <- nchar(new_id)
#   
#   new_id <- str_sub(new_id, start = 1, end = num -1)
#   
#   return(new_id)
# }
# 
# 
# change_id <- function(pop_data){
#   
#   city_id_only <- pop_data %>% 
#     select(city_id)
#   
#   city_id_list <- city_id_only %>% 
#     unlist() %>%
#     as.character()
#   
#   new_id_list <- lapply(city_id_list, five_func) %>% 
#     unlist()
#   
#   new_data <- pop_data %>% 
#     mutate(city_id = new_id_list)
#   
#   return(new_data)
#   
# }


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
    dplyr::select(city_id, city_name, treatment_year)
  
  adjust_list$city_id <- as.numeric(adjust_list$city_id)
  pop_data$city_id <- as.numeric(pop_data$city_id)
  
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
    filter(city_id != 45442) 
  
  return(final_data)
}

save_table <- function(integrated_treatment){
  
  folder_name <- here::here('03.build','integrate','data','treatment_data')
  file_name <- paste0(folder_name,'.csv')
  
  write.csv(integrated_treatment, file = file_name, fileEncoding = "CP932",
            row.names = FALSE)
  
  
}

save_table(integrated_treatment)




library(dplyr)
library(tidyr)
