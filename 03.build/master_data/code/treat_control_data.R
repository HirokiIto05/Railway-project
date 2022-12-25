main <- function(){
  
  pop_data <- load_data("city_adjust","all_data.csv") %>% 
    dplyr::mutate(city_id = as.character(city_id)) 
  
  main_data <- readxl::read_xlsx(here::here('02.raw','main','main_data.xlsx')) %>% 
    dplyr::select(-old_id, -old_name)
  
  treatment_data <- treatment_only(main_data, pop_data) 
  control_data <-  control_only(main_data, pop_data) 
  
}

load_data <- function(folder_name, file_name){
  
  output_data <- read.csv(here::here('03.build',folder_name,'data', file_name),
                          fileEncoding = "CP932")
  return(output_data)
  
}

add_middle_age <- function(pop_data){
  
  output_data <- pop_data %>% 
    dplyr::group_by(city_id,year) %>% 
    dplyr::mutate(middle = sum(r20_24,r25_29,
                               r30_34,r35_39,
                               r40_44,r45_49,
                               r50_54,r55_59,
                               r60_64)) %>% 
    dplyr::ungroup()
  return(output_data)
  
}


treatment_only <- function(main_data, pop_data){
  
  treatment_list <- main_data %>% 
    dplyr::filter(dummy == 1) %>% 
    dplyr::distinct(city_id) %>% 
    unlist() %>% 
    as.character()
  
  pop_treatment <- pop_data %>% 
    dplyr::filter(city_id %in% treatment_list)
  
  main_data <- main_data %>% 
    dplyr::distinct(city_id, .keep_all = TRUE) %>% 
    dplyr::select(-city_name)
  
  output_data <- dplyr::left_join(pop_treatment, main_data, by = c("city_id")) 
  
  output_data <- output_data %>% 
    add_middle_age()
  
  file_name <- paste0(here::here('03.build','master_data', 'data', 'treatment_data.csv'))
  
  write.csv(output_data, file = file_name, fileEncoding = "CP932",
            row.names = FALSE)
  
  return(output_data)
  
}


control_only <- function(main_data, pop_data){
  
  control_list<- main_data %>% 
    dplyr::filter(dummy == 0) %>% 
    dplyr::distinct(city_id) %>% 
    unlist() %>% 
    as.character()
  
  pop_control <- pop_data %>% 
    dplyr::filter(city_id %in% control_list)
  
  sort(control_list)
  
  main_data <- main_data %>% 
    dplyr::distinct(city_id, .keep_all = TRUE) %>% 
    dplyr::select(-city_name)
  
  output_data <- dplyr::left_join(pop_control, main_data, by = c("city_id"))
  
  output_data <- output_data %>% 
    add_middle_age()
  
  file_name <- paste0(here::here('03.build','master_data', 'data', 'control_data.csv'))
  
  write.csv(output_data, file = file_name, fileEncoding = "CP932",
            row.names = FALSE)
  
  return(output_data)
  
}

