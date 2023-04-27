library(dplyr)

main <- function(){
  
  age_data <- load_data("city_adjust", "age_data.csv") %>% 
    dplyr::mutate(city_id = as.character(city_id)) 
  
  pop_data <- load_data("city_adjust", "pop_data.csv") %>% 
    dplyr::select(city_id, household, year) %>% 
    dplyr::mutate(city_id = as.character(city_id)) 
  
  main_data <- readxl::read_xlsx(here::here('02.raw','main','main_data.xlsx')) %>% 
    dplyr::select(-old_id, -old_name)

  #トリートメント群としてスピルオーバーなどの観点から不適格な地域を除外している。  
  treatment_data <- treatment_only(main_data, age_data) %>% 
    dplyr::filter(city_name != "揖斐郡大野町",
                  city_name != "本巣郡北方町",
                  city_name != "珠洲市",
                  city_name != "能登町",
                  city_name != "鳳珠郡能登町",
                  city_name != "十和田市",
                  city_name != "行方市") %>% 
    dplyr::left_join(pop_data, by = c("year", "city_id")) %>% 
    dplyr::relocate(household, .after = total) %>% 
    dplyr::ungroup()
  
  control_data <-  control_only(main_data, age_data) %>% 
    dplyr::left_join(pop_data, by = c("year", "city_id")) %>% 
    dplyr::relocate(household, .after = total) %>% 
    dplyr::ungroup()
  
  save_table(treatment_data, "treatment_data.csv")
  save_table(control_data, "control_data.csv")
  
}

load_data <- function(folder_name, file_name){
  
  output_data <- read.csv(here::here('03.build',folder_name, 'data', file_name),
                          fileEncoding = "CP932")
  return(output_data)
  
}

add_middle_age <- function(age_data){
  
  output_data <- age_data %>% 
    dplyr::group_by(city_id,year) %>% 
    dplyr::mutate(middle = sum(r20_24,r25_29,
                               r30_34,r35_39,
                               r40_44,r45_49,
                               r50_54,r55_59,
                               r60_64)) %>% 
    dplyr::ungroup()
  return(output_data)
  
}


treatment_only <- function(main_data, age_data){
  
  treatment_list <- main_data %>% 
    dplyr::filter(dummy == 1) %>% 
    dplyr::distinct(city_id) %>% 
    unlist() %>% 
    as.character()
  
  pop_treatment <- age_data %>% 
    dplyr::filter(city_id %in% treatment_list)
  
  main_data <- main_data %>% 
    dplyr::distinct(city_id, .keep_all = TRUE) %>% 
    dplyr::select(-city_name)
  
  output_data <- dplyr::left_join(pop_treatment, main_data, by = c("city_id")) 
  
  output_data <- output_data %>% 
    add_middle_age()
  
  return(output_data)
  
}


control_only <- function(main_data, age_data){
  
  control_list<- main_data %>% 
    dplyr::filter(dummy == 0) %>% 
    dplyr::distinct(city_id) %>% 
    unlist() %>% 
    as.character()
  
  pop_control <- age_data %>% 
    dplyr::filter(city_id %in% control_list)
  
  sort(control_list)
  
  main_data <- main_data %>% 
    dplyr::distinct(city_id, .keep_all = TRUE) %>% 
    dplyr::select(-city_name)
  
  output_data <- dplyr::left_join(pop_control, main_data, by = c("city_id"))
  
  output_data <- output_data %>% 
    add_middle_age()
  
  return(output_data)
  
}


save_table <- function(data, file_name){
  
  write.csv(data, file = here::here('03.build','master_data', 'data', file_name),
            fileEncoding = "CP932", row.names = FALSE)
  
}

