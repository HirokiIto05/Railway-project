main <- function(){
  
  main_data <- load_main()
  age_data <- load_age()
  
  age_treatment <- integrate_id(main_data, age_data)
  
  
}

load_main <- function(){
  
  new_data <- readxl::read_xlsx(here::here('02.raw','adjust_local.xlsx'))
  
  return(new_data)
  
}

load_age <- function(){
  
  new_data <- read.csv(here::here('03.build','aggregate','data','age.csv'),
                       fileEncoding = "CP932") %>% 
      filter(gender == "計") %>% 
      group_by(city_id,year) %>% 
      mutate(young = sum(r0_4,r5_9,r10_14)) %>% 
      mutate(old = sum(r70_74,r75_79, r80_over)) %>% 
      ungroup()
  
  new_data <- new_data %>%
    ungroup() %>% 
    group_by(city_id) %>% 
    dplyr::mutate(young_percent = ((young-lag(young))/young)*100) 
  
  return(new_data)
}


integrate_id <- function(main_data, age_data){
  
  adjust_list <- distinct(main_data, city_id) %>% 
    na.omit()
  
  current_id <- dplyr::distinct(main_data, adjust_id) %>% 
    na.omit() %>% 
    unlist() %>% 
    as.character()
  
  old_new_number <- main_data %>% 
    dplyr::select(city_id, adjust_id,treatment_year) %>% 
    na.omit()
  
  current_city_name <- main_data %>% 
    dplyr::filter(city_id %in% current_id) %>% 
    dplyr::select(city_id, city_name, treatment_year)
  
  pop_old_data <- dplyr::left_join(adjust_list, age_data, by = "city_id")
  
  pop_new_data <- dplyr::left_join(pop_old_data, old_new_number, by = "city_id") %>% 
    dplyr::relocate(adjust_id, .after = city_id) %>% 
    dplyr::group_by(adjust_id, year)
  
  
  new_data <- dplyr::summarise(pop_new_data,
                               total = sum(total),
                               r0_4 = sum(r0_4),
                               r5_9 = sum(r5_9),
                               r10_14 = sum(r10_14),
                               r15_19 = sum(r15_19),
                               r20_24 = sum(r20_24),
                               r25_29 = sum(r25_29),
                               r30_34 = sum(r30_34),
                               r35_39 = sum(r35_39),
                               r40_44 = sum(r40_44),
                               r45_49 = sum(r45_49),
                               r50_54 = sum(r50_54),
                               r55_59 = sum(r55_59),
                               r60_64 = sum(r60_64),
                               r65_69 = sum(r65_69),
                               r70_74 = sum(r70_74),
                               r75_79 = sum(r75_79),
                               r80_over = sum(r80_over),
                               young = sum(young),
                               old = sum(old))
                               
  
  new_data <- new_data %>%
    group_by(adjust_id) %>% 
    dplyr::mutate(young_percent = ((young-lag(young))/young)*100) 
  
  output_data <- current_city_name %>% 
    left_join(new_data, by = c("city_id" = "adjust_id")) 
  
  output_data <- output_data %>% 
    filter(city_id != 45442) 
  
  return(output_data)
}

save_table <- function(integrated_treatment){
  
  folder_name <- here::here('03.build','age_analysis','data','age_treatment')
  file_name <- paste0(folder_name,'.csv')
  
  write.csv(age_treatment, file = file_name, fileEncoding = "CP932",
            row.names = FALSE)
  
  
}

save_table(age_treatment)




library(dplyr)
library(tidyr)
library(ggplot2)
