main <- function(){
  
  main_data <- road_main()
  pop_data <- road_pop()
  
  
  age_treatment <- integrate_id(main_data, pop_data)
  
  
}


colnames(ss)

road_main <- function(){
  
  new_data <- readxl::read_xlsx(here::here('02.raw','adjust_local.xlsx'))
  
  return(new_data)
  
}

road_pop <- function(){
  
  new_data <- read.csv(here::here('03.build','aggregate','data','age.csv'),
                       fileEncoding = "CP932") 
  
  
  # new_data$total <- as.numeric(new_data$total)
  
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
    dplyr::select(city_id, city_name, treatment_year)
  
  pop_old_data <- dplyr::left_join(adjust_list, pop_data, by = "city_id")
  
  pop_new_data <- dplyr::left_join(pop_old_data, old_new_number, by = "city_id") %>% 
    dplyr::relocate(adjust_id, .after = city_id) %>% 
    dplyr::group_by(adjust_id, year)
  
  
  new_data <- dplyr::summarise(pop_new_data, total = sum(total),
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
                               r80_over = sum(r80_over))
                               
                               
  # new_data <- new_data %>% 
  #   mutate(percentage = (social_increase/lag(pop))*100)
  
  final_data <- current_city_name %>% 
    left_join(new_data, by = c("city_id" = "adjust_id")) 
  
  final_data <- final_data %>% 
    filter(city_id != 45442) 
  
  return(final_data)
}

save_table <- function(integrated_treatment){
  
  folder_name <- here::here('03.build','age_data','data','treatment_data')
  file_name <- paste0(folder_name,'.csv')
  
  write.csv(integrated_treatment, file = file_name, fileEncoding = "CP932",
            row.names = FALSE)
  
  
}

save_table(integrated_treatment)



library(dplyr)
library(tidyr)
