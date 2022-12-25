main <- function(){
  
  year_list <- seq(1995,2015,by= 5)
  
  all_working_data <- purrr::map(year_list, read_working_csv) %>% 
    dplyr::bind_rows()
  
  all_working_data <- all_working_data %>% 
    dplyr::mutate(workforce_pop = as.numeric(sub(",", "", workforce_pop)),
                  working_pop = as.numeric(sub(",", "", working_pop)),
                  student_pop = as.numeric(sub(",", "", student_pop))
    )
  
}


read_working_csv <- function(year_n){
  
  file_name <- paste0(year_n, ".csv")
  
  working_data <- read.csv(here::here('02.raw', 'covariates', 'working', file_name),
                             fileEncoding = "CP932") 
  
  if(year_n == 1995){
    
    output_data <- working_data %>% 
      dplyr::select(6, 9, 10, 12, 13, 14) %>% 
      dplyr::mutate(year = year_n) 
    
    colnames(output_data) <- c("age_type", "city_id", "city_name",
                               "workforce_pop", "working_pop", "student_pop", "year")
    
    unique(output_data$year_type)
    
    work_student_data <- output_data %>% 
      dplyr::filter(age_type == "総数（15歳以上）") %>% 
      dplyr::select(-age_type) %>% 
      dplyr::relocate(year, .after = city_name)
    
  } else if(year_n == 2000){
    
    output_data <- working_data %>% 
      dplyr::select(10, 13, 14, 16, 17, 18) %>% 
      dplyr::mutate(year = year_n) 
    
    colnames(output_data) <- c("age_type", "city_id", "city_name",
                               "workforce_pop", "working_pop", "student_pop", "year")
    
    unique(output_data$age_type)
    
    work_student_data <- output_data %>% 
      dplyr::filter(age_type == "総数") %>% 
      dplyr::select(-age_type) %>% 
      dplyr::relocate(year, .after = city_name)
    
  } else if(year_n == 2005){
    
    output_data <- working_data %>% 
      dplyr::select(8, 11, 12, 14, 15, 16) %>% 
      dplyr::mutate(year = year_n) 
    
    colnames(output_data) <- c("age_type", "city_id", "city_name",
                               "workforce_pop", "working_pop", "student_pop", "year")
    
    unique(output_data$age_type)
    
    work_student_data <- output_data %>% 
      dplyr::filter(age_type == "総数（15歳以上）") %>% 
      dplyr::select(-age_type) %>% 
      dplyr::relocate(year, .after = city_name)
    
  }else if(year_n == 2010){
    
    output_data <- working_data %>% 
      dplyr::select(7, 8, 12, 14, 15, 16) %>% 
      dplyr::mutate(year = year_n) 
    
    colnames(output_data) <- c("city_id", "city_name", "age_type",
                               "workforce_pop", "working_pop", "student_pop", "year")
    
    unique(output_data$age_type)
    
    work_student_data <- output_data %>% 
      dplyr::filter(age_type == "総数（年齢）") %>% 
      dplyr::select(-age_type) %>% 
      dplyr::relocate(year, .after = city_name)
    
  }else if(year_n == 2015){
    
    output_data <- working_data %>% 
      dplyr::select(5, 6, 10, 12, 13, 14) %>% 
      dplyr::mutate(year = year_n) 
    
    colnames(output_data) <- c("city_id", "city_name", "age_type",
                               "workforce_pop", "working_pop", "student_pop", "year")
    
    work_student_data <- output_data %>% 
      dplyr::filter(age_type == "総数（年齢）") %>% 
      dplyr::select(-age_type) %>% 
      dplyr::relocate(year, .after = city_name)
  }
  
  work_student_data <- work_student_data %>%  
    dplyr::mutate(city_id = as.character(city_id)) 
  
  return(work_student_data)
  
}


save_csv(all_working_data, "working", "all.csv")
