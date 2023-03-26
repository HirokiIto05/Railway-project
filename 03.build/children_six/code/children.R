main <- function(){
  
  year_list <- seq(1995,2015,by= 5)
  
  all_children_data <- purrr::map(year_list, read_child_csv) %>% 
    dplyr::bind_rows()
  
  save_table(all_children_data)
  
  
}


read_child_csv <- function(year_n){
  
  file_name <- paste0(year_n, ".csv")
  
  child_data <- read.csv(here::here('02.raw', 'covariates', 'children_six', file_name),
                         fileEncoding = "CP932") 
  
  if(year_n < 2010){
    output_data <- child_data %>% 
      dplyr::select(7,8,10,11) %>% 
      dplyr::mutate(year = year_n)
  }else{
    output_data <- child_data %>% 
      dplyr::select(5, 6, 8, 9) %>% 
      dplyr::mutate(year = year_n)
  }
  
  colnames(output_data) <- c("city_id", "city_name", "children_household", "children_pop", "year")
  
  output_data <- output_data %>% 
    dplyr::mutate(city_id = as.character(city_id),
                  children_household = as.numeric(sub(",","",children_household)),
                  children_pop = as.numeric(sub(",","",children_pop))) %>% 
    dplyr::relocate(year, .after = city_name) 　
  
  return(output_data)
  
}


save_table <- function(all_children_data){
  
  write.csv(all_children_data, 
            file = here::here('03.build','children_six','data','all_children_data.csv'),
            fileEncoding = "CP932", row.names = FALSE)
  
}



