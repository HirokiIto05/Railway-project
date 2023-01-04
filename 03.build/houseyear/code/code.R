main <- function(){
  
  year_list <- seq(2000,2010,by= 10)
  
  all_houseyear_data <- purrr::map(year_list, read_houseyear_csv) %>% 
    dplyr::bind_rows()
  
}

read_houseyear_csv <- function(year_n){
  
  file_name <- paste0(year_n, ".csv")
  
  houseyear_data <- read.csv(here::here('02.raw', 'covariates', 'house_year', file_name),
                             fileEncoding = "CP932") 
  
  
  if(year_n == 2000){
    
    output_data <- houseyear_data %>% 
      dplyr::select(4, 9, 10, 12, 13) %>% 
      dplyr::mutate(year = year_n)
    
    colnames(output_data) <- c("houseyear", "city_id", "city_name", "houseyear_household", 
                               "houseyear_pop", "year")
    
    unique(output_data$houseyear)
    
    output_data <- output_data %>% 
      dplyr::filter(houseyear == "２０年以上") %>% 
      dplyr::select(-1)
    
  } else if(year_n == 2010){
    
    output_data <- houseyear_data %>% 
      dplyr::select(1, 2, 14, 22) %>% 
      dplyr::mutate(year = year_n)
    
    colnames(output_data) <- c("city_id", "city_name",
                               "houseyear_household", "houseyear_pop", "year")
    
  }
  
  output_data <- output_data %>% 
    dplyr::mutate(city_id = as.character(city_id)) 
  
  return(output_data)
  
}


save_csv(all_houseyear_data, "houseyear", "all.csv")
