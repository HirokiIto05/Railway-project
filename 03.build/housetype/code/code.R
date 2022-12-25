main <- function(){
  
  year_list <- seq(1995,2015,by= 5)
  
  all_housetype_data <- purrr::map(year_list, read_housetype_csv) %>% 
    dplyr::bind_rows()
  
  all_housetype_data <- all_housetype_data %>% 
    dplyr::mutate(own_household = as.numeric(sub(",", "", own_household)),
                  own_pop = as.numeric(sub(",", "", own_pop)),
                  rent_household = as.numeric(sub(",", "", rent_household)),
                  rent_pop = as.numeric(sub(",", "", rent_pop)))
  
}


read_housetype_csv <- function(year_n){
  
  file_name <- paste0(year_n, ".csv")
  
  housetype_data <- read.csv(here::here('02.raw', 'covariates', 'housetype', file_name),
                         fileEncoding = "CP932") 
  
  if(year_n < 2010){
    
    output_data <- housetype_data %>% 
      dplyr::select(4, 7, 8, 10, 11) %>% 
      dplyr::mutate(year = year_n)
    
    colnames(output_data) <- c("housetype", "city_id", "city_name", "household_housetype", "pop_housetype", "year")
    
    own_data <- output_data %>% 
      dplyr::filter(housetype == "持ち家") %>% 
      dplyr::rename(own_household = household_housetype,
                    own_pop = pop_housetype) %>% 
      dplyr::select(-housetype)
    
    rent_data <- output_data %>% 
      dplyr::filter(housetype == "民営の借家") %>% 
      dplyr::rename(rent_household = household_housetype,
                    rent_pop = pop_housetype) %>% 
      dplyr::select(-housetype)
    
    own_rent_data <- dplyr::left_join(own_data, rent_data) %>% 
      dplyr::relocate(year, .after = city_name)
    
  } else if(year_n == 2010){
    
    output_data <- housetype_data %>% 
      dplyr::select(3, 4, 6, 8, 9) %>% 
      dplyr::mutate(year = year_n)
    
    colnames(output_data) <- c("city_id", "city_name", "housetype", "household_housetype", "pop_housetype", "year")
    
    own_data <- output_data %>% 
      dplyr::filter(housetype == "主世帯 持ち家") %>% 
      dplyr::rename(own_household = household_housetype,
                    own_pop = pop_housetype) %>% 
      dplyr::select(-housetype)
    
    rent_data <- output_data %>% 
      dplyr::filter(housetype == "主世帯 民営の借家") %>% 
      dplyr::rename(rent_household = household_housetype,
                    rent_pop = pop_housetype) %>% 
      dplyr::select(-housetype)
    
    own_rent_data <- dplyr::left_join(own_data, rent_data) %>% 
      dplyr::relocate(year, .after = city_name)
  } else if(year_n == 2015){
    
    output_data <- housetype_data %>% 
      dplyr::select(6, 7, 8, 11, 13) %>% 
      dplyr::mutate(year = year_n)
    
    colnames(output_data) <- c("var_type", "city_id", "city_name", "own_type", "rent_type", "year")
    
    pop_data <- output_data %>% 
      dplyr::filter(var_type == "一般世帯人員【人】") %>% 
      dplyr::rename(own_pop = own_type,
                    rent_pop = rent_type) %>% 
      dplyr::select(-var_type)
    
    household_data <- output_data %>% 
      dplyr::filter(var_type == "一般世帯数【世帯】") %>% 
      dplyr::rename(own_household = own_type,
                    rent_household = rent_type) %>% 
      dplyr::select(-var_type)
    
    own_rent_data <- dplyr::left_join(pop_data, household_data) %>% 
      dplyr::relocate(year, .after = city_name) %>% 
      dplyr::relocate(own_household, .before = own_pop) %>% 
      dplyr::relocate(rent_household, .before = rent_pop)
  }
  
  own_rent_data <- own_rent_data %>% 
    dplyr::mutate(city_id = as.character(city_id)) 
  
  return(own_rent_data)
  
}


save_csv(all_housetype_data, "housetype", "all.csv")
