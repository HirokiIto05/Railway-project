main <- function() {
  
  
  # str(df_geometry)
  df_geometry <- read_geometry() 
  
  list_geometry_city_name <- df_geometry %>% 
    dplyr::distinct() %>% 
    select(city_name) %>% 
    unlist() %>% 
    as.character()
  
  df_control <- read_main_list("control")
  
  df_test <- purrr::map(list_geometry_city_name, extract_one_station_city,
                        df_geometry) %>% 
    dplyr::bind_rows()

  
}


read_geometry <- function() {
  
  geometry_master <- sf::read_sf(here::here('02.raw', 
                                            'geometry_data',
                                            'geometry_master.gpkg')) %>% 
    tidyr::as_tibble()
    
  output_df <- geometry_master %>% 
    dplyr::select(3, 4, 5, 9)
  
  colnames(output_df) <- c("line_name", "company_name",
                           "station_name", "city_name")
  
  return(output_df)
  
}


read_main_list <- function(file_name_n) {
  
  file_name <- paste0(file_name_n, '.xlsx')
  
  file_path <- here::here('02.raw',
                          'municiparities_list',
                          file_name)
  
  output_df <- readxl::read_xlsx(file_path)
  
  return(output_df)
}


extract_one_station_city <- function(city_name_n, df_geometry) {
  print(city_name_n)
  
  df_one_city <- df_geometry %>% 
    dplyr::filter(city_name == city_name_n)
  
  df_line_only <- df_one_city %>% 
    dplyr::select(line_name) %>% 
    dplyr::distinct() 
  
  num_line <- nrow(df_line_only)
  
  if(num_line == 1) {
    
    output_df <- df_one_city %>% 
      distinct() %>% 
      select(line_name, company_name, city_name)
    
  } else {
    
  } 
  
  return(output_df)
  
}

joint_df <- function(df_geometry, df_control) {
  
  
  df_new <- df_control %>% 
    dplyr::left_join(df_geometry)
  
  
  
}


  
