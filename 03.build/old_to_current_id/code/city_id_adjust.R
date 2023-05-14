main <- function() {
  
  df_all_city_code <- read_city_adjust()
  
  list_current_city <- df_all_city_code %>% 
    select(id_muni2020)
  
  df_current_old <- purrr::map(list_current_city, create_df_old_current_id,
                               df_all_city_code) %>% 
    dplyr::bind_rows()
  
  save_df(df_current_old, 'old_to_current_id',
          'current_old_id')
  
}


read_df_code <- function() {
  
  df_based <- readxl::read_xls(here::here('02.raw',
                                           'municipalities_code',
                                           'municipalities_code.xls')) %>% 
    dplyr::select(1,2,3)
    
  colnames(df_based) <- c('city_id', 'prefecture_name',
                          'city_name')
  
  df_output <- df_based %>% 
    dplyr::mutate(city_id = str_sub(city_id, start = 1, end = 5)) %>% 
    dplyr::mutate(city_id = as.numeric(city_id))
  
  
  return(df_output)
}


read_city_adjust <- function() {
  
  df_based <- readxl::read_xlsx(here::here('02.raw',
                                           'municipality_converter',
                                           'municipality_converter.xlsx')) 
  
  return(df_based)
}


create_df_old_current_id <- function(id_n, df_all_city_code) {
  print(id_n)
  
  list_old_id <- df_all_city_code %>% 
    dplyr::filter(id_muni2020 == id_n) %>% 
    apply( 1, unique) %>% 
    unlist() %>% 
    as.numeric() %>% 
    na.omit() %>% 
    unique() 
  
  output_df <- tibble(id_old = list_old_id) %>% 
    dplyr::mutate(id_current = id_n)
  
  return(output_df)

}

