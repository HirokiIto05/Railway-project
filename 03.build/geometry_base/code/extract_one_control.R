main <- function() {
  
  geo_current <- read_geometry("geometry_continue") 
  
  list_current_city <- geo_current |> 
    select(city_name) |> 
    dplyr::distinct() |> 
    unlist() |> 
    as.character()
  
  df_control <- read_main_list("control")
  
  df_local_jr <- read_local_line() 
  
  # jr以外の路線
  df_local <- df_control |> 
    dplyr::filter(jr == 0) |> 
    dplyr::select(-jr)
  
  df_local_jr <- df_local_jr |> 
    dplyr::left_join(geo_current) |> 
    dplyr::select(line_name, company_name) |> 
    distinct()
  
  # jrとjr以外のdfを結合
  df_local_both <- dplyr::bind_rows(df_local, 
                                    df_local_jr) |> 
    distinct()
  
  
  df_one_line <- purrr::map(list_current_city, extract_one_line_city,
                            geo_current) |> 
    dplyr::bind_rows() |> 
    distinct(.keep_all = TRUE) 

  # control路線と市町村内に駅が１つの市町村を結合
  df_control_city <- left_join(df_local_both, df_one_line) |> 
    dplyr::mutate(year_end = 0, .after = prefecture_name) |> 
    dplyr::relocate(line_name, .before = station_name) |> 
    dplyr::relocate(company_name, .before = line_name)
  
  save_df_csv(df_control_city, 'geometry_base', 'df_control_city')

}


read_geometry <- function(file_name_n) {
  
  geo_based <- read_df_xlsx("geometry_base", file_name_n)
    
  output_df <- geo_based |> 
    dplyr::select(3, 4, 5, 6, 9, 10)
  
  colnames(output_df) <- c("line_name", "company_name",
                           "station_name", "prefecture_name", 
                           "city_name", "city_id")
  
  return(output_df)
  
}


read_main_list <- function(file_name_n) {
  
  file_name <- paste0(file_name_n, '.xlsx')
  
  file_path <- here::here('02.raw',
                          'municipalities_list',
                          file_name)
  
  output_df <- readxl::read_xlsx(file_path)
  
  return(output_df)
}


read_local_line <- function() {
  
  df_local_jr <- readxl::read_xlsx(here::here('02.raw', 'municipalities_list', 'local_line_list_JR.xlsx'),
                                   col_names = FALSE) 
  
  colnames(df_local_jr) <- "line_name"
  
  return(df_local_jr)
  
}


extract_one_line_city <- function(city_name_n, geo_current) {
  print(city_name_n)
  
  df_one_city <- geo_current |> 
    dplyr::filter(city_name == city_name_n)
  
  df_line_only <- df_one_city |> 
    dplyr::select(line_name, company_name, city_id) |> 
    dplyr::distinct() 
  
  num_line <- nrow(df_line_only)
  
  if(num_line == 1) {
    
    output_df <- df_one_city |> 
      distinct() |> 
      select(city_id, city_name, line_name, prefecture_name, company_name, station_name)
    
  } else {
    output_df <- tibble()
  } 
  
  return(output_df)
  
}

joint_df <- function(df_one_line, df_control) {

  df_output <- df_control |> 
    dplyr::left_join(df_one_line)
  
  
  return(df_output)
  
}



  
