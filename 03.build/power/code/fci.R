main <- function(){
  
  year_list <- seq(2005, 2019)
  
  fci_data <- purrr::map(year_list, read_power) |> 
    dplyr::bind_rows()
  
  df_master <- read_df_csv("master", "master_2")
  
  list_master <- df_master |> 
    dplyr::mutate(
      index_gun = regexpr("郡", city_name)
      # city_name = stringr::str_replace_all(city_name, "群", "a")
    ) |> 
    mutate(
      city_name = if_else(index_gun != -1,
                          str_sub(city_name, start = index_gun + 1, end = -1),
                          city_name)
    ) |> 
    dplyr::filter(
      treatment == 1
    ) |> 
    distinct(city_name) |> 
    pull()
  
  df_id_name <- read.xlsx("02.raw/municipalities_code/df_id_name.xlsx") |> 
    dplyr::mutate(
      index_gun = regexpr("郡", city_name)
      # city_name = stringr::str_replace_all(city_name, "群", "a")
    ) |> 
    mutate(
      city_name = if_else(index_gun != -1,
                          str_sub(city_name, start = index_gun + 1, end = -1),
                          city_name)
    ) |> 
    dplyr::select(
      city_name, city_id
    )
  
  fci_data_test <- fci_data |> 
    left_join(df_id_name)
  
  
  write.csv(fci_data_test, 
            here::here('03.build',
                       'power', 'data', 
                       'fci_data.csv'),
            fileEncoding = "CP932", 
            row.names = FALSE)
  
    
  list_name_a <- fci_data_test |> 
    pull(city_name)
  
  list_name_b <- df_id_name |> 
    pull(city_name)
  
  setdiff(list_master, list_name_a)
  
}


read_power <- function(year_n){
  
  if(year_n <= 2007){
    file_name <- here::here('02.raw',
                            'power',
                            paste0(year_n,'.xls'))
    based_data <- readxl::read_xls(file_name, 
                                   col_names = FALSE,
                                   skip = 2) |> 
      dplyr::select(1,2,6)
    
  }else if(year_n <= 2010){
    file_name <- here::here('02.raw',
                            'power',
                            paste0(year_n,'.xls'))
    based_data <- readxl::read_xls(file_name, 
                                   col_names = FALSE,
                                   skip = 2) |>
      dplyr::select(1,2,3)
  
  }
  else if(year_n <= 2015){
    file_name <- here::here('02.raw',
                            'power',
                            paste0(year_n,'.xls'))
    
    based_data <- readxl::read_xls(file_name, 
                                   col_names = FALSE,
                                   skip = 2) |>
      dplyr::select(2,3,4)

  }else if(year_n <= 2019){
    file_name <- here::here('02.raw',
                            'power',
                            paste0(year_n,'.xlsx'))
    based_data <- readxl::read_xlsx(file_name, 
                                    col_names = FALSE,
                                    skip = 2) |>
      dplyr::select(2,3,4) 
  
  }
  
  colnames(based_data) <- c("region_name", "city_name", "fci")
  
  output_data <- based_data |> 
    dplyr::mutate(year = year_n, .after = city_name) |> 
    dplyr::mutate(fci = as.numeric(fci))
  
  return(output_data)
  
}
