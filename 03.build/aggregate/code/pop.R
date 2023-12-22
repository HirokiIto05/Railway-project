main <- function(){
  
  list_colname <- create_colname_list()
  
  df_city_id <- read_df_city_id()
  
  year_list <- seq(1996, 2020)
  non_numeric_variables <- c("prefecture_name", "city_name")
  
  df_pop <- purrr::map(year_list,
                        aggregate_pop,
                        list_colname = list_colname) |> 
    dplyr::bind_rows() |>
    dplyr::mutate_at(dplyr::vars(-non_numeric_variables), as.numeric) |>
    dplyr::mutate(city_id = str_sub(city_id, start = 1, end = -2)) |>
    dplyr::mutate(city_id = as.numeric(city_id))
  
  save_df_csv(df_pop, "aggregate", "pop")

}


read_df_city_id <- function(){
  
  df_city_id <- 
    readxl::read_xls(here::here('02.raw',
                                'municipalities_code',
                                'municipalities_code.xls')) |> 
    dplyr::select(1,2,3)
  
  colnames(df_city_id) <- c("city_id",
                            "prefecture_name",
                            "city_name")
  
  output_df <- df_city_id |> 
    tidyr::drop_na(city_name) |> 
    dplyr::mutate(city_id = as.numeric(city_id)) |> 
    dplyr::select(-city_name)
  
  
  return(output_df)
} 

create_colname_list <- function() {
  
  colname_list <- c("city_id",
                    "prefecture_name",
                    "city_name",
                    "male",
                    "female",
                    "total", 
                    "household",
                    "moving_in",
                    "birth", 
                    "other_in",
                    "increase_total",
                    "moving_out",
                    "mortality",
                    "other_out",
                    "decrease_total",
                    "change",
                    "change_rate",
                    "natural",
                    "natural_rate",
                    "social",
                    "social_rate")
  
  return(colname_list)
  
}


aggregate_pop <- function(year_n, list_colname){
  
  if(year_n <= 2012){
    file_name <- here::here('02.raw', 'population', 'japanese', paste0(year_n, '.xls'))
    new_df <-  readxl::read_xls(file_name, col_names = FALSE) 
  } else if(year_n <= 2020) {
    file_name <- here::here('02.raw', 'population', 'japanese', paste0(year_n, '.xls'))
    new_df <-  readxl::read_xls(file_name, col_names = FALSE) |> 
      dplyr::select(-7, -8, -10,-11, -14, -15, -18, -19, -22, -23)
    
  } else if(year_n >= 2021) {
    file_name <- here::here('02.raw', 'population', 'japanese', paste0(year_n, '.xlsx'))
    new_df <-  readxl::read_xlsx(file_name, col_names = FALSE) |> 
      dplyr::select(-7, -8, -10,-11, -14, -15, -18, -19, -22, -23)
  }
  
  colnames(new_df) = list_colname
  
  df_output <- new_df |> 
    tidyr::drop_na(total, city_name) |> 
    dplyr::mutate(year = year_n, .after = city_name)
  
  return(df_output)
  
}


