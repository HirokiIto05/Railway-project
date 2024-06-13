main <- function() {
  
  # 日本語の列名を英語に変換
  list_colname <- create_list_colname()

  year_list <- seq(1996, 2020)
  
  non_numeric_variables <- c("prefecture_name", "city_name")

  # clean data ---------------------------------------------------------------------
  df_pop <- purrr::map(year_list,
                       aggregate_pop,
                       list_colname = list_colname,
                       nationality = "japanese") |>
    dplyr::bind_rows() |>
    dplyr::mutate(dplyr::across(-dplyr::any_of(non_numeric_variables), as.numeric)) |> 
    dplyr::mutate(city_id = stringr::str_sub(city_id, start = 1, end = -2)) |> 
    dplyr::mutate(dplyr::across(c(city_name), ~dplyr::na_if(., "-"))) |>
    dplyr::mutate(dplyr::across(c(city_name), ~dplyr::na_if(., "\u001f"))) |> 
    tidyr::drop_na(city_name) |>
    dplyr::select(-c(change_rate, natural_rate, social_rate))

  
  # save -----------------------------------------------------------------
  write.csv(df_pop, here::here("01_data", "intermediate", "population", "population.csv"), fileEncoding = "cp932", row.names = FALSE)

}

# Functions -----------------------------------------------------------------

# generate a colname's list 
create_list_colname <- function() {

  list_colname <- c(
    "city_id",
    "prefecture_name",
    "city_name",
    "male",
    "female",
    "total", 
    "household",
    "moving_in",
    "birth", 
    "increase_total",
    "moving_out",
    "mortality",
    "decrease_total",
    "change",
    "change_rate",
    "natural",
    "natural_rate",
    "social",
    "social_rate")
  
  return(list_colname)
  
}

year_n <- 2005

# aggregate for changing raw data to panel data
aggregate_pop <- function(year_n, list_colname, nationality) {
  print(year_n)

  if(year_n < 2013){
    raw_name <- paste0((stringr::str_sub(as.character(year_n), start = 3, end = 4)), "03sjin")
  } else {
    raw_name <- paste0((stringr::str_sub(as.character(year_n), start = 3, end = 4)), "07nsjin")
  }
  

  if (year_n <= 2012){

    file_name <- here::here(
      '01_data', 
      'raw',
      'jumin_kihon_daicho', 
      paste0(raw_name, ".xls"))

    
    if(year_n != 2005){
      new_df <-  readxl::read_xls(file_name, col_names = FALSE) |>
        dplyr::select(-c(10,14))
    } else if(year_n == 2005){
      new_df <-  readxl::read_xls(file_name, col_names = FALSE) |>
        dplyr::select(-c(7, 9, 12, 16))
    }

  } else if (year_n <= 2020) {

    file_name <- here::here(
      "01_data",
      'raw',
      'jumin_kihon_daicho',
      paste0(raw_name, ".xls"))
    
    new_df <- readxl::read_xls(file_name, col_names = FALSE) |>
      dplyr::select(-c(7, 8, 10, 11, 14, 15,
                        16, 18, 19, 22, 23, 24))

  } else if (year_n >= 2021) {

    file_name <- here::here(
      "01_data",
      'raw',
      'jumin_kihon_daicho',
      paste0(raw_name, ".xlsx"))
    
    new_df <- readxl::read_xlsx(file_name) |>
      dplyr::select(-c(7, 8, 10, 11, 14, 15,
                        16, 18, 19, 22, 23, 24))
    
  }
  colnames(new_df) = list_colname

  output_df <- new_df |>
    dplyr::mutate(year = year_n, .after = city_name) |> 
    tidyr::drop_na(city_id, total) |> 
    dplyr::ungroup()
  
  return(output_df)
}


main()
