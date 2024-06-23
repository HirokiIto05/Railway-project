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


  # 市町村合併データ ----------------------------------------------------------------
  adjust_df <- readxl::read_xlsx(here::here("01_data", "raw", "municipality_converter", "municipality_converter_jp.xlsx"))
  
  df_city_id_2020 <- adjust_df |> 
    dplyr::distinct(id_muni2020) |> 
    dplyr::rename(
      city_id = id_muni2020
    )

  list_2020_id <- df_city_id_2020 |> 
    dplyr::pull(city_id)

  # 市町村名とidのデータフレーム ---------------------------------------------------------
  df_name_id <- df_pop |> 
    dplyr::select(
      city_id,
      city_name,
      prefecture_name,
      year
    ) |> 
    dplyr::filter(year == 2020) |> 
    dplyr::filter(city_id %in% list_2020_id) |>
    distinct() |>
    dplyr::select(-year)

  # 市町村合併の調整  
  df_pop <- purrr::map(list_2020_id, adjust_city_id, df_pop, adjust_df) |> 
    dplyr::bind_rows()
   

  df_pop_output <- df_pop |>
    dplyr::left_join(df_name_id, by = "city_id") |>
    dplyr::select(
      city_id, city_name, prefecture_name, year, dplyr::everything()
    )


  # 対数変化率の計算。1, 5年のラグを伴った変数の作成
  df_add_pop <- add_lag_variables(df_pop_output)
df_add_pop |> View()

  # データ保存
  write.csv(df_add_pop, here::here("01_data", "intermediate", "outcome", "population.csv"), fileEncoding = "cp932", row.names = FALSE)


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

adjust_city_id <- function(id_n, df_pop, adjust_df){
  print(id_n)
  
  list_id <- adjust_df |> 
    dplyr::filter(id_muni2020 == id_n) |> 
    dplyr::select(seq(2,10)) |> 
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "year",
      values_to = "id"
    ) |> 
    dplyr::distinct(id) |> 
    dplyr::pull()
  
  df_id_n <- df_pop |> 
    dplyr::filter(city_id %in% list_id) |> 
    dplyr::mutate(
      year = as.character(year),
      city_id = as.character(city_id)
      )
  
  city_data <- df_pop |>
    dplyr::filter(year == 2020,
                  city_id == id_n) |> 
    dplyr::select(city_id, city_name, prefecture_name)
  
  city_id_n <- city_data |> 
    dplyr::pull(city_id)
  city_name_n <- city_data |> 
    dplyr::pull(city_name)
  prefecture_name_n <- city_data |> 
    dplyr::pull(prefecture_name)
  
  list_vars <- c(
    "male",
    "female",
    "total",
    "household",
    "moving_in_dom",
    "moving_in_int",
    "moving_in_total",
    "birth",
    "increase_total",
    "moving_out_dom",
    "moving_out_int",
    "moving_out_total",
    "mortality",
    "decrease_total",
    "change",
    "natural",
    "social"
  )


  output_data <- df_id_n |>
    dplyr::reframe(
      dplyr::across(dplyr::any_of(list_vars), ~sum(., na.rm = TRUE)),
      .by = year
    ) |>
    dplyr::mutate(
      city_id = city_id_n
      )
  
  return(output_data)
  
}


add_lag_variables <- function(df_input) {
  
  df_based <- df_input |>
    dplyr::group_by(city_id) |>
    dplyr::mutate(
      lag_total = dplyr::lag(total),
      change_rate = change/dplyr::lag(total)*100,
      natural_rate = natural/dplyr::lag(total)*100,
      social_rate = (social/dplyr::lag(total))*100
    ) |> 
    dplyr::select(
      city_id,
      city_name,
      prefecture_name,
      year,
      male,
      female,
      total,
      lag_total,
      household,
      birth,
      increase_total,
      mortality,
      decrease_total,
      change,
      change_rate,
      natural,
      natural_rate,
      social,
      social_rate
    )
  
  return(df_based)
  
}



main()

