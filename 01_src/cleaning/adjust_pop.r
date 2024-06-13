main <- function() {
  
  df_pop <- read_df_csv("aggregate", "pop")
  
  library(data.table)
  
  adjust_df <- readxl::read_xlsx("02.raw/municipality_converter/municipality_converter_jp.xlsx")
  
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
    dplyr::filter(
      year == 2020
    ) |>
    dplyr::distinct() |> 
    dplyr::right_join(
      df_city_id_2020
    ) |> 
    dplyr::select(-year)
  
  # 市町村合併の調整  
  
  df_adjusted <- purrr::map(list_2020_id, adjust_city_id, df_pop, adjust_df) |> 
    bind_rows()
  
  df_pop_output <- df_adjusted |> 
    dplyr::left_join(df_name_id) |> 
    add_variabels()
  
  aa <- df_pop_output |>
    ungroup() |> 
    dplyr::summarise(
      n = dplyr::n(),
      .by = city_id
    )
  
  # df_add_rate <- df_adjusted |> 
  #   dplyr::group_by(city_id) |> 
  #   dplyr::mutate(lag_total = dplyr::lag(total), .after = total) |> 
  #   dplyr::mutate(change_rate = change/dplyr::lag(total)*100, .after = change) |> 
  #   dplyr::mutate(natural_rate = natural/dplyr::lag(total)*100, .after = natural) |> 
  #   dplyr::mutate(social_rate = (social/dplyr::lag(total))*100, .after = social) 
  # 
  
  save_df_csv(df_pop_output, "city_adjust", "pop")
  
}

# id_n <- 17204

id_n = 40231

adjust_city_id <- function(id_n, df_pop, adjust_df){
  print(id_n)
  
  list_id <- adjust_df |> 
    dplyr::filter(id_muni2020 == id_n) |> 
    dplyr::select(seq(1,9)) |> 
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "year",
      values_to = "id"
    ) |> 
    dplyr::distinct(id) |> 
    dplyr::pull() |> 
    na.omit()
  
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
  
  output_data <- setDT(df_id_n)[, .(
    male = sum(male, na.rm = TRUE),
    female = sum(female, na.rm = TRUE),
    total = sum(total, na.rm = TRUE),
    household = sum(household, na.rm = TRUE),
    moving_in = sum(moving_in, na.rm = TRUE),
    birth = sum(birth, na.rm = TRUE),
    other_in = sum(other_in, na.rm = TRUE),
    increase_total = sum(increase_total, na.rm = TRUE),
    moving_out = sum(moving_out, na.rm = TRUE),
    mortality = sum(mortality, na.rm = TRUE),
    other_out = sum(other_out, na.rm = TRUE),
    change = sum(change, na.rm = TRUE),
    natural = sum(natural, na.rm = TRUE),
    social = sum(social, na.rm = TRUE)
  ), by = year] |> 
    dplyr::mutate(
      city_id = city_id_n
    )
  
  
  return(output_data)
  
}


add_variabels <- function(df_input) {
  
  df_output <- df_input |> 
    dplyr::group_by(city_id) |> 
    dplyr::mutate(lag_total = dplyr::lag(total), .after = total) |> 
    dplyr::mutate(change_rate = change/dplyr::lag(total)*100, .after = change) |> 
    dplyr::mutate(natural_rate = natural/dplyr::lag(total)*100, .after = natural) |> 
    dplyr::mutate(social_rate = (social/dplyr::lag(total))*100, .after = social) |> 
    dplyr::select(
      city_id,
      city_name,
      prefecture_name,
      year,
      dplyr::everything()
    )
  
  return(df_output)
  
}


