main <- function() {
  
  # read data ####
  # 駅データ
  # df_c <- read_df_csv("geometry_base", "df_control_city") 
  df_t <- read_df_csv("geometry_base", "df_treatment_city") 
  # 人口・年齢別人口データ
  df_pop <- read_df_csv("city_adjust", "pop")
  # df_age <- read_df_csv("city_adjust", "age_data") |> 
    # dplyr::mutate(city_name = str_replace_all(city_name, fixed("*"), "")) 
  
  df_year_end <- df_t |> 
    dplyr::select(
      city_id,
      year_end
      ) |> 
    distinct()
  
  list_treatment <- df_t |> 
    dplyr::filter(year_end <= 2012,
                  !city_name %in% c("鳳珠郡能登町", "珠洲市")) |> 
    dplyr::distinct(city_id) |> 
    dplyr::pull()
  
  df_pop <- df_pop |> 
    dplyr::mutate(
      treatment = if_else(city_id %in% list_treatment, "1", "0")
    ) |> 
    left_join(
      df_year_end
    )
  
  # 輸送密度・災害地域データ
  # df_density <- read_df_csv("density", "master_density") |> 
    # dplyr::select(-station_name) |> 
    # dplyr::distinct()
  df_disaster <- read_df_csv("disaster", "master_disaster")
  
  # 隣接市町村データ
  df_adjacent_city <- read_df_csv("adjacent_city", "adjacent_city")
  
  # 輸送密度・災害情報を加えるセクション ####
  df_master_int <- add_density_disaster(df_pop, df_disaster)
  
  # 隣接市町村を除くセクション
  df_master_int <- remove_adjacent(df_master_int, df_adjacent_city)
  
  
  #　共変量を加えるセクション
  df_children <- read_df_csv("city_adjust", "children") |> 
    dplyr::select(-city_name)
  df_housetype <- read_df_csv("city_adjust", "housetype") |> 
    dplyr::select(-city_name)
  # df_transport <- read_df_csv("city_adjust", "transport") |> 
  #   change_transport_data() 
  df_working <- read_df_csv("city_adjust", "working") |> 
    dplyr::select(-city_name)
  df_houseyear <- read_df_csv("city_adjust", "old_house") |> 
    dplyr::select(-city_name)
  
  df_master <- add_covariates(df_master_int, 
                              df_children, 
                              df_housetype, 
                              df_transport, 
                              df_working, 
                              df_houseyear) |> 
    dplyr::distinct()
  
  df_master <- df_master |> 
    dplyr::distinct() |> 
    tidyr::drop_na(city_name) |> 
    dplyr::mutate(cut_off = ifelse((treatment == 1 & year_end <= year), 1, 0),
                  .after = city_id) |> 
    dplyr::filter(year_end <= 2012 | is.na(year_end)) |> 
    dplyr::filter(city_name != "鳳珠郡能登町",
                  city_name != "珠洲市") |> 
    dplyr::relocate(
      year_end, .after = year
    )
  
  
  # save_df_csv(df_master, "master", "master")
  save_df_csv(df_master, "master", "master_2")
  
  list_omitted <- c(1233,
                    4505,
                    7213,
                    11381,
                    12421,
                    43348)
  
} 


add_density_disaster <- function(df_pop, df_disaster) {
  
  # 異なるデータを合わせる際に、市町村名や都道府県名は異なる
  # 可能性があるため、idのみでleft_joinを行っている
  df_input_id <- df_pop |> 
    dplyr::select(-prefecture_name) |> 
    dplyr::distinct()
  df_disaster_id <- df_disaster |> 
    dplyr::select(-city_name, -prefecture_name) |> 
    dplyr::distinct()
  
  df_output <- df_input_id |>
    dplyr::left_join(df_disaster_id,
                     by = "city_id",
                     relationship = "many-to-many") |>
    distinct()
  
  return(df_output)
}


change_transport_data <- function(data){
  
  train_df <- data |> 
    ungroup() |> 
    dplyr::select(year,
                  city_id,
                  train_only_pop,
                  train_car_pop,
                  train_bus_pop,
                  train_motorcycle_pop,
                  train_bicycle_pop) |>
    dplyr::filter(year %in% c(2000,2010)) 
  
  output_df <- train_df |>
    dplyr::mutate(train_all_pop = rowSums(train_df[,c("train_only_pop", 
                                                      "train_car_pop", 
                                                      "train_bus_pop", 
                                                      "train_motorcycle_pop", 
                                                      "train_bicycle_pop")], na.rm = TRUE))
  return(output_df)
}

add_covariates <- function(df_input, df_children, df_housetype, 
                           df_transport, df_working, df_houseyear){
  
  df_based <- df_input |> 
    dplyr::left_join(df_children) |> 
    dplyr::left_join(df_housetype) |> 
    # dplyr::left_join(df_transport) |>
    dplyr::left_join(df_working) |> 
    dplyr::left_join(df_houseyear)
  
  
  output_df <- df_based |> 
    dplyr::group_by(city_id, year) |> 
    dplyr::mutate(household_with_children_rate = (children_household/household), 
                  .after = children_household) |> 
    dplyr::mutate(own_household_rate = (own_household/household), 
                  .after = own_household) |> 
    dplyr::mutate(workforce_rate = (workforce_pop/total), 
                  .after = workforce_pop) |> 
    dplyr::mutate(student_rate = (student_pop/total), 
                  .after = student_pop) |> 
    # dplyr::mutate(train_using_rate = (train_all_pop/total), 
    #               .after = train_all_pop) |> 
    dplyr::mutate(old_house_rate = (old_house_household/household), 
                  .after = old_house_pop) |>  
    dplyr::ungroup()
  
  
  return(output_df)
  
}

# df_test <- df_master_int

remove_adjacent <- function(df_master_int, df_adjacent_city) {
  
  list_treatment_city <- df_master_int |> 
    dplyr::filter(treatment == 1) |> 
    dplyr::distinct(city_id) |> 
    dplyr::pull()
  
  df_adjacent_city <- df_adjacent_city |> 
    dplyr::filter(
      !adjacent_id %in% list_treatment_city
    )
  
  list_adjacent <- df_adjacent_city |> 
    dplyr::distinct(adjacent_id) |> 
    dplyr::pull()
  
  
  df_output <- df_master_int |> 
    dplyr::filter(!city_id  %in% list_adjacent)
  
  return(df_output)
  
}


