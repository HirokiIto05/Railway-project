main <- function() {
  # データ読み込み
  df_pop <- read.csv(here::here("01_data", "intermediate", "population", "population_adjusted.csv"), fileEncoding = "cp932")
  
  # 対数変化率の計算。1, 5年のラグを伴った変数の作成
  df_add_pop <- add_lag_variables(df_pop)
  
  # データ保存
  write.csv(df_add_pop, here::here("01_data", "intermediate", "population", "population_master.csv"), fileEncoding = "cp932", row.names = FALSE)
  
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