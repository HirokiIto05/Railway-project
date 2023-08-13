df_pop <- read_df_csv("aggregate", "pop")

output_data <- adjust_df |> 
  dplyr::select(id_muni2020) |> 
  distinct() |> 
  dplyr::rename(
    city_id = id_muni2020
  )

city_data <- df_pop |> 
  dplyr::filter(year == 2020) |> 
  dplyr::select(city_id, city_name, prefecture_name) |> 
  dplyr::distinct()


df_name_id <- output_data |> 
  dplyr::left_join(city_data) |> 
  dplyr::mutate(
    city_id = as.character(city_id)
  )


openxlsx::write.xlsx(df_name_id, "02.raw/municipalities_code/df_id_name.xlsx")

  
  
  
  
  