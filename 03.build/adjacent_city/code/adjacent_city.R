main <- function() {

  df_city_id <- read_xls("02.raw/population/japanese/2020.xls", 
                         col_names = FALSE) |> 
    select(1, 3) |> 
    tidyr::drop_na()
  
  colnames(df_city_id) <- c("city_id", "city_name")
  
  df_adjucent_city <- read_xlsx("02.raw/adjacent_cities/adjacent_city.xlsx") |> 
    dplyr::left_join(df_city_id, by = c("adjacent_city" = "city_name"))
  
  df_adjucent_city <- df_adjucent_city |> 
    rename(adjacent_id = city_id) |> 
    mutate(adjacent_id = as.numeric(adjacent_id)) |> 
    mutate(adjacent_id = stringr::str_sub(adjacent_id, start = 1, end = -2))
  
  save_df_csv(df_adjucent_city, "adjacent_city", "adjacent_city")

}



  
