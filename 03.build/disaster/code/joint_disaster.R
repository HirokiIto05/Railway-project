main <- function() {
  
  df_disaster <- read_disaster_df()
  df_muni <- read_muni_id()
  
  joint_disaster <- left_join(df_muni, df_disaster, by = "city_name") |> 
    dplyr::mutate(city_id = as.numeric(city_id)) |> 
    dplyr::mutate(city_id = str_sub(city_id, start = 1, end = -2))
  

  joint_disaster <- joint_disaster |> 
    dplyr::select(-where)
  
  save_df_csv(joint_disaster, "disaster", "master_disaster")
  
}


read_disaster_df <- function() {
  
  df_higashi <- read_df_xlsx("disaster", "higashi") |> 
    dplyr::mutate(
      where = "higashi"
    ) |>  
    list()
  df_hanshin <- read_df_xlsx("disaster", "hanshin") |> 
    dplyr::mutate(
      where = "hanshin"
    ) |>  
    list()
  df_oita <- read_df_xlsx("disaster", "oita") |> 
    dplyr::mutate(
      where = "oita"
    ) |>  
    list()
  df_kumamoto <- read_df_xlsx("disaster", "kumamoto") |> 
    dplyr::mutate(
      where = "kumomoto"
    ) |>  
    list()
  
  list_disaster <- c(df_higashi, df_hanshin, df_kumamoto, df_oita)
  
  df_disaster <- bind_rows(list_disaster) |> 
    dplyr::mutate(city_name = str_replace(city_name, "一関市※", "一関市"),
                  city_name = str_replace(city_name, "塩竃市", "塩竈市"),
                  city_name = str_replace(city_name, "富谷町", "富谷市"),
                  city_name = str_replace(city_name, "龍ヶ崎市", "龍ケ崎市"),
                  city_name = str_replace(city_name, "川口市※", "川口市"),
                  city_name = str_replace(city_name, "栃木市※", "栃木市"),
                  city_name = str_replace(city_name, "鎌ヶ谷市", "鎌ケ谷市"),
                  city_name = str_replace(city_name, "袖ヶ浦市", "袖ケ浦市"),
                  city_name = str_replace(city_name, "玉東市", "玉東町"),
                  city_name = str_replace(city_name, "和水市", "和水町"),
                  city_name = str_replace(city_name, "八代町", "八代市"),
                  city_name = str_replace(city_name, "山鹿町", "山鹿市"),
                  city_name = str_replace(city_name, "芦北市", "芦北町"),
                  city_name = str_replace(city_name, "相良町", "相良村"),
                  city_name = str_replace(city_name, "久重町", "九重町"))  |> 
    dplyr::filter(!city_name  %in% c("津名町", "淡路町",
                                     "北淡町", "五色町",
                                     "緑町", "西淡町",
                                     "三原町", "南淡町"))
  
  # 合併された地域なので新たに列を加える
  df_merged_city <- tibble("city_name" = c("南あわじ市", "淡路市"),
                           "victims" = c(9999, 9999),
                           "destroyed_all" = c(9999, 9999),
                           "destroyed_half" = c(9999, 9999))
  
  df_output <- df_disaster |> 
    dplyr::bind_rows(df_merged_city)

  return(df_output)
  
}


read_muni_id <- function() {
  
  muni_id <- read_xls("02.raw/municipalities_code/municipalities_code.xls") |> 
    dplyr::select(1, 2, 3)
  
  colnames(muni_id) <- c("city_id", "prefecture_name", "city_name")
  
  muni_id <- muni_id |> 
    tidyr::drop_na(city_name)
  
  return(muni_id)
  
}


