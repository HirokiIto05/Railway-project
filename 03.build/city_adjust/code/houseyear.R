main <- function(){
  
  library(data.table)
  
  houseyear_household_data <- read_df_csv("covariates", "houseyear") 
  
  df_id_name <- read.xlsx("02.raw/municipalities_list/df_id_name.xlsx")
  
  colnames(houseyear_household_data)
  
  #国勢調査以降に合併した市町村については、データ処理の観点から除外している。
  adjust_df <- adjust_data() 
    # dplyr::filter(id_muni2020 != 4216, #2016
    #               id_muni2020 != 40231, #2018
    #               id_muni2020 != 3216, #2014
    #               id_muni2020 != 11246, #2012
    #               id_muni2020 != 12239, #2013
    #               id_muni2020 != 17212, #2011
    #               id_muni2020 != 23238, #2012
    #               id_muni2020 != 43100) #2012
  
  current_cityid_list <- city_id_list20(adjust_df)
  
  fin_data <- purrr::map(current_cityid_list, adjust_city_id, houseyear_household_data, adjust_df, 
                         df_id_name) |> 
    bind_rows()
  
  fin_data_a <- fin_data |> 
    dplyr::left_join(df_id_name)
  
  save_df_csv(fin_data_a, "city_adjust", "old_house")
  
}


adjust_data <- function(){
  
  output_data <- readxl::read_xlsx("02.raw/municipality_converter/municipality_converter_jp.xlsx")
  
  return(output_data)
}


city_id_list20 <- function(data){
  
  output_data <- data |> 
    dplyr::select(id_muni2020) |> 
    distinct() |> 
    unlist() |> 
    as.character()
  
  return(output_data)
  
}

adjust_city_id <- function(id_n, houseyear_household_data, 
                           adjust_df,
                           df_id_name){
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
  
  
  df_id_n <- houseyear_household_data |> 
    dplyr::filter(city_id %in% list_id) |> 
    dplyr::mutate(
      year = as.character(year),
      city_id = as.character(city_id)
    )
  
  city_data <- df_id_name |>
    dplyr::filter(city_id == id_n) |> 
    dplyr::select(city_id, city_name, prefecture_name)
  
  city_id_n <- city_data |> 
    dplyr::pull(city_id)
  city_name_n <- city_data |> 
    dplyr::pull(city_name)
  prefecture_name_n <- city_data |> 
    dplyr::pull(prefecture_name)
  
  
  
  df_id_n <- df_id_n |> 
    dplyr::mutate(old_house_household = gsub(old_house_household,
                                             pattern = ",",
                                             replacement = ""),
                  old_house_pop = gsub(old_house_pop,
                                             pattern = ",",
                                             replacement = "")) |> 
    dplyr::mutate(old_house_household = as.numeric(old_house_household),
                  old_house_pop = as.numeric(old_house_pop))
  
  output_data <- setDT(df_id_n)[, .(
    old_house_household = sum(old_house_household, na.rm = TRUE),
    old_house_pop = sum(old_house_pop, na.rm = TRUE)
  ), by = year] |> 
    dplyr::mutate(
      city_id = city_id_n
    ) 
  
  
  
  return(output_data)
  
}




save_table <- function(data, file_name){
  
  write.csv(data, here::here('03.build', 'city_adjust', 'data', file_name),
            fileEncoding = "CP932", row.names = FALSE)
  
}


