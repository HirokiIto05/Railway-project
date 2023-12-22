main <- function(){
  
  child_household_data <- read_df_csv("covariates", "children") 
  
  df_id_name <- readxl::read_excel("02.raw/municipalities_list/df_id_name.xlsx") |> 
    dplyr::mutate(
      city_id = as.character(city_id)  
    )
  
  #国勢調査以降に合併した市町村については、データ処理の観点から除外している。
  adjust_df <- adjust_data() 
  
  current_cityid_list <- city_id_list20(adjust_df)
  
  fin_data <- purrr::map(current_cityid_list, adjust_city_id, child_household_data, adjust_df, df_id_name) |> 
    bind_rows()
  
  str(fin_data)
  
  df_children <- fin_data |> 
    dplyr::left_join(df_id_name) |> 
    dplyr::select(
      city_id,
      city_name,
      prefecture_name,
      dplyr::everything()
    ) 
    dplyr::mutate(
      dplyr::across(c(children_household, children_pop), 
                    ~ifelse(is.na(.), 0, .)),
    )
    
    df_a <- df_children |> 
      dplyr::filter(
        is.na(children_household) | is.na(children_pop)
      )
    
  save_df_csv(df_children, "city_adjust", "children")
  
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


adjust_city_id <- function(id_n, child_household_data, adjust_df, df_id_name){
  
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
  
  
  df_id_n <- child_household_data |> 
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
  
  # output_data <- setDT(df_id_n)[, .(
  #   old_house_household = sum(old_house_household, na.rm = TRUE),
  #   old_house_pop = sum(old_house_pop, na.rm = TRUE)
  # ), by = year] |> 
  #   dplyr::mutate(
  #     city_id = city_id_n
  #   ) 

  df_id_n <- child_household_data |> 
    dplyr::filter(city_id %in% list_id) |> 
    dplyr::mutate(
      year = as.character(year),
      city_id = as.character(city_id)
    )
  
  output_data <- df_id_n |> 
    dplyr::reframe(
      dplyr::across(where(is.numeric), sum),
      .by = year) |> 
    dplyr::mutate(
      city_id = id_n)
  
  return(output_data)
  
}


library(data.table)
