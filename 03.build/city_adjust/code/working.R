main <- function(){
  
  all_working_data <- read_df_csv("covariates", "working")
  
  df_id_name <- readxl::read_excel("02.raw/municipalities_list/df_id_name.xlsx") |> 
    dplyr::mutate(
      city_id = as.character(city_id)
    )
  
  #国勢調査以降に合併した市町村については、データ処理の観点から除外している。
  adjust_df <- adjust_data()
  
  current_cityid_list <- city_id_list20(adjust_df)
  
  fin_data <- purrr::map(current_cityid_list, adjust_city_id, all_working_data, adjust_df) |> 
    bind_rows()
  
  df_working <- fin_data |> 
    dplyr::left_join(df_id_name) |> 
    dplyr::select(
      city_id,
      city_name,
      prefecture_name,
      dplyr::everything()
    )
  
  
  df_a <- df_working |> 
    dplyr::filter(
      is.na(workforce_pop) | is.na(working_pop) | is.na(student_pop)
    )
    
    dplyr::mutate(
      across(c(workforce_pop, working_pop, student_pop),
             ~dplyr::if_else(is.na(.), 0, .))
    )
  
  save_df_csv(df_working, "city_adjust", "working")
  
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

# id_n = 40231

adjust_city_id <- function(id_n, all_working_data, adjust_df){
  
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

  df_id_n <- all_working_data |> 
    dplyr::filter(city_id %in% list_id) |> 
    dplyr::mutate(
      year = as.character(year),
      city_id = as.character(city_id)
    ) |> 
    dplyr::mutate(
      across(c(workforce_pop, working_pop, student_pop),
      as.numeric
      ) 
    )
  
  output_data <- df_id_n |> 
    dplyr::reframe(
      dplyr::across(where(is.numeric), sum),
      .by = year) |> 
    dplyr::mutate(
      city_id = id_n)
  
  
  return(output_data)
  
}




