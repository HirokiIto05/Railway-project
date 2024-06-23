main <- function() {
  
  df_treatment <- read_df_xlsx('geometry_base', 'geometry_treatment') 
  
  df_current <- read_df_xlsx("geometry_base", "geometry_continue") |> 
    dplyr::select(3, 4, 5, 9, 10)
  colnames(df_current) <- c("line_name", "company_name",
                             "station_name", "city_name", "city_id")
  
  
  df_treatment_mod <- extract_treatment_city(df_current, 
                                             df_treatment) |> 
    dplyr::select(city_id, city_name, prefecture_name, year_end, company_name,
                  line_name, station_name)
  
  save_df_csv(df_treatment_mod, 'geometry_base', 'df_treatment_city')
  
}


read_geometry <- function(file_name_n) {
  
  df_current <- read_df_xlsx("geometry_base", file_name_n)
  
  output_df <- df_current |> 
  
  colnames(output_df) <- c("line_name", "company_name",
                           "station_name", "city_name")
  
  return(output_df)
  
}


extract_treatment_city <- function(df_current, df_treatment) {
  
  df_current_name_id <- df_current |> 
    select(city_name, city_id) |> 
    dplyr::mutate(check = 1) |> 
    distinct() 
  
  df_treat_name_id <- df_treatment |> 
    select(city_name, city_id) |> 
    distinct() 
  
  df_check <- dplyr::left_join(df_treat_name_id, df_current_name_id) |> 
    distinct() |> 
    dplyr::filter(is.na(check)) 
  
  list_current_city <- unique(df_check$city_name)
  
  df_treatment_one <- df_treatment |> 
    dplyr::filter(city_name %in% list_current_city,
                  year_end >= 1999) 
  
  list_city_one_end <- df_treatment_one |> 
    dplyr::distinct(year_end, city_name) |>
    group_by(city_name) |> 
    dplyr::filter(n() == 1) |> 
    distinct(city_name) |> 
    unlist() |> 
    as.character()

  df_output <- df_treatment |> 
    dplyr::filter(city_name %in% list_city_one_end)

  return(df_output)

}

