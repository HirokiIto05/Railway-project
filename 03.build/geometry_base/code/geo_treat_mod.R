main <- function() {

  geo_based <- read_df_base()
  list_t_city <- readxl::read_xlsx(here::here('02.raw', 'municipalities_list',
                                            'treatment_t.xlsx')) |> 
    dplyr::distinct() |> 
    unlist() |> 
    as.character() 


  geo_t <- read_mod_geo_treat(geo_input = geo_based, list_t_city)
  
  save_df_xlsx(geo_t, 'geometry_base', 'geometry_treatment')
  
}

read_df_base <- function() {
  
  geo_output <- readxl::read_xlsx(here::here('02.raw', 'geometry_data', 
                                             'geo_base_treatment.xlsx')) |> 
    dplyr::select(2,3,5,6,11,12,15,16)
  
  colnames(geo_output) <- c('line_name', 'company_name', 
                           'year_start', 'year_end',
                           'station_name', 'prefecture_name',
                           'city_name', 'city_id')
  
  return(geo_output)
  
}

geo_input = geo_based


read_mod_geo_treat <- function(geo_input, list_t_city) {

  df_based <- geo_input |> 
    dplyr::mutate(company_name = 
                    str_replace_all(company_name, "（旧国鉄）", " "))
  
  df_output <- df_based |> 
    dplyr::filter(line_name %in% list_t_city) |> 
    dplyr::filter(year_end < 9999,
                  # year_end >= 1999,
                  year_end != 999)

  return(df_output)  
}


