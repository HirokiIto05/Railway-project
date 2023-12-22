main <- function(){
  
  df_master <- read_df_csv("master", "master_2") 
  
  list_control <- df_c <- read_df_csv("geometry_base", "df_control_city") |> 
    dplyr::distinct(city_id) |> 
    dplyr::pull()
  
  df_master <- df_master |> 
    dplyr::filter(
      treatment == 1 | city_id %in% list_control
    ) |> 
    dplyr::group_by(city_id) |> 
    dplyr::mutate(
      cum_social = cumsum(replace_na(social_rate, 0))
    ) |> 
    dplyr::ungroup()
  
  df_treatment <- df_master |> 
    dplyr::filter(
      treatment == 1
    )
  
  treatment_name_lists <- unique(df_treatment$city_name)
  
  placebo_df <- purrr::map(treatment_name_lists, create_placebo_df, treatment_data) |> 
    dplyr::bind_rows() 
  
  write.csv(placebo_df, here::here('04.analyze','synthetic_control',
                                   'add_outcome_predictor',
                                   'placebo_table', 'placebo_data.csv'),
            fileEncoding = "CP932", row.names = FALSE)
  
}


create_placebo_df <- function(city_name_t, treatment_data){
  
  print(city_name_t)
  
  file_name <- paste0(city_name_t, ".rds")
  
  base_plot <- readRDS(here::here('04.analyze','synthetic_control',
                                  'add_outcome_predictor',
                                  'table', file_name))
  
  placebo_df_each <- base_plot |> 
    tidysynth::plot_placebos()
  
  placebo_df_each <- placebo_df_each$data 
    
  placebo_df_each <- placebo_df_each |>   
    dplyr::rename(city_id = .id)
  
  treatment_one <- treatment_data |> 
    dplyr::filter(city_name == city_name_t)
  
  treated_name <- unique(treatment_one$city_name)
  treated_year <- unique(treatment_one$treatment_year)
  
  output_df <- placebo_df_each |> 
    dplyr::mutate(city_name = treated_name, 
                  treatment_year = treated_year)
  
  return(output_df)
  
}
