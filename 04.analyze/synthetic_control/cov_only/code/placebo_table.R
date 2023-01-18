main <- function(){
  
  treatment_data <- load_csv('complete', 'treatment_data.csv') %>% 
    dplyr::filter(treatment_year <= 2015,
                  city_name != "揖斐郡大野町",
                  city_name != "本巣郡北方町",
                  city_name != "珠洲市",
                  city_name != "鳳珠郡能登町",
                  city_name != "十和田市",
                  city_name != "行方市"
                  )   
  
  
  treatment_name_lists <- unique(treatment_data$city_name)
  
  
  placebo_df <- purrr::map(treatment_name_lists, create_placebo_df, treatment_data) %>% 
    dplyr::bind_rows() 
  
  write.csv(placebo_df, here::here('04.analyze','synthetic_control',
                                   'cov_only',
                                   'placebo_table', 'placebo_data.csv'),
            fileEncoding = "CP932", row.names = FALSE)
  
}


create_placebo_df <- function(city_name_t, treatment_data){
  
  print(city_name_t)
  
  file_name <- paste0(city_name_t, ".rds")
  
  base_plot <- readRDS(here::here('04.analyze','synthetic_control',
                                  'cov_only',
                                  'table', file_name))
  
  placebo_df_each <- base_plot %>% 
    tidysynth::plot_placebos()
  
  placebo_df_each <- placebo_df_each$data 
    
  placebo_df_each <- placebo_df_each %>%   
    dplyr::rename(city_id = .id)
  
  treatment_one <- treatment_data %>% 
    dplyr::filter(city_name == city_name_t)
  
  treated_name <- unique(treatment_one$city_name)
  treated_year <- unique(treatment_one$treatment_year)
  
  output_df <- placebo_df_each %>% 
    dplyr::mutate(city_name = treated_name, 
                  treatment_year = treated_year)
  
  return(output_df)
  
}