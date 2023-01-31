

main <- function(){
  
  
  placebo_after_all <- purrr::map(treatment_name_lists, create_placebo_average,
                                  treatment_data) %>% 
    dplyr::bind_rows()
  
  placebo_average_pvalue <- placebo_after_all %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(city_name, .id, .placebo) %>% 
    dplyr::summarise(diff_mean = mean(diff, na.rm = TRUE))
  
  
  p_value_df <- purrr::map(treatment_name_lists, calculate_p_value, placebo_average_pvalue) %>% 
    dplyr::bind_rows()
  
  p_value_df_ad <- p_value_df %>% 
    dplyr::mutate(p_value = round(p_value, 3)) %>% 
    dplyr::mutate(Effect = round(Effect, 3)) %>% 
    dplyr::select(-over_num) %>% 
    dplyr::relocate(placebo_all, .after = Effect)
  
  write.csv(p_value_df_ad, 
            here::here('04.analyze',
                       'synthetic_control',
                       'add_outcome_predictor',
                       'pvalue',
                       'p_value_table.csv'),
            fileEncoding = "CP932",
            row.names = FALSE)
  
}


create_placebo_average <- function(city_name_t, treatment_data){
  
  file_name <- paste0(city_name_t, ".rds")
  
  synth_based <- readRDS(here::here('04.analyze', 'synthetic_control',
                                    'add_outcome_predictor', 'table', file_name))
  
  treatment_year_df <- treatment_data %>% 
    dplyr::filter(city_name == city_name_t) %>%
    dplyr::select(city_name, 
                  treatment_year)
  
  
  placebo_data <- synth_based %>% 
    tidysynth::plot_placebos()
  
  placebo_data <- placebo_data$data 
  
  placebo_data <- placebo_data %>% 
    dplyr::mutate(city_name = city_name_t) %>% 
    dplyr::left_join(treatment_year_df) %>% 
    dplyr::relocate(treatment_year, .after = time_unit)
  
  placebo_after_df <- placebo_data %>% 
    dplyr::mutate(after = time_unit - treatment_year +1) %>% 
    dplyr::filter(time_unit >= treatment_year) %>% 
    dplyr::distinct()
  
  
  return(placebo_after_df)
  
  
}


calculate_p_value <- function(city_name_t, placebo_average_pvalue){
  
  treatment_one <- placebo_average_pvalue %>% 
    dplyr::filter(city_name == city_name_t)
  
  placebo_id_list <-unique(treatment_one$.id)
  
  placebo_count <- 0
  
  treated_df <- treatment_one %>% 
    dplyr::filter(.placebo == 0) 
  
  treatment_num <- unique(treated_df$diff_mean)
  
  for(i in placebo_id_list){
    
    placebo_i <- treatment_one%>% 
      dplyr::filter(.id == i)
    
    placebo_num <- unique(placebo_i$diff_mean)
    
    if(abs(treatment_num) < abs(placebo_num)){
      
      placebo_count <- placebo_count + 1
      
    }
    
  }
  
  output_df <- data.frame(city_name = city_name_t,
                          over_num = placebo_count,
                          placebo_all = length(placebo_id_list)) %>% 
    dplyr::mutate(p_value = over_num /placebo_all) %>% 
    dplyr::mutate(Effect = treatment_num)
  
  
  return(output_df)
  
}


