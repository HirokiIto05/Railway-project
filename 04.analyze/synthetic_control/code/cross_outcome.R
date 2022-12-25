main <- function(){
  
  treatment_data <- load_csv("complete", "treatment_data.csv")
  
  treatment_id_lists <- unique(treatment_data$city_id)
  
  five_lag_outcome_all <- purrr::map(treatment_id_lists, read_synth_data, 
                                     treatment_data) %>% 
    dplyr::bind_rows()
  
  five_lag_id_only <- unique(five_lag_outcome_all$city_id)
  
  add_length_data <- add_length(treatment_data, five_lag_outcome_all,
                                five_lag_id_only)
  
  plot_based_data <- add_cross_cov(treatment_data, add_length_data)
    
  write.csv(five_lag_outcome_all, file = here::here('04.analyze', 'synthetic_control',
                                                    'data', 'five_lag_data', 'five_lag.csv'),
            fileEncoding = "CP932", row.names = FALSE)

}


read_synth_data <- function(id_n, treatment_data){
  print(id_n)
  file_name <- paste0(id_n, '.rds')
  synth_data <- readRDS(here::here('04.analyze', 'synthetic_control', 
                                   'figure', 'synth_cov', 'table', file_name))
  
  treatment_one <- treatment_data %>% 
    dplyr::filter(city_id == id_n)
  
  int_year <- unique(treatment_one$treatment_year)
  
  five_lag_outcome <- synth_data %>%
    tidysynth::grab_synthetic_control() %>% 
    dplyr::mutate(city_id = id_n, .before = time_unit) %>% 
    dplyr::filter(time_unit == int_year + 5) %>% 
    dplyr::mutate(diff = synth_y - real_y) %>% 
    dplyr::distinct()
  
  return(five_lag_outcome)
  
}

add_length <- function(treatment_data, five_lag_outcome_all,
                       five_lag_id_only){
  
  five_lag_treatment <- treatment_data %>% 
    dplyr::filter(city_id %in% five_lag_id_only) %>% 
    dplyr::distinct(city_id, line_name)
  
  line_name_only <- unique(five_lag_treatment$line_name)
  
  line_length <- data.frame(line_name = line_name_only,
                            length = c(140, 14.7, 20.9, 27.2,
                                       20.4, 61.0, 12.7,
                                       7.3, 46.2,35.3, 20.9))
  
  id_length_name_data <- dplyr::left_join(treatment_data, line_length) %>% 
    dplyr::select(city_id, city_name, line_name, length)
  
  add_length_treatment_data <- dplyr::left_join(five_lag_outcome_all_test, id_length_name_data) 
  
  return(add_length_treatment_data)
  
}

add_cross_cov <- function(treatment_data, add_length_data){
  
  cov_only_data <- treatment_data %>% 
    dplyr::select(city_name, city_id, year, treatment_year, line_name, total,
                  middle, children_household_percent, own_household_percent,
                  train_pop_percent)
  
  cov_mean_data <- cov_only_data %>% 
    group_by(city_id) %>% 
    summarise(own_percent_mean = mean(own_household_percent, na.rm = TRUE),
              total_mean = mean(total, na.rm = TRUE),
              middle_mean = mean(middle, na.rm = TRUE),
              children_mean = mean(children_household_percent, na.rm = TRUE),
              train_mean = mean(train_pop_percent, na.rm = TRUE),
              treatment_year = treatment_year)
  
  output_data <- left_join(add_length_treatment_data, cov_mean_data)
  
  return(output_data)
  
}


