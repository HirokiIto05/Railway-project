main <- function(){
  
  
  treatment_data <- load_csv('complete', 'treatment_data.csv') %>% 
    dplyr::filter(treatment_year <= 2015,
                  city_id != 21403,
                  city_id != 21421)  

  treatment_id_lists <- unique(treatment_data$city_id)
  
  
  treated_data <- purrr::map(treatment_id_lists, read_synth)
  
  
}

asdf <- readRDS("/Users/ito_hiroki/01.Research/Railway-project/04.analyze/synthetic_control/cov_only/table/上北郡七戸町.rds") %>% 
  tidysynth::plot_placebos()

asdf_data <- asdf$data
  


read_synth <- function(city_name_t){
  
  file_name <- paste0(city_name_t, ".rds")
  
  synth_based <- readRDS(here::here('04.analyze', 'synthetic_control',
                             'after_2015', 'table', file_name))
  
  placebo_data <- synht_based$data
  
  treated_data <- placebo_data %>% 
    dplyr::filter(.placebo == 0)
  
  treated_mean <- calculate_treated_average(treated_data)
  
  each_placebo <- placebo_data %>% 
    dplyr::filter(.placebo == 1) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(.id) %>% 
    dplyr::summarise(diff_mean = mean(diff, na.rm = TRUE))
  
  only_over <- each_placebo %>% 
    filter(mean_diff <= )
  
  
}

calculate_treated_average <- function(treated_data){
  
  output_data <- treated_data %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(time_unit) %>% 
    dplyr::summarise(diff_mean = mean(diff, na.rm = TRUE))
  
  return(output_data)
  
}


calculate_p_value <- function(treated_ave){
  
  
  
  
  
  
}

  
  
