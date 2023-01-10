main <- function(){
  
  
  treatment_data <- load_csv('complete', 'treatment_data.csv') %>% 
    dplyr::filter(treatment_year <= 2015,
                  city_id != 21403,
                  city_id != 21421)  

  treatment_name_lists <- unique(treatment_data$city_name)
  
  treatment_name_lists
  
  treated_placebo <- purrr::map(treatment_name_lists, read_synth) %>% 
    dplyr::bind_rows()
  
 write.csv(treated_placebo, 
           file = here::here('04.analyze', 
                             'synthetic_control',
                             'after_2015',
                             'p_value', 'p_value_data.csv'),
           fileEncoding = "CP932",
           row.names = FALSE)
   
}


read_synth <- function(city_name_t){
  
  file_name <- paste0(city_name_t, ".rds")
  
  synth_based <- readRDS(here::here('04.analyze', 'synthetic_control',
                                    'after_2015', 'table', file_name))
  
  placebo_data <- synth_based %>% 
    tidysynth::plot_placebos(prune = FALSE)
  placebo_data
  
  placebo_data <- placebo_data$data
  
  treated_data <- placebo_data %>% 
    dplyr::filter(.placebo == 0)
  
  treated_mean <- calculate_treated_average(treated_data)
  
  cut_num <- unique(treated_mean$diff_mean)
  
  each_placebo <- placebo_data %>% 
    dplyr::filter(.placebo == 1) %>% 
    dplyr::select(.id, time_unit, diff)
  
  
  year_list <- seq(1995, 2019)
  
  placebo_count_df <- purrr::map(year_list, calculate_p_value, treated_mean,
                                 each_placebo, city_name_t) %>% 
    dplyr::bind_rows()
  
  return(placebo_count_df)
  
}

calculate_treated_average <- function(treated_data){
  
  output_data <- treated_data %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(time_unit) %>% 
    dplyr::summarise(diff_mean = mean(diff, na.rm = TRUE))
  
  return(output_data)
  
}


calculate_p_value <- function(year_n, treated_mean, each_placebo,
                              city_name_t){
  
  treatment_year_n <- treated_mean %>% 
    dplyr::filter(time_unit == year_n) %>% 
    dplyr::select(diff_mean) %>% 
    as.numeric()

  
  placebo_id_list <-unique(each_placebo$.id)
  
  placebo_count <- 0
  
  for(i in placebo_id_list){
    
    placebo_i <- each_placebo %>% 
      dplyr::filter(time_unit == year_n,
                    .id == i)
    
    placebo_num <- unique(placebo_i$diff)
    
    
    if(abs(treatment_year_n) < abs(placebo_num)){
      
      placebo_count <- placebo_count + 1
      
    }
    
  }
  
  output_df <- data.frame(year = year_n,
                          over_num = placebo_count,
                          placebo_all = length(placebo_id_list)) %>% 
    dplyr::mutate(p_value = over_num /placebo_all) %>% 
    dplyr::mutate(city_name = city_name_t, .before = year)
  
  
  return(output_df)
  
}


  
