main <- function(){
  
  pop_data <- load_data("city_adjust","all_data.csv") %>% 
    dplyr::mutate(city_id = as.character(city_id)) 
  
  main_data <- readxl::read_xlsx(here::here('02.raw','main','main_data.xlsx')) %>% 
    dplyr::select(-old_id, -old_name)
  
  treatment_data <- load_data('master_data', 'treatment_data.csv')
  control_data <- load_data('master_data', 'control_data.csv')
   
  treatment_ready <- add_percent(treatment_data, var = working)
  
  each_synth(treatment_ready, control_data, var = working)

}

load_data <- function(folder_name, file_name){

  output_data <- read.csv(here::here('03.build',folder_name,'data', file_name),
                          fileEncoding = "CP932")
  return(output_data)

}


each_synth <- function(treatment_ready, control_data, var){
  
  var <- rlang::enquo(var)
  
  city_id_lists <- treatment_ready %>% 
    dplyr::distinct(city_id) %>% 
    unlist() %>% 
    as.character()
  
  folder_name <-  rev(colnames(treatment_ready))[2]
  
  purrr::map(city_id_lists, joint_treatment_control, treatment_ready, control_data,
             !!var, folder_name)
  
}

map_synth <- function(id, data, int_year, folder_name){
  
  data <- data %>%
    ungroup()

  synth_data <- data %>%
    group_by(city_id) %>%
    dplyr::mutate(cut_off = ifelse((dummy == 1 & treatment_year <= year), 1, 0),
                  .after = city_id)
  
  synth_data <- synth_data %>%
    dplyr::mutate(percent_cov = percent) 
  
  print(id)

  output_synth <- synth_data %>%
    # initial the synthetic control object
    synthetic_control(
      outcome = percent, # outcome
      unit = city_id, # unit index in the panel data
      time = year, # time index in the panel data
      i_unit = id, # unit where the intervention occurred
      i_time = int_year, # time period when the intervention occurred
      generate_placebos=T # generate placebo synthetic controls (for inference)
    ) %>%

    generate_predictor(time_window = 1995:int_year - 1,
                       cov = mean(percent_cov, na.rm = T)) %>%

  generate_weights(optimization_window = 1995:int_year - 1, 
                   margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6
  ) %>% 
    generate_control()

  print(id)
  
  p <-  output_synth %>%
    plot_trends()

  file_city_name <- data %>% 
    dplyr::filter(dummy == 1) %>% 
    dplyr::distinct(city_id) %>%
    unlist() %>%
    as.character()
  
  table_name <- paste0(file_city_name,'.rds')
  
  pdf_name <- paste0(file_city_name,'.pdf')

  file_name_figure <- paste0(here::here('04.analyze','synthetic_control', 'figure',
                                 'synth_each', folder_name, 'figure', pdf_name))
  
  file_name_table <- paste0(here::here('04.analyze','synthetic_control', 'figure',
                                       'synth_each', folder_name, 'table', table_name))
    
  ggsave(p, filename = file_name_figure)
  
  saveRDS(output_synth, file = file_name_table)

  return(output_synth)
}


joint_treatment_control <- function(id, treatment_ready, control_data, var, folder_name){
  
  var <- rlang::enquo(var)
  
  treatment_data <- treatment_ready %>% 
    dplyr::filter(city_id == id)
  print(id)
  int_year <- treatment_data %>% 
    dplyr::distinct(treatment_year) %>% 
    unlist() %>% 
    as.numeric()
  print(id)
  
  control_list <- unique(control_data$city_id)
    
    
  control_per_df <- purrr::map(control_list, calculate_percent_control, control_data, 
                               int_year, !!var) %>% 
    dplyr::bind_rows()
  
  joint_data <- dplyr::bind_rows(treatment_data, control_per_df)
  
  print(id)
  
  purrr::map(id, map_synth, data = joint_data, int_year, folder_name)

  return(joint_data)
  
}

add_percent <- function(data, var){
  
  var <- rlang::enquo(var)
  
  city_id_lists <- unique(data$city_id)
  
  output_data <- purrr::map(city_id_lists, calculate_percent, data,  !!var) %>% 
    dplyr::bind_rows()
  
  return(output_data)
}


add_percent_control <- function(data, var){
  
  var <- rlang::enquo(var)
  
  city_id_lists <- unique(data$city_id)
  
  output_data <- purrr::map(city_id_lists, calculate_percent_control, data, int_year, !!var) %>% 
    dplyr::bind_rows()
  
  return(output_data)
}

calculate_percent <- function(id, data, var){
  print(id)
  
  var <- rlang::enquo(var)
  
  data <- data %>% 
    dplyr::filter(city_id == id)
  
  int_year <- data %>% 
    dplyr::distinct(treatment_year) %>% 
    unlist() %>% 
    as.numeric()
  
  int_num <- data %>% 
    dplyr::filter(year == int_year) %>% 
    dplyr::distinct(!!var) %>% 
    unlist() %>% 
    as.numeric()
  
  new_data <- data %>% 
    dplyr::filter(city_id == id) %>% 
    dplyr::mutate(percent = !!var/int_num)
  
  
  return(new_data)
  
}

calculate_percent_control <- function(id_control, data, int_year, var){
  
  var <- rlang::enquo(var)
  
  data <- data %>% 
    dplyr::filter(city_id == id_control) %>% 
    dplyr::ungroup()
  
  int_num <- data %>% 
    dplyr::filter(year == int_year) %>% 
    dplyr::distinct(!!var, .keep_all = FALSE) %>% 
    unlist() %>% 
    as.numeric()
  
  
  new_data <- data %>% 
    dplyr::filter(city_id == id_control) %>% 
    dplyr::mutate(percent = !!var/int_num)
    
  return(new_data)
  
}



# install.packages("tidysynth")
library(tidysynth)
