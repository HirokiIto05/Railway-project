main <- function(){
  
  treatment_name_lists <- master_data %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(dummy == 1) %>% 
    dplyr::distinct(city_name) %>% 
    unlist() %>% 
    as.character()
  
  city_name_list <- unique(adjust_after_df$city_name)
  
  ff <- purrr::map(city_name_list, calculate_outcome, adjust_after_df) %>% 
    dplyr::bind_rows() %>% 
    distinct()
  
}


create_average_df <- function(){
  
  
  treatment_data <- master_data %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(after = year - treatment_year + 1,
                  .after = year) %>% 
    dplyr::filter(dummy == 1 & after %in% seq(-12,12))
  
  treatment_interim <- purrr::map(treatment_name_lists, calculate_outcome, treatment_data) %>% 
    dplyr::bind_rows() 
  
  treatment_outcome <- treatment_interim %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(after, dummy) %>% 
    dplyr::summarise(outcome_percent = mean(outcome_percent, na.rm = TRUE)) %>% 
    dplyr::mutate(city_id = "treatment_average", .before = after)
  
  
  control_data <- master_data %>%
    dplyr::filter(dummy == 0) %>% 
    dplyr::group_by(city_name) %>% 
    dplyr::mutate(after = seq(-12, 12),
                  .after = year) %>% 
    dplyr::ungroup()
  
  control_name_list <- unique(control_data$city_name)
  
  control_interim <- purrr::map(control_name_list, calculate_outcome,
                                control_data) %>% 
    dplyr::bind_rows() 
    
  control_outcome <- control_interim %>% 
    dplyr::select(city_id, after, dummy, outcome_percent) %>% 
    dplyr::mutate(city_id = as.character(city_id))
  
  
  base_average_df <- dplyr::bind_rows(treatment_outcome, 
                                      control_outcome)

  return(base_average_df)
  
}


synth_data <- dplyr::left_join(base_average_df, covariate_mean)

calculate_outcome<- function(city_name_n, data){
  
  data_one <- data %>% 
    dplyr::filter(city_name == city_name_n) %>% 
    dplyr::select(city_id, city_name, middle, year, 
                  after, treatment_year, dummy)
  
  base_num <- data_one %>% 
    dplyr::filter(after == 0) %>% 
    dplyr::distinct(middle) %>% 
    unlist() %>% 
    as.numeric()
  
  output_data <- data_one %>% 
    dplyr::mutate(outcome_percent = middle/base_num,
                  .after = year)
  
  return(output_data)
  
}


add_cov <- function(treatment_outcome, control_outcome, treatment_data, control_data){
  
  covariate_mean_t <- treatment_data %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(dummy) %>% 
    dplyr::summarise(children_mean = mean(children_household_percent, na.rm = TRUE),
                     own_mean = mean(own_household_percent, na.rm = TRUE),
                     train_mean = mean(train_pop_percent, na.rm = TRUE),
                     workforce_mean = mean(workforce_percent,na.rm = TRUE),
                     student_mean = mean(student_percent,na.rm = TRUE),
                     houseyear_mean =   mean(houseyear_pop_percent,na.rm = TRUE)) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(city_id = "treatment_average", .before = dummy) 
  
  
  covariate_c <- control_data %>% 
    dplyr::select(city_id,
                  after,
                  dummy,
                  children_household_percent,
                  own_household_percent,
                  train_pop_percent,
                  workforce_percent,
                  student_percent,
                  houseyear_pop_percent) %>% 
    dplyr::mutate(city_id = as.character(city_id))
    
  colname_t <- c(colnames(covariate_c))
  colname_t <- colname_t[-2]
  
  colnames(covariate_mean_t) <- colname_t
  
  treatment_ready <- dplyr::left_join(treatment_outcome, covariate_mean_t)
  
  control_ready <- dplyr::left_join(control_outcome, covariate_c)
  
  synth_data <- dplyr::bind_rows(treatment_ready, 
                             control_ready)
  
  return(synth_df)
  
}

synth_data <- synth_data %>% 
  dplyr::mutate(rep_outcome = outcome_percent) %>% 
  dplyr::ungroup()


output_synth <- synth_data %>%
  
  synthetic_control(
    outcome = outcome_percent, 
    unit = city_id, 
    time = after, 
    i_unit = "treatment_average", 
    i_time = 1, 
    generate_placebos=T 
  ) %>%
  
  
  
  generate_predictor(time_window = -12:0,
                     children = mean(children_household_percent, na.rm = TRUE),
                     own = mean(own_household_percent, na.rm = TRUE),
                     workforce = mean(workforce_percent, na.rm = TRUE),
                     student = mean(student_percent, na.rm = TRUE),
                     train = mean(train_pop_percent, na.rm = TRUE),
                     house_20year = mean(houseyear_pop_percent, na.rm =TRUE),
                     population = mean(rep_outcome, na.rm = TRUE)) %>%
  
  generate_weights(optimization_window = -12:0, 
                   margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6
  ) %>% 
  generate_control()


p <-  output_synth %>%
  plot_trends() +
  theme_bw(base_family = "HiraKakuPro-W3") +
  labs(title = "Average", y = "population",
       x = "after") 

p

table_name <- paste0("average",'.rds')

file_name_table <- paste0(here::here('04.analyze','synthetic_control',
                                     'add_outcome_predictor',
                                     'table', table_name))

# ggsave(p, filename = file_name_figure)

saveRDS(output_synth, file = file_name_table)


weight_plot <- output_synth %>%
  tidysynth::plot_weights() +
  # labs(title = title_name,
  #      y = "population",
  #      caption = 'figure 3') +
  theme_bw(base_family = "HiraKakuPro-W3") 
# theme(legend.position = 'none')

# weight_plot

weight_file_name <- paste0("average",".png")

weight_file_name <- paste0(here::here('04.analyze','synthetic_control',
                                      'add_outcome_predictor', 'weight',
                                      weight_file_name))

ggsave(weight_plot, filename = weight_file_name)



