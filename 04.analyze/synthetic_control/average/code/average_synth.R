main <- function(){
  
  main_data <- readxl::read_xlsx(here::here('02.raw','main','main_data.xlsx')) %>% 
    dplyr::select(-old_id, -old_name)
  
  treatment_data <- load_csv('complete', 'treatment_data.csv') %>%
    dplyr::filter(treatment_year <= 2010,
                  city_id != 21403,
                  city_id != 21421)  
  
  treatment_ave <- timing_ave %>% 
    dplyr::mutate(id = 1, .before = timing)
  
  control_data <- read_control_data()
  
  master_data <- dplyr::bind_rows(treatment_ave, control_data) %>% 
    group_by(id) %>%
    dplyr::mutate(cut_off = ifelse((-1 < timing), 1, 0),
                  .after = timing) 
  
  
}

treatment_year_arrange <- treatment_data %>% 
  arrange(treatment_year) %>% 
  relocate(treatment_year, .after = year) %>% 
  relocate(line_name, .before = city_name) %>% 
  relocate(year, .before = city_id) 



read_control_data <- function(){
  
  num_list <- sort(rep(2:127, 25))
  
  control_data <- load_csv('complete', 'control_data.csv') %>% 
    dplyr::filter(passenger <= 1) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(city_id) %>% 
    dplyr::mutate(timing = seq(-12,12), .after = year) 
  
  
  control_id_list <- unique(control_data$city_id)
  
  output_data <- purrr::map(control_id_list, calculate_control,
                            control_data) %>% 
    dplyr::bind_rows()
  
  output_data <- output_data %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(id = num_list, .before = timing) %>%
    dplyr::select(id, timing, outcome_percent,
                  children_household_percent,
                  own_household_percent, 
                  train_pop_percent, 
                  workforce_percent,
                  student_percent,
                  houseyear_pop_percent)
  
  colnames(output_data) <-c("id", "timing",
                            "outcome",
                            "children_covariate",
                            "own_covariate",
                            "train_covariate",
                            "workforce_covariate",
                            "student_covariate",
                            "houseyear_covariate")
  
  return(output_data)
}


map_synth <- function(master_data){
  
  tictoc::tic()
  
  synth_data <- master_data %>% 
    dplyr::mutate(id = as.character(id),
                  rep_outcome = outcome)
  
  
  output_synth <- synth_data %>%
    
    synthetic_control(
      outcome = outcome, 
      unit = id, 
      time = timing, 
      i_unit = "1", 
      i_time = 0, 
      generate_placebos=T 
    ) %>%
    
    generate_predictor(time_window = -12:-1,
                       children = mean(children_covariate, na.rm = TRUE),
                       own = mean(own_covariate, na.rm = TRUE),
                       workforce = mean(workforce_covariate, na.rm = TRUE),
                       student = mean(student_covariate, na.rm = TRUE),
                       train = mean(train_covariate, na.rm = TRUE)) %>% 
    
    generate_predictor(time_window = -12:-1,
                       outcome_start_year = mean(rep_outcome)) %>%
    # generate_predictor(time_window = -12,
    #                    outcome_start_year = rep_outcome) %>%
    # generate_predictor(time_window = int_year - 6,
    #                    outcome_five_year = rep_outcome) %>%
    # generate_predictor(time_window = int_year -1,
    #                    cigsale_last_year = rep_outcome) %>%
    
    generate_weights(optimization_window = -12:- 1, 
                     margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6
    ) %>% 
    generate_control()

  p <-  output_synth %>%
    plot_trends() +
    theme_bw(base_family = "HiraKakuPro-W3") +
    labs(title = "Average 19 municiparities", y = "population",
         x = "year") 
  
  p
  
  table_name <- "average.rds"
  
  png_name <- "average.png"
  
  file_name_figure <- paste0(here::here('04.analyze','synthetic_control',
                                        'average', 'figure', png_name))
  
  file_name_table <- paste0(here::here('04.analyze','synthetic_control',
                                       'average', 'figure', table_name))
  
  ggsave(p, filename = file_name_figure)
  
  saveRDS(output_synth, file = file_name_table)
  
  tictoc::toc()
  
  return(output_synth)
}


synth_ready <- function(id_n, master_data){
  
  treatment_ready <- master_data %>% 
    dplyr::filter(city_id == id_n)
  
  int_year <- unique(treatment_ready$treatment_year)
  
  int_treatment_num <- treatment_ready %>% 
    dplyr::filter(year == int_year) %>% 
    dplyr::distinct(middle) 
  
  base_num <- unique(int_treatment_num$middle)
  
  treatment_ready <- treatment_ready %>% 
    dplyr::mutate(outcome_percent = middle/base_num)
  
  control_ready <- master_data %>% 
    dplyr::filter(dummy == 0) 
  
  control_city_id <- unique(control_ready$city_id)
  
  control_ready <- purrr::map(control_city_id, calculate_control, control_ready,  int_year) %>% 
    dplyr::bind_rows()
  
  synth_base_data <- dplyr::bind_rows(treatment_ready, control_ready)
  
  return(synth_base_data)
  
}

calculate_control <- function(id_c, control_ready){
  
  control_one <- control_ready %>% 
    dplyr::filter(city_id == id_c)
  
  int_control_num <- control_one %>% 
    dplyr::filter(year == 2007) %>% 
    dplyr::distinct(middle)
  
  base_num <- unique(int_control_num$middle)
  
  control_one <- control_one %>% 
    dplyr::group_by(city_id) %>% 
    dplyr::mutate(outcome_percent = middle/base_num,
                  .after = middle)
  
  return(control_one)
  
}


# install.packages("tidysynth")
library(tidysynth)
library(ggplot2)
library(grDevices)

