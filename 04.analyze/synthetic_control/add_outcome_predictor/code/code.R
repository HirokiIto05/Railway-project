main <- function(){
  
  main_data <- readxl::read_xlsx(here::here('02.raw','main','main_data.xlsx')) %>% 
    dplyr::select(-old_id, -old_name)
  
  treatment_data <- load_csv('complete', 'treatment_data.csv') %>% 
    dplyr::filter(treatment_year <= 2015,
                  city_id != 21403,
                  city_id != 21421,
                  city_id != 17205,# 珠洲市
                  city_id != 17463,#能登町
                  city_id != 2206 #十和田市
                  )  
  
  test_df <- treatment_data %>% 
    dplyr::filter(city_name == "檜山郡江差町") %>%
    dplyr::select(city_name, city_id, year, treatment_year,
                  total, social, social_rate, natural, natural_rate, 
                  change, change_rate) %>% 
    dplyr::mutate(percent = (social/total)*100) %>% 
    dplyr::mutate(cumulative = cumsum(percent))
  
  
  unique(treatment_data$city_name)
  
  control_data <- load_csv('complete', 'control_data.csv') %>% 
    dplyr::filter(passenger <= 1) %>% 
    dplyr::filter(city_id != 1208, #北見市
                  city_id != 1457, #上川町,
                  city_id != 1644, #池田町,
                  city_id != 1649, #十勝郡浦幌町
                  city_id != 1668, #白糠郡白糠町
                  ) 
  
  treatment_id_lists <- unique(treatment_data$city_id)
  
  master_data <- dplyr::bind_rows(treatment_data, control_data) %>% 
    group_by(city_id) %>%
    dplyr::mutate(cut_off = ifelse((dummy == 1 & treatment_year <= year), 1, 0),
                  .after = city_id) 
  
  purrr::map(treatment_id_lists, map_synth, master_data)
  
}

id = 1361



map_synth <- function(id_n, master_data){
  
  tictoc::tic()
  
  synth_data <- synth_ready(id_n, master_data) %>% 
    dplyr::relocate(outcome_percent, .after = year)
  
  synth_data <- synth_data %>% 
    dplyr::mutate(rep_outcome = outcome_percent) 
    # dplyr::select(city_id, cut_off,
    #               city_name, region_name,
    #               year, treatment_year, total,
    #               middle, outcome_percent,
    #               rep_outcome,dummy,
    #               children_household_percent, 
    #               own_household_percent,
    #               workforce_percent,
    #               student_percent, 
    #               houseyear_pop_percent,
    #               train_pop_percent)
    # 
  treatment_one <- synth_data %>% 
    dplyr::filter(dummy == 1) 
  int_year <- unique(treatment_one$treatment_year)
  
  print(id_n)
  
  output_synth <- synth_data %>%
    
    synthetic_control(
      outcome = outcome_percent, 
      unit = city_id, 
      time = year, 
      i_unit = id_n, 
      i_time = int_year, 
      generate_placebos=T 
    ) %>%
    
    

    generate_predictor(time_window = 1995:int_year - 1,
                       children = mean(children_household_percent, na.rm = TRUE),
                       own = mean(own_household_percent, na.rm = TRUE),
                       workforce = mean(workforce_percent, na.rm = TRUE),
                       student = mean(student_percent, na.rm = TRUE),
                       train = mean(train_pop_percent, na.rm = TRUE),
                       house_20year = mean(houseyear_pop_percent, na.rm =TRUE)) %>%
    
    generate_predictor(time_window = 1995,
                       outcome_start = rep_outcome) %>%
    generate_predictor(time_window = int_year - 5,
                       outcome_middle_year = rep_outcome) %>%
    generate_predictor(time_window = int_year - 2,
                       outcome_last_year = rep_outcome) %>%
    
    generate_weights(optimization_window = 1995:int_year - 1, 
                     margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6
    ) %>% 
    generate_control()
  
  print(id)
  
  city_name_t <- unique(treatment_one$city_name)
  
  p <-  output_synth %>%
    plot_trends() +
    theme_bw(base_family = "HiraKakuPro-W3") +
    labs(title = city_name_t, y = "population",
         x = "year") 
  
  p
  
  file_city_name <- synth_data %>% 
    dplyr::filter(dummy == 1) %>% 
    dplyr::distinct(city_id) %>%
    unlist() %>%
    as.character()
  
  table_name <- paste0(city_name_t,'.rds')
  
  # pdf_name <- paste0( city_name_t,'.png')
  
  # file_name_figure <- paste0(here::here('04.analyze','synthetic_control','after_2015_outcome',
                                        # 'figure', pdf_name))
  
  file_name_table <- paste0(here::here('04.analyze','synthetic_control', 'after_2015',
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
  
  weight_file_name <- paste0(city_name_t,".png")
  
  weight_file_name <- paste0(here::here('04.analyze','synthetic_control',
                                        'after_2015', 'weight',
                                        weight_file_name))
  
  ggsave(weight_plot, filename = weight_file_name)
  
  tictoc::toc()
  
  return(output_synth)
}


synth_ready <- function(id_n, master_data){
  
  treatment_ready <- master_data %>% 
    dplyr::filter(city_id == id_n)
  
  int_year <- unique(treatment_ready$treatment_year)
  
  int_treatment_num <- treatment_ready %>% 
    dplyr::filter(year == int_year - 1) %>% 
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

calculate_control <- function(id_c, control_ready, int_year){
  
  control_one <- control_ready %>% 
    dplyr::filter(city_id == id_c)
  
  int_control_num <- control_one %>% 
    dplyr::filter(year == int_year - 1) %>% 
    dplyr::distinct(middle)
  
  base_num <- unique(int_control_num$middle)
  
  control_one <- control_one %>% 
    dplyr::group_by(city_id) %>% 
    dplyr::mutate(outcome_percent = middle/base_num)
  
  return(control_one)
  
}


# lag_covariates <- function(synth_data){
#   
#   year_former_list <- seq(1995,1999)
#   year_latter_list <- seq(2000, 2019)
#   
#   output_data <- synth_data %>% 
#     dplyr::filter(year %in% year_former_list) %>% 
#     dplyr::mutate(children_household_percent = dplyr::lag(children_household_percent),
#                   own_household_percent = dplyr::lag(own_household_percent), 
#                   workforce_percent = dplyr::lag(workforce_percent), 
#                   student_percent = dplyr::lag(student_percent))
#   
#   latter_df <- synth_data %>% 
#     dplyr::filter(year %in% year_latter_list)
#   
#   joint_data <- dplyr::bind_rows(output_data, latter_df) %>% 
#     dplyr::filter(year != 1995)
#   
#   return(joint_data)
#   
# }


# install.packages("tidysynth")
library(tidysynth)
library(ggplot2)
library(grDevices)

