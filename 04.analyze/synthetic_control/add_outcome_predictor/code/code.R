main <- function(){
  
  df_master <- read_df_csv("master", "master_2") 
  
  list_control <- df_c <- read_df_csv("geometry_base", "df_control_city") |> 
    dplyr::distinct(city_id) |> 
    dplyr::pull()
  
  df_master <- df_master |> 
    dplyr::filter(
      treatment == 1 | city_id %in% list_control
    )

  treatment_id_lists <- df_master |> 
    dplyr::filter(treatment == 1) |> 
    dplyr::distinct(city_id) |> 
    dplyr::pull(city_id)
  
  # df_below_ave <- create_robust_df(df_master)
  
  purrr::map(treatment_id_lists, map_synth, df_master)
  
}

create_robust_df <- function(df_master) {
  
  df_average <- df_master |> 
    dplyr::group_by(city_id) |> 
    dplyr::reframe(
      average_total = mean(total, na.rm = TRUE)
    )
  
  list_quantile_total <- quantile(df_average$average_total)
  
  list_above_ave <- df_average |> 
    dplyr::filter(
      average_total <= list_quantile_total[3]
    ) |> 
    dplyr::pull(city_id)
  
  df_master_robust <- df_master |> 
    dplyr::filter(
      city_id %in% list_above_ave
    )
  
  return(df_master_robust)
  
}



id_n <- 21421

map_synth <- function(id_n, df_master)　{
  
  tictoc::tic()
  
  synth_data <- df_master 
  
  treatment_one <- synth_data |> 
    dplyr::filter(city_id == id_n) 
  
  year_end_n <- unique(treatment_one$year_end)
  
  print(id_n)
    
  ss <- synth_data |>
    dplyr::ungroup() |> 
    dplyr::filter(treatment == 0 | city_id == id_n) |> 
    distinct() 
    dplyr::filter(!city_id  %in% c(7367, 39303, 39304, 40231, 4216)) 
  
  
  df_nan_check <- ss |> 
    dplyr::filter( 
      year >= 1996 & year <= year_end_n + 1
      ) |> 
    dplyr::summarise(
      children = mean(household_with_children_rate, na.rm = TRUE),
      own = mean(own_household_rate, na.rm = TRUE),
      workforce = mean(workforce_rate, na.rm = TRUE),
      student = mean(student_rate, na.rm = TRUE),
      old_house = mean(old_house_rate, na.rm = TRUE),
      # population = mean(rep_outcome, na.rm = TRUE)
      .by = c(city_id, treatment)
    ) 
  
  list_in <- df_nan_check |> 
    tidyr::drop_na() |> 
    dplyr::distinct(city_id) |> 
    dplyr::pull()
  
  df_treatment <- df_nan_check |> 
    dplyr::filter(
      treatment == 1
    )
  
  ss <- ss |> 
    dplyr::filter(city_id %in% list_in) |> 
    dplyr::select(
      -c(destroyed_all, destroyed_half)
    ) |> 
    distinct() |> 
    dplyr::group_by(city_id) |> 
    dplyr::mutate(
      cum_social = cumsum(replace_na(social_rate, 0))
    ) |> 
    dplyr::mutate(rep_outcome = cum_social) |> 
    dplyr::ungroup()
  
  
  # main synth ####
  
  if (id_n == 1361) {
    
    
    output_synth <- ss |>
      
      synthetic_control(
        outcome = cum_social, 
        unit = city_id, 
        time = year, 
        i_unit = id_n, 
        i_time = year_end_n,
        generate_placebos = F
      ) |>
      
      generate_predictor(
        time_window = 1995:year_end_n + 1,
        children = mean(household_with_children_rate, na.rm = TRUE),
        # own = mean(own_household_rate, na.rm = TRUE),
        workforce = mean(workforce_rate, na.rm = TRUE),
        student = mean(student_rate, na.rm = TRUE),
        old_house = mean(old_house_rate, na.rm = TRUE),
        population = mean(rep_outcome, na.rm = TRUE)
        ) |>
      
                         # population = mean(rep_outcome, na.rm = TRUE)) |>
      # generate_predictor(time_window = year_end_n + 1,
      #                    pre_year_outcome = rep_outcome) |> 
      
      generate_weights(
        optimization_window = 1995:year_end_n + 1,
        margin_ipop = .02, sigf_ipop = 7, bound_ipop = 6
      ) |>
      generate_control()
    
  } else if (id_n == 1362){
    
    output_synth <- ss |>
    
    synthetic_control(
      outcome = cum_social, 
      unit = city_id, 
      time = year, 
      i_unit = id_n, 
      i_time = year_end_n,
      generate_placebos = F
    ) |>
    
    generate_predictor(
      time_window = 1995:year_end_n + 1,
      children = mean(household_with_children_rate, na.rm = TRUE),
      own = mean(own_household_rate, na.rm = TRUE),
      # workforce = mean(workforce_rate, na.rm = TRUE),
      student = mean(student_rate, na.rm = TRUE),
      old_house = mean(old_house_rate, na.rm = TRUE),
      population = mean(rep_outcome, na.rm = TRUE)
    ) |>
    
    # population = mean(rep_outcome, na.rm = TRUE)) |>
    # generate_predictor(time_window = year_end_n + 1,
    #                    pre_year_outcome = rep_outcome) |> 
    
    generate_weights(
      optimization_window = 1995:year_end_n + 1,
      margin_ipop = .02, sigf_ipop = 7, bound_ipop = 6
    ) |>
    generate_control()
    
  } else if(id_n == 1648) {
    
    output_synth <- ss |>
    
    synthetic_control(
      outcome = cum_social, 
      unit = city_id, 
      time = year, 
      i_unit = id_n, 
      i_time = year_end_n,
      generate_placebos = F
    ) |>
    
    generate_predictor(
      time_window = 1995:year_end_n + 1,
      children = mean(household_with_children_rate, na.rm = TRUE),
      own = mean(own_household_rate, na.rm = TRUE),
      workforce = mean(workforce_rate, na.rm = TRUE),
      # student = mean(student_rate, na.rm = TRUE),
      old_house = mean(old_house_rate, na.rm = TRUE),
      population = mean(rep_outcome, na.rm = TRUE)
    ) |>
    
    # population = mean(rep_outcome, na.rm = TRUE)) |>
    # generate_predictor(time_window = year_end_n + 1,
    #                    pre_year_outcome = rep_outcome) |> 
    
    generate_weights(
      optimization_window = 1995:year_end_n + 1,
      margin_ipop = .02, sigf_ipop = 7, bound_ipop = 6
    ) |>
    generate_control()
  }
  
  print(id_n)
  
  city_name_t <- ss |> 
    dplyr::filter(
      treatment == 1
    ) |> 
    dplyr::distinct(city_name) |> 
    dplyr::pull()
  
  table_name <- paste0(city_name_t,'.rds')
  
  file_name_table <- paste0(here::here('04.analyze/synthetic_control/add_outcome_predictor/table/total_cities_without_old_house/',
                                       table_name))
  
  
  saveRDS(object =  output_synth,
          file = file_name_table)
  
  tictoc::toc()
  
  return(output_synth)
}


synth_ready <- function(id_n, df_master){
  
  treatment_ready <- df_master |> 
    dplyr::filter(city_id == id_n)
  
  # year_end_n <- unique(treatment_ready$year_end)
  year_end_n <- treatment_ready |> 
    dplyr::distinct(year_end) |> 
    dplyr::pull()
  
  int_treatment_num <- treatment_ready |> 
    dplyr::filter(year == year_end_n + 1) |> 
    dplyr::distinct(total) 
    # dplyr::distinct(middle) 
  
  # base_num <- unique(int_treatment_num$middle)
  # base_num <- unique(int_treatment_num$total)
  
  
  
  treatment_ready <- treatment_ready |> 
    dplyr::mutate(outcome_percent = total/base_num)
    # dplyr::mutate(outcome_percent = middle/base_num)
  
  control_ready <- df_master |> 
    dplyr::filter(treatment == 0) 
  
  control_city_id <- unique(control_ready$city_id)
  
  control_ready <- purrr::map(control_city_id, 
                              calculate_control, 
                              control_ready, 
                              year_end_n) |> 
    dplyr::bind_rows()
  
  synth_base_data <- dplyr::bind_rows(treatment_ready, control_ready)
  
  return(synth_base_data)
  
}


calculate_control <- function(id_c, control_ready, year_end_n){
  
  control_one <- control_ready |> 
    dplyr::filter(city_id == id_c)
  
  int_control_num <- control_one |> 
    dplyr::filter(year == year_end_n - 1) |> 
    dplyr::pull(total)
    # dplyr::pull(middle)
  
  control_one <- control_one |> 
    dplyr::group_by(city_id) |> 
    dplyr::mutate(outcome_percent = total / int_control_num)
    # dplyr::mutate(outcome_percent = middle/int_control_num)
  
  return(control_one)
  
}


# lag_covariates <- function(synth_data){
#   
#   year_former_list <- seq(1995,1999)
#   year_latter_list <- seq(2000, 2019)
#   
#   output_data <- synth_data |> 
#     dplyr::filter(year %in% year_former_list) |> 
#     dplyr::mutate(children_household_percent = dplyr::lag(children_household_percent),
#                   own_household_percent = dplyr::lag(own_household_percent), 
#                   workforce_percent = dplyr::lag(workforce_percent), 
#                   student_percent = dplyr::lag(student_percent))
#   
#   latter_df <- synth_data |> 
#     dplyr::filter(year %in% year_latter_list)
#   
#   joint_data <- dplyr::bind_rows(output_data, latter_df) |> 
#     dplyr::filter(year != 1995)
#   
#   return(joint_data)
#   
# }


# install.packages("tidysynth")
library(tidysynth)
library(ggplot2)
library(patchwork)
