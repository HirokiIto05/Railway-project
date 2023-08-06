main <- function(){
  
  
  treatment_data <- load_csv("complete", "treatment_data.csv") |> 
    dplyr::filter(treatment_year <= 2015,
                  city_id != 21403,
                  city_id != 21421)
  
  unique(treatment_data$city_id)
  
  treatment_name_list <- unique(treatment_data$city_name)
  
  
  treatment_ready <- purrr::map(treatment_name_list, synth_ready, treatment_data) |> 
    dplyr::bind_rows()
  
  master_timing <- purrr::map(treatment_name_list, add_timing,
                                treatment_ready) |> 
    dplyr::bind_rows() |> 
    dplyr::select(city_name, city_id, year,
                  outcome_percent,
                  timing, children_household_percent,
                  own_household_percent,
                  train_pop_percent,
                  workforce_percent,
                  student_percent,
                  houseyear_pop_percent) |> 
    dplyr::filter(timing >= -12 & timing <= 12) 
  
  
  average_treatment <- master_timing |> 
    dplyr::ungroup() |> 
    dplyr::group_by(timing) |> 
    dplyr::summarise(timing = timing,
                     outcome = mean(outcome_percent, 
                                    na.rm = TRUE),
                     children_covariate = mean(children_household_percent, 
                                               na.rm = TRUE),
                     own_covariate = mean(own_household_percent, 
                                          na.rm = TRUE),
                     train_covariate = mean(train_pop_percent, 
                                            na.rm = TRUE),
                     workforce_covariate = mean(workforce_percent, 
                                                na.rm = TRUE),
                     student_covariate = mean(student_percent, 
                                              na.rm = TRUE),
                     houseyear_covariate = mean(houseyear_pop_percent, 
                                                na.rm = TRUE)) |> 
    dplyr::distinct()
  
  
  
  synth_timing <- master_timing |> 
    dplyr::ungroup() |> 
    dplyr::group_by(timing) 
    
}



create_synth_base_timing <- function(data){
  
  dplyr::summarise(timing = timing,
                   children_covariate = mean(children_household_percent, 
                                             na.rm = TRUE),
                   own_covariate = mean(own_household_percent, 
                                        na.rm = TRUE),
                   train_covariate = mean(train_pop_percent, 
                                          na.rm = TRUE),
                   workforce_covariate = mean(workforce_percent, 
                                              na.rm = TRUE),
                   student_covariate = mean(student_percent, 
                                            na.rm = TRUE),
                   houseyear_covariate = mean(houseyear_pop_percent, 
                                              na.rm = TRUE)) |> 
    dplyr::distinct()
  
  
}



test_tt <- master_timing |> 
  dplyr::select(city_name, city_id, year,
                outcome_percent,
                timing, children_household_percent,
                own_household_percent,
                train_pop_percent,
                workforce_percent,
                student_percent,
                houseyear_pop_percent)

timing_num <- seq(-20,17)

sample_table <- purrr::map(timing_num, table_sample_size, 
                           test_tt) |> 
  dplyr::bind_rows()

i = -20

sss <- seq(-20,-8)

hhh <- purrr::map(sss, ggg, sample_table) |> 
  dplyr::bind_rows()

38-25

colnames(test_tt)

max_timing <- test_tt |> 
  dplyr::filter(timing >= -12 & timing <= 12) 

timing_ave <- max_timing |> 
  dplyr::ungroup() |> 
  dplyr::group_by(timing) |> 
  dplyr::summarise(timing = timing,
                   outcome = mean(outcome_percent, 
                                       na.rm = TRUE),
                   children_covariate = mean(children_household_percent, 
                                             na.rm = TRUE),
                   own_covariate = mean(own_household_percent, 
                                        na.rm = TRUE),
                   train_covariate = mean(train_pop_percent, 
                                          na.rm = TRUE),
                   workforce_covariate = mean(workforce_percent, 
                                              na.rm = TRUE),
                   student_covariate = mean(student_percent, 
                                            na.rm = TRUE),
                   houseyear_covariate = mean(houseyear_pop_percent, 
                                              na.rm = TRUE)) |> 
  dplyr::distinct()


ggg <- function(i, sample_table){
  
  sample_num <- sample_table |> 
    dplyr::filter(timing >= i & timing <= i + 24) |> 
    dplyr::summarise(samplesize_sum = sum(sample_size)) |> 
    unlist() |> 
    as.numeric()
  
  output_data <- data.frame(timing = i,
                            samplesize_num = sample_num)
  
  return(output_data)
  
}

table_sample_size <- function(timing_n, test_tt){
  
  timing_one <- test_tt |> 
    dplyr::filter(timing == timing_n)
  
  sample_size_n <- nrow(timing_one)
  
  output_data <- data.frame(timing = timing_n,
                            sample_size = sample_size_n)
  
  return(output_data)
  
}

for(i in seq(-20, 17)){
  
  timing_n <- test_tt |> 
    dplyr::filter(timing == i)
  
}


synth_ready <- function(name_t, treatment_data){
  
  treatment_ready <- treatment_data |>
    dplyr::filter(city_name == name_t)
  
  int_year <- unique(treatment_ready$treatment_year)
  
  int_treatment_num <- treatment_ready |> 
    dplyr::filter(year == int_year) |> 
    dplyr::distinct(middle) 
  
  base_num <- unique(int_treatment_num$middle)
  
  treatment_ready <- treatment_ready |> 
    dplyr::mutate(outcome_percent = middle/base_num,
                  .after = middle)
  
  return(treatment_ready)
}


add_timing <- function(city_name_t, treatment_ready){
  
  treatment_one <- treatment_ready |> 
    dplyr::filter(city_name == city_name_t)
  
  int_year <- unique(treatment_one$treatment_year)
  
  start_num <- 1995 - int_year
  end_num <- 2019 - int_year
  
  timing_list <- seq(start_num, end_num)
  
  output_data <- treatment_one |> 
    dplyr::mutate(timing = timing_list,
                  .after = year)
    
  return(output_data)

}
