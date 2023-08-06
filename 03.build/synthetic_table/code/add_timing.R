main <- function(){
  
  treatment_data <- load_data("master_data","treatment_data.csv") |> 
    dplyr::mutate(timing = year - treatment_year)
  treatment_synth <- load_data("synthetic_table","all_synth_data.csv")
  
  timing_data <- add_variable(treatment_data, treatment_synth)
  
  sample_n_df <- purrr::map(num_list, timing_sample, data = test) |> 
    tibble()
  
  
}



load_data <- function(folder_name, file_name){
  
  output_data <- read.csv(here::here('03.build',folder_name,'data', file_name),
                          fileEncoding = "CP932")
  return(output_data)
  
}



add_variable <- function(treatment_data, treatment_synth){
  
  treatment_year_only <- treatment_data |> 
    dplyr::select(city_id, year, treatment_year, timing) |> 
    dplyr::mutate(city_id = as.character(city_id))
  
  output_data <- treatment_synth |> 
    dplyr::ungroup()
  
  output_data <- output_data |> 
    dplyr::mutate(city_id = as.character(city_id))
  
  output_data <- output_data |> 
    left_join(treatment_year_only, by = c("city_id", "time_unit" = "year"))
  
  
  output_data <- output_data |> 
    dplyr::ungroup() |> 
    dplyr::mutate(difference = real_y - synth_y) |> 
    dplyr::group_by(timing) |> 
    dplyr::summarise(mean_diff = mean(difference),
                     mean_real = mean(real_y),
                     mean_synth = mean(synth_y))
  
  return(output_data)

}


save_table <- function(data){
  
  file_name <- "added_timing_data.csv"
  
  write.csv(data, here::here('03.build','synthetic_table', 'table',file_name),
            fileEncoding = "CP932", row.names = FALSE)
  
  
}

real <- output_data |> 
  dplyr::select(timing, mean_real) |> 
  dplyr::rename(mean = mean_real) |> 
  dplyr::mutate(treatment = 1)

synth <- output_data |> 
  dplyr::select(timing, mean_synth) |> 
  dplyr::rename(mean = mean_synth) |> 
  dplyr::mutate(treatment = 0)


test <- dplyr::bind_rows(real,synth)

ggplot(test, aes(x = timing, y = mean, colour = treatment)) +
  geom_point() +
  geom_line() 
                     

ggplot(output_data, aes(x = timing, y = mean_diff)) +
  geom_point() +
  geom_line() 




