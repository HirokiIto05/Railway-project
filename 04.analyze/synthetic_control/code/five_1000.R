main <- function(){
  
  treatment_data <- load_csv('complete', 'treatment_data.csv') %>% 
    dplyr::filter(treatment_year <= 2010,
                  city_id != 21403,
                  city_id != 21421)  
  
  treatment_name_lists <- unique(treatment_data$city_name)
  
  
  five_year_table <- purrr::map(treatment_name_lists, create_five_df, treatment_data) %>% 
    dplyr::bind_rows()
  
  
}


print(city_name_list)


create_five_df <- function(city_name_t, treatment_data){
  print(city_name_t)
  
  treatment_one <- treatment_data %>% 
    dplyr::filter(city_name == city_name_t) 
  
  int_year <- unique(treatment_one$treatment_year)
  
  file_name <- paste0(city_name_t, ".rds")
  
  base_plot <- readRDS(here::here('04.analyze','synthetic_control', 'figure',
                                  'synth_cov', 'density_1000','table', file_name))
  
  synth_table <- base_plot %>%
    grab_synthetic_control() %>% 
    dplyr::filter(time_unit == int_year + 5)
  
  return(synth_table)
  
}


create_bar <- function(data){
  
  
  synth_five_bar <- ggplot(data, mapping = aes(x = city_name,))
  
  
  
  
  
  
}



