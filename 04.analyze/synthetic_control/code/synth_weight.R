main <- function(){
  
  treatment_data <- load_csv("complete", "treatment_data.csv")
  
  treatment_id_lists
  
  purrr::map(treatment_id_lists, save_synth_weight, treatment_data)
  
}


save_synth_weight <- function(id_n, treatment_data){
  
  print(id_n)
  file_name <- paste0(id_n, '.rds')
  synth_data <- readRDS(here::here('04.analyze', 'synthetic_control', 
                                   'figure', 'synth_cov', 'table', file_name))
  
  treatment_one <- treatment_data %>% 
    dplyr::filter(city_id == id_n) 
  
  city_name_n <- unique(treatment_one$city_name)
  
  plot_name <- paste0(city_name_n, ".pdf")
  save_folder_name <- here::here('04.analyze', 'synthetic_control','figure',
                                 'synth_cov','weight', plot_name)
  
  synth_weight <- synth_data %>% 
    plot_weights()
  
  ggsave(synth_weight, file = save_folder_name)
  
  # return(five_lag_outcome)
}

treatment_id_lists

id_n = 1481

print(id_n)
file_name <- paste0(id_n, '.rds')
synth_data <- readRDS(here::here('04.analyze', 'synthetic_control', 
                                 'figure', 'synth_cov', 'table', file_name))

library(forcats)

synth_data %>% plot_weights()

synth_data %>% grab_balance_table()

synth_data %>% plot_placebos()

synth_data %>% grab_synthetic_control()

