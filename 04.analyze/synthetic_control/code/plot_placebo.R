main <- function(){
  
  treatment_data <- load_csv("complete", "treatment_data.csv")
  
  treatment_id_lists <- unique(treatment_data$city_id)

  purrr::map(treatment_id_lists, synth_placebo, treatment_data)  
  
}


synth_placebo <- function(id_n, treatment_data){
  
  print(id_n)
  file_name <- paste0(id_n, '.rds')
  synth_data <- readRDS(here::here('04.analyze', 'synthetic_control', 
                                   'figure', 'synth_cov', 'table', file_name))
  
  treatment_one <- treatment_data %>% 
    dplyr::filter(city_id == id_n)
  
  int_year <- unique(treatment_one$treatment_year)
  
  city_name_t <- unique(treatment_one$city_name)
  
  title_name <- paste0("Placebo test ","'", city_name_t,"'")
  
  placebo_plot <- synth_data %>%
    tidysynth::plot_placebos() +
    labs(title = title_name,
         y = "population",
         caption = 'figure 3') +
    theme_bw(base_family = "HiraKakuPro-W3") +
    theme(legend.position = 'none')

  pdf_name <- paste0(city_name_t,".png")
  
  file_name_figure <- paste0(here::here('04.analyze','synthetic_control', 'figure',
                                        'placebo', pdf_name))

  ggsave(placebo_plot, filename = file_name_figure)
  
  return(placebo_plot)
  
}
