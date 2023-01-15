main <- function(){
  
  treatment_data <- load_csv('complete', 'treatment_data.csv') %>% 
    dplyr::filter(treatment_year <= 2015,
                  city_name != "揖斐郡大野町",
                  city_name != "本巣郡北方町",
                  city_name != "珠洲市",
                  city_name != "能登町",
                  city_name != "十和田市"
    )   
  
  treatment_name_lists <- distinct(treatment_data,city_name) %>% 
    unlist() %>% 
    as.character()
  
  
  purrr::map(treatment_name_lists, synth_weight, treatment_data)  
  
}


synth_weight <- function(name_t, treatment_data){
  
  print(name_t)
  file_name <- paste0(name_t, '.rds')
  synth_data <- readRDS(here::here('04.analyze', 'synthetic_control',
                                   'add_outcome_predictor', 'table', file_name))
  
  # synth_data <- readRDS(here::here('04.analyze','synthetic_control', 'figure',
  #                                  'synth_cov', 'density_1000','table', file_name))
  # 
  treatment_one <- treatment_data %>% 
    dplyr::filter(city_name == name_t)
  
  int_year <- unique(treatment_one$treatment_year)
  
  city_name_t <- unique(treatment_one$city_name)
  
  # title_name <- paste0("Placebo test ","'", city_name_t,"'")
  
  weight_plot <- synth_data %>%
    tidysynth::plot_weights() +
    # labs(title = title_name,
    #      y = "population",
    #      caption = 'figure 3') +
    theme_bw(base_family = "HiraKakuPro-W3") 
    # theme(legend.position = 'none')
  
  pdf_name <- paste0(city_name_t,".png")
  
  file_name_figure <- paste0(here::here('04.analyze','synthetic_control',
                                        'add_outcome_predictor', 'weight',
                                        pdf_name))
  # 
  # file_name_figure <- paste0(here::here('04.analyze','synthetic_control',
  #                                       'figure','synth_cov',
  #                                       'density_1000','placebo', pdf_name))
  
  ggsave(weight_plot, filename = file_name_figure)
  
  return(weight_plot)
  
}
