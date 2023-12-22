main <- function(){
  
  # treatment_data <- load_csv('complete', 'treatment_data.csv') |> 
  #   dplyr::filter(treatment_year <= 2015,
  #                 city_name != "揖斐郡大野町",
  #                 city_name != "本巣郡北方町",
  #                 city_name != "珠洲市",
  #                 city_name != "能登町",
  #                 city_name != "十和田市"
  #   )   
  # 
  # treatment_name_lists <- distinct(treatment_data,city_name) |> 
  #   unlist() |> 
  #   as.character()
  
  
  list_data <- list.files("04.analyze/synthetic_control/table_300/data/")
  
  purrr::map(list_data, synth_weight, treatment_data)  
  
}


synth_weight <- function(file_path, treatment_data){
  
  print(file_path)
  
  city_name_t <- stringr::str_sub(file_path, start = 1, end = -5)
  
  # file_name <- paste0(city_name_t, ".rds")
  # 
  # base_plot <- readRDS(here::here('04.analyze','synthetic_control',
  #                                 'add_outcome_predictor','table', file_name))
  
  base_plot <- readRDS(paste0("04.analyze/synthetic_control/new_table/new_table/table/", file_path))
  
  
  # print(city_name_t)
  # file_name <- paste0(city_name_t, '.rds')
  # synth_data <- readRDS(here::here('04.analyze', 'synthetic_control',
  #                                  'add_outcome_predictor', 'table', file_name))
  
  # synth_data <- readRDS(here::here('04.analyze','synthetic_control', 'figure',
  #                                  'synth_cov', 'density_1000','table', file_name))
  # 
  
  # title_name <- paste0("Placebo test ","'", city_name_t,"'")
  
  weight_plot <- base_plot |>
    tidysynth::plot_weights() +
    # labs(title = title_name,
    #      y = "population",
    #      caption = 'figure 3') +
    theme_bw(base_family = "HiraKakuPro-W3") 
    # theme(legend.position = 'none')
  
  pdf_name <- paste0(city_name_t,".png")
  
  file_name_figure <- paste0(here::here('04.analyze','synthetic_control',
                                        'table_300', 'weight', pdf_name))
  # 
  # file_name_figure <- paste0(here::here('04.analyze','synthetic_control',
  #                                       'figure','synth_cov',
  #                                       'density_1000','placebo', pdf_name))
  
  ggsave(weight_plot, filename = file_name_figure)
  
  return(weight_plot)
  
}
