main <- function(){
  
  treatment_data <- load_data("base_treatment.csv")
  control_data <- load_data("base_control.csv")
  
  treatment_id <- treatment_data %>%
    dplyr::filter(treatment_year >= 2003) %>% 
    dplyr::distinct(city_id) %>% 
    unlist() %>% 
    as.character() 


  purrr::map(treatment_id, synth_func, treatment_data, control_data)  
  
  
}



load_data <- function(file_name){
  
  output_data <- read.csv(here::here('03.build','base_percent','data',file_name),
                          fileEncoding = "CP932") 
  return(output_data)
  
}


synth_func <- function(i, treatment_data, control_data){
  
  for (i in treatment_id) {
    
    treatment_only <- treatment_data %>% 
      dplyr::filter(city_id == i)
    
    all_data <- bind_rows(treatment_only, control_data) 
    
    all_data <- all_data %>% 
      dplyr::mutate(treatment = ifelse((treatment_year != 0), 1,0),
                    .after = city_name) %>% 
      ungroup()
    
    all_data <- all_data %>%
      ungroup() %>% 
      group_by(city_id) %>% 
      dplyr::mutate(cut_off = ifelse((treatment == 1 & treatment_year <= year), 1, 0),
                    .after = treatment) %>% 
      tidyr::replace_na(list(cut_off = 0, treatment = 0, treatment_year = 0, 
                             city_name = "NNN"))
    
    tidy_data <- all_data %>% 
      dplyr::filter(city_id != 2407,
                    city_id != 12441,
                    city_id != 15217,
                    city_id != 31325,
                    city_id != 43433) %>% 
      dplyr::filter(year != 1995)
    
    tidy_df <- tidy_data %>% 
      drop_na()
    
    
    
    gsynth.out <- gsynth(percent ~ cut_off, data = tidy_df,
                         index = c("city_id","year"), force = "two-way", 
                         CV = TRUE, r = c(0, 5), se = TRUE, 
                         inference = "parametric", nboots = 1000,
                         parallel = TRUE)
    
    p <- plot(gsynth.out, type = "counterfactual", raw = "none", main="")
    
    file_city_name <- dplyr::distinct(treatment_only, city_name) %>% 
      unlist() %>% 
      as.character() 
    
    pdf_name <- paste0(file_city_name,'.png')
    
    file_name <- paste0(here::here('04.analyze','synthetic_control', 'figure',
                                   'each',pdf_name))
    
    ggsave(p, filename = file_name)
  }
  
}
    

