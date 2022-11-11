save_plot <-  function(integrated_data){
  
  integrated_data <- read.csv(here::here('03.build','age_analysis','data','age_treatment.csv'),
                               fileEncoding = "CP932")
  
  current_id <- dplyr::distinct(integrated_data, city_id) %>% 
    na.omit() %>% 
    unlist() %>% 
    as.character()
  
  purrr::map(current_id, create_plot, plot_based = integrated_data)
  
}


create_plot <- function(based_id, plot_based){
  
  plot_data <- filter(plot_based, city_id == based_id)
  
  cutoff <- dplyr::distinct(plot_data, treatment_year)
  cutoff <- as.numeric(unlist(cutoff))
  
  file_city_name <- dplyr::distinct(plot_data, city_name) %>% 
    as.character()
  
  pdf_name <- paste0(file_city_name,'.png')
  
  file_name <- paste0(here::here('04.analyze','figure', 'treatment', 'young_percent' , pdf_name))
  
  save_data <- plot_data %>% 
    dplyr::mutate(after = case_when(year >= cutoff ~"1",
                                    year <  cutoff ~ "0"))
  
  
  save_plot <- ggplot(save_data, aes(x = year, y = young_percent, colour = after))+
    geom_point(size = 4) +
    geom_vline(xintercept = cutoff-0.5) +
    labs(title = file_city_name) +
    theme_gray(base_family = "HiraKakuPro-W3",
             base_size = 15)

  save_plot 
  
  
  ggsave(save_plot, filename = file_name)
  
}


save_plot(integrated_treatment)

