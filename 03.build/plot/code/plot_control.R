
save_plot <-  function(integrated_data){
    
    current_id <- dplyr::distinct(integrated_data, city_id) %>% 
      na.omit() %>% 
      unlist() %>% 
      as.character()
    
    purrr::map(current_id, create_plot, plot_based = integrated_data)
    
  }


plot_based <- integrated_control

based_id <- '02387'

create_plot <- function(based_id, plot_based){
  
  plot_data <- filter(plot_based, city_id == based_id)
  
  # cutoff <- dplyr::distinct(plot_data, treatment_year)
  # cutoff <- as.numeric(unlist(cutoff))
  
  file_city_name <- dplyr::distinct(plot_data, city_name) %>% 
    as.character()
  
  pdf_name <- paste0(file_city_name,'.png')
  
  file_name <- paste0(here::here('04.analyze','figure', 'control','percentage', pdf_name))
  
  save_data <- plot_data
  
  save_plot <- ggplot(save_data, aes(x = year, y = percentage))+
    geom_point(size = 4) +
    labs(title = file_city_name) +
    theme_gray(base_family = "HiraKakuPro-W3",
               base_size = 15)
  
  save_plot 
  
  
  ggsave(save_plot, filename = file_name)
  
}


save_plot(integrated_control)

