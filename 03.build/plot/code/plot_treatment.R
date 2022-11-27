save_plot <-  function(integrated_data){
  
  treatment_data <- load_data("base_percent","base_treatment.csv")
  
  current_id <- dplyr::distinct(treatment_data, city_id) %>% 
    na.omit() %>% 
    unlist() %>% 
    as.character()
  
  treatment_data$city_id <- as.character(treatment_data$city_id)
  
  purrr::map(current_id, create_plot, plot_based = treatment_data)
  
}

load_data <- function(folder_name, file_name){
  
  output_data <- read.csv(here::here('03.build',folder_name,'data',file_name),
                          fileEncoding = "CP932") 
  
  return(output_data)
  
}


plot_based <- treatment_data

based_id <- "17204"

create_plot <- function(based_id, plot_based){
  
  plot_data <- plot_based %>% 
    dplyr::filter(city_id == based_id)
  
  cutoff <- dplyr::distinct(plot_data, treatment_year)
  cutoff <- as.numeric(unlist(cutoff))
  
  file_city_name <- dplyr::distinct(plot_data, city_name) %>% 
    as.character()
  
  pdf_name <- paste0(file_city_name,'.png')
  
  file_name <- paste0(here::here('04.analyze','figure', 'treatment', 'working' , pdf_name))
  
  save_data <- plot_data %>% 
    dplyr::mutate(after = case_when(year >= cutoff ~"1",
                                    year <  cutoff ~ "0"))
  
  
  save_plot <- ggplot(save_data, aes(x = year, y = percent, colour = after))+
    geom_point(size = 4) +
    geom_vline(xintercept = cutoff-0.5) +
    labs(title = file_city_name) +
    theme_gray(base_family = "HiraKakuPro-W3",
             base_size = 15)

  save_plot 
  
  
  ggsave(save_plot, filename = file_name)
  
}


save_plot(integrated_treatment)

