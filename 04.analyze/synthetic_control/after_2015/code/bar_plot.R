main <- function(){
  
  treatment_data <- load_csv('complete', 'treatment_data.csv') %>% 
    dplyr::filter(treatment_year <= 2015,
                  city_id != 21403,
                  city_id != 21421,
                  city_id != 17205,# 珠洲市
                  city_id != 17463,#能登町
    )  
  
  treatment_name_lists <- unique(treatment_data$city_name)

  diff_all_data <- purrr::map(treatment_name_lists, read_plot,
                              treatment_data) %>% 
    dplyr::bind_rows()
  
  
  
}



read_plot <- function(city_name_t, treatment_data){
  
  print(city_name_t)
  
  file_name <- paste0(city_name_t, ".rds")
  
  base_plot <- readRDS(here::here('04.analyze','synthetic_control',
                                  'after_2015','table', file_name))
  
  title_id <- as.character(city_name_t)
  
  treatment_one <- treatment_data %>% 
    dplyr::filter(city_name == city_name_t)
  
  int_year = unique(treatment_one$treatment_year)
  
  bar_base_data <- base_plot %>% 
    grab_synthetic_control() %>% 
    dplyr::mutate(diff = real_y - synth_y) %>% 
    dplyr::mutate(treatment_year = int_year,
                  .after = time_unit) %>% 
    dplyr::mutate(city_name = city_name_t)
  
  return(bar_base_data)
  
}

control_city_name <- control_data %>% 
  dplyr::distinct(city_name)



create_bar_plot <- function(year_i, diff_all_data){
  
  year_bar_df <- diff_all_data %>% 
    mutate(after = time_unit - treatment_year + 1) %>% 
    dplyr::filter(after == 5)
  
  output_plot <- ggplot(year_bar_df, aes(x = reorder(city_name, diff), y = diff)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    theme_gray(base_family = "HiraKakuPro-W3") 
    
  output_plot
    
    geom_line(aes(y = real_y)) +
    geom_line(aes(y = synth_y), linetype="dashed", size = 0.5) +
    geom_point(aes(x = time_unit, y = real_y), size = 1.1)+
    geom_vline(xintercept = int_year - 0.5, linetype = "dotted", size = 0.8) +
    labs(title = title_id,
         y = "population") +
    theme_gray(base_family = "HiraKakuPro-W3") +
    theme(plot.title = element_text(size = 13),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    scale_y_continuous(breaks = seq(0.6,1.4, 0.2)) +
    scale_x_continuous(breaks = seq(1995,2015,5))
  
  
  # pdf_name <- paste0(city_name_t, ".png")
  # 
  # file_name_figure <- paste0(here::here('04.analyze','synthetic_control',
  #                                       'after_2015',
  #                                       'figure', pdf_name))
  
  # ggsave(output_plot, filename = file_name_figure)
  
  
}
