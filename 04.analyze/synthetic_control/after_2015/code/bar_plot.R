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
  
  
  write.csv(diff_all_data, here::here('04.analyze',
                                      'synthetic_control',
                                      'after_2015',
                                      'diff_ten_data',
                                      'diff_ten_data.csv'),
            row.names = FALSE, fileEncoding = "CP932")
  
  
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



output_plot_five

create_bar_plot <- function(year_i, diff_all_data){
  
  year_bar_df <- diff_all_data %>% 
    mutate(after = time_unit - treatment_year + 1) %>% 
    dplyr::filter(after == 5)
  
  output_plot_five <- ggplot(year_bar_df, aes(x = reorder(city_name, diff), y = diff)) +
    geom_bar(stat = "identity", width = 0.8) +
    coord_flip() +
    labs(title = "Five year later population",
         y = "Real Outcome ー Counterfactual Outcome") +
    theme_gray(base_family = "HiraKakuPro-W3") +
    theme(axis.title.y = element_blank(),
          plot.title = element_text(size = 13),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10)) +
    ylim(c(-0.1, 0.1))
    
  output_plot_five
  
  file_name_figure <- paste0(here::here('04.analyze','synthetic_control',
                                        'after_2015',
                                        'bar_chart', "Five_year_later_bar.png"))
  
  ggsave(output_plot_five, filename = file_name_figure, width = 4.5, height = 3.2)
  
    
  ten_year_bar_df <- diff_all_data %>% 
    mutate(after = time_unit - treatment_year + 1) %>% 
    dplyr::filter(after == 10)
  
  output_plot_ten <- ggplot(ten_year_bar_df, aes(x = reorder(city_name, diff), y = diff)) +
    geom_bar(stat = "identity", width = 0.8) +
    coord_flip() +
    labs(title = "Ten year later population",
         y = "Real Outcome ー Counterfactual Outcome") +
    theme_gray(base_family = "HiraKakuPro-W3") +
    theme(axis.title.y = element_blank(),
          plot.title = element_text(size = 13),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10)) +
    ylim(c(-0.1, 0.1))
  
  output_plot_ten
  
  mean(year_bar_df$diff)*100
  mean(ten_year_bar_df$diff)*100

  file_name_figure <- paste0(here::here('04.analyze','synthetic_control',
                                        'after_2015',
                                        'bar_chart', "Ten_year_later_bar.png"))
  
  ggsave(output_plot_ten, filename = file_name_figure, width = 4.5, height = 3.2 *(13/17))
  
  
}
