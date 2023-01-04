main <- function(){
  
  treatment_data <- load_csv("complete", "treatment_data.csv") %>% 
    dplyr::filter(treatment_year <= 2010,
                  city_id != 21403,
                  city_id != 21421)

  treatment_name_lists <- unique(treatment_data$city_name)
  
  five_lag_outcome_all <- purrr::map(treatment_name_lists, lag_synth_data, 
                                     treatment_data) %>% 
    dplyr::bind_rows()
  
  five_lag_name_only <- unique(five_lag_outcome_all$city_name)
  
  add_length_data <- add_length(treatment_data, five_lag_outcome_all,
                                five_lag_name_only)
  
  add_length_ten <- add_length(treatment_data, ten_lag_df,
                               five_lag_name_only)
  
  plot_based_data <- add_cross_cov(treatment_data, add_length_ten)
  
  
    
  write.csv(five_lag_outcome_all, file = here::here('04.analyze', 'synthetic_control',
                                                    'data', 'five_lag_data', 'five_lag.csv'),
            fileEncoding = "CP932", row.names = FALSE)

}


lag_synth_data <- function(city_name_t, treatment_data){
  print(city_name_t)
  file_name <- paste0(city_name_t, '.rds')
  synth_data <- readRDS(here::here('04.analyze', 'synthetic_control', 
                                   'figure', 'synth_cov', 
                                   'density_1000', 'table', file_name))
  
  treatment_one <- treatment_data %>% 
    dplyr::filter(city_name == city_name_t)
  
  int_year <- unique(treatment_one$treatment_year)
  
  five_lag_outcome <- synth_data %>%
    tidysynth::grab_synthetic_control() %>% 
    dplyr::mutate(city_name = city_name_t, .before = time_unit) %>% 
    dplyr::filter(time_unit == int_year + 5) %>% 
    dplyr::mutate(diff = real_y - synth_y) %>% 
    dplyr::distinct()
  
  return(five_lag_outcome)
  
}

add_length <- function(treatment_data, five_lag_outcome_all,
                       five_lag_name_only){
  
  five_lag_treatment <- treatment_data %>% 
    dplyr::filter(city_name %in% five_lag_name_only) %>% 
    dplyr::distinct(city_name, line_name)
  
  line_name_only <- unique(five_lag_treatment$line_name)
  
  print(line_name_only)
  
  line_length <- data.frame(line_name = line_name_only,
                            length = c(140, 20.9, 27.2,
                                       20.4, 61.0,7.3,
                                       46.2,35.3, 20.9))
  
  id_length_name_data <- dplyr::left_join(treatment_data, line_length) %>% 
    dplyr::select(city_name, city_name, line_name, length)
  
  add_length_treatment_data <- dplyr::left_join(five_lag_outcome_all, id_length_name_data) 
  
  return(add_length_treatment_data)
  
}

add_cross_cov <- function(treatment_data, add_length_data){
  
  cov_only_data <- treatment_data %>% 
    dplyr::select(city_name, city_name, year, treatment_year, line_name, total,
                  middle, children_household_percent, own_household_percent,
                  train_pop_percent)
  
  cov_mean_data <- cov_only_data %>% 
    group_by(city_name) %>% 
    summarise(own_percent_mean = mean(own_household_percent, na.rm = TRUE),
              total_mean = mean(total, na.rm = TRUE),
              middle_mean = mean(middle, na.rm = TRUE),
              children_mean = mean(children_household_percent, na.rm = TRUE),
              train_mean = mean(train_pop_percent, na.rm = TRUE),
              treatment_year = treatment_year)
  
  output_data <- left_join(add_length_data, cov_mean_data)
  
  return(output_data)
  
}

data <- five_lag_outcome_all

create_five_bar <- function(data){
  
  data <- data %>% 
    mutate(order = fct_reorder(city_name, diff))
  
  five_bar <- ggplot(data, mapping = aes(x = order, y = diff))+
    geom_bar(stat = "identity") +
    theme_gray(base_family = "HiraKakuPro-W3") +
    labs(x = "City name", y = "Difference", title = "Diff real - synth") +
    coord_flip()
    # theme(axis.text.x = element_text(angle = 270))

  five_bar  
  
  ggsave(five_bar, filename = here::here('04.analyze','synthetic_control', 'figure',
                                           'synth_cov', 'density_1000','bar.png'),
         device = "png",   width = 10, height = 5)
  
  
}

library(forcats)
