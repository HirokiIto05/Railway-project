main <- function(){
  
  treatment_data <- load_csv('complete', 'treatment_data.csv') %>% 
    dplyr::filter(treatment_year <= 2015,
                  city_name != "揖斐郡大野町",
                  city_name != "本巣郡北方町",
                  city_name != "珠洲市",
                  city_name != "能登町",
                  city_name != "鳳珠郡能登町",
                  city_name != "十和田市",
                  city_name != "行方市"
    )   

  treatment_name_lists <- unique(treatment_data$city_name)
  
  # five_df <- purrr::map(treatment_name_lists, lag_synth_data, 
                                     # treatment_data) %>% 
    # dplyr::bind_rows()
  
  # five_df_name <- unique(five_df$city_name)
  
  
  five_df <- diff_all_data %>% 
    mutate(after = time_unit - treatment_year + 1) %>% 
    dplyr::filter(after == 5)
  
  ten_df <- diff_all_data %>% 
    mutate(after = time_unit - treatment_year + 1) %>% 
    dplyr::filter(after == 10)
  
  df_name_list <- unique(five_df$city_name)
  
  add_length_data <- add_length(treatment_data, five_df,
                                df_name_list)
  
  add_length_data <- add_length(treatment_data, five_df,
                                five_df_name)
  
  plot_based_data <- add_cross_cov(treatment_data, add_length_data)
  
  
    
  # write.csv(five_df, file = here::here('04.analyze', 'synthetic_control',
  #                                                   'data', 'five_lag_data', 'five_lag.csv'),
  #           fileEncoding = "CP932", row.names = FALSE)

}


# lag_synth_data <- function(city_name_t, treatment_data){
#   print(city_name_t)
#   file_name <- paste0(city_name_t, '.rds')
#   synth_data <- readRDS(here::here('04.analyze', 'synthetic_control', 
#                                    'figure', 'synth_cov', 
#                                    'density_1000', 'table', file_name))
#   
#   treatment_one <- treatment_data %>% 
#     dplyr::filter(city_name == city_name_t)
#   
#   int_year <- unique(treatment_one$treatment_year)
#   
#   five_lag_outcome <- synth_data %>%
#     tidysynth::grab_synthetic_control() %>% 
#     dplyr::mutate(city_name = city_name_t, .before = time_unit) %>% 
#     dplyr::filter(time_unit == int_year + 5) %>% 
#     dplyr::mutate(diff = real_y - synth_y) %>% 
#     dplyr::distinct()
#   
#   return(five_lag_outcome)
#   
# }


five_name <-unique(five_df$city_name)

line_name_list <- treatment_data %>% 
  dplyr::filter(city_name %in% five_name) %>% 
  dplyr::distinct(city_name, line_name) 

sortunique(line_name_list$line_name)



line_name_only <- sort(unique(line_name_list$line_name))



line_name <- c("江差線", "ふるさと銀河線", "南部縦貫鉄道線", "十和田観光電鉄線", "七尾線",
               "八百津線", "可部線", "島原鉄道線", "高千穂線")

track_length <- c(42.1, 140.0, 20.9, 14.7, 20.4, 7.3, 46.2, 35.3, 29.1)

length_df <- data.frame(line_name = line_name,
                        track_length = track_length)

add_length <- function(treatment_data, after_df,
                       after_df_name){
  
  line_name_list <- treatment_data %>% 
    dplyr::filter(city_name %in% after_df_name) %>% 
    dplyr::distinct(city_name, line_name)
  
  line_name_only <- sort(unique(line_name_list$line_name))
  
  print(line_name_only)
  
  line_length <- data.frame(line_name = line_name_only,
                            length = c(140, 20.4, 7.3,
                                       20.9, 46.2, 35.3,
                                       29.1, 27.2))
  
  #"ふるさと銀河線" "七尾線"         "八百津線"       "南部縦貫鉄道線"
  # [5] "可部線"         "島原鉄道線"     "高千穂線"       "鹿島鉄道線" 
  
  id_length_name_data <- dplyr::left_join(treatment_data, line_length) %>% 
    dplyr::select(city_name, city_name, line_name, length) %>% 
    dplyr::distinct()
  
  add_length_treatment_data <- dplyr::left_join(after_df, id_length_name_data) 
  
  return(add_length_treatment_data)
  
}

add_cross_cov <- function(treatment_data, add_length_data){
  
  cov_only_data <- treatment_data %>% 
    dplyr::select(city_name, city_name, year, treatment_year, line_name, total,
                  middle, children_household_percent, own_household_percent,
                  train_pop_percent, houseyear_pop_percent)
  
  colnames(treatment_data)
  
  cov_mean_data <- cov_only_data %>% 
    group_by(city_name) %>% 
    summarise(own_percent_mean = mean(own_household_percent, na.rm = TRUE),
              total_mean = mean(total, na.rm = TRUE),
              middle_mean = mean(middle, na.rm = TRUE),
              children_mean = mean(children_household_percent, na.rm = TRUE),
              train_mean = mean(train_pop_percent, na.rm = TRUE),
              treatment_year = treatment_year)
  
  output_data <- left_join(add_length_data, cov_mean_data) %>% 
    dplyr::distinct()
  
  return(output_data)
  
}

data <- after_df

library(forcats)
