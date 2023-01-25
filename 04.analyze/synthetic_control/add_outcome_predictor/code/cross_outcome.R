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
  
  treatment_data <- treatment_data %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(city_id, year) %>% 
    dplyr::mutate(elderly = sum(r65_69,
                                r70_74,
                                r75_79,
                                r80_over),
                  .after = r80_over) %>% 
    dplyr::mutate(aging_rate = elderly/total,
                  .after = elderly)   
  
  
  treatment_name_lists <- unique(treatment_data$city_name)
  
  covariate_df <- add_cross_cov(treatment_data)
  
  five_ten_table <- read.csv(here::here('04.analyze', 'synthetic_control',
                                        'add_outcome_predictor',
                                        'five_ten_table', 'five_ten_table.csv'),
                             fileEncoding = "CP932")
  
  all_data_df <- dplyr::left_join(five_ten_table, covariate_df)
  
  write.csv(all_data_df, file = here::here('04.analyze',
                                           'synthetic_control',
                                           'add_outcome_predictor',
                                           'cross_plot_base',
                                           'Jan_24_data.csv'),
            fileEncoding = "CP932", row.names = FALSE)
  
  
  
  five_df <- diff_all_data %>% 
    mutate(after = time_unit - treatment_year + 1) %>% 
    dplyr::filter(after == 5)
  
  ten_df <- diff_all_data %>% 
    mutate(after = time_unit - treatment_year + 1) %>% 
    dplyr::filter(after == 10)
  
  df_name_list <- unique(five_df$city_name)
  
  plot_based_data <- add_cross_cov(treatment_data, add_length_data)
  
  plot_based_data <- read.csv(file = here::here('04.analyze',
                                                'synthetic_control',
                                                'add_outcome_predictor',
                                                'cross_plot_base',
                                                'Jan_24_data.csv'),
                              fileEncoding = "CP932")
  
  
  fci_df <- change_fci(treatment_name_lists)
  
  plot_based_data <- dplyr::left_join(plot_based_data, fci_df) %>% 
    dplyr::relocate(region_name, .after = city_name)
  
  write.csv(plot_based_data, file = here::here('04.analyze',
                                               'synthetic_control',
                                               'add_outcome_predictor',
                                               'cross_plot_base',
                                               'Jan_25_data.csv'),
            fileEncoding = "CP932", row.names = FALSE)
  
  
}



change_fci <- function(treatment_name_lists){
  
  fci_data <- read.csv(here::here('03.build',
                                  'power',
                                  'data', 'fci_data.csv'),
                       fileEncoding = "CP932")
  
  fci_average_df <- fci_data %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(city_name, region_name) %>%
    dplyr::summarise(fci_mean = mean(FCI, na.rm = TRUE))
    
  fci_city_name <- c("江差町", "上ノ国町", "訓子府町",
                     "置戸町", "本別町", "足寄町",
                     "陸別町", "七戸町", "六戸町",
                     "輪島市", "八百津町", "安芸太田町",
                     "南島原市", "高千穂町", "日之影町")
  
  adjust_df <- data.frame("city_name" = treatment_name_lists,
                          "city_name_fci" = fci_city_name)
  
  fci_treatment_df <- fci_average_df %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(city_name %in% fci_city_name) %>% 
    dplyr::rename(city_name_fci = city_name) %>% 
    dplyr::left_join(adjust_df) %>% 
    dplyr::relocate(city_name, .before = city_name_fci) %>% 
    dplyr::select(-city_name_fci)
  
  return(fci_treatment_df)
  
}



add_cross_cov <- function(treatment_data){
  
  line_name <- c("江差線", "ふるさと銀河線", "南部縦貫鉄道線", "十和田観光電鉄線", "七尾線",
                 "八百津線", "可部線", "島原鉄道線", "高千穂線")
  
  track_length <- c(42.1, 140.0, 20.9, 14.7, 20.4, 7.3, 46.2, 35.3, 29.1)
  
  length_df <- data.frame(line_name = line_name,
                          track_length = track_length)
  
  cov_only_data <- treatment_data %>% 
    dplyr::select(city_name, city_name, line_name, year, treatment_year, line_name, total,
                  middle, children_household_percent, own_household_percent,
                  train_pop_percent, houseyear_pop_percent, aging_rate)
  
  colnames(treatment_data)
  
  cov_mean_data <- cov_only_data %>% 
    group_by(city_name) %>% 
    summarise(line_name = line_name,
              own_percent_mean = mean(own_household_percent, na.rm = TRUE),
              total_mean = mean(total, na.rm = TRUE),
              middle_mean = mean(middle, na.rm = TRUE),
              children_mean = mean(children_household_percent, na.rm = TRUE),
              train_mean = mean(train_pop_percent, na.rm = TRUE),
              treatment_year = treatment_year,
              aging_mean = mean(aging_rate, na.rm = TRUE))
  
  output_data <- left_join(cov_mean_data, length_df) %>% 
    dplyr::distinct()
  
  return(output_data)
  
}

data <- after_df

library(forcats)
