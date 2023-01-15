treatment_variable <- treatment_data %>% 
  dplyr::select(city_id, city_name, region_name,
                year, treatment_year, total,
                line_name, middle, dummy,
                children_household_percent,
                own_household_percent,
                workforce_percent,
                student_percent,
                houseyear_pop_percent,
                train_pop_percent)


line_name_list <- unique(treatment_variable$line_name)

line_name_list

add_cross_cov <- function(treatment_data, add_length_data){
  
  treatment_cov_df <- treatment_data %>% 
    dplyr::select(city_name, city_name, year, treatment_year, line_name, total,
                  middle, children_household_percent, own_household_percent,
                  train_pop_percent, houseyear_pop_percent)
  
  
  colnames(treatment_data)
  
  cov_mean_data <- treatment_cov_df %>% 
    group_by(city_name) %>% 
    summarise(own_percent_mean = mean(own_household_percent, na.rm = TRUE),
              total_mean = mean(total, na.rm = TRUE),
              middle_mean = mean(middle, na.rm = TRUE),
              children_mean = mean(children_household_percent, na.rm = TRUE),
              train_mean = mean(train_pop_percent, na.rm = TRUE),
              treatment_year = treatment_year) %>% 
    dplyr::distinct()
  
  
  
  
  # output_data <- left_join(add_length_data, cov_mean_data) %>% 
  #   dplyr::distinct()
  
  return(output_data)
  
}


main_city_df <- readxl::read_xlsx(here::here('02.raw',
                                             'main_city_distance.xlsx'))


cross_variable_df <- dplyr::left_join(cross_df, main_city_distance)



ten_df_variable <- dplyr::left_join(ten_year_bar_df,
                                    treatment_variable)



cross_df <- left_join(cov_mean_data, treatment_df)



treatment_variable <- dplyr::left_join(treatment_variable, 
                                       length_df)


treatment_df <- treatment_data %>% 
  dplyr::select(city_name, line_name) %>% 
  dplyr::left_join(length_df) %>% 
  dplyr::distinct()




line_name <- c("江差線", "ふるさと銀河線", "南部縦貫鉄道線", "十和田観光電鉄線", "七尾線",
               "八百津線", "可部線", "島原鉄道線", "高千穂線")

track_length <- c(42.1, 140.0, 20.9, 14.7, 20.4, 7.3, 46.2, 35.3, 29.1)

length_df <- data.frame(line_name = line_name,
                        track_length = track_length)


# write.csv(treatment_variable, file = here::here('04.analyze', 'synthetic_control',
#                                                 'add_outcome_predictor', 'length_treatment',
#                                                 'data.csv'),
#           fileEncoding = "CP932")


length_data <- read.csv(here::here('02.raw', 'length',
                                   'length.csv'), 
                        fileEncoding = "CP932")
