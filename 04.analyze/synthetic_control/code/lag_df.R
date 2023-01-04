main <- function(){
  
  treatment_data <- load_csv("complete", "treatment_data.csv") %>% 
    dplyr::filter(treatment_year <= 2010,
                  city_id != 21403,
                  city_id != 21421)
  
  treatment_name_lists <- unique(treatment_data$city_name)
  
  five_lag_df <- purrr::map(treatment_name_lists, lag_synth_data, 
                                     treatment_data, lag_year = 5) %>% 
    dplyr::bind_rows()
  
  ten_lag_df <- purrr::map(treatment_name_lists, lag_synth_data, 
                           treatment_data, lag_year = 10) %>% 
    dplyr::bind_rows()
  
  five_lag_id_only <- unique(five_lag_outcome_all$city_name)
  
  # write.csv(five_lag_outcome_all, file = here::here('04.analyze', 'synthetic_control',
  #                                                   'data', 'five_lag_data', 'five_lag.csv'),
  #           fileEncoding = "CP932", row.names = FALSE)
  
}


lag_synth_data <- function(city_name_t, treatment_data, lag_year){
  
  
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
    dplyr::filter(time_unit == int_year + lag_year) %>% 
    dplyr::mutate(diff = real_y - synth_y) %>% 
    dplyr::distinct()
  
  return(five_lag_outcome)
  
}

data <- five_lag_outcome_all


ten

ten_av <- ten_lag_df %>% 
  summarise(mean_diff = mean(diff)) %>% 
  unlist() %>% 
  as.numeric()

five_av <- five_lag_df %>% 
  summarise(mean_diff = mean(diff)) %>% 
  unlist() %>% 
  as.numeric()

ten_av*100
five_av*100

ten <- create_lag_bar(ten_lag_df, 10)
five <- create_lag_bar(five_lag_df, 5)

five
ten

ggsave(five, filename = here::here('04.analyze','synthetic_control', 'figure',
                                       'synth_cov', 'density_1000','five_lag.png'),
       device = "png",   width = 12, height = 5) 

ggsave(ten, filename = here::here('04.analyze','synthetic_control', 'figure',
                                   'synth_cov', 'density_1000','ten_lag.png'),
       device = "png",   width = 12, height = 5) 


five_ten <- five + ten + plot_layout(ncol = 1 )

ggsave(five_ten, filename = here::here('04.analyze','synthetic_control', 'figure',
                                  'synth_cov', 'density_1000','five_ten.png'),
       device = "png",   width = 9, height = 7) 


create_lag_bar <- function(data, lag_year){
  
  title_name <- paste0("Difference ",lag_year, " later")
  
  data <- data %>% 
    mutate(order = fct_reorder(city_name, diff))
  
  lag_bar <- ggplot(data, mapping = aes(x = order, y = diff))+
    geom_bar(stat = "identity") +
    theme_gray(base_family = "HiraKakuPro-W3") +
    labs(x = "City name", y = "Difference", title = title_name) +
    coord_flip() +
    theme(plot.title = element_text(size = 15),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    scale_y_continuous(breaks = seq(-0.08, 0.08, 0.02))+
    ylim(-0.08, 0.08)
  
  
  # ggsave(five_bar, filename = here::here('04.analyze','synthetic_control', 'figure',
                                         # 'synth_cov', 'density_1000','bar.png'),
         # device = "png",   width = 10, height = 5)
  
  
  return(lag_bar)
}

library(forcats)
