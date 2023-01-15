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
  
  treatment_year_df <- treatment_data %>% 
    dplyr::select(city_name, treatment_year)

  placebo_df <- purrr::map(treatment_name_lists, read_placebo) %>% 
    dplyr::bind_rows()
    
  
  placebo_df_year <- dplyr::left_join(placebo_df, treatment_year_df) %>% 
    dplyr::distinct() 
  
  ten_df <- placebo_df_year %>% 
    dplyr::mutate(after = time_unit - treatment_year + 1) %>% 
    dplyr::filter(after == 10)
  
}

ten_name_list <- unique(ten_df$city_name)


read_placebo <- function(city_name_t){
  
  print(city_name_t)
  
  file_name <- paste0(city_name_t, ".rds")
  
  synth_based <- readRDS(here::here('04.analyze', 'synthetic_control',
                                    'after_2015', 'table', file_name))
  placebo_data <- synth_based %>% 
    tidysynth::plot_placebos()
  
  placebo_data <- placebo_data$data %>% 
    dplyr::mutate(city_name = city_name_t, .after = .id)
  
  return(placebo_data)
  
}
  
treatment_name_lists  

purrr::map(treatment_name_lists, create_normal)

create_normal <- function(city_name_t){
  
  one_df <- placebo_df_year %>%  
    dplyr::filter(city_name == city_name_t) %>% 
    dplyr::mutate(after = time_unit - treatment_year + 1) %>% 
    dplyr::filter(after == 10)
  
  one_diff <- distinct(one_df, diff)  
  
  hist_plot <- ggplot(one_diff, aes(x = diff))+
    geom_histogram(bins = 40)
  
  png_name <- paste0(city_name_t,'.png')
  
  file_name_hist <- paste0(here::here('04.analyze','synthetic_control',
                                     'after_2015', 'normal_hist',
                                     png_name))
  
  ggsave(hist_plot, filename = file_name_hist)

  
}


city_name_t <- "西臼杵郡高千穂町"

purrr::map(ten_name_list, calculate_normal) 


ff <- unlist(f)

calculate_normal <- function(city_name_t){
  
  print(city_name_t)
    
    one_df <- placebo_df_year %>%  
      dplyr::mutate(after = time_unit - treatment_year + 1) %>% 
      dplyr::filter(after == 10) %>% 
      dplyr::filter(city_name == city_name_t) 
    
    one_diff <- distinct(one_df, diff)
    
    table <- ks.test(one_diff, "pnorm", alternative = "two.sided")
    
    
    
    print(city_name_t)
    table
  
}

table


ks.test(dnorm(100,0,1), "pnorm", alternative = "two.sided")
