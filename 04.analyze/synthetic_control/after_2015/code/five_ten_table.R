


create_bar_plot <- function(year_i, diff_all_data){
  
  year_five_df <- diff_all_data %>% 
    mutate(after = time_unit - treatment_year + 1) %>% 
    dplyr::filter(after == 5) %>% 
    dplyr::select(city_name, diff) %>% 
    dplyr::rename(diff_five_later = diff)
  
  year_ten_df <- diff_all_data %>% 
    mutate(after = time_unit - treatment_year + 1) %>% 
    dplyr::filter(after == 10) %>% 
    select(city_name, diff) %>% 
    dplyr::rename(diff_ten_later = diff) 
  
  
  test <- left_join(year_five_df, year_ten_df)
  
  table_five_ten <- test %>% 
    kbl() %>%
    kable_classic_2(full_width = F) %>% 
    save_kable(file = here::here('04.analyze','synthetic_control',
                                 'after_2015','kable',
                                 "Five_year_later_bar.pdf"))
  , self_contained = T)
  
  
  table_fivE_ten
  
  library(kableExtra)
  
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