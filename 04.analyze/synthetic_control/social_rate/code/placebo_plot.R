main <- function() {
  
   placebo_year_df <- read.csv(file = here::here('04.analyze',
                                                 'synthetic_control',
                                                 'after_2015',
                                                 'after_placebo_table',
                                                 'p_value_data.csv'),
                               fileEncoding = "CP932")
   
   treatment_name_lists <- unique(placebo_year_df$city_name)
   
   treatment_diff_data <- read.csv(here::here('04.analyze',
                                              'synthetic_control',
                                              'after_2015',
                                              'diff_ten_data',
                                              'diff_ten_data.csv'),
                                   fileEncoding = "CP932") %>% 
     dplyr::select(city_name, time_unit, treatment_year, diff) %>% 
     dplyr::rename(year = time_unit)
   
   
   
   purrr::map(treatment_name_lists, create_each_pvalue_plot,  placebo_year_df)
   
}


city_name_t <- "足寄郡陸別町"

treatment_id_lists


  
create_each_pvalue_plot <- function(city_name_t, placebo_year_df){
  
  based_data <- placebo_year_df %>% 
    dplyr::filter(city_name == city_name_t)
  
  output_plot <- ggplot(based_data, aes(x = after, y = p_value)) +
    geom_point() +
    labs(title = paste0("p-value ", "'",city_name_t,"'"),
         y = "p-value",
         x = "after year") +
    theme_gray(base_family = "HiraKakuPro-W3") +
    ylim(c(0,1))
  # theme(axis.title.y = element_blank(),
  #       plot.title = element_text(size = 13),
  #       axis.text.x = element_text(size = 10),
  #       axis.text.y = element_text(size = 10)) +
  # ylim(c(-0.1, 0.1))
  output_plot
  
  
  
  file_name <- paste0(city_name_t, ".png")
  
  ggsave(output_plot, file = here::here('04.analyze',
                                        'synthetic_control',
                                        'after_2015',
                                        'after_placebo_plot', file_name))
  # write.csv(ten_year_placebo_df,
            # file = here::here('04.analyze',
            #                   'synthetic_control',
            #                   'after_2015',
            #                   'p_value', 'placebo_after.csv'),
            # fileEncoding = "CP932",
  #           row.names = FALSE)
  
  
}

create_placebo_table <- function(placebo_year_df){
  
  
  ten_table <- placebo_year_df %>% 
    dplyr::filter(after == 10)
  
  
  colnames(ten_table)
  
  table_based_df <- ten_table %>% 
    dplyr::select(city_name, year, placebo_all, p_value,
                  diff)

  kableExtra::kbl(table_based_df) %>% 
    kableExtra::kable_classic_2()
}

ten_table_mod <- ten_table %>% 
  dplyr::select(city_name, diff, p_value) %>% 
  dplyr::mutate(p_value = round(p_value, 3),
                diff = round(diff, 4))

write.csv(ten_table_mod, 
          file = here::here('04.analyze', 
                            'synthetic_control',
                            'after_2015',
                            'after_placebo_table', 
                            'p_value_diff_ten.csv'),
          fileEncoding = "CP932",
          row.names = FALSE)


sign_df <- table_based_df %>% 
  dplyr::filter(p_value < 0.2)

mean(sign_df$diff)
  
  
  