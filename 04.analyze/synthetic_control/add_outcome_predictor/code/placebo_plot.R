main <- function() {
  
   placebo_df <- read.csv(file = here::here('04.analyze',
                                                 'synthetic_control',
                                                 'add_outcome_predictor',
                                                 'placebo_table', 'placebo_data.csv'),
                               fileEncoding = "CP932")
   
   
   treatment_name_lists <- unique(placebo_df$city_name)
   
   placebo_df_after <- placebo_df %>% 
     dplyr::mutate(after = time_unit - treatment_year + 1)
   
   purrr::map(treatment_name_lists, create_placebo_plot ,  placebo_df_after)
   
}

treatment_name_lists <- unique(treatment_data$city_name)

treatment_name_lists

city_name_t <- "常呂郡訓子府町"
file_name <- paste0(city_name_t, ".rds")

base_plot <- readRDS(here::here('04.analyze','synthetic_control',
                                'add_outcome_predictor',
                                'table', file_name))

balance_table <- base_plot %>% 
  tidysynth::grab_balance_table()


colnames(balance_table) <- c("")


treatment_name_lists
  
  
create_placebo_plot <- function(city_name_t, placebo_df_after){
  
  based_data <- placebo_df_after %>% 
    dplyr::filter(city_name == city_name_t) %>% 
    dplyr::arrange(-city_id) %>% 
    dplyr::mutate(city_id = as.character(city_id),
                  .placebo = as.character(.placebo)) 
  
  title_name <- unique(based_data$city_name)
  
  treatment_unit <- based_data %>% 
    dplyr::filter(.placebo == 0) 
  
  control_unit <- based_data %>% 
    dplyr::filter(.placebo == 1) 
  
  
  int_year <- unique(treatment_unit$treatment_year) 
  
  
  
  # output_plot <- ggplot(based_data, aes(x = time_unit,
  #                                       y = diff, 
  #                                       group = city_id,
  #                                       colour = .placebo,
  #                                       size = .placebo)) +
  #   geom_line() +
  #   theme_bw(base_family = "HiraKakuPro-W3") +
  #   theme(legend.position = "none") +
  #   scale_color_manual(values = c("black", "gray")) +
  #   labs(title = title_name,
  #        y = "population",
  #        x = "year") +
  #   scale_size_manual(values = c(1, 0.5))
  
  
  output_plot <- ggplot() +
    geom_line(control_unit, mapping = aes(x = time_unit, 
                                y = diff,
                                group = city_id,
                                colour = .placebo)) +
    theme_bw(base_family = "HiraKakuPro-W3") +
    theme(legend.position = "none") +
    scale_color_manual(values = c("gray","black")) +
    geom_line(treatment_unit,mapping = aes(x = time_unit, 
                                  y = diff,
                                  group = city_id,
                                  colour = "black"),
              size = 1) +
    theme_bw(base_family = "HiraKakuPro-W3") +
    geom_vline(xintercept = int_year - 1, linetype = "dashed", size = 0.8) +
    theme(plot.title = element_text(size = 13),
          axis.text.x = element_text(size = 11),
                                     # angle = 45,
                                     # hjust = 0.5,
                                     # vjust = 0.5),
          axis.text.y = element_text(size = 11),
          axis.title.x = element_blank(),
          # axis.title.y = element_blank(),
          legend.position = "none",
          legend.text = element_text(size=11)) +
    labs(title = title_name,
         y = "処置効果",
         x = "Year") +
    scale_size_manual(values = c(1, 0.5)) +
    scale_x_continuous(breaks = c(1995,int_year - 1)) 
    
  output_plot
  
  file_name <- paste0(city_name_t, ".png")
  # file_name <- "example_placebo.png"

  ggsave(output_plot, file = here::here('04.analyze',
                                        'synthetic_control',
                                        'add_outcome_predictor',
                                        'placebo_figure', file_name))
  
  return(output_plot)
}


create_placebo_table <- function(placebo_df_after){
  
  
  ten_table <- placebo_df_after %>% 
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
                            'add_',
                            'after_placebo_table', 
                            'p_value_diff_ten.csv'),
          fileEncoding = "CP932",
          row.names = FALSE)


sign_df <- table_based_df %>% 
  dplyr::filter(p_value < 0.2)

mean(sign_df$diff)
  

plot_1
  


plot_1 <- create_placebo_plot("檜山郡江差町", placebo_df_after)
plot_2 <- create_placebo_plot("檜山郡上ノ国町", placebo_df_after)
plot_3 <- create_placebo_plot("常呂郡訓子府町", placebo_df_after)
plot_4 <- create_placebo_plot("常呂郡置戸町", placebo_df_after)
plot_5 <- create_placebo_plot("中川郡本別町", placebo_df_after)
plot_6 <- create_placebo_plot("足寄郡足寄町", placebo_df_after)   
plot_7 <- create_placebo_plot("足寄郡陸別町", placebo_df_after)
plot_8 <- create_placebo_plot("上北郡七戸町", placebo_df_after)
plot_9 <- create_placebo_plot("上北郡六戸町", placebo_df_after)
plot_10 <- create_placebo_plot("輪島市", placebo_df_after)
plot_11 <- create_placebo_plot("加茂郡八百津町", placebo_df_after)
plot_12 <- create_placebo_plot("山県郡安芸太田町", placebo_df_after)
plot_13 <- create_placebo_plot("南島原市", placebo_df_after)
plot_14 <- create_placebo_plot("西臼杵郡高千穂町", placebo_df_after)
plot_15 <- create_placebo_plot("西臼杵郡日之影町", placebo_df_after)

p_sum <- (plot_1 + plot_2 +plot_3+
            plot_4 + plot_5 +plot_6+
            plot_7 + plot_8 +plot_9+
            plot_10 + plot_11 +plot_12+
            plot_13 + plot_14 +plot_15)+
  plot_layout(nrow = 5,
              ncol = 3,
              widths = c(1,1,1),
              heights = c(1,1,1,1,1))



p_sum

ggsave(p_sum, filename = here::here('04.analyze','synthetic_control', 
                                    'add_outcome_predictor', 'appendix','placebo_plot.png'),
       device = "png",  width = 7, height = 7*1.41421356)



library(patchwork)

control_df <- master_data %>% 
  dplyr::filter(dummy == 0)

name_cont <- distinct(control_df, city_name)
