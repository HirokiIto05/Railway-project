main <- function(){
  # plot_based_data <- plot_based_data %>% 
  #   distinct()
  
  
  
  plot_based_data <- read.csv(here::here('04.analyze','synthetic_control',
                                         'cov_only', 'cross_plot_base',
                                         'data.csv'), fileEncoding = "CP932") %>% 
    dplyr::rename(ten_diff = diff)
    
  
  
  add_five_df <- five_year_bar_df %>% 
    dplyr::select(city_name, diff) %>% 
    dplyr::rename(five_diff = diff) %>% 
    dplyr::right_join(plot_based_data)  
    
  
  plot_based_data <- add_five_df 
  
  plot_based_data <- plot_based_data %>% 
    drop_na()  
    
  
  write.csv(add_five_df,here::here('04.analyze','synthetic_control',
                                   'cov_only', 'cross_plot_base',
                                   'five_ten_data.csv'), fileEncoding = "CP932",
            row.names = FALSE)
  
  
  str(plot_based_data)
  
  main_diff <- create_cross_plot(plot_based_data, distance,
                                 "Between Treated City and Main City",
                                 five_diff)
  
  total_diff <- create_cross_plot(plot_based_data, total_mean,
                                  "Total population",
                                  five_diff)
  
  length_diff <- create_cross_plot(plot_based_data, track_length,
                                   "Track length",
                                   five_diff)
  
  children_diff <- create_cross_plot(plot_based_data, children_mean,
                                     "Household with children percent",
                                     five_diff)
  
  intyear_diff <- create_cross_plot(plot_based_data, treatment_year,
                                    "Treatment year",
                                    five_diff)
  
  own_diff <- create_cross_plot(plot_based_data, own_percent_mean,
                                "Own household percent",
                                five_diff)
  
  
  main_diff_ten <- create_cross_plot(plot_based_data, distance,
                                 "Between Treated City and Main City",
                                 ten_diff)
  
  total_diff_ten <- create_cross_plot(plot_based_data, total_mean,
                                  "Total population",
                                  ten_diff)
  
  length_diff_ten <- create_cross_plot(plot_based_data, track_length,
                                   "Track length",
                                   ten_diff)
  
  children_diff_ten <- create_cross_plot(plot_based_data, children_mean,
                                     "Household with children percent",
                                     ten_diff)
  
  intyear_diff_ten <- create_cross_plot(plot_based_data, treatment_year,
                                    "Treatment year",
                                    ten_diff)
  
  own_diff_ten <- create_cross_plot(plot_based_data, own_percent_mean,
                                "Own household percent",
                                ten_diff)
  
}




# str(plot_based_data)
# 
# plot_cross <- save_cross_plot(children_diff, "children")
# plot_cross <- save_cross_plot(own_diff, "own")
# plot_cross <- save_cross_plot(total_diff, "total")
# plot_cross <- save_cross_plot(length_diff, "length")
# plot_cross <- save_cross_plot(intyear_diff, "intyear")


library(patchwork)

five_cross <-  (own_diff | total_diff)/
  (length_diff | children_diff)/ 
  (intyear_diff + main_diff)
  # (length_diff + children_diff)

five_cross




ten_cross <-  (own_diff_ten | total_diff_ten)/
  (length_diff_ten | children_diff_ten)/ 
  (intyear_diff_ten + main_diff_ten)
# (length_diff + children_diff)

ten_cross


save_cross_plot(five_cross, "five_cross")

cor.test()

library(rlang)

ggsave(five_cross, filename = here::here('04.analyze','synthetic_control', 
                                         'cov_only','cross_plot',
                                         "five_cross.png"),
       device = "png",  width = 8, height = 10)

ggsave(ten_cross, filename = here::here('04.analyze','synthetic_control', 
                                         'cov_only','cross_plot',
                                         "ten_cross.png"),
       device = "png",  width = 8, height = 10)


create_cross_plot <- function(plot_based_data, cross_var, var_cha, y_var){
  
  cross_var = rlang::enquo(cross_var)
  y_var = rlang::enquo(y_var)
  
  # cor_relation = plot_based_data %>%
  #   summarize(length_cor = cor(length, diff),
  #             own_cor = cor(own_percent_mean, diff),
  #             total_cor = cor(total_mean, diff),
  #             middle_cor = cor(middle_mean, diff),
  #             children_cor = cor(children_mean, diff),
  #             train_cor = cor(train_mean, diff))
  # 
  # colnames(plot_based_data)
    
  # cor(140, -0.0485)
  
  # help(cor)
  
  # cor <- plot_based_data %>% 
  #   group_by(city_name) %>% 
  #   cor(length, plot_based_data$diff)
  
  title_t <- paste0("Difference and ", var_cha)
  
  
  par(family = "HiraginoSans-W3")
  cross_var_plot<- ggplot(plot_based_data, aes(x = !!cross_var, y = !!y_var)) +
    geom_point(size = 3) +
    # geom_text(aes(x = !!cross_var, y = diff)) +
    stat_smooth(method = "lm", se = FALSE, colour = "black",linetype = "dashed") +
    labs(title = var_cha,
         x = var_cha, y = "difference") +
    theme_bw(base_family = "HiraKakuProN-W3")
  
  cross_var_plot
  
  return(cross_var_plot)
  
}  


save_cross_plot <- function(plot_cross, var_name){
  
  file_name <- paste0(var_name, ".png")
  
  ggsave(plot = plot_cross, filename = here::here('04.analyze','synthetic_control', 
                                                  'after_2015','cross_plot',
                                                  file_name),
         device = "png",  width = 8, height = 10)
  
}

install.packages("ggrepel")

library(ggrepel)


length_list <- unique(add_five_df$track_length)

sum(length_list)
