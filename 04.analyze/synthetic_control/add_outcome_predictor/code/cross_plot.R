main <- function(){
  # plot_based_data <- plot_based_data %>% 
  #   distinct()
  
  
  
  plot_based_data <- read.csv(here::here('04.analyze','synthetic_control', 
                                                  'add_outcome_predictor','cross_plot_base',
                                         'data.csv'), fileEncoding = "CP932") %>% 
    
  plot_based_data <- plot_based_data drop_na(plot_based_data)  
    
  
  str(plot_based_data)
  
  main_diff <- create_cross_plot(plot_based_data, distance,
                                 "Between Treated City and Main City")

  main_diff
  
  total_diff <- create_cross_plot(plot_based_data, total_mean,
                                  "Total population")
  
  total_diff
  
  length_diff <- create_cross_plot(plot_based_data, track_length,
                                   "Track length")
  
  length_diff
  
  children_diff <- create_cross_plot(plot_based_data, children_mean,
                                     "Household with children percent")
  
  children_diff
  
  intyear_diff <- create_cross_plot(plot_based_data, treatment_year,
                                    "Treatment year")
  
  intyear_diff
  
  own_diff <- create_cross_plot(plot_based_data, own_percent_mean,
                                "Own household percent")
  
  own_diff
  
}

str(plot_based_data)

plot_cross <- save_cross_plot(children_diff, "children")
plot_cross <- save_cross_plot(own_diff, "own")
plot_cross <- save_cross_plot(total_diff, "total")
plot_cross <- save_cross_plot(length_diff, "length")
plot_cross <- save_cross_plot(intyear_diff, "intyear")


library(patchwork)

five_cross <-  (own_diff | total_diff)/
  (length_diff | children_diff)/ 
  (intyear_diff + main_diff)
  # (length_diff + children_diff)


five_cross

save_cross_plot(five_cross, "five_cross")

cor.test()

library(rlang)

create_cross_plot <- function(plot_based_data, cross_var, var_cha){
  
  cross_var = rlang::enquo(cross_var)
  
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
  cross_var_plot<- ggplot(plot_based_data, aes(x = !!cross_var, y = diff)) +
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

