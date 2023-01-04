main <- function(){
  plot_based_data <- plot_based_data %>% 
    distinct()

  total_diff <- create_cross_plot(plot_based_data, total_mean,
                                  "Total population")
  
  total_diff
  
  length_diff <- create_cross_plot(plot_based_data, length,
                                   "Track length")
  
  children_diff <- create_cross_plot(plot_based_data, children_mean,
                                     "Household with children percent")
  
  intyear_diff <- create_cross_plot(plot_based_data, treatment_year,
                                    "Treatment year")
  
  own_diff <- create_cross_plot(plot_based_data, own_percent_mean,
                                "Own household percent")
  
}

plot_cross <- save_cross_plot(children_diff, "children")
plot_cross <- save_cross_plot(own_diff, "own")
plot_cross <- save_cross_plot(total_diff, "total")
plot_cross <- save_cross_plot(length_diff, "length")
plot_cross <- save_cross_plot(intyear_diff, "intyear")


five_cross <-  (own_diff + total_diff)/
  (length_diff + children_diff)/
  (intyear_diff + plot_spacer())


save_cross_plot(five_cross, "five_cross")

create_cross_plot <- function(plot_based_data, cross_var, var_cha){
  
  cross_var = rlang::enquo(cross_var)
  
  title_t <- paste0("Difference and ", var_cha)
  
  par(family = "HiraginoSans-W3")
  cross_var_plot<- ggplot(plot_based_data, aes(x = !!cross_var, y = diff)) +
    geom_point(size = 3) +
    labs(title = var_cha,
         x = var_cha, y = "difference") +
    theme_bw(base_family = "HiraKakuProN-W3")
  
  return(cross_var_plot)
  
}  


save_cross_plot <- function(plot_cross, var_name){
  
  file_name <- paste0(var_name, ".png")
  
  ggsave(plot = plot_cross, filename = here::here('04.analyze','synthetic_control', 'figure',
                                                  'cross_point','ten', file_name),
         device = "png",  width = 8, height = 10)
  
}

install.packages("ggrepel")

library(ggrepel)

