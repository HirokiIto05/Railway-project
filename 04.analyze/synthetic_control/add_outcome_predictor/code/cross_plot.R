main <- function(){
  
  str(plot_based_data)
  
  
  fci_diff <- create_cross_plot(plot_based_data, fci_mean,
                                "：5年後",five_diff, 
                                x_axis = "財政力指数")
  
  fci_diff
  fci_diff_ten <- create_cross_plot(plot_based_data, fci_mean,
                                    "：10年後 ",ten_diff,
                                    x_axis = "財政力指数")
  
  fci_main <- fci_diff + fci_diff_ten
  
  fci_main
  
  main_diff <- create_cross_plot(plot_based_data, distance,
                                 "：5年後",five_diff,
                                 x_axis = "主要な市との直線距離")
  
  main_diff_ten <- create_cross_plot(plot_based_data, distance,
                                     "：10年後", ten_diff,
                                     x_axis = "主要な市との直線距離")
  
  main_main <- main_diff + main_diff_ten
  
  main_main
  
  total_diff <- create_cross_plot(plot_based_data, total_mean,
                                  "：5年後",five_diff,
                                  x_axis =  "総人口")
  
  total_diff_ten <- create_cross_plot(plot_based_data, total_mean,
                                      "：10年後", ten_diff,
                                      x_axis =  "総人口")
  
  total_main <- create_patchwork(total_diff, total_diff_ten,
                                 "Total population")
  
  total_main <- total_diff + total_diff_ten
    
    
  length_diff <- create_cross_plot(plot_based_data, track_length,
                                   "：5年後",five_diff,
                                   "路線距離")
  
  length_diff_ten <- create_cross_plot(plot_based_data, track_length,
                                       "：10年後", ten_diff,
                                       "路線距離")
  
  
  # children_diff <- create_cross_plot(plot_based_data, children_mean,
  #                                    "：5年後",five_diff,
  #                                    "6歳以下の子供のいる世帯割合")
  # 
  # children_diff_ten <- create_cross_plot(plot_based_data, children_mean,
  #                                        "：10年後", ten_diff,
  #                                        "6歳以下の子供のいる世帯割合")
  # 
  intyear_diff <- create_cross_plot(plot_based_data, treatment_year,
                                    "：5年後", five_diff,
                                    "廃線になった年")
  
  intyear_diff_ten <- create_cross_plot(plot_based_data, treatment_year,
                                        "：10年後", ten_diff,
                                        "廃線になった年")
  
  own_diff <- create_cross_plot(plot_based_data, own_percent_mean,
                                "：5年後",five_diff,
                                "持ち家の世帯割合")
  
  own_diff_ten <- create_cross_plot(plot_based_data, own_percent_mean,
                                    "：10年後", ten_diff,
                                    "持ち家の世帯割合")
  
}


create_children_cross_five <- function(plot_based_data, y_var){
  
  y_var <- rlang::enquo(y_var)
  
  cross_var_children <- ggplot(plot_based_data, aes(x = children_mean, y = five_diff)) +
    geom_hline(yintercept = 0, linetype = "solid",
               size = 0.8, colour = "gray") +
    geom_point(size = 3) +
    # geom_text(aes(x = !!cross_var, y = diff)) +
    stat_smooth(method = "lm", 
                se = FALSE, 
                colour = "black",
                linetype = "longdash") +
    labs(title = "廃線5年後",
         x = "6歳以下の子供のいる世帯割合",
         y = "処置効果") +
    ylim(c(-0.1, 0.1)) +
    theme_bw(base_family = "HiraKakuProN-W3") +
    theme(axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10)) +
    annotate("text", x = 0.07, y = 0.09,
             label = "R-squared = 0.56", vjust = -0.5,
             family="HiraKakuPro-W3") 
  
  cross_var_children
  
  return(cross_var_children)
  
}

# install.packages("ggpisc")
# library(ggpmisc)
# devtools::install_github("aphalo/ggpmisc")
# 
# install.packages(c("devtools", "usethis"))
# library(devtools)
# library(usethis)
create_children_cross_ten <- function(plot_based_data, y_var){
  
  y_var <- rlang::enquo(y_var)
  
  cross_var_children <- ggplot(plot_based_data, aes(x = children_mean, y = ten_diff)) +
    geom_hline(yintercept = 0, linetype = "solid",
               size = 0.8, colour = "gray") +
    geom_point(size = 3) +
    # geom_text(aes(x = !!cross_var, y = diff)) +
    stat_smooth(method = "lm", se = FALSE, colour = "black",linetype = "longdash") +
    labs(title = "廃線10年後",
         x = "6歳以下の子供のいる世帯割合", 
         y = "処置効果") +
    ylim(c(-0.1, 0.1)) +
    theme_bw(base_family = "HiraKakuProN-W3") +
    theme(axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10)) +
    annotate("text", x = 0.07, y = 0.09,
             label = "R-squared = 0.52", vjust = -0.5,
             family="HiraKakuPro-W3") 
  
  cross_var_children
  
  
  return(cross_var_children)

}
cross_var_five <- create_children_cross_five(plot_based_data, five_diff)
cross_var_five

fit_five <- lm(five_diff ~ children_mean, data = plot_based_data)

summary(fit_five)

cross_var_ten <- create_children_cross_ten(plot_based_data, ten_diff)
cross_var_ten

fit_ten <- lm(ten_diff ~ children_mean, data = plot_based_data)

summary(fit_ten)

children_two_plot <- cross_var_five + cross_var_ten

children_two_plot



ggsave(children_two_plot, filename = here::here('04.analyze',
                                                'synthetic_control', 
                                                'add_outcome_predictor',
                                                'cross_plot',
                                                "children.png"),
       device = "png",  width = 8, height = 3.5)





# blanklabelplot<-ggplot()+labs(y="Difference")+theme_classic()+ 
  # guides(x = "none", y = "none")

blanklabelplot + children_two_plot + plot_layout(widths=c(1,1000))




children_two_plot <- create_patchwork(
  plot_1 = children_diff, plot_2 = children_diff_ten,
  title_name = "Household with Children")


fci_two_plot <- create_patchwork(
  plot_1 = children_diff, plot_2 = children_diff_ten,
  title_name = "Household with Children")




create_patchwork <- function(plot_1, plot_2,
                             title_name){
  
  output_plot <- plot_1 + plot_2 +
    plot_annotation(title = title_name)
  
  return(output_plot)
  
}


library(patchwork)

ggsave(children_two_plot, 
       filename = here::here('04.analyze',
                             'synthetic_control',
                             'add_outcome_predictor',
                             'cross_plot',
                             'children.png'),
       width = 8, height = 3.5)




# str(plot_based_data)
# 
# plot_cross <- save_cross_plot(children_diff, "children")
# plot_cross <- save_cross_plot(own_diff, "own")
# plot_cross <- save_cross_plot(total_diff, "total")
# plot_cross <- save_cross_plot(length_diff, "length")
# plot_cross <- save_cross_plot(intyear_diff, "intyear")


library(patchwork)
# 
# five_cross <-  (own_diff | total_diff)/
#   (intyear_diff | main_diff)/
#   (length_diff + fci_diff)
# 
# ten_cross <-  (own_diff_ten | total_diff_ten)/
#   (intyear_diff_ten + main_diff_ten)/
#   (length_diff_ten | fci_diff_ten) 
# 
# group_first <- 
#   (own_diff | own_diff_ten)/
#   (total_diff | total_diff_ten)/
#   (length_diff | length_diff_ten)
# 
# group_first
# 
# group_second <- 
#   (main_diff | main_diff_ten)/
#   (fci_diff | fci_diff_ten)/
#   (intyear_diff | intyear_diff_ten)
# 
# group_second
# 
# ggsave(group_first, filename = here::here('04.analyze','synthetic_control', 
#                                          'add_outcome_predictor','cross_plot',
#                                          "first_group.png"),
#        device = "png",  width = 8, height = 10)
# 
# ggsave(group_second, filename = here::here('04.analyze','synthetic_control', 
#                                         'add_outcome_predictor','cross_plot',
#                                         "second_group.png"),
#        device = "png",  width = 8, height = 10)


create_cross_plot <- function(plot_based_data, cross_var, 
                              title_name, y_var, x_axis){
  
  cross_var = rlang::enquo(cross_var)
  y_var = rlang::enquo(y_var)
  
  par(family = "HiraginoSans-W3")
  cross_var_plot<- ggplot(plot_based_data, aes(x = !!cross_var, y = !!y_var)) +
    geom_point(size = 3) +
    stat_smooth(method = "lm", se = FALSE, colour = "black",linetype = "longdash") +
    labs(title = paste0(x_axis, title_name),
         x = x_axis,
         y = "処置効果") +
    theme_bw(base_family = "HiraKakuProN-W3")
  
  cross_var_plot
  
  return(cross_var_plot)
  
}  


