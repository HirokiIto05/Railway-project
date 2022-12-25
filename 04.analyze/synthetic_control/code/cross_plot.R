main <- function(){
  
  
  
  
}

create_plot <- function(plot_based_data, cross_var, city_name_t){

  cross_var = rlang::enquo(cross_var)
  
  y_labrl_name <- as.character(!!cross_var)
  
  own_diff <- ggplot(plot_based_data, aes(x = own_percent_mean, y = !!cross_var)) +
    geom_point() +
    labs(title="Difference and ",
         x ="city_id", y = "difference")
    
    
  ggsave(own_diff, file = here::here('04.analyze','synthetic_control',
                                     'figure','cross',  'figure','own_diff.pdf'))
  total_diff <- ggplot(plot_based_data, aes(x = total_mean, y = diff)) +
    geom_point()
  ggsave(total_diff, file = here::here('04.analyze','synthetic_control',
                                     'figure','cross',  'figure','total_diff.pdf'))
  length_diff <- ggplot(plot_based_data, aes(x = length, y = diff)) +
    geom_point()
  ggsave(length_diff, file = here::here('04.analyze','synthetic_control',
                                     'figure','cross',  'figure', 'length_diff.pdf'))
  children_diff <- ggplot(plot_based_data, aes(x = children_mean, y = diff)) +
    geom_point()
  ggsave(children_diff, file = here::here('04.analyze','synthetic_control',
                                     'figure','cross',  'figure','children_diff.pdf'))
  middle_diff <- ggplot(plot_based_data, aes(x = middle_mean, y = diff)) +
    geom_point()
  ggsave(middle_diff, file = here::here('04.analyze','synthetic_control',
                                     'figure','cross',  'figure','middle_diff.pdf'))
  train_diff <- ggplot(plot_based_data, aes(x = train_mean, y = diff)) +
    geom_point()
  ggsave(train_diff, file = here::here('04.analyze','synthetic_control',
                                     'figure','cross',  'figure','train_diff.pdf'))
  treatmentyear_diff <- ggplot(plot_based_data, aes(x = treatment_year, y = diff)) +
    geom_point()
  ggsave(treatmentyear_diff, file = here::here('04.analyze','synthetic_control',
                                     'figure','cross',  'figure','treatmentyear_diff.pdf'))
}

test_cross_plot <- function(plot_based_data, cross_var, city_name_t){
  
  cross_var = rlang::enquo(cross_var)
  y_labrl_name <- as.character(!!cross_var)
  file_name <- paste0()
  
  own_diff <- ggplot(plot_based_data, aes(x = own_percent_mean, y = !!cross_var)) +
    geom_point() +
    labs(title="Difference and ",
         x ="city_id", y = "difference")
  
  
  ggsave(own_diff, file = here::here('04.analyze','synthetic_control',
                                     'figure','cross',  'figure','own_diff.pdf'))
  
}

treatmentyear_diff
