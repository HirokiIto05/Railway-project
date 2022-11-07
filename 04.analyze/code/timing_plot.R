main <- function(){
  
  timing_data <- read_data()
  
  category_list <- create_four_categories(timing_data)  
  
  
}


read_data <- function(){
  
  new_data <- read.csv(here::here('03.build','timing','data','timing.csv'),
                       fileEncoding = "CP932")
  
  return(new_data)
  
}


create_four_categories <- function(timing_data){
  
  treat_city_id <- distinct(timing_data, city_id) %>% 
    unlist() %>% 
    as.character()  
  
  one_name_list <- treat_city_id[1:5]
  two_name_list <- treat_city_id[6:10]
  three_name_list <- treat_city_id[11:15]
  four_name_list <- treat_city_id[16:20] 
  
  category_list <- list(a_first = one_name_list,
                        b_second = two_name_list,
                        c_third =  three_name_list,
                        d_fourth = four_name_list)
  
  return(category_list)
  
}


plot_four_categories <- function(timing_data, category_list){
  
  for (i in 1:4) {
    plot_data <- timing_data %>% 
      filter(city_id %in% unlist(category_list[i]))
  
    file_num <- colnames(as.data.frame(category_list[i]))
    
    folder_name <- here::here('04.analyze','figure','treatment','did')
    file_name0 <- paste0(folder_name,'/',file_num,".png")
    
    dd_plot <- ggplot(data = plot_data, aes(x = timing, y = percentage, colour = city_name))+
      geom_point() +
      geom_line() +
      geom_vline(xintercept = -1) +
      theme_gray(base_family = "HiraKakuPro-W3",
                 base_size = 15)
    
      ggsave(filename = file_name0, plot = dd_plot)
      
  }

}


average_plot <- function(timing_data){
  
  folder_name <- here::here('04.analyze','figure','treatment','did')
  file_name0 <- paste0(folder_name,'/',"average",".png")
  
  average_data <- timing_data %>% 
    group_by(timing)
  
  mean_list <- summarise(average_data, mean = mean(percentage, na.rm = TRUE))
  
  
  average_plot <- ggplot(data = mean_list, aes(x = timing, y = mean))+
    geom_point() +
    geom_line() +
    labs(title = "mean") +
    geom_vline(xintercept = 0) +
    theme_gray(base_family = "HiraKakuPro-W3",
               base_size = 15)
  
  average_plot
  
  # ggsave(filename = file_name0, plot = average_plot)
  
}

plot_four_categories(timing_data, category_list)

average_plot(timing_data)



all_average



