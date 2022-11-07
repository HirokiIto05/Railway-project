main <- function(){
  
  main_data <- road_main()
  pop_data <- road_pop()
  
  main_data$adjust_id <- as.character(main_data$adjust_id)
  city_id_list <- dplyr::distinct(main_data, adjust_id)
  
  purrr::map(city_id_list, adjust_data, main_data, pop_data)
  
}


road_main <- function(){ 
  
  new_data <- read.csv(here::here('02.raw','aggregated_table.csv'),
                        fileEncoding = "CP932") %>% 
    dplyr::select(-X) %>% 
    dplyr::rename(city_id = adjust_id)
  
  new_data$city_id <- as.character(new_data$city_id)
  
  return(new_data)
}


road_pop <- function(){
  
  new_data <- read.csv(here::here('02.raw','population','pop_all.csv'),
                       fileEncoding = "CP932") %>% 
    dplyr::select(-X)
  
  return(new_data)
  
}

based_id <- "17204"

plot_each_id <- function(main_data,pop_data, city_id_list){
  
  adjust_data(city_id_list, main_data, pop_data)
  
}

adjust_data <- function(based_id, main_data, pop_data){
  
  new_main <- filter(main_data, city_id == based_id) %>% 
    dplyr::arrange(city_name) %>% 
    dplyr::mutate(cutoff = ifelse(year < , "0", "1"))
  
  new_pop <- pop_data %>% 
    filter(city_id == based_id) 
  
  # if(nrow(new_main) == nrow(new_pop)){
  #   plot_data <-  bind_cols(new_main, new_pop)
  # }
  
  # plot_data <-  bind_cols(new_main, new_pop)
  plot_data <- left_join(new_main, new_pop, by = c("year","city_id"))
  
  plot_save(plot_data, social_increase)
  
  return(plot_data)
}

plot_save <- function(plot_data, y_a){
  
  y_a <- rlang::enquo(y_a)
  
  gg <- ggplot(plot_data, aes(x = year, y = social_increase, colour = city_name))+
      geom_point()+
      scale_y_continuous(limits=c(-300, 0), breaks=c(-300, -200, -100, 0))+
      # scale_y_continuous(limits=c(-2.5, 0), breaks=c(-2.5, -2, -1, 0))+
      theme_gray (base_family = "HiraKakuPro-W3")
  
  gg
  
  city_name <- dplyr::distinct(plot_data,city_name)
  save_id <- paste0(city_name,'.pdf')
  
  ggsave(gg,filename = here::here('01.cover','figure',save_id))
  
}
  
  
library(dplyr)
library(ggplot2)
