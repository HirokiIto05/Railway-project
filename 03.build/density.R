library(dplyr)
library(readxl)
library(ggplot2)

data_len <- readxl::read_xlsx(here::here('02.raw','length','2016_ori.xlsx')) %>% 
  select(c(1,2,3))

colnames(data_len) <- c("rail_name","line_name","length")
data_len <- data_len %>% 
  tidyr::replace_na( list(rail_name = 0))


name_rail <- data_len$rail_name 

data_dens <- readxl::read_xlsx(here::here('02.raw','density','2016.xlsx')) %>% 
  select(1,2,5,6,7,8,9) 

colnames(data_dens) <- c("rail_name","line_name","workers","students",
                        "commuters","not_commuters","total")

data_dens <- data_dens %>% 
  tidyr::replace_na( list(rail_name = 0))

connect_fun <- function(data){
  
  for (i in 1:nrow(data)) {
    new_val <- data$rail_name[i]
    
    if(new_val != 0){
      data$line_name[i] <- new_val 
    } 
    
  }
  
  return(data)
  
}


len_data <- connect_fun(data_len) %>% 
  select(-1)
dens_data <- connect_fun(data_dens) %>% 
  select(-1)

connect_data <- left_join(len_data, dens_data) %>% 
  tidyr::drop_na(line_name)

connect_data$total <- as.numeric(connect_data$total)
connect_data$length <- as.numeric(connect_data$length)

connect_data <- connect_data %>% 
  dplyr::mutate(density = round(((total/length)/365)*1000))

control_group <- connect_data %>% 
  dplyr::filter(density < 1000,
                line_name != "中小計")


