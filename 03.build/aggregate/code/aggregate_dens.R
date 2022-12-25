main <- function(){
  
  
  col_name <- c("rail_name","line_name","workers","students",
                "commuters","not_commuters","total")
  
  len_data <- read_len_data()
  
  emp_data <- create_base(10000, col_name)
  
  cont_data <- aggregate_dens(col_name, len_data)  
  
  
}


create_base <- function(row_num, col_name){
  
  create_emp <- function( row_num, col_num, col_name){
    
    if( missing( col_num ) && length( col_name ) > 0 ){
      col_num = length(col_name)
    }
    return_data <- data.frame( matrix(vector(), row_num, col_num))
    
    colnames(return_data) <- col_name
      
    return(return_data)
  }
  
  emp_data =create_emp(row_num = 10000,
                      col_name = c("rail_name","line_name","workers","students",
                                      "commuters","not_commuters","total") )

  return(emp_data)
}

read_len_data <- function(){
  
  new_data <- read.csv(file = here::here('02.raw','length','length.csv'),
                       fileEncoding = "CP932")
  
  new_data <- new_data %>% 
    tidyr::replace_na(list(rail_name = 0))
  
  new_data <- connect_fun(new_data)
  
  new_data <- new_data %>% 
    dplyr::select(-c(1,2))
    
  
  return(new_data)
  
}

  
connect_fun <- function(data){
  
  for (i in 1:nrow(data)) {
    new_val <- data$rail_name[i]
    
    if(new_val != 0){
      data$line_name[i] <- new_val 
    } 
    
  }
  
  return(data)
  
}

aggregate_dens <- function(col_name, len_data){
  
  base_data <- data.frame(matrix(ncol = 9)[0, ])
  
  for(base_year in 2012:2018){
      file_name <- here::here('02.raw','density',paste0(base_year,'.xlsx'))
      new_data <-  readxl::read_xlsx(file_name) %>% 
        dplyr::select(c(1,2,5,6,7,8,9))
      
      colnames(new_data) <- col_name
      new_data <- new_data %>% 
        tidyr::replace_na( list(rail_name = 0))
      
      new_data <- connect_fun(new_data)
      
      new_data <- new_data %>% 
        dplyr::select(-1) %>% 
        dplyr::mutate(year = base_year, .after = line_name)
      
      
      jointed_data <- left_join(len_data, new_data) %>% 
        tidyr::drop_na(line_name)
      
      jointed_data$total <- as.numeric(jointed_data$total)
      jointed_data$length <- as.numeric(jointed_data$length)
      
      jointed_data <- jointed_data %>% 
        dplyr::mutate(density = round(((total/length)/365)*1000))
      
      control_group <- jointed_data %>% 
        dplyr::filter(density < 4000,
                      line_name != "中小計")
      
      base_data <- rbind(base_data, control_group)
      
  }
    
  return(base_data)
    
}

cont_data <- aggregate_dens(col_name, len_data)  

write.csv(cont_data, file = here::here('03.build','aggregate','data','dens_data.csv'),
          fileEncoding = "CP932")

library(dplyr)
library(ggplot2)
library(tidyr)
library(kableExtra)
