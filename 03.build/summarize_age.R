main <- function(){
  
  long_lists <- make_long_lists()
  short_lists <- make_short_lists()

  age_data <- aggregate_pop(short_lists, long_lists)
  
  save_age(age_data)
  
}


make_long_lists <- function(){
  
  long_lists <- c('city_id','prefecture','city_name','gender','total',
                  'r0_4','r5_9','r10_14','r15_19','r20_24','r25_29','r30_34',
                  'r35_39','r40_44','r45_49','r50_54','r55_59','r60_64','r65_69',
                  'r70_74','r75_79','r80_84','r85_89','r90_94','r95_99','r100_over')
  
  return(long_lists)
  
}


make_short_lists <- function(){

  short_lists <- c('city_id','prefecture','city_name','gender','total',
                      'r0_4','r5_9','r10_14','r15_19','r20_24','r25_29','r30_34',
                      'r35_39','r40_44','r45_49','r50_54','r55_59','r60_64','r65_69',
                      'r70_74','r75_79','r80_over')
  
  return(short_lists)
}


aggregate_pop <- function(short_lists, long_lists){
  
  base_data <- data.frame(matrix(ncol = 23)[0, ])
  
  for(base_year in 1995:2019){
    if(base_year <= 2014){
      file_a <- paste0(base_year, '.xls') 
      file_name <- here::here('02.raw', 'age', file_a) 
      new_data <-  readxl::read_xls(file_name) 
      
      
      colnames(new_data) <- short_lists
      
      num_only <- new_data %>% 
        dplyr::select(-prefecture, -city_name, -gender)
      
      cha_only <- new_data %>% 
        dplyr::select(prefecture, city_name, gender)
      
      num_only <- num_only %>% 
        apply(2, as.numeric) 
      
      new_data <- bind_cols(cha_only, num_only)
      
    } else {
      file_a <- paste0(base_year, '.xls') 
      file_name <- here::here('02.raw', 'age', file_a) 
      new_data <-  readxl::read_xls(file_name) 
      
      colnames(new_data) <- long_lists
      
      num_only <- new_data %>% 
        dplyr::select(-prefecture, -city_name, -gender)
      
      cha_only <- new_data %>% 
        dplyr::select(prefecture, city_name, gender)
      
      num_only <- num_only %>% 
        apply(2, as.numeric) 
      
      new_data <- bind_cols(cha_only, num_only)
      
      new_data <- new_data %>% 
        dplyr::mutate(r80_over = dplyr::select(.,c(r80_84,r85_89,r90_94,r95_99,r100_over)) %>% 
                        rowSums(na.rm = TRUE)) %>% 
        dplyr::select(-r80_84, -r85_89, -r90_94, -r95_99, -r100_over)
      
    }
    
    new_data <- new_data %>% 
      dplyr::mutate(year = base_year) %>% 
      dplyr::relocate(year, .after = city_id)

   base_data <- rbind(base_data, new_data)
    
     
  }
  
  return(base_data)
  
}


save_age <- function(age_data){
  
  write.csv(age_data, file = here::here('02.raw','age','age_data.csv'),
            fileEncoding = 'CP932',
            row.names = FALSE)

  
}



library(dplyr)
library(tidyr)

main()

