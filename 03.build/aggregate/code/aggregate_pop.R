col_name <- c("city_id","region_name","city_name","male","female","total","household",
              "fluctuation","fluctuation_rate","natural_increase","natural_increase_rate",
              "social_increase","social_increase_rate")

base_year <- 2010

aggregate_pop <- function(col_name){
  
  base_data <- data.frame(matrix(ncol = 14)[0, ])
  
  for(base_year in 1995:2019){
    if(base_year <= 2012){
      file_name <- paste0("/Users/ito_hiroki/01.Research/Railway-project/02.raw/population/", base_year,'.xls')
      new_data <-  readxl::read_xls(file_name) %>% 
        dplyr::select(c(1,2,3,4,5,6,7,16,17,18,19,20,21))
    } else {
      file_name <- paste0("/Users/ito_hiroki/01.Research/Railway-project/02.raw/population/", base_year,'.xls')
      new_data <-  readxl::read_xls(file_name) %>% 
        dplyr::select(c(1,2,3,4,5,6,7,20,21,22,23,24,25))

    }
    
    colnames(new_data) <- col_name
    
    new_data <- new_data %>% 
      dplyr::mutate(year = base_year, .after = city_name)
    
    new_data <- new_data %>% 
      slice(c(-1,-2,-3,-4))

    base_data <- rbind(base_data, new_data)
    
  }
  
  return(base_data)
  
}


pop <- aggregate_pop(col_name)

write.csv(pop, here::here('03.build','aggregate','data','pop_all.csv'),
                   fileEncoding = "CP932", row.names = FALSE) 
