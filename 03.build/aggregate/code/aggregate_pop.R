main <- function(){
  
  colname_list <- create_colname_list()
  
  pop_data <- aggregate_pop(colname_list)
  
  output_data <- change_id(pop_data)
  
  save_table(output_data)

}

create_colname_list <- function(){
  
  colname_list <- c("city_id","region_name","city_name","male","female","total","household",
                    "birth", "move_out" ,"mortality","change","change_rate",
                    "natural","natural_rate","social","social_rate")
  
  return(colname_list)
  
}


aggregate_pop <- function(colname_list){
  
  base_data <- data.frame(matrix(ncol = 14)[0, ])
  
  for(base_year in 1995:2019){
    if(base_year <= 2012){
      file_name <- paste0("/Users/ito_hiroki/01.Research/Railway-project/02.raw/population/", base_year,'.xls')
      new_data <-  readxl::read_xls(file_name) %>% 
        dplyr::select(c(1,2,3,4,5,6,7,9,12,13,16,17,18,19,20,21))
    } else {
      file_name <- paste0("/Users/ito_hiroki/01.Research/Railway-project/02.raw/population/", base_year,'.xls')
      new_data <-  readxl::read_xls(file_name) %>% 
        dplyr::select(c(1,2,3,4,5,6,7,11,16,17,20,21,22,23,24,25))

    }
    
    colnames(new_data) <- colname_list
    
    new_data <- new_data %>% 
      dplyr::mutate(year = base_year, .after = city_name)
    
    new_data <- new_data %>% 
      slice(c(-1,-2,-3,-4))

    base_data <- rbind(base_data, new_data)
    
  }
    
  output_data <- base_data %>% 
    drop_na(any_of("city_id"))
  
  return(output_data)
  
}


five_func <- function(id){
  
  new_id <- id
  
  num <- nchar(new_id)
  
  start_num <- stringr::str_sub(new_id, 1, 1)
    
  if (start_num == "0"){
    new_id <- stringr::str_sub(new_id, start = 2, end = num -1)
  } else if(start_num != "0"){
    new_id <- stringr::str_sub(new_id, start = 1, end = num -1)
  }
  
  
  return(new_id)
}

change_id <- function(new_data){
  
  city_id_list <- new_data %>% 
    select(city_id) %>% 
    unlist() %>%
    as.character() 
  
  new_id_list <- purrr::map(city_id_list, five_func) %>% 
    unlist()
  
  new_data <- new_data %>% 
    mutate(city_id = new_id_list) %>% 
    dplyr::mutate(across(.cols = c(city_id), ~ as.character(.x)))
  
  
  return(new_data)
  
}


save_table <- function(data){
  
  write.csv(data, file = here::here('03.build','aggregate','data','pop.csv'),
            fileEncoding = "CP932", row.names = FALSE)
  
  
}


