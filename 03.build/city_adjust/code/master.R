main <- function(){
  
  pop_data <- load_data("city_adjust", "pop_data.csv")
  age_data <- load_data("city_adjust", "age_data.csv")
  
  master_data <- create_complete(pop_data, age_data)
  
  save_table(master_data, "city_adjust", "all_data.csv")
    
}

load_data <- function(folder_name, file_name){
  
  new_data <- read.csv(here::here('03.build', folder_name, 'data', file_name),
                       fileEncoding = "CP932")
  
  return(new_data)
  
}


create_complete <- function(pop_data, age_data){
  
  age_data <- age_data |> 
    dplyr::select(-city_name, -region_name, -total)
  
  output_data <- left_join(pop_data, age_data, by = c("year", "city_id"))
  
  return(output_data)

}


save_table <- function(data, folder_name, file_name){
  
  write.csv(data, here::here('03.build', folder_name, 'data', file_name),
            fileEncoding = "CP932", row.names = FALSE)
  
}
