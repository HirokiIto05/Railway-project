main <- function(){
  
  pop_data <- load_pop() |> 
    dplyr::mutate(city_id = as.character(city_id))
  
  main_data <- load_main() |> 
    dplyr::filter(dummy == 1) |> 
    dplyr::distinct(adjust_id, .keep_all = TRUE) |> 
    dplyr::select(-adjust_id, -city_name, -new_name) |> 
    dplyr::mutate(city_id = as.character(city_id))
  
  treatment_pop <- add_pop(main_data, pop_data)
  
  percent_treatment <- add_percent(data)
  
  save_table(percent_treatment)
  
}


check_na <- pop_data |> 
  dplyr::distinct(city_id, .keep_all = TRUE)


load_main <- function(){
  
  new_data <- readxl::read_xlsx(here::here('02.raw', 'main' , 'main_data.xlsx'))
  
  return(new_data)
  
}


load_pop <- function(){
  
  output_data <- read.csv(here::here('03.build','city_adjust','data','all_data.csv'),
                   fileEncoding = "CP932")
  
  return(output_data)
  
}



add_pop <- function(main_data, pop_data){
  
  treatment_id <- main_data |> 
    dplyr::distinct(city_id) |> 
    unlist() |> 
    as.character() 
  
  test_data <- dplyr::filter(pop_data, city_id %in% treatment_id)
  
  output_data <- dplyr::left_join(test_data, main_data, by = "city_id")

  return(output_data)
}

add_percent <- function(data){
  
  city_id_lists <- unique(data$city_id)
  
  output_data <- lapply(city_id_lists, calculate_percent, data) |> 
    dplyr::bind_rows()
  
  return(output_data)
}


calculate_percent <- function(id,data){
  
  id_n <- data |> 
    dplyr::filter(city_id == id,
                  year == 1995) |> 
    dplyr::select(working) |> 
    as.numeric()
    
  new_data <- data |> 
    dplyr::filter(city_id == id) |> 
    dplyr::mutate(percent = working/id_n)
  
  return(new_data)
}

save_table <- function(data){
  
  write.csv(data, file = here::here('03.build','base_percent','data','base_treatment.csv'),
            fileEncoding = "CP932", row.names = FALSE)
  
}





