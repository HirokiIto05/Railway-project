main <- function(){
  
  treatment_data <- load_data("master_data", "treatment_data.csv")
  
  treatment_id_list <- treatment_data |> 
    dplyr::distinct(city_id) |> 
    unlist() |> 
    as.character()
  
  all_synthetic_data <- aggregate_synth(treatment_data)
  
}


load_data <- function(folder_name, file_name){
  
  output_data <- read.csv(here::here('03.build',folder_name,'data', file_name),
                          fileEncoding = "CP932")
  return(output_data)
  
}


read_each_treatment <- function(id){
  
  file_name <- paste0(id,'.rds')
  
  synth_data <- readRDS(here::here('04.analyze','synthetic_control', 'figure',
                                   'synth_each', 'working', 'table', file_name))
  
  output_data <- synth_data |> 
    grab_synthetic_control() |> 
    dplyr::mutate(city_id = id, .before = time_unit)
  
  return(output_data)
  
}


aggregate_synth <- function(treatment_data){
  
  id_lists <- treatment_data |> 
    dplyr::distinct(city_id) |> 
    unlist() |> 
    as.character()
  
  output_data <- purrr::map(id_lists, read_each_treatment) |> 
    dplyr::bind_rows()
  
  return(output_data)
  
}


save_table <- function(data){
  
  file_name <- "all_synth_data.csv"
  
  write.csv(data, here::here('03.build','synthetic_table', 'table',file_name),
            fileEncoding = "CP932", row.names = FALSE)
  
}

