
# change_file_names -------------------------------------------------------

change_file_names <- function(treatment_name) {
  
  originals <- list.files(paste0("04.analyze/new/01_data/data/leave_one_out/", treatment_name))
  
  list_originals <- stringr::str_remove(originals, pattern = ".rds")
  
  df_id_name <- df_master |> 
    dplyr::select(
      city_name,
      city_id) |> 
    dplyr::distinct()
  
  
  df_originals <- tibble(
    city_name = list_originals
  ) |> 
    dplyr::mutate(
      file_name = paste0(list_originals, '.rds')
    ) |> 
    left_join(df_id_name)
  
  list_id <- df_originals |>
    dplyr::distinct(city_id) |> 
    tidyr::drop_na(city_id) |> 
    pull()
  
  for (id_n in list_id){
    
    original_name <- df_originals |> 
      dplyr::filter(
        city_id == id_n
      ) |> 
      dplyr::distinct(file_name) |> 
      pull()
    
    replacement_id <- df_originals |> 
      dplyr::filter(
        city_id == id_n
      ) |> 
      dplyr::distinct(city_id) |> 
      pull() |> 
      paste0(".rds")
    
    original <- here::here("04.analyze/new/01_data/data/leave_one_out", treatment_name, original_name)
    replacement <-  here::here("04.analyze/new/01_data/data/leave_one_out", treatment_name, replacement_id)
    
    file.rename(original, replacement)
  }
  
}


change_file_names('常呂郡置戸町')

change_file_names('足寄郡陸別町')
