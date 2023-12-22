main <- function(){
  
  df_master <- read_df_csv("master", "master") 
  
  # list_file_path <- list.files("04.analyze/new/01_data/data/main/")
  list_treatment_files <- df_master |> 
    dplyr::filter(
      treatment == 1
    ) |> 
    arrange(city_id) |> 
    distinct(city_name) |> 
    pull() %>%
    paste0(., ".rds")
  
  # treatment_name_lists <- unique(df_treatment$city_name)
  
  placebo_df <- purrr::map(list_treatment_files, create_placebo_df, df_master) |> 
    dplyr::bind_rows() 
  
  write.csv(placebo_df, here::here("04.analyze/new/03_plot/03_placebo/data/placebo_table_pre_year.csv"),
            fileEncoding = "CP932", row.names = FALSE)
  
}

create_placebo_df <- function(file_path, df_master){
  
  print(file_path)
  
  city_name_t <- stringr::str_sub(file_path, start = 1, end = -5)
  base_plot <- readRDS(paste0("04.analyze/new/01_data/data/pre_year/", file_path))
  
  placebo_df_each <- base_plot |> 
    tidysynth::plot_placebos()
  
  placebo_df_each <- placebo_df_each$data 
  
  placebo_df_each <- placebo_df_each |>   
    dplyr::rename(city_id = .id)
  
  df_treatment <- df_master |> 
    dplyr::filter(city_name == city_name_t)
  
  treated_name <- df_treatment |> 
    dplyr::distinct(city_name) |> 
    dplyr::pull()
  year_end <- df_treatment |> 
    dplyr::distinct(year_end) |> 
    dplyr::pull()
  
  output_df <- placebo_df_each |> 
    dplyr::mutate(city_name = treated_name, 
                  year_end = year_end,
                  which_placebo = city_name_t)
  
  return(output_df)
  
}
