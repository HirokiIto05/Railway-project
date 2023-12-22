list_files <- list.files("04.analyze/new/01_data/data/leave_one_out/")

list_files <- list_files[2:17]

df_master <- read_df_csv("master", "master")

treatment_name <- "上北郡六戸町"

list_treatment <- df_master |> 
  dplyr::filter(
    treatment == 1
  ) |> 
  dplyr::distinct(city_name) |> 
  pull()

file_path

purrr::map(list_treatment, create_loo_data, df_master)

create_loo_data <- function(treatment_name, df_master) {
  
  # list_files <- list.files(paste0("04.analyze/new/01_data/data/leave_one_out/", treatment_name))
  
  path_treatment <- readRDS(here::here("04.analyze/new/01_data/data/pre_year/", paste0(treatment_name, '.rds')))
  
  id_treatment = df_master |> 
    dplyr::filter(
      city_name == treatment_name
    ) |> 
    distinct(city_id) |> 
    pull()
    
  df_based <- path_treatment |> 
    tidysynth::grab_synthetic_control() |> 
    dplyr::mutate(
      diff = real_y - synth_y,
      city_id = as.character(id_treatment),
      placebo = "0"
    )
  
  list_files <- path_treatment |> 
    tidysynth::grab_unit_weights() |> 
    dplyr::filter(
      weight > 0
    )
    distinct(unit) |> 
    pull() %>% 
    paste0(., '.rds')
  
  print(length(list_files))
  
  for (file_path in list_files) {
    
    file_path = '1212.rds'
    
    print(file_path)
    
    city_id_loo <- stringr::str_sub(file_path, start = 1, end = -5)
    
    path_leave_one_out <- readRDS(here::here("04.analyze/new/01_data/data/leave_one_out", 
                                             treatment_name, 
                                             file_path))
  
    
    df_plot_leave_one_out <- path_leave_one_out |>
      tidysynth::grab_unit_weights()
      # tidysynth::grab_synthetic_control() |> 
      # dplyr::mutate(
      #   diff = real_y - synth_y,
      #   city_id = city_id_loo,
      #   placebo = "1"
      # ) 
    
    df_based <- bind_rows(df_based, df_plot_leave_one_out)
    
  }
  
  year_end <- df_master |> 
    dplyr::filter(
      city_id == id_treatment
    ) |> 
    dplyr::distinct(year_end) |> 
    pull()
  
  df_output <- df_based |> 
    dplyr::mutate(
      year_end = year_end,
      after = time_unit - year_end,
      which_placebo = id_treatment
    )
    
  write.csv(df_output, 
            here::here("04.analyze/new/01_data/data/leave_one_out/output", 
                       paste0(id_treatment, ".csv")),
            row.names = FALSE
            )
  
  return(df_output)
  
}

for (treatment_name in list_treatment_name){
  
  path_treatment <- readRDS(here::here("04.analyze/new/01_data/data/pre_year/", paste0(treatment_name, '.rds')))
  
  list_files <- path_treatment |> 
    tidysynth::grab_unit_weights() |> 
    dplyr::filter(
      weight > 0
    ) |> 
    distinct(unit) |> 
    pull() %>% 
    paste0(., '.rds')
  
  print(length(list_files))
  
}

# 
# for (treatment_name in list_treatment_name){
#   
#   path_treatment <- readRDS(here::here("04.analyze/new/01_data/data/pre_year/", paste0(treatment_name, '.rds')))
#   
#   list_files <- path_treatment |> 
#     tidysynth::grab_signficance() |>  
#     dplyr::filter(
#       type == "Treated"
#     ) |> 
#     distinct(unit) |> 
#     pull() %>% 
#     paste0(., '.rds')
#   
#   print(length(list_files))
#   
# }
# 
# df_based <- path_treatment |> 
#   tidysynth::grab_signficance()
#   dplyr::mutate(
#     diff = real_y - synth_y,
#     city_id = as.character(id_treatment),
#     placebo = "0"
#   )
#   
check_mspe <- function(treatment_name, df_master) {
  
  # list_files <- list.files(paste0("04.analyze/new/01_data/data/leave_one_out/", treatment_name))
  
  path_treatment <- readRDS(here::here("04.analyze/new/01_data/data/pre_year/", paste0(treatment_name, '.rds')))
  
  id_treatment = df_master |> 
    dplyr::filter(
      city_name == treatment_name
    ) |>  
    distinct(city_id) |> 
    pull()
  
  df_based <- path_treatment |> 
    tidysynth::grab_signficance() |> 
    dplyr::filter(
      type == "Treated"
    ) |> 
    dplyr::mutate(
      city_id = as.character(id_treatment),
      placebo = "0",
      which_id = as.character(id_treatment)
    )
  
  list_files <- path_treatment |> 
    tidysynth::grab_unit_weights() |> 
    dplyr::filter(
      weight > 0
    ) |> 
  distinct(unit) |> 
    pull() %>% 
    paste0(., '.rds')
  
  for (file_path in list_files) {
    
    print(file_path)
    
    city_id_loo <- stringr::str_sub(file_path, start = 1, end = -5)
    
    path_leave_one_out <- readRDS(here::here("04.analyze/new/01_data/data/leave_one_out", 
                                             treatment_name, 
                                             file_path))
    
    
    df_plot_leave_one_out <- path_leave_one_out |> 
      tidysynth::grab_signficance() |>  
      dplyr::filter(
        type == "Treated"
      ) |> 
      dplyr::mutate(
        city_id = as.character(id_treatment),
        placebo = "1",
        which_id = as.character(city_id_loo)
      )
    
    df_based <- bind_rows(df_based, df_plot_leave_one_out)
    
  }
  
  year_end <- df_master |> 
    dplyr::filter(
      city_id == id_treatment
    ) |> 
    dplyr::distinct(year_end) |> 
    pull()
  
  df_output <- df_based |> 
    dplyr::mutate(
      year_end = year_end,
      after = time_unit - year_end,
      which_placebo = id_treatment,
      which_name = treatment_name
    )
  
  return(df_output)
  
}

df_check <-purrr::map(list_treatment_name, check_mspe, df_master)


df_check_a


path_leave_one_out <- readRDS(here::here("04.analyze/new/01_data/data/leave_one_out", 
                                         treatment_name, 
                                         '15405.rds'))


df_plot_leave_one_out <- path_leave_one_out |> 
  tidysynth::grabjjj


aa <- path_treatment |> 
  grab_unit_weights()


