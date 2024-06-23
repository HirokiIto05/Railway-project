main <- function(){
  
  # year_list <- seq(1995,2010,by= 5)
  list_filenames <- c("1995_a.csv", "1995_b.csv", "2000.csv", "2005.csv", "2010.csv")
  
  # aggregate
  df_worker_all <- purrr::map(list_filenames, read_worker) |> 
    dplyr::bind_rows()

  # clean
  df_worker_all <- df_worker_all |>
    dplyr::mutate(
      across(c(city_id, year, value), as.numeric)
    ) |>
    dplyr::mutate(
      city_id = as.character(city_id)
    ) |>
    dplyr::rename(
      worker = value
    )

  # merger
  df_merger <- readxl::read_xlsx(here::here("01_data", "raw", "municipality_converter", "municipality_converter_jp.xlsx"))
  
  list_current_id <- df_merger |> 
    dplyr::distinct(id_muni2020) |> 
    dplyr::pull()

  list_vars <- c("worker")
  
  df_master_worker <- purrr::map(list_current_id, adjust_city_id, df = df_worker_all, df_merger, list_vars) |> 
    dplyr::bind_rows()

  # check; an output is only "1741"
  df_master_worker %>%
    dplyr::summarise(
        unique_id = length(unique(.$city_id)),
        .by = year
    ) |>
    dplyr::distinct(unique_id)

  # save    
  write.csv(
    df_master_worker,
    here::here("01_data", "intermediate", "covariates", "worker.csv"),
    fileEncoding = "cp932", 
    row.names = FALSE)

  
}

read_worker <- function(file_name_i){

  df_raw <- read.csv(here::here("01_data", 'raw', 'worker', file_name_i), fileEncoding = "CP932")

  df_output <- df_raw |> 
    dplyr::select(
      city_id = X.area,
      year = X.time,
      value = X.
      ) |>
    dplyr::mutate(
      year = stringr::str_sub(year, 1, 4)
    ) |>
    dplyr::mutate(
      dplyr::across(dplyr::everything(), as.character)
    )
  
  return(df_output)
  
}


adjust_city_id <- function(id_n, df, df_merger, list_vars){
  print(id_n)

  list_id <- df_merger |> 
    dplyr::filter(id_muni2020 == id_n) |> 
    dplyr::select(seq(2,10)) |> 
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "year",
      values_to = "id"
    ) |> 
    dplyr::distinct(id) |> 
    dplyr::pull()

  df_id_n <- df |> 
    dplyr::filter(city_id %in% list_id) |> 
    dplyr::mutate(
      year = as.character(year),
      city_id = as.character(city_id)
      )

  output_data <- df_id_n |>
    dplyr::reframe(
      dplyr::across(dplyr::any_of(list_vars), ~sum(., na.rm = TRUE)),
      .by = year
    ) |>
    dplyr::mutate(
      city_id = id_n
      # city_name = city_name_i
    )

  return(output_data)
  
}



adjust_city_id <- function(id_n, df, df_merger, list_vars){
  print(id_n)

  list_id <- df_merger |> 
    dplyr::filter(id_muni2020 == id_n) |> 
    dplyr::select(seq(2,10)) |> 
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "year",
      values_to = "id"
    ) |> 
    dplyr::distinct(id) |> 
    dplyr::pull()

  df_id_n <- df |> 
    dplyr::filter(city_id %in% list_id) |> 
    dplyr::mutate(
      year = as.character(year),
      city_id = as.character(city_id)
      )

  output_data <- df_id_n |>
    dplyr::reframe(
      dplyr::across(dplyr::any_of(list_vars), ~sum(., na.rm = TRUE)),
      .by = year
    ) |>
    dplyr::mutate(
      city_id = id_n
      # city_name = city_name_i
    )

  return(output_data)
  
}


main()