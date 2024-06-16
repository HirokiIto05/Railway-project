main <- function(){
  
  year_list <- seq(1995,2010,by= 5)
  
  # aggregate
  df_worker_all <- purrr::map(year_list, read_worker) |> 
    dplyr::bind_rows()

  # clean
  df_worker_all <- df_worker_all |>
    dplyr::mutate(
      across(c(city_id, year, value), as.numeric)
    ) |>
    dplyr::mutate(
      city_id = as.character(city_id)
    )  |>
    dplyr::rename(
      household_with_worker = value
    )

  # merger
  df_merger <- readxl::read_xlsx(here::here("01_data", "raw", "municipality_converter", "municipality_converter_jp.xlsx"))
  
  list_current_id <- df_merger |> 
    dplyr::distinct(id_muni2020) |> 
    dplyr::pull()

  list_vars <- c("household_with_worker")
  
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
    here::here("01_data", "intermediate", "covariates", "worker_master.csv"),
    fileEncoding = "cp932", 
    row.names = FALSE)

  
}


year_i <- 1995

read_worker <- function(year_i){

  # 1995_a; FEH_00200521_240616150337.csv
  # 1995_b; FEH_00200521_240616151544.csv
  # 2000;   FEH_00200521_240616151140.csv
  # 2005;   FEH_00200521_240616151348.csv
  # 2010;   FEH_00200521_240616151753.csv

  
  file_name <- paste0(year_i, ".csv")
  
  if(year_i == 1995){
    df_raw_1 <- read.csv(here::here("01_data", 'raw', 'worker', "FEH_00200521_240616150337.csv"), fileEncoding = "CP932")

    df_raw_1 |> View()

    df_output_1 <- df_raw_1 |> 
      dplyr::select(
        category_area  = 1,
        category_gender = 2,
        category_age = 3,
        city_id = 4,
        city_name = 5,
        year = 12,
        unit,
        value
        ) |>
      dplyr::mutate(
        dplyr::across(dplyr::everything(), ~stringr::str_replace_all(., "　", ""))
      ) |>
      dplyr::filter(
        category_area == "全域",
        category_gender == "総数",
        category_age == "総数（15歳以上）",
        category_status == "就業者"
      ) |> 
      dplyr::select(
        city_id,
        city_name,
        year,
        value
      )

    df_raw_2 <- read.csv(here::here("01_data", 'raw', 'worker', "FEH_00200521_240616151544.csv"), fileEncoding = "CP932")

    df_output_2 <- df_raw_2 |> 
      dplyr::select(
        category_area  = 2,
        category_gender = 4,
        category_age = 6,
        category_status = 8,
        city_id = 9,
        city_name = 10,
        year = 12,
        unit,
        value
        ) |>
      dplyr::mutate(
        dplyr::across(dplyr::everything(), ~stringr::str_replace_all(., "　", ""))
      ) |>
      dplyr::filter(
        category_area == "全域",
        category_gender == "総数",
        category_age == "総数（15歳以上）",
        category_status == "就業者"
      ) |> 
      dplyr::select(
        city_id,
        city_name,
        year,
        value
      )
  } else if(year_i == 2000){

    df_raw <- read.csv(here::here("01_data", 'raw', 'worker', "FEH_00200521_240616151140.csv"), fileEncoding = "CP932")

    df_worker <- df_raw_1 |> 
      dplyr::select(
        category_area  = 2,
        category_gender = 4,
        category_age = 6,
        category_status = 8,
        city_id = 9,
        city_name = 10,
        year = 12,
        unit,
        value
        ) |>
      dplyr::mutate(
        dplyr::across(dplyr::everything(), ~stringr::str_replace_all(., "　", ""))
      ) |>
      dplyr::filter(
        category_area == "全域",
        category_gender == "総数",
        category_age == "総数（15歳以上）",
        category_status == "就業者"
      ) |> 
      dplyr::select(
        city_id,
        city_name,
        year,
        value
      )
    
  } else if(year_i == 2005){

    df_raw <- read.csv(here::here("01_data", 'raw', 'worker', "FEH_00200521_240616151348.csv"), fileEncoding = "CP932")

    df_worker <- df_raw_1 |> 
      dplyr::select(
        category_area  = 2,
        category_gender = 4,
        category_age = 6,
        category_status = 8,
        city_id = 9,
        city_name = 10,
        year = 12,
        unit,
        value
        ) |>
      dplyr::mutate(
        dplyr::across(dplyr::everything(), ~stringr::str_replace_all(., "　", ""))
      ) |>
      dplyr::filter(
        category_area == "全域",
        category_gender == "総数",
        category_age == "総数（15歳以上）",
        category_status == "就業者"
      ) |> 
      dplyr::select(
        city_id,
        city_name,
        year,
        value
      )
    
  } else if (year_i == 2010){
    df_worker <- df_worker |> 
      dplyr::select(
        category_household = 2,
        category_area  = 4,
        category_family = 6,
        city_id = 7,
        city_name = 8,
        year = 10,
        household = 11,
        value = 12
        ) |> 
      dplyr::filter(
        category_household == category_name,
        category_area == "全域",
        category_family == "総数（世帯人員）",
        household == "世帯"
      ) |> 
      dplyr::mutate(
        year = stringr::str_replace_all(year, "年", ""),
        year = as.numeric(year)) |> 
      dplyr::select(
        city_id,
        city_name,
        year,
        value
      ) 
  }

  df_worker <- df_worker |>
    dplyr::mutate(
      across(everything(), as.character)
    )
  
  return(df_worker)
  
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


read_worker_3 <- function(year_i){
  
  file_name <- paste0(year_i, ".csv")
  
  if(year_i == 1995){
    df_raw <- read.csv(here::here("01_data", 'raw', 'worker_3', "1995.csv"), fileEncoding = "CP932")
    df_raw |> View()

    df_output <- df_raw |> 
      dplyr::select(
        category_area  = 2,
        category_position = 4,
        category_gender = 6,
        category_industry = 8,
        category_job = 10,
        city_id = 11,
        city_name = 12,
        year = 14,
        unit,
        value
        ) |>
      dplyr::mutate(
        dplyr::across(dplyr::everything(), ~stringr::str_replace_all(., "　", ""))
      ) |>
      dplyr::filter(
        category_area == "全域",
        category_position == "総数",
        category_gender == "総数",
        category_job == "総数"
      ) |> 
      dplyr::select(
        city_id,
        city_name,
        year,
        value
      )
    
  } else if(year_i == 2000){
    df_raw <- read.csv(here::here("01_data", 'raw', 'worker_3', "2000.csv"), fileEncoding = "CP932")

    df_output <- df_raw |> 
      dplyr::select(
        category_position = 2,
        category_gender = 4,
        category_age = 6,
        category_job = 8,
        city_id = 9,
        city_name = 10,
        year = 12,
        unit,
        value
        ) |>
      dplyr::mutate(
        dplyr::across(dplyr::everything(), ~stringr::str_replace_all(., "　", ""))
      ) |>
      dplyr::filter(
        category_position == "総数",
        category_gender == "総数",
        category_age == "総数（15歳以上）",
        category_job == "総数"
      ) |> 
      dplyr::select(
        city_id,
        city_name,
        year,
        value
      )

    } else if (year_i == 2005){
    df_raw <- read.csv(here::here("01_data", 'raw', 'worker_3', "2005_a.csv"), fileEncoding = "CP932")

    df_raw |>colnames()

    df_output <- df_raw |> 
      dplyr::select(
        category_position = 2,
        category_gender = 4,
        category_age = 6,
        category_job = 8,
        city_id = 9,
        city_name = 10,
        year = 12,
        unit,
        value
        ) |>
      dplyr::mutate(
        dplyr::across(dplyr::everything(), ~stringr::str_replace_all(., "　", ""))
      ) |>
      dplyr::filter(
        category_position == "全域",
        category_gender == "総数",
        category_age == "総数（15歳以上）",
        category_job == "総数"
      ) |> 
      dplyr::select(
        city_id,
        city_name,
        year,
        value
      )
    } else if (year_i == 2010){
    df_worker <- df_worker |> 
      dplyr::select(
        category_household = 2,
        category_area  = 4,
        category_family = 6,
        city_id = 7,
        city_name = 8,
        year = 10,
        household = 11,
        value = 12
        ) |> 
      dplyr::filter(
        category_household == category_name,
        category_area == "全域",
        category_family == "総数（世帯人員）",
        household == "世帯"
      ) |> 
      dplyr::mutate(
        year = stringr::str_replace_all(year, "年", ""),
        year = as.numeric(year)) |> 
      dplyr::select(
        city_id,
        city_name,
        year,
        value
      ) 
  }

  df_worker <- df_worker |>
    dplyr::mutate(
      across(everything(), as.character)
    )
  
  return(df_worker)
  
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
