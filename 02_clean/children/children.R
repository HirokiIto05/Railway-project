main <- function(){
  
  year_list <- seq(1995,2010,by= 5)
  
  # aggregate
  df_child_all <- purrr::map(year_list, read_child) |> 
    dplyr::bind_rows()

  # clean
  df_child_all <- df_child_all |>
    dplyr::mutate(
      across(c(city_id, year, value), as.numeric)
    ) |>
    dplyr::mutate(
      city_id = as.character(city_id)
    )  |>
    dplyr::rename(
      household_with_child = value
    )

  # merger
  df_merger <- readxl::read_xlsx(here::here("01_data", "raw", "municipality_converter", "municipality_converter_jp.xlsx"))
  
  list_current_id <- df_merger |> 
    dplyr::distinct(id_muni2020) |> 
    dplyr::pull()

  list_vars <- c("household_with_child")
  
  df_master_child <- purrr::map(list_current_id, adjust_city_id, df = df_child_all, df_merger, list_vars) |> 
    dplyr::bind_rows()

  # check; an output is only "1741"
  df_master_child %>%
    dplyr::summarise(
        unique_id = length(unique(.$city_id)),
        .by = year
    ) |>
    dplyr::distinct(unique_id)

  # save    
  write.csv(
    df_master_child,
    here::here("01_data", "intermediate", "covariates", "children_master.csv"),
    fileEncoding = "cp932", 
    row.names = FALSE)

  
}



read_child <- function(year_i){
  
  file_name <- paste0(year_i, ".csv")
  
  df_child <- read.csv(here::here("01_data", 'raw', 'children', file_name), fileEncoding = "CP932")

  if(year_i == 1995){
    category_name = "６歳未満親族のいる一般世帯数"
  } else if(year_i == 2000){
    category_name = "６歳未満の親族のいる一般世帯数"
  } else if(year_i == 2005){
    category_name = "６歳未満親族のいる一般世帯数"
  } else if(year_i == 2010){
    category_name = "6歳未満世帯員のいる一般世帯数"
  } 


  if(year_i <= 2005){
    df_child <- df_child |> 
      dplyr::select(
        category_area  = 2,
        category_family = 4,
        category_household = 6,
        city_id = 7,
        city_name = 8,
        year = 10,
        household = 11,
        value = 12
        ) |> 
      dplyr::filter(
        category_area == "全域",
        category_family == "総数",
        category_household == category_name,
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
  } else if (year_i == 2010){
    df_child <- df_child |> 
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

  df_child <- df_child |>
    dplyr::mutate(
      across(everything(), as.character)
    )
  
  return(df_child)
  
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

