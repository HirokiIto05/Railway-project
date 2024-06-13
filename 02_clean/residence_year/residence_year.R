main <- function(){
  
  list_year <- seq(2000, 2010, by = 10)

  # aggregate
  df_residence_year_all <- purrr::map(year_list, read_residence_year) |> 
    dplyr::bind_rows() |>
    dplyr::distinct()

  # clean
  df_residence_year_all <- df_residence_year_all |>
    dplyr::mutate(
      category_residence_year = dplyr::case_when(
        category_residence_year %in% c("5年以上10年未満", "５年以上１０年未満") ~ "between5and10",
        category_residence_year %in% c("10年以上20年未満", "１０年以上２０年未満") ~ "between10and20",
        category_residence_year %in% c("20年以上", "２０年以上") ~ "over20years"
      )) |>
    tidyr::pivot_wider(
      names_from = category_residence_year,
      values_from = value
    ) |>
    dplyr::mutate(
      year = stringr::str_replace_all(year, "年", ""),
      year = as.numeric(year)
    ) |>
    dplyr::group_by(city_id, year) |>
    dplyr::mutate(
      across(c(between5and10, between10and20, over20years), as.numeric),
      over5years = sum(between5and10, between10and20, over20years),
      over10years = sum(between10and20, over20years)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      -c("between5and10", "between10and20")
    )

  # merger
  df_merger <- readxl::read_xlsx(here::here("01_data", "raw", "municipality_converter", "municipality_converter_jp.xlsx"))
  
  list_current_id <- df_merger |> 
    dplyr::distinct(id_muni2020) |> 
    dplyr::pull()

  list_vars <- c("over5years", "over10years", "over20years")
    
  df_master_residence_year <- purrr::map(list_current_id, adjust_city_id, df = df_residence_year_all, df_merger, list_vars) |> 
    dplyr::bind_rows()

  write.csv(
    df_master_residence_year,
    here::here("01_data", "intermediate", "covariates", "residenceyear_master.csv"),
    fileEncoding = "cp932", 
    row.names = FALSE)

  
}

read_residence_year <- function(year_i){
  
  if(year_i == 2000){
    
  df_raw <- read.csv(here::here("01_data", "raw", "residence_year", "2000.csv"), fileEncoding = "CP932")
    
    df_output <- df_raw |> 
      dplyr::select(
        category_area = 2, 
        category_residence_year = 4, 
        category_housetype = 6,
        category_family = 8,
        city_id = 9, 
        city_name = 10, 
        year = 12, 
        unit,
        value) |> 
      dplyr::mutate(
        dplyr::across(dplyr::everything(), ~stringr::str_replace_all(., "　", ""))
      ) |>
      dplyr::filter(
        category_area == "全域",
        category_residence_year %in% c("２０年以上", "１０年以上２０年未満", "５年以上１０年未満"), 
        category_family == "一般世帯数",
        category_housetype == "総数",
        unit == "世帯"
      )
    
  } else if(year_i == 2010){
    df_raw <- read.csv(here::here("01_data", "raw", "residence_year", "2010.csv"), fileEncoding = "CP932")

    df_output <- df_raw |> 
      dplyr::select(
        category_residence_year = 6, 
        category_household = 4,
        category_family = 2,
        city_id = 7, 
        city_name = 8, 
        year = 10, 
        unit,
        value) |>
      dplyr::mutate(
        dplyr::across(dplyr::everything(), ~stringr::str_replace_all(., "　", ""))
      ) |>
      dplyr::filter(
        category_household == "総数（家族類型）",
        category_residence_year %in% c("20年以上", "10年以上20年未満", "5年以上10年未満"), 
        category_family == "一般世帯数",
        unit == "世帯"
      ) 
 
  }

  df_output <- df_output |>
    dplyr::select(
      city_id, 
      city_name,
      year, 
      category_residence_year,
      unit, 
      value
    ) |>
    dplyr::mutate(
      across(everything(), as.character)
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
    )

  return(output_data)
  
}
