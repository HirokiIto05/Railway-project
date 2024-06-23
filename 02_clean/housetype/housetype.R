main <- function() {
  
  list_year <- seq(1995, 2010, by = 5)

  # aggregate 
  df_housetype_all <- purrr::map(list_year, read_housetype) |>
    dplyr::bind_rows()

  # clean 
  df_housetype_all <- df_housetype_all |>
    dplyr::mutate(
      category_housetype = dplyr::case_when(
        category_housetype == "住宅に住む一般世帯" ~ "total_household",
        category_housetype == "うち住宅に住む一般世帯" ~ "total_household",
        category_housetype == "持ち家" ~ "owned",
        category_housetype == "主世帯 持ち家" ~ "owned"
      )) |>
    tidyr::pivot_wider(
      names_from = category_housetype,
      values_from = value
    ) |>
    dplyr::mutate(
      year = stringr::str_replace_all(year, "年", ""),
      year = as.numeric(year),
      across(c("total_household", "owned"), as.numeric)
    )

  # merger municipalities
  df_merger <- readxl::read_xlsx(here::here("01_data", "raw", "municipality_converter", "municipality_converter_jp.xlsx"))
  
  list_current_id <- df_merger |> 
    dplyr::distinct(id_muni2020) |> 
    dplyr::pull()

    list_vars <- c("owned", "total_household")
  
  df_master_housetype <- purrr::map(list_current_id, adjust_city_id, df = df_housetype_all, df_merger, list_vars) |> 
    dplyr::bind_rows()

  write.csv(
    df_master_housetype,
    here::here("01_data", "intermediate", "covariates", "housetype.csv"),
    fileEncoding = "cp932", 
    row.names = FALSE)

}


read_housetype <- function(year_i) {

  if(year_i == 1995){

    df_raw <- read.csv(here::here("01_data", "raw", "housetype", "1995.csv"), fileEncoding = "CP932")
    
    df_output <- df_raw |>
      dplyr::select(
        category_area = 2,
        category_housetype = 4,
        category_family = 6,
        city_id = 7,
        city_name = 8,
        year = 10,
        unit = 11,
        value = 12
      ) |> 
      dplyr::mutate(
        dplyr::across(dplyr::everything(), ~stringr::str_replace_all(., "　", ""))
      ) |>
      dplyr::filter(
        category_area == "全域",
        category_family == "一般世帯数",
        unit == "世帯",
        category_housetype %in% c("持ち家", "住宅に住む一般世帯")
      ) |>
      dplyr::select(-category_family, -category_area)

  } else if (year_i == 2000){

    df_raw <- read.csv(here::here("01_data", "raw", "housetype", "2000_a.csv"), fileEncoding = "CP932")

    df_output_1 <- df_raw |> 
      dplyr::select(
        category_area = 2,
        category_housetype = 4,
        category_family = 6,
        city_id = 7,
        city_name = 8,
        year = 10,
        unit = 11,
        value = 12
      ) |>
      dplyr::mutate(
        dplyr::across(dplyr::everything(), ~stringr::str_replace_all(., "　", ""))
      ) |>
      dplyr::filter(
        category_area == "全域",
        category_family == "一般世帯数",
        unit == "世帯",
        category_housetype %in% c("持ち家", "住宅に住む一般世帯")
      ) |>
      dplyr::select(-category_family, -category_area)

    df_raw <- read.csv(here::here("01_data", "raw", "housetype", "2000_b.csv"), fileEncoding = "CP932")

    df_output_2 <- df_raw |>
      dplyr::select(
        category_area = 2,
        category_housetype = 4,
        category_family = 6,
        city_id = 7,
        city_name = 8,
        year = 10,
        unit = 11,
        value = 12
      ) |>
      dplyr::mutate(
        dplyr::across(dplyr::everything(), ~stringr::str_replace_all(., "　", ""))
      ) |>
      dplyr::filter(
        category_area == "全域",
        category_family == "一般世帯数",
        unit == "世帯",
        category_housetype %in% c("持ち家", "住宅に住む一般世帯")
      ) |>
      dplyr::select(-category_family, -category_area)

  df_output <- df_output_1 |>
    dplyr::bind_rows(df_output_2)

  } else if (year_i == 2005){

    df_raw <- read.csv(here::here("01_data", "raw", "housetype", "2005.csv"), fileEncoding = "CP932")

    df_output <- df_raw |>
      dplyr::select(
        category_area = 2,
        category_housetype = 4,
        category_family = 6,
        city_id = 7,
        city_name = 8,
        year = 10,
        unit = 11,
        value = 12
      ) |>
      dplyr::mutate(
        dplyr::across(dplyr::everything(), ~stringr::str_replace_all(., "　", ""))
      ) |>
      dplyr::filter(
        category_area == "全域",
        category_family == "一般世帯数",
        unit == "世帯",
        category_housetype %in% c("持ち家", "住宅に住む一般世帯")
      ) |>
      dplyr::select(-category_family, -category_area)

  }else if(year_i == 2010){

    df_raw <- read.csv(here::here("01_data", "raw", "housetype", "2010.csv"), fileEncoding = "CP932")

    df_output <- df_raw |>
      dplyr::select(
        category_family = 2,
        category_housetype = 6,
        category_household = 4,
        city_id = 7,
        city_name = 8,
        year = 10,
        unit = 11,
        value = 12
      )  |> 
      dplyr::mutate(
        dplyr::across(dplyr::everything(), ~stringr::str_replace_all(., "　", ""))
      ) |> 
      dplyr::filter(
        category_family == "一般世帯数",
        category_household == "総数（家族類型）",
        unit == "世帯",
        category_housetype %in% c("主世帯 持ち家", "うち住宅に住む一般世帯")
      ) |>
      dplyr::select(-category_family, -category_household)

  }

  df_output <- df_output |>
    dplyr::select(
      city_id, 
      city_name,
      year, 
      category_housetype,
      unit, 
      value
    ) |>
    dplyr::mutate(
      across(everything(), as.character)
    )

  return(df_output)

}


adjust_city_id <- function(id_i, df, df_merger, list_vars){
  print(id_i)

  list_id <- df_merger |> 
    dplyr::filter(id_muni2020 == id_i) |> 
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
      city_id = id_i
    )

  return(output_data)
  
}


main()