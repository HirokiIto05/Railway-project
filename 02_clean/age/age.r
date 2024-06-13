main <- function(){
  
  list_year <- seq(1995, 2020)

  # aggregate
  df_age_all <- purrr::map(list_year, read_age) |> 
    dplyr::bind_rows()

  # clean
  df_age_all <- df_age_all |>
    dplyr::mutate(city_id = stringr::str_sub(city_id, start = 1, end = -2)) |> 
    dplyr::mutate(across(-c(city_name, year), as.numeric))
    
  # merger
  df_merger <- readxl::read_xlsx(here::here("01_data", "raw", "municipality_converter", "municipality_converter_jp.xlsx"))
  
  list_current_id <- df_merger |> 
    dplyr::distinct(id_muni2020) |> 
    dplyr::pull()

  list_vars <- df_age_all |> 
    dplyr::select(-city_id, -city_name, -year) |>
    colnames()
    
  df_master_age <- purrr::map(list_current_id, adjust_city_id, df = df_age_all, df_merger, list_vars) |> 
    dplyr::bind_rows()

  # check; an output is only "1741"
  df_master_age %>%
    dplyr::summarise(
        unique_id = length(unique(.$city_id)),
        .by = year
    ) |>
    dplyr::distinct(unique_id)

  # save
  write.csv(
    df_master_age,
    here::here("01_data", "intermediate", "covariates", "age_master.csv"),
    fileEncoding = "cp932", 
    row.names = FALSE)

  
}


read_age <- function(year_i){
  print(year_i)

  if(year_i < 2015){
    if(year_i < 2013){
      raw_name <- paste0((stringr::str_sub(as.character(year_i), start = 3, end = 4)), "04snen")
    } else if(year_i < 2015){
      raw_name <- paste0((stringr::str_sub(as.character(year_i), start = 3, end = 4)), "08nsnen")
    }

    file_name <- here::here('01_data', 'raw', 'age', paste0(raw_name, ".xls"))

    df_raw <-  readxl::read_xls(file_name, skip = 1)

    df_raw <- df_raw |>
      dplyr::select(
        city_id    = "団体コード",
        prefecture = "都道府県名",
        city_name  = "市区町村名",
        gender     = "性別",
        total      = "総数",
        r0_4       = "0～4歳",
        r5_9       = "5～9",
        r10_14     = "10～14",
        r15_19     = "15～19",
        r20_24     = "20～24",
        r25_29     = "25～29",
        r30_34     = "30～34",
        r35_39     = "35～39",
        r40_44     = "40～44",
        r45_49     = "45～49",
        r50_54     = "50～54",
        r55_59     = "55～59",
        r60_64     = "60～64",
        r65_69     = "65～69",
        r70_74     = "70～74",
        r75_79     = "75～79",
        r80_over   = "80歳以上"
        )
    
    df_output <- df_raw |> 
      dplyr::filter(gender == "計") |>
      tidyr::drop_na()
    
  } else if(year_i < 2021) {
    year_i <- 2016
    if(year_i %in% c(2016, 2017)){
      raw_name <- paste0((stringr::str_sub(as.character(year_i), start = 3, end = 4)), "08nanen")
    } else if(year_i < 2015){
      raw_name <- paste0((stringr::str_sub(as.character(year_i), start = 3, end = 4)), "08nsnen")
    }

    file_name <- here::here('01_data', 'raw', 'age', paste0(raw_name, ".xls"))

    df_raw <-  readxl::read_xls(file_name, skip = 1)

    df_raw <- df_raw |>
      dplyr::select(
        city_id    = 1,
        prefecture = 2,
        city_name  = 3,
        gender     = "性別",
        total      = "総数",
        r0_4       = "0～4歳",
        r5_9       = "5～9",
        r10_14     = "10～14",
        r15_19     = "15～19",
        r20_24     = "20～24",
        r25_29     = "25～29",
        r30_34     = "30～34",
        r35_39     = "35～39",
        r40_44     = "40～44",
        r45_49     = "45～49",
        r50_54     = "50～54",
        r55_59     = "55～59",
        r60_64     = "60～64",
        r65_69     = "65～69",
        r70_74     = "70～74",
        r75_79     = "75～79",
        r80_84     = "80～84",
        r85_89     = "85～89",
        r90_94     = "90～94",
        r95_99     = "95～99",
        r100_over  = "100歳以上"
        )
    
    df_output <- df_raw |> 
      dplyr::filter(gender == "計") |>
      tidyr::drop_na()
    
    df_output <- df_output |>
      dplyr::group_by(city_id) |>
      dplyr::mutate(
        r80_over = sum(r80_84, r85_89, r90_94, r95_99, r100_over, na.rm = TRUE)) |>
      dplyr::ungroup()

  }
  
  df_output <- df_output |> 
    dplyr::mutate(year = year_i) |> 
    dplyr::select(
      city_id,
      city_name,
      year,
      total,
      r0_4,
      r5_9,
      r10_14,
      r15_19,
      r20_24,
      r25_29,
      r30_34,
      r35_39,
      r40_44,
      r45_49,
      r50_54,
      r55_59,
      r60_64,
      r65_69,
      r70_74,
      r75_79,
      r80_over
    ) |>
    dplyr::mutate(
        across(everything(), as.character))
  
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
