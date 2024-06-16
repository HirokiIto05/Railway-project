# 事業所・企業統計調査
# 1986; 0000040355
# 1991; 0000040728
# 1994; 0000040882 # only a total num, must estimate the number of private companies.
# 1996; 0000041027 # contain only private companies.
# 1999; 0000041313 # contain only private companies.
# 2001; 0000041455
# 2004; 0000041658 # contain only private companies.
# 2006; 0003000452 # ?

# 経済センサス - 基礎調査
# 2009; 0003032532
# 2014; 0003117639
# 2019_a; 0003422213 - private
# 2019_b; 0003414253 - public

# 経済センサス - 活動調査
# 2012; 0003085552 # contain only private companies.
# 2016; 0003218500 # contain only private companies.

main <- function() {
  
  list_year <- c(
    1986,
    1991,
    1994,
    1996,
    1999,
    2001,
    2004,
    2006,
    2009, 
    2014,
    2019,
    2012,
    2016)

  # aggregate 
  df_office_all <- purrr::map(list_year, read_office, folder_name = "private_companies") |>
    dplyr::bind_rows()
  
  a <- df_office_all |> 
    dplyr::mutate(
      year = stringr::str_replace_all(year, "年", ""),
      year = as.numeric(year),
      across(-c("city_name"), as.numeric)
    ) 
  a |> 
  dplyr::filter(city_id == 1100) |> 
  arrange(year) |>
  mutate(
    lag_company_num = lag(company_num),
    increase_rate = (company_num - lag_company_num) / lag_company_num,
    heisei = year - 1988) |>
  View()


  # merger municipalities
  df_merger <- readxl::read_xlsx(here::here("01_data", "raw", "municipality_converter", "municipality_converter_jp.xlsx"))
  
  list_current_id <- df_merger |> 
    dplyr::distinct(id_muni2020) |> 
    dplyr::pull()

    list_vars <- c("owned", "total_household")
  
  df_master_housetype <- purrr::map(list_current_id, adjust_city_id, df = df_housetype_all, df_merger, list_vars) |> 
    dplyr::bind_rows()

    write.csv(
      here::here("01_data", "intermediate", "covariates", "private_companies_master.csv"),
      fileEncoding = "cp932", 
      row.names = FALSE)

}

read_office <- function(year_i, folder_name) {
  print(year_i)

  if(year_i == 1986){

    df_raw <- read.csv(here::here("01_data", "raw", folder_name, paste0(year_i, ".csv")), fileEncoding = "CP932")

    df_output <- df_raw |>
      dplyr::select(
        category_org = 2,
        category_industry = 4,
        category_emp = 6,
        city_id = 7,
        city_name = 8,
        year = 10,
        value = 12
      ) |>
      dplyr::mutate(
        dplyr::across(dplyr::everything(), ~stringr::str_replace_all(., "　", ""))
      ) |>
      dplyr::filter(
        category_org == "うち民営",
        category_industry == "全産業",
        category_emp == "総数"
      ) 

  } else if (year_i == 1991){

    df_raw <- read.csv(here::here("01_data", "raw", folder_name, paste0(year_i, ".csv")), fileEncoding = "CP932")

    df_output <- df_raw |> 
      dplyr::select(
        category_org = 4,
        category_industry = 2,
        category_emp = 6,
        city_id = 5,
        city_name = 6,
        year = 8,
        unit,
        value
      ) |>
      dplyr::mutate(
        dplyr::across(dplyr::everything(), ~stringr::str_replace_all(., "　", ""))
      ) |>
      dplyr::filter(
        category_org == "民営",
        category_industry == "全産業",
        category_emp == "総数"        
      )


  } else if (year_i == 1994){


    df_raw <- read.csv(here::here("01_data", "raw", folder_name, paste0(year_i, ".csv")), fileEncoding = "CP932")

    df_output <- df_raw |>
      dplyr::select(
        category_org = 2,
        city_id = 3,
        city_name = 4,
        year = 6,
        unit,
        value
      ) |>
      dplyr::mutate(
        dplyr::across(dplyr::everything(), ~stringr::str_replace_all(., "　", ""))
      ) |>
      dplyr::filter(
        category_org == "総数事業所数"
      ) 

  }else if(year_i == 1996){

    df_raw <- read.csv(here::here("01_data", "raw", folder_name, paste0(year_i, ".csv")), fileEncoding = "CP932")

    df_output <- df_raw |> 
      dplyr::select(
        category_org = 2,
        city_id = 3,
        city_name = 4,
        year = 6,
        unit,
        value
      ) |>
      dplyr::mutate(
        dplyr::across(dplyr::everything(), ~stringr::str_replace_all(., "　", ""))
      ) |>
      dplyr::filter(
        category_org == "総数民営事業所数",
      )
    
  } else if(year_i == 1999){

    df_raw <- read.csv(here::here("01_data", "raw", folder_name, paste0(year_i, ".csv")), fileEncoding = "CP932")

    df_output <- df_raw |> 
      dplyr::select(
        category_org = 4,
        category_industry = 2,
        city_id = 5,
        city_name = 6,
        year = 8,
        unit,
        value
      ) |>
      dplyr::mutate(
        dplyr::across(dplyr::everything(), ~stringr::str_replace_all(., "　", ""))
      ) |>
      dplyr::filter(
        category_org == "事業所数総数",
        category_industry == "Ａ〜Ｌ全産業",
      )

  } else if(year_i == 2001){

    df_raw <- read.csv(here::here("01_data", "raw", folder_name, paste0(year_i, ".csv")), fileEncoding = "CP932")

    df_output <- df_raw |> 
      dplyr::select(
        category_org = 4,
        category_industry = 2,
        category_emp = 6,
        city_id = 7,
        city_name = 8,
        year = 10,
        unit,
        value
      ) |>
      dplyr::mutate(
        dplyr::across(dplyr::everything(), ~stringr::str_replace_all(., "　", ""))
      ) |>
      dplyr::filter(
        category_org == "民営",
        category_industry == "A〜M 全産業",
        category_emp == "事業所数"
      )

  } else if(year_i == 2004){

    df_raw <- read.csv(here::here("01_data", "raw", folder_name, paste0(year_i, ".csv")), fileEncoding = "CP932")

    df_output <- df_raw |> 
      dplyr::select(
        category_org = 2,
        city_id = 3,
        city_name = 4,
        year = 6,
        unit,
        value
      ) |>
      dplyr::mutate(
        dplyr::across(dplyr::everything(), ~stringr::str_replace_all(., "　", ""))
      ) |>
      dplyr::filter(
        category_org == "事業所数平成１６年"
      )
  } else if(year_i == 2006){

    df_raw <- read.csv(here::here("01_data", "raw", folder_name, paste0(year_i, ".csv")), fileEncoding = "CP932")

    df_output <- df_raw |> 
      dplyr::select(
        category_org = 4,
        category_emp = 8,
        category_industry = 6,
        category_office = 2,
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
        category_org == "民営",
        category_emp == "総数",
        category_industry == "A〜R全産業",
        category_office == "事業所数"
      )

  } else if(year_i == 2009){
    df_raw <- read.csv(here::here("01_data", "raw", folder_name, paste0(year_i, ".csv")), fileEncoding = "CP932")

    df_output <- df_raw |> 
      dplyr::select(
        category_office = 2,
        category_org = 4,
        city_id = 5,
        city_name = 6,
        year = 8,
        unit,
        value
      ) |>
      dplyr::mutate(
        dplyr::across(dplyr::everything(), ~stringr::str_replace_all(., "　", ""))
      ) |>
      dplyr::filter(
        category_office == "事業所数（事業内容等不詳を含む）",
        category_org == "うち民営"
      )

  } else if(year_i == 2014){

    df_raw <- read.csv(here::here("01_data", "raw", folder_name, paste0(year_i, ".csv")), fileEncoding = "CP932")

    df_output <- df_raw |> 
      dplyr::select(
        category_org = 6,
        category_office = 2,
        city_id = 3,
        city_name = 4,
        year = 8,
        unit,
        value
      ) |>
      dplyr::mutate(
        dplyr::across(dplyr::everything(), ~stringr::str_replace_all(., "　", ""))
      ) |>
      dplyr::filter(
        category_org == "民営",
        category_office == "事業所数（事業内容等不詳を含む）"
      )

  } else if(year_i == 2019){

    df_raw <- read.csv(here::here("01_data", "raw", folder_name, paste0(year_i, ".csv")), fileEncoding = "CP932")

    df_output <- df_raw |> 
      dplyr::select(
        category_office = 2,
        category_continue = 4,
        city_id = 5,
        city_name = 6,
        year = 8,
        unit,
        value
      ) |>
      dplyr::mutate(
        dplyr::across(dplyr::everything(), ~stringr::str_replace_all(., "　", ""))
      ) |>
      dplyr::filter(
        category_continue == "総数(存続・新規把握)"
      )

  } else if(year_i == 2012){

    df_raw <- read.csv(here::here("01_data", "raw", folder_name, paste0(year_i, ".csv")), fileEncoding = "CP932")

    df_output <- df_raw |> 
      dplyr::select(
        category_office = 2,
        city_id = 3,
        city_name = 4,
        year = 6,
        unit,
        value
      ) |>
      dplyr::mutate(
        dplyr::across(dplyr::everything(), ~stringr::str_replace_all(., "　", ""))
      ) |>
      dplyr::filter(
        category_office == "事業所数（事業内容等不詳を含む）"
      )

  } else if(year_i == 2016){

    df_raw <- read.csv(here::here("01_data", "raw", folder_name, paste0(year_i, ".csv")), fileEncoding = "CP932")

    df_output <- df_raw |> 
      dplyr::select(
        category_office = 2,
        city_id = 3,
        city_name = 4,
        year = 6,
        unit,
        value
      ) |>
      dplyr::mutate(
        dplyr::across(dplyr::everything(), ~stringr::str_replace_all(., "　", ""))
      ) |>
      dplyr::filter(
        category_office == "事業所数（事業内容等不詳を含む）"
      )

  } 

  df_output <- df_output |>
    dplyr::select(
      city_id, 
      city_name,
      year,
      company_num = value
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


