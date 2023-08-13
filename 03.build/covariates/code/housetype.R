main <- function(){
  
  year_list <- seq(1995, 2010, by= 5)
  
  all_housetype_data <- purrr::map(year_list, read_housetype_csv) |> 
    dplyr::bind_rows() %>%
    dplyr::mutate(
      dplyr::across(c(household_own, household_rent, pop_own, pop_rent), 
                    ~ifelse(is.na(.), 0, .)),
    )
  
  
  df_large <- read_large_cities()
    
  df_housetype_data <- all_housetype_data |> 
    dplyr::bind_rows(df_large) |> 
    dplyr::distinct()

  save_df_csv(df_housetype_data, "covariates", "housetype")
  
  # 三宅村 2000年存在せず
  
}

year_n = 2000


read_housetype_csv <- function(year_n){
  print(year_n)
  
  file_name <- paste0(year_n, ".csv")
  
  if(year_n == 1995) {
    housetype_data <- read.csv(here::here('02.raw', 'covariates', 'housetype', file_name), 
                               fileEncoding = "cp932") 
    
    df_based <- housetype_data |> 
      dplyr::select(
        area_range = "全域.集中の別030002",
        housetype =  "住居種類所有９030894",
        year = "時間軸.年次.",
        city_id = "支庁.市区町村030114.コード",
        city_name = "支庁.市区町村030114",
        household = "一般世帯数.世帯.",
        pop = "世帯人員.人."
      )
    
    df_based <- df_based |> 
      dplyr::mutate(
        dplyr::across(c(household, pop), 
                      ~stringr::str_replace_all(., ",", "")),
        year = as.numeric(stringr::str_replace_all(year, "年", ""))
      ) |> 
      dplyr::mutate(
        across(c(household, pop), 
               as.numeric)
      )
    
    df_output <- df_based |> 
      dplyr::mutate(
        housetype_dummy = dplyr::case_when(
          housetype == "持ち家" ~ "own",
          housetype %in% c("公営・公団・公社の借家",
                           "民営の借家") ~ "rent"
        ) 
        )|> 
      dplyr::filter(
        !is.na(housetype_dummy)
      ) |> 
      dplyr::summarise(
        household = sum(household, na.rm = TRUE),
        pop = sum(pop, na.rm = TRUE),
        .by =  c(city_id,
                 city_name,
                 year,
                 housetype_dummy)
      ) |> 
      tidyr::pivot_wider(
        names_from = "housetype_dummy",
        values_from = c("household", "pop")
      ) 
    
  } else if(year_n == 2000) {
    
    housetype_data <- read.csv(here::here('02.raw', 'covariates', 'housetype', file_name), 
                               fileEncoding = "cp932") 
    
    df_based <- housetype_data |> 
      dplyr::select(
        area_range = "全域.集中の別030184",
        housetype = "住居種類所有９030894",
        year =  "時間軸.年次.",
        city_id = "５０除市区町村030191.コード",
        city_name = "５０除市区町村030191",
        household =  "一般世帯数.世帯.",
        pop =  "世帯人員.人."
      ) |> 
      dplyr::mutate(
        dplyr::across(c(household, pop), 
                      ~stringr::str_replace_all(., ",", "")),
        year = as.numeric(stringr::str_replace_all(year, "年", ""))
      ) |> 
      dplyr::mutate(
        across(c(household, pop), 
               as.numeric)
      )
    
    df_output <- df_based |> 
      dplyr::mutate(
        housetype_dummy = dplyr::case_when(
          housetype == "持ち家" ~ "own",
          housetype %in% c("公営・公団・公社の借家",
                           "民営の借家") ~ "rent"
        ) 
      ) |> 
      dplyr::filter(
        !is.na(housetype_dummy)
      ) |> 
      dplyr::summarise(
        household = sum(household, na.rm = TRUE),
        pop = sum(pop, na.rm = TRUE),
        .by =  c(city_id,
                 city_name,
                 year,
                 housetype_dummy)
      ) |> 
      tidyr::pivot_wider(
        names_from = "housetype_dummy",
        values_from = c("household", "pop")
      ) 
    
  } else if(year_n == 2005){
    
    # housetype_data <- readr::read_csv(here::here('02.raw', 'covariates', 'housetype', file_name)) 
    
    housetype_data <- read.csv(here::here('02.raw', 'covariates', 'housetype', '2005.csv'), 
                               fileEncoding = "cp932") 
    df_based <- housetype_data |> 
      dplyr::select(
        area_range = "全域.集中の別030184",
        housetype = "住居の種類.住宅の所有関係031504",
        year = "時間軸.年次.",
        city_id = "地域030282.コード",
        city_name = "地域030282",
        household = "一般世帯数.世帯.",
        pop = "一般世帯人員.人."
      ) |> 
      dplyr::mutate(
        dplyr::across(c(household, pop), 
                      ~stringr::str_replace_all(., ",", "")),
        year = as.numeric(stringr::str_replace_all(year, "年", ""))
      ) |> 
      dplyr::mutate(
        across(c(household, pop), 
               as.numeric)
      )
    
    df_output <- df_based |>
      dplyr::mutate(
        housetype_dummy = dplyr::case_when(
        housetype == "持ち家" ~ "own",
        housetype %in% c("公営の借家"     ,
                         "都市機構・公社の借家",
                         "民営の借家") ~ "rent"
      ) 
      ) |> 
      dplyr::filter(
        !is.na(housetype_dummy)
      ) |> 
      dplyr::summarise(
        household = sum(household, na.rm = TRUE),
        pop = sum(pop, na.rm = TRUE),
        .by =  c(city_id,
                 city_name,
                 year,
                 housetype_dummy)
      ) |> 
      tidyr::pivot_wider(
        names_from = "housetype_dummy",
        values_from = c("household", "pop")
      ) 
    

  } else if(year_n == 2010){
    
    housetype_data <- readxl::read_excel("02.raw/covariates/housetype/2010.xlsx")
    
    df_based <- housetype_data |> 
      dplyr::select(
        area_range = "全域・人口集中地区2010",
        year = "時間軸（年次）",
        value_definition = "表章項目",
        city_id = "地域（2010） コード",
        city_name = "地域（2010）",
        own = "主世帯.持ち家",
        rent_public = "主世帯.公営.都市再生機構.公社の借家",
        rent_private = "主世帯.民営の借家"
      ) |> 
      dplyr::mutate(
        dplyr::across(c(own, rent_public, rent_private), 
                      ~stringr::str_replace_all(., ",", "")),
        year = as.numeric(stringr::str_replace_all(year, "年", ""))
      ) |> 
      dplyr::mutate(
        across(c(own, rent_public, rent_private),
               as.numeric),
        rent = rent_public + rent_private
      ) |> 
      dplyr::select(
        -c(rent_public, rent_private)
      )
    
    df_output <- df_based |>
      tidyr::pivot_wider(
        names_from = "value_definition",
        values_from = c("own", "rent")
      ) |> 
      dplyr::select(
        city_id,
        city_name,
        year,
        household_own = "own_一般世帯数【世帯】" ,
        pop_own = "own_一般世帯人員【人】" ,
        household_rent = "rent_一般世帯数【世帯】",
        pop_rent = "rent_一般世帯人員【人】"
      )
    
  } 
  # else if (year_n == 2015) {
  #   
  #   housetype_data <- readr::read_csv("02.raw/covariates/housetype/2015.csv")
  #   
  #   df_based <- housetype_data |> 
  #     dplyr::select(
  #       area_range = "全域・人口集中地区（2015）",
  #       year = "時間軸（年次）",
  #       value_definition = "表章項目",
  #       city_id = "地域（2015） コード",
  #       city_name = "地域（2015）",
  #       own = "主世帯 持ち家",
  #       rent_public = "主世帯 公営・都市再生機構・公社の借家",
  #       rent_private = "主世帯 民営の借家"
  #     )  
  #     
  #     dplyr::mutate(
  #       rent_public = as.numeric(rent_public), 
  #       rent_private = as.numeric(rent_private),
  #       rent = rent_public + rent_private
  #     ) |> 
  #     dplyr::select(
  #       -c(rent_public, rent_private)
  #     )
  #   
  #   df_output <- df_based |>
  #     tidyr::pivot_wider(
  #       names_from = "value_definition",
  #       values_from = c("own", "rent")
  #     ) |> 
  #     dplyr::select(
  #       year:city_name,
  #       own_household = "own_一般世帯数【世帯】" ,
  #       own_pop = "own_一般世帯人員【人】" ,
  #       rent_household = "rent_一般世帯数【世帯】",
  #       rent_pop = "rent_一般世帯人員【人】"
  #     )
  #   
  # }
  
  df_output <- df_output |> 
    dplyr::mutate(city_id = as.character(city_id)) 
  
  return(df_output)
  
}


read_large_cities <- function() {
  
  data <- openxlsx::read.xlsx("02.raw/covariates/housetype/2000_large.xlsx")
  
  df_based <- data |> 
    dplyr::select(
       city_id = "area_code",
       city_name = "県・市郡・５０030189",
       year = "時間軸（年次）" ,
       area_range = "全域・集中の別030184",
       housetype = "住居種類等１０031152",
       value_definition = "一般世帯数等031153",
       # "unit" = "unit",
       value = "value"
    ) |> 
    dplyr::filter(
      area_range == "全域"
    ) |> 
    dplyr::mutate(
      year = as.numeric(
        stringr::str_replace_all(year, "年", "")
      ),
      housetype =  stringr::str_replace_all(housetype, "　", ""),
      housetype =  stringr::str_replace_all(housetype, " ", "")
    ) 
  
  df_output <- df_based |> 
    dplyr::mutate(
      housetype_dummy = dplyr::case_when(
        housetype == "持ち家" ~ "own",
        housetype %in% c("公営の借家",
                         "公団・公社の借家",
                         "民営の借家") ~ "rent"
      ) 
    ) |> 
    dplyr::summarise(
      value = sum(value, na.rm = TRUE),
      .by =  c(city_id,
               city_name,
               year,
               value_definition,
               housetype_dummy)
    ) |> 
    dplyr::filter(
      value_definition %in% c("一般世帯数", "世帯人員")
    ) |> 
    dplyr::filter(
      !is.na(housetype_dummy)
    ) |> 
    tidyr::pivot_wider(
      names_from = "value_definition",
      values_from = "value"
    ) |> 
    tidyr::pivot_wider(
      names_from = "housetype_dummy",
      values_from = c("一般世帯数", "世帯人員")
    ) |> 
    dplyr::select(
      city_id:year,
      household_own = "一般世帯数_own" ,
      household_rent = "一般世帯数_rent",
      pop_own = "世帯人員_own",
      pop_rent = "世帯人員_rent"
    )
  
  return(df_output)
  
}

