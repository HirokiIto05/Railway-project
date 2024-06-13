# packages
library(dplyr)
library(tidyr)
library(stringr)
library(estatapi)
library(tidyverse)

# local constants
appid <- "12604ebf628834b46b3867721893644141fe9a34"

# @id 1991: 0000040728 産業大分類（１３）
# @id 1996: 0000040993 産業大分類（１３）
# @id 2001: 0000041455 産業大分類（１ー５）
# @id 2009: 0003032597
# @id 2014: 0003111168

df_stats_id <- tribble(
  ~year, ~stats_id,
  # 1991, "0000040728",
  # 1996, "0000040993",
  1996, "0000040951", 
  1999, "0000041206", 
  2001, "0000041426",
  2006, "0003000996", 
  2009, "0003032597",
  2014, "0003111168"
)

list_stats_id <- df_stats_id |> 
  distinct(stats_id) |> 
  pull()


# check stats id
census_old <- estatapi::estat_getStatsList(appId = appid, searchWord = "事業所・企業統計調査")
census_new <- estatapi::estat_getStatsList(appId = appid, searchWord = "経済センサス")

# census_old |>   
census_new |>
    dplyr::filter(stringr::str_detect(TITLE, "市区町村")) |> 
    dplyr::filter(stringr::str_detect(TITLE, "経営組織")) |>  
    dplyr::filter(stringr::str_detect(TITLE, "事業所")) |> 
    select(SURVEY_DATE, OVERALL_TOTAL_NUMBER, TITLE, everything()) |> 
    filter(str_detect(SURVEY_DATE, "2009")) |>
    dplyr::mutate(
      OVERALL_TOTAL_NUMBER = as.numeric(OVERALL_TOTAL_NUMBER)
    ) |> 
    # dplyr::filter(str_detect(TITLE, "男女別従業者数及び"),
    View()


########################################################
############# clean data
########################################################


id <- "0003000996"

generate_census_data <- function(year_n, df_stats_id){

  id <- df_stats_id |> 
    filter(year == year_n) |> 
    pull(stats_id)

  df <- estatapi::estat_getStatsData(appId = appid, statsDataId = id)

  df |>colnames()
  df |>View()
  
  df |> distinct(!!rlang::sym("事業所数、従業者数")) |>View()

  # df <- df |> 
  if(year == 1996){
    # df |> 
    df <- df |>
      select(
        # industry = "産業大分類１３040301", 
        category = "経営事業男女従040279",
        city_id = "area_code",
        city_name = "全県市区町村040049",
        year = "時間軸(年次)",
        value
      ) |> 
      mutate(
        year = str_replace_all(year, "年", ""),
        city_id = as.numeric(city_id),
        city_id = as.character(city_id)) |>
      filter(category %in% c("総数　事業者数", "総数　従業者数　総数")) |>
      mutate(
        category = if_else(category == "総数　事業者数", "establishments", category),
        category = if_else(category == "総数　従業者数　総数", "employees", category)
      )

  } else if(year == 1999){
    df <- df |> 
      select(
        # industry = "産業大分類１２040384", 
        category = "事業所従業者数040402",
        city_id = "area_code",
        city_name = "市区町村040074",
        year = "時間軸(年次)",
        value
      ) |> 
      mutate(
        year = str_replace_all(year, "年", ""),
        city_id = as.numeric(city_id),
        city_id = as.character(city_id)) |> 
      filter(category %in% c("事業所数　総数　平成１１年", "従業者数　総数　平成１１年")) |>
      mutate(
        category = if_else(category == "事業所数　総数　平成１１年", "establishments", category),
        category = if_else(category == "従業者数　総数　平成１１年", "employees", category)
      )

  } else if(year == 2001){
    df <- df |>
    filter("経営組織４－２040501" == "総数") |>
      select(
        # industry = "産業大分１－５040524", 
        category = "事業所従業者数040496",
        city_id = "area_code",
        city_name = "全県郡市区町村040091",
        year = "時間軸(年次)",
        value
      ) |> 
      mutate(
        year = str_replace_all(year, "年", ""),
        city_id = as.numeric(city_id),
        city_id = as.character(city_id)) |> 
      filter(category %in% c("事業所数", "従業者総数")) |> 
      mutate(
        category = if_else(category == "事業所数", "establishments", category),
        category = if_else(category == "従業者総数", "employees", category)
      ) 

  } else if(year == 2006){
    df <- df |>
      filter(
        !!rlang::sym("経営組織（総数）") == "総数"
      ) |>
      select(
        category = "事業所数、従業者数",
        city_id = "area_code",
        city_name = "H18地域",
        year = "時間軸(年次)",
        value
      ) |> 
      mutate(
        year = str_replace_all(year, "年", ""),
        city_id = as.numeric(city_id),
        city_id = as.character(city_id)
      )|> 
      filter(category %in% c("事業所数", "従業者数　総数")) 

  } else if(year == 2009){
      df <- df |>
      filter(
        !!rlang::sym("経営組織（総数）") == "総数"
      ) |>
      select(
        category = "事業所数、従業者数",
        city_id = "area_code",
        city_name = "H18地域",
        year = "時間軸(年次)",
        value
      ) |> 
      mutate(
        year = str_replace_all(year, "年", ""),
        city_id = as.numeric(city_id),
        city_id = as.character(city_id)
      )|> 
      filter(category %in% c("事業所数", "従業者数　総数")) 

  }




}

  
df_06 |> distinct(!!rlang::sym("産業分類（大・中・小）"))

df_01 <- estatapi::estat_getStatsData(appId = appid, statsDataId = "0000041455")
df_06 <- estatapi::estat_getStatsData(appId = appid, statsDataId = "0003000452")
df_09 <- estatapi::estat_getStatsData(appId = appid, statsDataId = "0003032597")
# df_14 <- estatapi::estat_getStatsData(appId = appid, statsDataId = "0003111168")
