main <- function() {
  
  library(data.table)
  
  df_pop <- read_df_csv("aggregate", "pop")
  adjust_df <- adjust_data()
  
  current_cityid_list <- city_id_list20(adjust_df)
  
  df_adjusted <- purrr::map(current_cityid_list, adjust_city_id, df_pop, adjust_df) |> 
    bind_rows()
  
  df_add_rate <- df_adjusted |> 
    dplyr::group_by(city_id) |> 
    dplyr::mutate(lag_total = dplyr::lag(total), .after = total) |> 
    dplyr::mutate(change_rate = change/dplyr::lag(total)*100, .after = change) |> 
    dplyr::mutate(natural_rate = natural/dplyr::lag(total)*100, .after = natural) |> 
    dplyr::mutate(social_rate = (social/dplyr::lag(total))*100, .after = social) 
  

  save_df_csv(df_add_rate, "city_adjust", "pop")

}


load_pop <- function(){
  
  new_data <- read.csv(here::here('03.build','aggregate','data','pop.csv'),
                       fileEncoding = "CP932", colClasses = "character") |> 
    dplyr::mutate(across(.cols = -c(city_id, prefecture_name, city_name), ~ as.numeric(.x)))
  
  new_data$total <- as.numeric(new_data$total)
  
  return(new_data)
}


adjust_data <- function(){
  
  output_data <- readxl::read_xlsx("02.raw/municipality_converter/municipality_converter_jp.xlsx")
  
  return(output_data)
}


city_id_list20 <- function(data){

  output_data <- data |> 
    dplyr::select(id_muni2020) |> 
    distinct() |> 
    unlist() |> 
    as.character()
  
  return(output_data)
  
}

adjust_city_id <- function(id_n, df_pop, adjust_df){
  print(id_n)
  
  list_id <- adjust_df |> 
    dplyr::filter(id_muni2020 == id_n) |>  
    dplyr::select(seq(2,9)) |> 
    unlist() |> 
    unique() |> 
    na.omit()
  
  df_id_n <- df_pop |> 
    dplyr::filter(city_id %in% list_id) 
  
  city_data <- df_pop |>
    dplyr::filter(year == 2020,
                  city_id == id_n) |> 
    select(city_id, city_name, prefecture_name)
  
  city_id_n <- city_data |> 
    dplyr::pull(city_id)
  city_name_n <- city_data |> 
    dplyr::pull(city_name)
  prefecture_name_n <- city_data |> 
    dplyr::pull(prefecture_name)

  
  output_data <- setDT(df_id_n)[, .(
    male = sum(male),
    female = sum(female),
    total = sum(total),
    household = sum(household),
    moving_in = sum(moving_in),
    birth = sum(birth),
    other_in = sum(other_in),
    increase_total = sum(increase_total),
    moving_out = sum(moving_out),
    mortality = sum(mortality),
    other_out = sum(other_out),
    change = sum(change),
    natural = sum(natural),
    social = sum(social)
  ), by = year] |> 
    dplyr::mutate(
      city_id = city_id_n,
      city_name = city_name_n,
      prefecture_name = prefecture_name_n,
      .before = year
    )
  
  # output_data <- pop_id_n |>
  #   dplyr::group_by(year) |>
  #   dplyr::summarise(
  #     male = sum(male),
  #     female = sum(female),
  #     total  = sum(total),
  #     household = sum(household),
  #     moving_in = sum(moving_in),
  #     birth  = sum(birth),
  #     other_in = sum(other_in),
  #     increase_total = sum(increase_total),
  #     moving_out = sum(moving_out),
  #     mortality = sum(mortality),
  #     other_out = sum(other_out),
  #     change    = sum(change),
  #     natural  = sum(natural),
  #     social = sum(social)) |>
  #   dplyr::mutate(city_id = city_id_n,
  #                 city_name = city_name_n,
  #                 prefecture_name = prefecture_name_n, .before = year)
  
  
  
  return(output_data)
  
}
  



# save_table <- function(data){
#   
#   write.csv(data, here::here('03.build', 'city_adjust', 'data', 'df_pop.csv'),
#             fileEncoding = "CP932", row.names = FALSE)
#   
# }
# 
# 
# 
