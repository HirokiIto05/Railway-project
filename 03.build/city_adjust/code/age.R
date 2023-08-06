main <- function() {
  
  age_data <- read_age()
  adjust_df <- adjust_data() 
  
  current_cityid_list <- city_id_list20(adjust_df)
  
  fin_age <- purrr::map(current_cityid_list, adjust_city_id, age_data, adjust_df) |> 
    bind_rows() 
  
  save_df_csv(fin_age, "city_adjust", "age_data")
  
}


read_age <- function(){
  
  new_data <- read.csv(here::here('03.build','aggregate','data','age.csv'),
                       fileEncoding = "CP932", colClasses = "character") |>
    dplyr::mutate(across(.cols = -c(city_id, prefecture, gender, city_name), ~ as.numeric(.x))) |> 
    dplyr::rename(prefecture_name = prefecture) |> 
    dplyr::filter(gender == "計")
  
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


adjust_city_id <- function(id_n, df_age, adjust_df){
  print(id_n)
  
  list_id <- adjust_df |> 
    dplyr::filter(id_muni2020 == id_n) |>  
    dplyr::select(seq(2,9)) |> 
    unlist() |> 
    unique() |> 
    na.omit()
  
  df_id_n <- df_age |> 
    dplyr::filter(city_id %in% list_id) 
  
  city_data <- df_age |>
    dplyr::filter(year == 2019,
                  city_id == id_n) |> 
    select(city_id, city_name, prefecture_name)
  
  city_id_n <- city_data |> 
    dplyr::pull(city_id)
  city_name_n <- city_data |> 
    dplyr::pull(city_name)
  prefecture_name_n <- city_data |> 
    dplyr::pull(prefecture_name)
  
  
  output_data <- setDT(df_id_n)[, .(
    total = sum(total),
    r0_4 = sum(r0_4),
    r5_9 = sum(r5_9),
    r10_14 = sum(r10_14),
    r15_19 = sum(r15_19),
    r20_24 = sum(r20_24),
    r25_29 = sum(r25_29),
    r30_34 = sum(r30_34),
    r35_39 = sum(r35_39),
    r40_44 = sum(r40_44),
    r45_49 = sum(r45_49),
    r50_54 = sum(r50_54),
    r55_59 = sum(r55_59),
    r60_64 = sum(r60_64),
    r65_69 = sum(r65_69),
    r70_74 = sum(r70_74),
    r75_79 = sum(r75_79),
    r80_over = sum(r80_over)
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

modify_age_data <- function(age_data){
  
  city_name_list <- age_data |> 
    dplyr::select(city_name) |> 
    unlist() |> 
    as.character() 
  
  tt <- "檜山郡江差町*"
  
  nchar(tt)
  
  stringr::str_replace(tt, pattern = "", replacement = "")

  new_city_name_list <- stringr::str_replace_all(city_name_list, 
                                                 pattern = "[*]",
                                                 replacement = "")
  
  new_city_name_df <- tibble(old_name = city_name_list,
                             new_name = new_city_name_list)
  
  return(new_city_name_list)
  
}


save_table <- function(data){
  
  write.csv(data, here::here('03.build', 'city_adjust', 'data', 'age_data.csv'),
            fileEncoding = "CP932", row.names = FALSE)
  
}




