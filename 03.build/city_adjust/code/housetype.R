main <- function(){
  
  all_housetype_data <- read_df_csv("covariates", "housetype")

  #国勢調査以降に合併した市町村については、データ処理の観点から除外している。
  adjust_df <- adjust_data() |> 
    dplyr::filter(id_muni2020 != 4216,
                  id_muni2020 != 40231) #2015年以降合併市町村
  
  current_cityid_list <- city_id_list20(adjust_df)
  
  fin_data <- purrr::map(current_cityid_list, adjust_city_id, all_housetype_data, adjust_df) |> 
    bind_rows()
  
  save_df_csv(fin_data, "city_adjust", "housetype")
  
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


adjust_city_id <- function(id_n, all_housetype_data, adjust_df){
  print(id_n)
  
  new_data <- adjust_df |> 
    dplyr::filter(id_muni2020 == id_n)
  
  new_data <- lapply(new_data, long) |> 
    bind_rows() |> 
    t() |> 
    as.data.frame()
  
  colnames(new_data) <- "city_id"
  
  new_data <- na.omit(new_data)
  
  each_id <- unique(new_data$city_id) 
  
  pop_id_n <- all_housetype_data |> 
    dplyr::filter(city_id %in% each_id) |> 
    group_by(year)
  
  city_data <- all_housetype_data |>
    dplyr::filter(year == 2015,
                  city_id == id_n) |> 
    select(city_id, city_name)
  
  city_id_n = city_data[,1]
  city_name_n = city_data[,2]
  
  output_data <- dplyr::summarise(pop_id_n,
                           own_household = sum(own_household),
                           own_pop = sum(own_pop),
                           rent_household = sum(rent_household),
                           rent_pop = sum(rent_pop),
  ) |> 
    dplyr::mutate(city_id = city_id_n,
                  city_name = city_name_n,
                  .before = year)
  
  
  
  return(output_data)
  
}


long <- function(data){
  
  output_data <- data |> 
    t()
  
  return(output_data)
}



