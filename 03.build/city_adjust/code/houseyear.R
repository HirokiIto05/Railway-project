main <- function(){
  
  houseyear_household_data <- read_df_csv("covariates", "houseyear") 
  
  colnames(houseyear_household_data)
  
  #国勢調査以降に合併した市町村については、データ処理の観点から除外している。
  adjust_df <- adjust_data() |> 
    dplyr::filter(id_muni2020 != 4216, #2016
                  id_muni2020 != 40231, #2018
                  id_muni2020 != 3216, #2014
                  id_muni2020 != 11246, #2012
                  id_muni2020 != 12239, #2013
                  id_muni2020 != 17212, #2011
                  id_muni2020 != 23238, #2012
                  id_muni2020 != 43100) #2012
  
  current_cityid_list <- city_id_list20(adjust_df)
  
  fin_data <- purrr::map(current_cityid_list, adjust_city_id, houseyear_household_data, adjust_df) |> 
    bind_rows()
  
  save_df_csv(fin_data, "city_adjust", "old_house")
  
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

adjust_city_id <- function(id_n, houseyear_household_data, adjust_df){
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
  
  pop_id_n <- houseyear_household_data |> 
    dplyr::filter(city_id %in% each_id) |> 
    group_by(year) 
  
  city_data <- houseyear_household_data |>
    dplyr::filter(year == 2010,
                  city_id == id_n) |> 
    select(city_id, city_name)
  
  city_id_n = city_data[,1]
  city_name_n = city_data[,2]
  
  pop_id_n <- pop_id_n |> 
    dplyr::mutate(old_house_household = gsub(old_house_household,
                                             pattern = ",",
                                             replacement = ""),
                  old_house_pop = gsub(old_house_pop,
                                             pattern = ",",
                                             replacement = "")) |> 
    dplyr::mutate(old_house_household = as.numeric(old_house_household),
                  old_house_pop = as.numeric(old_house_pop))
  
  
  output_data <- summarise(pop_id_n,
                           old_house_household = sum(old_house_household),
                           old_house_pop = sum(old_house_pop)
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


save_table <- function(data, file_name){
  
  write.csv(data, here::here('03.build', 'city_adjust', 'data', file_name),
            fileEncoding = "CP932", row.names = FALSE)
  
}


