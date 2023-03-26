main(){
  
  all_transport_data <- load_csv("transport", "all.csv")
  
  #国勢調査以降に合併した市町村については、データ処理の観点から除外している。
  adjust_df <- adjust_data() %>% 
    dplyr::filter(id_muni2020 != 4216,
                  id_muni2020 != 40231,
                  id_muni2020 != 3216,
                  id_muni2020 != 11246,
                  id_muni2020 != 12239,
                  id_muni2020 != 17212,
                  id_muni2020 != 23238,
                  id_muni2020 != 43100)
  
  current_cityid_list <- city_id_list20(adjust_df)
  
  fin_data <- purrr::map(current_cityid_list, adjust_city_id, all_transport_data, adjust_df) %>% 
    bind_rows()
  
  save_table(fin_data, 'all_transport_data.csv')
  
}


adjust_data <- function(){
  
  output_data <- readxl::read_xlsx(here::here('02.raw','municipality_converter.xlsx'))
  
  return(output_data)
}


city_id_list20 <- function(data){
  
  output_data <- data %>% 
    dplyr::select(id_muni2020) %>% 
    distinct() %>% 
    unlist() %>% 
    as.character()
  
  return(output_data)
  
}


adjust_city_id <- function(id_n, all_transport_data, adjust_df){
  print(id_n)
  
  new_data <- adjust_df %>% 
    dplyr::filter(id_muni2020 == id_n)
  
  new_data <- lapply(new_data, long) %>% 
    bind_rows() %>% 
    t() %>% 
    as.data.frame()
  
  colnames(new_data) <- "city_id"
  
  new_data <- na.omit(new_data)
  
  each_id <- unique(new_data$city_id) 
  
  pop_id_n <- all_transport_data %>% 
    dplyr::filter(city_id %in% each_id) %>% 
    group_by(year)
  
  city_data <- all_transport_data %>%
    dplyr::filter(year == 2010,
                  city_id == id_n) %>% 
    select(city_id, city_name)
  
  city_id_n = city_data[,1]
  city_name_n = city_data[,2]
  
  output_data <- summarise(pop_id_n,
                           train_only_pop = sum(train_only_pop),
                           train_bus_pop = sum(train_bus_pop),
                           train_commutebus_pop = sum(train_commutebus_pop),
                           train_car_pop = sum(train_car_pop),
                           train_motorcycle_pop = sum(train_motorcycle_pop),
                           train_bicycle_pop = sum(train_bicycle_pop),
  ) %>% 
    dplyr::mutate(city_id = city_id_n,
                  city_name = city_name_n,
                  .before = year)
  
  
  
  return(output_data)
  
}


long <- function(data){
  
  output_data <- data %>% 
    t()
  
  return(output_data)
}


save_table <- function(data, file_name){
  
  write.csv(data, here::here('03.build', 'city_adjust', 'data', file_name),
            fileEncoding = "CP932", row.names = FALSE)
  
}


