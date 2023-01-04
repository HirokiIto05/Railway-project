main(){
  
  reduce_id_list <- c(4216,40231)
  
  
  # houseyear_household_data <- load_csv("houseyear", "all.csv") %>%
  houseyear_household_data <- all_houseyear_data %>%
    dplyr::filter(city_id != 4216,
                  city_id != 40231,
                  city_id != 3216,
                  city_id != 11246,
                  city_id != 12239,
                  city_id != 17212,
                  city_id != 23238,
                  city_id != 43100)
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
  
  fin_data <- purrr::map(current_cityid_list, adjust_city_id, houseyear_household_data, adjust_df) %>% 
    bind_rows()
  
  save_table(fin_data, 'all_houseyear_data.csv')
  
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

id_n = 1100

adjust_city_id <- function(id_n, houseyear_household_data, adjust_df){
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
  
  pop_id_n <- houseyear_household_data %>% 
    dplyr::filter(city_id %in% each_id) %>% 
    group_by(year) 
  
  city_data <- houseyear_household_data %>%
    dplyr::filter(year == 2010,
                  city_id == id_n) %>% 
    select(city_id, city_name)
  
  city_id_n = city_data[,1]
  city_name_n = city_data[,2]
  
  pop_id_n <- pop_id_n %>% 
    dplyr::mutate(houseyear_household = gsub(houseyear_household,
                                             pattern = ",",
                                             replacement = ""),
                  houseyear_pop = gsub(houseyear_pop,
                                             pattern = ",",
                                             replacement = "")) %>% 
    dplyr::mutate(houseyear_household = as.numeric(houseyear_household),
                  houseyear_pop = as.numeric(houseyear_pop))
  
  
  output_data <- summarise(pop_id_n,
                           houseyear_household = sum(houseyear_household),
                           houseyear_pop = sum(houseyear_pop)
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


