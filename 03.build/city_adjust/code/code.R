main(){
  
  pop_data <- load_pop()
  adjust_df <- adjust_data()
  
  current_cityid_list <- city_id_list20(adjust_df)
  
  fin_data <- purrr::map(current_cityid_list, adjust_city_id, pop_data, adjust_df) %>% 
    bind_rows()
  
  save_table(fin_data)

}


load_pop <- function(){
  
  new_data <- read.csv(here::here('03.build','aggregate','data','pop.csv'),
                       fileEncoding = "CP932", colClasses = "character") %>% 
    dplyr::mutate(across(.cols = -c(city_id, region_name, city_name), ~ as.numeric(.x)))
  
  new_data$total <- as.numeric(new_data$total)
  
  return(new_data)
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


adjust_city_id <- function(id_n, pop_data, adjust_df){
  
  new_data <- adjust_df %>% 
    dplyr::filter(id_muni2020 == id_n)
  
  new_data <- lapply(new_data, long) %>% 
    bind_rows() %>% 
    t() %>% 
    as.data.frame()
  
  colnames(new_data) <- "city_id"
  
  each_id <- unique(new_data$city_id) %>% 
    na.omit()
  
  pop_id_n <- pop_data %>% 
    dplyr::filter(city_id %in% each_id) %>% 
    group_by(year)
  
  city_data <- pop_data %>%
    dplyr::filter(year == 2019,
                  city_id == id_n) %>% 
    select(city_id, city_name, region_name)
  
  city_id_n = city_data[,1]
  city_name_n = city_data[,2]
  region_name_n = city_data[,3]
  
  
  output_data <- summarise(pop_id_n,
                   male = sum(male),
                   female = sum(female),             
                   total  = sum(total),         
                   household = sum(household),           
                   birth  = sum(birth),         
                   move_out = sum(move_out),        
                   mortality = sum(mortality),        
                   change    = sum(change),       
                   change_rate = sum(change_rate),   
                   natural  = sum(natural),  
                   natural_rate = sum(natural_rate),
                   social = sum(social),
                   social_rate = sum(social_rate),
  ) %>% 
    dplyr::mutate(city_id = city_id_n,
                  city_name = city_name_n,
                  region_name = region_name_n, .before = year)
  
  
  
  return(output_data)
  
}
  
  
long <- function(data){
  
  output_data <- data %>% 
    t()
  
  return(output_data)
}


save_table <- function(data){
  
  write.csv(data, here::here('03.build', 'city_adjust', 'data', 'pop_data.csv'),
            fileEncoding = "CP932", row.names = FALSE)
  
}



