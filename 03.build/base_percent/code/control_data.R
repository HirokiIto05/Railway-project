main <- function(){
  
  age_data <- load_data()
  main_data <- load_main()
  
  control_age <- integrate_id(main_data, age_data) %>% 
    dplyr::filter(city_id != 12441,
                  city_id != 31325)
  
  percent_control <- add_percent(control_age)
  
  save_table(percent_control)
  
}


load_main <- function(){
  
  new_data <- readxl::read_xlsx(here::here('02.raw','continue.xlsx'))
  
  return(new_data)
  
}


load_data <- function(){
  
  output_data <- read.csv(here::here('03.build','aggregate','data','age.csv'),
                          fileEncoding = "CP932") 
  
  return(output_data)
  
}


integrate_id <- function(main_data, age_data){
  
  adjust_list <- distinct(main_data, city_id) %>% 
    na.omit()
  
  current_id <- dplyr::distinct(main_data, adjust_id) %>% 
    na.omit() %>% 
    unlist() %>% 
    as.character()
  
  old_new_number <- main_data %>% 
    dplyr::select(city_id, adjust_id) %>% 
    na.omit() 
  
  current_city_name <- main_data %>% 
    dplyr::filter(city_id %in% current_id) %>% 
    dplyr::select(city_id, city_name)
  
  adjust_list$city_id <- as.numeric(adjust_list$city_id)
  age_data$city_id <- as.numeric(age_data$city_id)
  
  pop_old_data <- dplyr::left_join(adjust_list, age_data, by = "city_id")
  
  pop_new_data <- dplyr::left_join(pop_old_data, old_new_number, by = "city_id") %>% 
    dplyr::relocate(adjust_id, .after = city_id) %>% 
    dplyr::group_by(adjust_id, year)
  
  
  new_data <- dplyr::summarise(pop_new_data, 
                               pop = sum(total),
                               working = sum(working))
  
  # new_data <- new_data %>% 
  #   mutate(percentage = (social_increase/lag(pop))*100)
  
  output_data <- current_city_name %>% 
    left_join(new_data, by = c("city_id" = "adjust_id")) 
  
  output_data <- output_data %>% 
    filter(city_id != 45442) %>% 
    dplyr::mutate(treatment_year = 0, .after = city_name)
  
  return(output_data)
}

add_percent <- function(data){
  
  city_id_lists <- unique(data$city_id)
  
  output_data <- lapply(city_id_lists, calculate_percent, data) %>% 
    dplyr::bind_rows()
  
  return(output_data)
}



calculate_percent <- function(id,data){
  
  id_n <- data %>% 
    dplyr::filter(city_id == id,
                  year == 1995) %>% 
    dplyr::select(working) %>% 
    as.numeric()
  
  new_data <- data %>% 
    dplyr::filter(city_id == id) %>% 
    dplyr::mutate(percent = working/id_n)
  
  return(new_data)
}

data <- control_age

for(id in city_id_lists){
  print(id)
  id_n <- data %>% 
    dplyr::filter(city_id == id,
                  year == 1995) %>% 
    dplyr::select(working) %>% 
    as.numeric()
}


save_table <- function(data){
  
  write.csv(data, file = here::here('03.build','base_percent','data','base_control.csv'),
            fileEncoding = "CP932", row.names = FALSE)
  
}





