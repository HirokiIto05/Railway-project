main(){
  
  age_data <- load_age()
  adjust_df <- adjust_data()
  
  current_cityid_list <- city_id_list20(adjust_df)
  
  fin_age <- purrr::map(current_cityid_list, adjust_city_id_age, age_data, adjust_df) %>% 
    bind_rows() 
  
}


load_age <- function(){
  
  new_data <- read.csv(here::here('03.build','aggregate','data','age.csv'),
                       fileEncoding = "CP932", colClasses = "character") %>%
    dplyr::select(-X) %>% 
    dplyr::mutate(across(.cols = -c(city_id, prefecture, gender, city_name), ~ as.numeric(.x)))
  
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

# id_n <- "1361"


adjust_city_id_age <- function(id_n, age_data, adjust_df){
  
  
  new_data <- adjust_df %>% 
    dplyr::filter(id_muni2020 == id_n)
  
  new_data <- lapply(new_data, long) %>% 
    bind_rows() %>% 
    t() %>% 
    as.data.frame()
  
  colnames(new_data) <- "city_id"
  
  each_id <- unique(new_data$city_id) %>%
    as.numeric() %>% 
    na.omit()
  
  pop_id_n <- age_data %>% 
    dplyr::filter(city_id %in% each_id) %>% 
    group_by(year)
  
  
  
  city_data <- age_data %>%
    dplyr::filter(year == 2019,
                  city_id == id_n) %>% 
    select(city_id, city_name, prefecture)
  
  city_id_n = city_data[,1]
  city_name_n = city_data[,2]
  region_name_n = city_data[,3]
  
  # str(age_data)
  
  
  output_data <- summarise(pop_id_n,
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

data <- fin_age

save_table <- function(data){
  
  write.csv(data, here::here('03.build', 'city_adjust', 'data', 'age_data.csv'))
  
}




