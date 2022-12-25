ehime_df <- readxl::read_xls("/Users/ito_hiroki/01.Research/Railway-project/02.raw/covariates/housetype/2000/ehime.xls")


main(){
  
  file_name_list <- list.files(here::here('02.raw','covariates','housetype','2000'))
  
  list_00 <- purrr::map(file_name_list, load_each_region, "2000") %>% 
    dplyr::bind_rows()
  
}

region_name <- file_name_list[2]

aomori_city <- region_data %>% 
  dplyr::distinct(...9)

aomori_2 <- region_data %>% 
  dplyr::distinct(...10)


region_data <- readxl::read_xls("/Users/ito_hiroki/Downloads/a022-2 (4).xls")

load_each_region <- function(region_name, year_n){
  
    file_name <- paste0(region_name)
    region_data <- readxl::read_xls(here::here('02.raw', 'covariates', 'housetype', year_n, file_name))
  
    own_data <- create_own_df(region_data)
    rent_data <- create_rent_df(region_data)
    
    output_region <- dplyr::left_join(own_data, rent_data)
    
    return(output_region)
  
}




data <- region_data

create_own_df <- function(data){
  
  own_df <- data %>% 
    dplyr::filter(!is.na(...8) |...14 %in% c("(d)")) %>% 
    dplyr::select(...8, ...9, "1", "2") 
  
  colnames(own_df) <- c("city_id","city_name","household","num")  
  
  output_own <- own_df %>% 
    dplyr::mutate(own_household = lead(household),
                  own_num = lead(num)) %>% 
    dplyr::filter(!is.na(city_name)) %>% 
    dplyr::select(-household, -num)
  
  return(output_own)
  
}


create_rent_df <- function(data){
  
  rent_df <- data %>% 
    dplyr::filter(!is.na(...8) |...14 %in% c("(f)")) %>% 
    dplyr::select(...8, ...9, "1", "2") 
  
  colnames(rent_df) <- c("city_id","city_name","household","num")  
  
  output_rent <- rent_df %>% 
    dplyr::mutate(rent_household = lead(household),
                  rent_num = lead(num)) %>% 
    dplyr::filter(!is.na(city_name)) %>% 
    dplyr::select(-household, -num)
  
  return(output_rent)
  
}





