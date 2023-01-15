disaster_df <- readxl::read_xlsx("/Users/ito_hiroki/01.Research/Railway-project/02.raw/disaster.xlsx") %>% 
  dplyr::select(c(1,5,45,50))

half_df <- disaster_df %>% 
  dplyr::select(2,4)
colnames(half_df) <- c("city_name", "half")

half_df <- half_df %>% 
  dplyr::mutate(half = as.numeric(half)) %>% 
  drop_na()

full_df <- disaster_df %>% 
  dplyr::select(2,3) 
colnames(full_df) <- c("city_name", "full")


full_df <- full_df %>% 
  dplyr::mutate(full = as.numeric(full)) %>% 
  drop_na()

joint_df <- left_join(half_df, full_df) 

disaster_name_list <- unique(joint_df$city_name)

disaster_name_df <- joint_df %>% 
  distinct(city_name)

print(disaster_name_list)

control_name_list <-unique(control_data$city_name)
control_name_df <- control_data %>% 
  distinct(city_name)

write.csv(control_name_df, file = here::here('02.raw','disaster','control.csv'),
          fileEncoding = "CP932")
write.csv(disaster_name_df, file = here::here('02.raw','disaster','disaster.csv'),
          fileEncoding = "CP932")


control_city_id

intersect(control_name_list, disaster_name_list)

print(control_name_list)

test_list <- c("榛原郡川根本町","周郡森町","加西市")

n <- regexpr("郡", test_list)

nchar("榛原郡川根本町")


new_control_name_list <- unlist(purrr::map(control_name_list, remove_gun))

intersect(new_control_name_list, disaster_name_list)



new_control_name_df <- as.data.frame(unlist(new_control_name_list))

remove_gun <- function(city_name_i){
  
  n <- regexpr("郡", city_name_i)
  if(n > 0){
    
    new_city_name <- str_sub(city_name_i, start = n+1, end = nchar(city_name_i))
    
  }else{
    
    new_city_name <- city_name_i
    
  }
  
  return(new_city_name)
  
  
  
}
