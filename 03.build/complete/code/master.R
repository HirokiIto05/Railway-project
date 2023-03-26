main <- function(){
  
  treatment_data <- load_csv("master_data", "treatment_data.csv")
  control_data <- load_csv("master_data", "control_data.csv")
  
  children_data <- read_covariates("all_children_six_data.csv") %>% 
    dplyr::select(-city_name)
  housetype_data <- read_covariates("all_housetype_data.csv")%>% 
    dplyr::select(-city_name)
  transport_data <- read_covariates("all_transport_data.csv")%>% 
    dplyr::select(-city_name)
  working_data <- read_covariates("all_working_data.csv")%>% 
    dplyr::select(-city_name)
  houseyear_data <- read_covariates("all_houseyear_data.csv")%>% 
    dplyr::select(-city_name)
  
  treatment_add <- add_covariates(treatment_data, children_data,housetype_data,
                                  transport_data, working_data, houseyear_data) 
  
  control_add <- add_covariates(control_data, children_data,housetype_data,
                                  transport_data, working_data, houseyear_data) 
  
  save_table(treatment_add, "treatment_data.csv")
  save_table(control_add, "control_data.csv")
  
}

nn_list <- unique(treatment_add$city_name)


read_covariates <- function(file_name){
  
  output_data <- read.csv(here::here('03.build', 'city_adjust','data', file_name),
                          fileEncoding = "CP932")
  return(output_data)
  
}

add_covariates <- function(data, children_data, housetype_data, 
                           transport_data, working_data, houseyear_data){
  
  output_data <- data %>% 
    dplyr::left_join(children_data) %>% 
    dplyr::left_join(housetype_data) %>% 
    dplyr::left_join(transport_data) %>%
    dplyr::left_join(working_data) %>% 
    dplyr::left_join(houseyear_data)
  
  output_data <- add_train_total(output_data)
  
  output_data <- output_data %>% 
    dplyr::group_by(city_id, year) %>% 
    dplyr::mutate(children_household_percent = (children_household/household), 
                  .after = children_household) %>% 
    dplyr::mutate(own_household_percent = (own_household/household), 
                  .after = own_household) %>% 
    dplyr::mutate(workforce_percent = (workforce_pop/total), 
                  .after = workforce_pop) %>% 
    dplyr::mutate(student_percent = (student_pop/total), 
                  .after = student_pop) %>% 
    dplyr::mutate(train_pop_percent = (train_all_pop/total), 
                  .after = train_all_pop) %>% 
    dplyr::mutate(houseyear_pop_percent = (houseyear_household/household), 
                  .after = houseyear_pop) 
  
  output_data <- ungroup(output_data)
  
  return(output_data)
  
}


add_train_total <- function(data){
  
  train_df <- data %>% 
    ungroup() %>% 
    dplyr::select(year,
                  city_id,
                  train_only_pop,
                  train_car_pop,
                  train_bus_pop,
                  train_motorcycle_pop,
                  train_bicycle_pop) %>%
    dplyr::filter(year %in% c(2000,2010)) 
  
  train_df <- train_df %>%
    dplyr::mutate(train_all_pop = rowSums(train_df[,c("train_only_pop", 
                                                     "train_car_pop", 
                                                     "train_bus_pop", 
                                                     "train_motorcycle_pop", 
                                                     "train_bicycle_pop")], na.rm = TRUE))
  
  output_data <- data %>% 
    left_join(train_df)
  
  return(output_data)
  
}



save_table <- function(data, file_name){
  
  write.csv(data, file = here::here('03.build','complete','data', file_name),
            fileEncoding = "CP932", row.names = FALSE)
  
}


