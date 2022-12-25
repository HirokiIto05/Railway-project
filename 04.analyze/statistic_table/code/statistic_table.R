main <- function(){
  
  treatment_data <- load_data('master_data', 'treatment_data.csv')
  control_data <- load_data('master_data', 'control_data.csv')
  
  master_data <- create_master(treatment_data, control_data)
  
  master_data <- add_variable(master_data)
  
  stat_table <- create_table(master_data)
  
  stat_table
  
  
  
}


load_data <- function(folder_name, file_name){
  
  output_data <- read.csv(here::here('03.build',folder_name,'data', file_name),
                          fileEncoding = "CP932")
  return(output_data)
  
}



create_master <- function(treatment_data, control_data){
  
  master_data <- dplyr::bind_rows(treatment_data, control_data)
  
  output_data <- add_variable(input_data = master_data)
  
  return(output_data)
  
  
}

input_data <- master_data

add_variable <- function(input_data){
  
  output_data <- input_data %>% 
    dplyr::group_by(city_id, year) %>% 
    dplyr::mutate(over_65 = sum(r65_69,
                                r70_74,r75_79,
                                r80_over)) %>% 
    dplyr::mutate(elderly_rate = (over_65/total)*100) %>% 
    dplyr::ungroup()
  
  return(output_data)
  
}


create_table <- function(master_data){
  
  
  master_data <- master_data %>% 
    dplyr::group_by(dummy)

  mean(treatment_data$total)
  
  master_table <- master_data %>% 
    dplyr::summarise(
      N = n_distinct(city_id),
      max_total = max(total),
      min_total = min(total),
      total  = mean(total),
      household = mean(household),           
      birth  = mean(birth),         
      move_out = mean(move_out),
      working = mean(working),
      over_65 = mean(over_65),
      elderly_rate = mean(elderly_rate)
      )
  

  test <- apply(master_table, 1, round) %>% 
    as_tibble()
  
  
  tidy_table <- master_table %>% 
    kableExtra::kbl() %>%
    kableExtra::kable_classic_2(full_width = F)
  
  return(tidy_table)
  
  file_name <- paste0(here::here('04.analyze', 'statistic_table', 'table', 'statistic_table.pdf'))
  
  stat_table %>%
    kableExtra::save_kable(file = file_name, self_contained = T)

  
  
  
}


library(kableExtra)

mean(master_table$total, trim = 0.05)





