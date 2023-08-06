main <- function(){
  
  treatment_data <- load_data('complete', 'treatment_data.csv')
  control_data <- load_data('complete', 'control_data.csv')
  
  master_data <- create_master(treatment_data, control_data)
  
  master_data <- add_variable(master_data)
  
  stat_table <- create_table(master_data)
  
  stat_table
  
}


tt <- treatment_data |> 
  dplyr::filter(treatment_year <= 2015) |> 
  dplyr::filter(city_id != 21403,
                city_id != 21421)  




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
  
  output_data <- input_data |> 
    dplyr::group_by(city_id, year) |> 
    dplyr::mutate(over_65 = sum(r65_69,
                                r70_74,r75_79,
                                r80_over)) |> 
    dplyr::mutate(elderly_rate = (over_65/total)) |> 
    dplyr::ungroup()
  
  return(output_data)
  
}

tt <- treatment_data |> 
  distinct(city_id)

treatment_five <- treatment_data |> 
  dplyr::filter(treatment_year <= 2015)

treatment_five |> 
  distinct(city_id)

create_table <- function(master_data){
  
  
  master_base_table <- master_data |> 
    dplyr::group_by(dummy)
  
  master_table <- master_base_table |> 
    dplyr::summarise(
      N = n_distinct(city_id),
      max_pop = max(total, na.rm = TRUE),
      min_pop = min(total, na.rm = TRUE),
      pop  = mean(total, na.rm = TRUE),
      household = mean(household, na.rm = TRUE),           
      working = mean(workforce_percent, na.rm = TRUE),
      elderly_rate = mean(elderly_rate, na.rm = TRUE)
      )
  

  t_master_table <- (master_table)
  
  str(t_master_table)
  
  colnames(t_master_table) <- c("control", "treatment")
  
  t_master_table <- t_master_table |> 
    dplyr::relocate(treatment, .before = control) 
    
  
  
  
  tidy_table <- master_table |> 
    kableExtra::kbl() |>
    kableExtra::kable_classic_2(full_width = F)
  
  return(tidy_table)
  
  file_name <- paste0(here::here('04.analyze', 'statistic_table', 'table', 'statistic_table.pdf'))
  
  stat_table |>
    kableExtra::save_kable(file = file_name, self_contained = T)

  
  
  
}


library(kableExtra)

mean(master_table$total, trim = 0.05)





