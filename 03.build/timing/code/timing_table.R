main <- function(){
  
  treatment_data <- load_data("base_treatment.csv")
  control_data <- load_data("base_control.csv")
  
  all_data <- create_all(treatment_data, control_data)
  
  treat_city_id <-unlist(distinct(treatment_data, city_id)) |> 
    as.character()
  
  timing_data <- create_lead_lag(percent_treatment, treat_city_id)
  
  save_table(timing_data)
  
}

mean_timing <- group_by(timing_data, timing) |> 
  summarise(outcome = mean(percent))


pp <- ggplot(mean_timing, aes(x = timing, y = outcome)) +
  geom_point() + 
  geom_line()

ggsave(pp, filename = "/Users/ito_hiroki/01.Research/Railway-project/04.analyze/table/cutoff_zero.pdf")

install.packages("panelView")
library(panelView)


load_data <- function(file_name){
  
  output_data <- read.csv(here::here('03.build','base_percent','data',file_name),
                          fileEncoding = "CP932") 
  return(output_data)
  # folder_name <- here::here('03.build','integrate','data')
  # file_name <- paste0(group_name,'_data','.csv')
  
  # new_data <- read.csv(here::here(folder_name, file_name), fileEncoding = "CP932")
  # 
  # return(new_data) 
}


create_all <- function(treatment_data, control_data){
  
  all_data <- bind_rows(treatment_data, control_data) |> 
    dplyr::mutate(treatment = ifelse((treatment_year != 0), 1,0),
                  .after = city_name) |> 
    ungroup()
  
  return(all_data)
  
}


create_lead_lag <- function(treatment_data, treat_city_id){
  
  emp_data <- data.frame(matrix(ncol = 11)[0, ])
  
  for(i in treat_city_id){
    
    new_data <- treatment_data |>
      dplyr::filter(city_id == i)
    
    c_num <- distinct(new_data, treatment_year) |>
      as.numeric()
    
    lead <- c_num - 1995
    lag <- 2019 - c_num
    
    timing_list <- seq(-lead, lag, by = 1) 
    
    new_data <- new_data |> 
      dplyr::mutate(timing = timing_list, .keep = c("unused")) 
    
    emp_data <- rbind(emp_data, new_data)
  } 
  
  return(emp_data)
  
}

save_table <- function(timing_data){
  
  write.csv(timing_data,
            file =  here::here('03.build','timing','data','timing.csv'),
            fileEncoding = "CP932")
  
  
}


