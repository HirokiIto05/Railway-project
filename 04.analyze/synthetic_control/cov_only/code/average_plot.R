main <- function(){
  
  
  
  treatment_name_lists <- master_data %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(dummy == 1) %>% 
    dplyr::distinct(city_name) %>% 
    unlist() %>% 
    as.character()
  
  city_name_list <- adjust_after_df
  
}


create_average_df <- function(){
  
  
  treatment_data <- master_data %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(after = year - treatment_year + 1,
                  .after = year) %>% 
    dplyr::filter(dummy ==1 & after %in% seq(-12,12)) 
  
  control_data <- master_data %>%
    dplyr::filter(dummy == 0) %>% 
    dplyr::group_by(city_name) %>% 
    dplyr::mutate(after = seq(-12, 12),
                  .after = year)
  
  adjust_after_df <- dplyr::bind_rows(treatment_data, control_data)
  
  return(adjust_after_df)
  
}


create_average_treatment <- function(){
  
  
  average_t <- average_data %>% 
    dplyr::filter(dummy == 1) %>% 
    dplyr::mutate(outcome = middle/)
  
  
}


calculate_treat <- function(city_name_n, data){
  
  data_one <- data %>% 
    dplyr::filter(city_name = city_name_n) %>% 
    dplyr::filter(after == 0) 
  
  base_num <- unique(data_one$middle)
  
  output_data <- data %>% 
    dplyr::mutate(outcome_percent = middle/base_num,
                  .after = year)
  
  return(control_one)
  
}





covariate_mean <- master_data %>% 
  dplyr::group_by(dummy) %>% 
  dplyr::summarise(children_mean = mean(children_household_percent, na.rm = TRUE),
                   own_mean = mean(own_household_percent, na.rm = TRUE),
                   train_mean = mean(train_pop_percent, na.rm = TRUE),
                   workforce_mean = mean(workforce_percent,na.rm = TRUE),
                   student_mean = mean(student_percent,na.rm = TRUE),
                   houseyear_mean =   mean(houseyear_pop_percent,na.rm = TRUE)) 

average_c <- 



synth_ready <- function(id_n, master_data){
  
  treatment_ready <- base_plot_df %>% 
    dplyr::filter(city_name == city_name_t)
  
  int_year <- unique(treatment_ready$treatment_year)
  
  int_treatment_num <- treatment_ready %>% 
    dplyr::filter(year == int_year - 1) %>% 
    dplyr::distinct(middle) 
  
  base_num <- unique(int_treatment_num$middle)
  
  treatment_ready <- treatment_ready %>% 
    dplyr::mutate(outcome_percent = middle/base_num)
  
  control_ready <- base_plot_df %>% 
    dplyr::filter(dummy == 0) 
  
  control_city_id <- unique(control_ready$city_id)
  
  control_ready <- purrr::map(control_city_id, calculate_control, control_ready,  int_year) %>% 
    dplyr::bind_rows()
  
  control_average_df <- control_ready %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(year) %>% 
    dplyr::summarise(outcome_percent = mean(outcome_percent),
                     dummy = 0)
  
  synth_base_data <- dplyr::bind_rows(treatment_ready, control_average_df)
  
  return(synth_base_data)
  
}

calculate_control <- function(id_c, control_ready, int_year){
  
  control_one <- control_ready %>% 
    dplyr::filter(city_id == id_c)
  
  int_control_num <- control_one %>% 
    dplyr::filter(year == int_year - 1) %>% 
    dplyr::distinct(middle)
  
  base_num <- unique(int_control_num$middle)
  
  control_one <- control_one %>% 
    dplyr::group_by(city_id) %>% 
    dplyr::mutate(outcome_percent = middle/base_num)
  
  return(control_one)
  
}

ggplot_base <- synth_base_data %>% 
  select(city_name, year, treatment_year, 
         dummy, outcome_percent)

ggplot_base_c <- ggplot_base %>% 
  dplyr::filter(dummy == 0) %>% 
  dplyr::rename(c_outcome = outcome_percent) %>% 
  ungroup() %>% 
  distinct(c_outcome)

ggplot_base_t <- ggplot_base %>% 
  dplyr::filter(dummy == 1) %>% 
  dplyr::rename(t_outcome = outcome_percent) %>% 
  ungroup()

base_df <- bind_cols(ggplot_base_t, ggplot_base_c)

output_plot <- ggplot(ggplot_base,
                      aes(x = year, y = outcome_percent, color = dummy, linetype = dummy)) +
  geom_line() 
  # scale_linetype_manual(values = c("solid", "longdash")) +
  # scale_color_manual(values = c("black", "black"))

output_plot <- ggplot(base_df)+
  geom_line(aes(x = year ,y = t_outcome,  linetype = "average_t")) +
  geom_line(aes(x = year, y = c_outcome, linetype = "average_c"), size = 0.5) +
  scale_linetype_manual(name = "value" ,values = c("average_t" = "solid",
                                                   "average_c" = "dashed")) +
  scale_color_manual(values = c("real_y" = "black",
                                "synth_y" = "black")) +
  geom_point(aes(x = year, y = t_outcome), size = 1.1)

output_plot  

  # geom_vline(xintercept = int_year - 0.5, linetype = "dotted", size = 0.8) +
  labs(title = title_id,
       y = "population") +
  theme_bw(base_family = "HiraKakuPro-W3") +
  theme(plot.title = element_text(size = 13),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_y_continuous(breaks = seq(0.6,1.4, 0.2)) +
  scale_x_continuous(breaks = seq(1995,2015,5)) 