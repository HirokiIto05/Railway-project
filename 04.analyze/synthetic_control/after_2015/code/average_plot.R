
base_plot_df <- master_data %>% 
  dplyr::filter(city_name == city_name_t | dummy == 0) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(dummy)


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