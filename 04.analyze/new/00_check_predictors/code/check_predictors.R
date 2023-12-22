df_master <- read_df_csv("master", "master") 

list_control <- df_c <- read_df_csv("geometry_base", "df_control_city") |> 
  dplyr::distinct(city_id) |> 
  dplyr::pull()

df_master <- df_master |> 
  dplyr::filter(
    treatment == 1 | city_id %in% list_control
  ) |> 
  dplyr::group_by(city_id) |> 
  dplyr::mutate(
    cum_social = cumsum(replace_na(social_rate, 0))
  ) |> 
  dplyr::ungroup()

treatment_id_lists <- df_master |> 
  dplyr::filter(treatment == 1) |> 
  dplyr::distinct(city_id) |> 
  dplyr::pull(city_id)


df_mean_check <- df_master |> 
  dplyr::filter( 
    year >= 1996 & year <= 2019
  ) |> 
  dplyr::summarise(
    children = mean(household_with_children_rate, na.rm = TRUE),
    own = mean(own_household_rate, na.rm = TRUE),
    workforce = mean(workforce_rate, na.rm = TRUE),
    student = mean(student_rate, na.rm = TRUE),
    old_house = mean(old_house_rate, na.rm = TRUE),
    population = mean(cum_social, na.rm = TRUE),
    .by = c(city_name, city_id, treatment)
  ) |> 
  tidyr::pivot_longer(
    cols = -c(city_name, city_id, treatment),
    names_to = "predictor",
    values_to = "value"
  ) 

df_mean_control <- df_mean_check |> 
  dplyr::filter(
    treatment == 0
  )
  # dplyr::mutate(
  #   across(-c(city_name, treatment), ~round(., digits = 3))
  # ) 
  

df_mean_treatment <- df_mean_check |> 
  dplyr::filter(
    treatment == 1
  )

a <- function(var) {
  
  # var = rlang::enquo(var)
  
  # ggplot() +
  #   geom_boxplot(data = df_mean_check, aes(x = !!var), outlier.shape = NA) +
  #   geom_point(data = df_mean_treatment, aes(x = !!var, y = 0)) +
  #   geom_text(data = df_mean_treatment, aes(x = !!var, y = 0, label = city_name),
  #             vjust = -0.5, hjust = -1, size = 3,
  #             angle = 70) +
  #   theme_bw(
  #     base_family = "HiraKakuPro-W3"
  #   ) 
  
  df_mean_treatment
  
  plot_output <- ggplot() +
    geom_boxplot(data = df_mean_check, aes(x = value), outlier.shape = NA) +
    geom_point(data = df_mean_treatment, aes(x = value, y = 0),
               color = "#3C8DAD") +
    # geom_text(data = df_mean_treatment, aes(x = value, y = 0, label = city_name),
    #           vjust = -1, hjust = -0.1, size = 2.5,
    #           angle = 60,
    #           family = "HiraKakuPro-W3") +
    theme_bw(base_family = "HiraKakuPro-W3") +
    # coord_flip() +
    theme(
      axis.text.y = element_blank(),
      axis.title = element_blank(),
      strip.background = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
      # panel.grid.major.y = element_line(color = "lightgray")
      # strip.text = element_text(size = 22),
      # strip.text.x = element_text(size = 20)
    ) +
    facet_wrap(.~ predictor, 
               scales = "free",
               ncol = 1) 
    
}

ggsave(plot_output, 
       file = "04.analyze/new/00_check_predictors/figure/check.png",
       height = 9,
       width = 10)

a(children) + a(own) + a(workforce) + a(student) + a(old_house) + a(population) +
  plot_layout(ncol = 1)


a(workforce)


