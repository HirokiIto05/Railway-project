df_master <- read_df_csv("master", "master") 

list_control <- df_c <- read_df_csv("geometry_base", "df_control_city") |> 
  dplyr::distinct(city_id) |> 
  dplyr::pull()

df_master <- df_master |> 
  dplyr::filter(
    treatment == 1 | city_id %in% list_control
  )

list_t_id <- df_master |> 
  dplyr::filter(
    treatment == 1 
  ) |> 
  dplyr::distinct(city_id) |> 
  dplyr::pull()


df_based <- tibble()

id_n <- 1361

for (id_n in list_t_id) {
  
  treatment_one <- df_master |> 
    dplyr::filter(city_id == id_n) 
  
  year_end_n <- unique(treatment_one$year_end)
  
  print(id_n)
  
  ss <- df_master |>
    dplyr::ungroup() |> 
    dplyr::filter(treatment == 0 | city_id == id_n) |> 
    distinct() |> 
  dplyr::filter(!city_id %in% c(7367, 39303, 39304, 40231, 4216)) 
  
  
  df_nan_check <- ss |> 
    dplyr::filter( 
      year >= 1996 & year <= year_end_n + 1
    ) |> 
    dplyr::summarise(
      children = mean(household_with_children_rate, na.rm = TRUE),
      own = mean(own_household_rate, na.rm = TRUE),
      workforce = mean(workforce_rate, na.rm = TRUE),
      student = mean(student_rate, na.rm = TRUE),
      old_house = mean(old_house_rate, na.rm = TRUE),
      # population = mean(rep_outcome, na.rm = TRUE)
      .by = c(city_id, city_name, treatment)
    ) 
  
  list_in <- df_nan_check |> 
    tidyr::drop_na() |> 
    dplyr::distinct(city_id) |> 
    dplyr::pull()
  
  df_treatment <- df_nan_check |> 
    dplyr::filter(
      treatment == 1
    )
  
  df_iqr <- df_nan_check |> 
    dplyr::filter(
      treatment == 0
    ) %>%
    dplyr::mutate(
      children_iqr = IQR(children, na.rm = TRUE),
      own_iqr = IQR(own, na.rm = TRUE),
      workforce_iqr = IQR(workforce, na.rm = TRUE),
      student_iqr = IQR(student, na.rm = TRUE),
      old_house_iqr = IQR(old_house, na.rm = TRUE),
      children_q1 = quantile(children, na.rm = TRUE)[2],
      own_q1 = quantile(own, na.rm = TRUE)[2],
      workforce_q1 = quantile(workforce, na.rm = TRUE)[2],
      student_q1 = quantile(student, na.rm = TRUE)[2],
      old_house_q1 = quantile(old_house, na.rm = TRUE)[2],
      children_q3 = quantile(children, na.rm = TRUE)[4],
      own_q3 = quantile(own, na.rm = TRUE)[4],
      workforce_q3 = quantile(workforce, na.rm = TRUE)[4],
      student_q3 = quantile(student, na.rm = TRUE)[4],
      old_house_q3 = quantile(old_house, na.rm = TRUE)[4],
      children_upper = children_q3 + (1.5 * children_iqr),
      own_upper = own_q3 + (1.5 * own_iqr),
      workforce_upper = workforce_q3 + (1.5 * workforce_iqr),
      student_upper = student_q3 + (1.5 * student_iqr),
      old_house_upper =old_house_q3 + (1.5 * old_house_iqr),
      children_lower = children_q1 - (1.5 * children_iqr),
      own_lower = own_q1 - (1.5 * own_iqr),
      workforce_lower = workforce_q1 - (1.5 * workforce_iqr),
      student_lower = student_q1 - (1.5 * student_iqr),
      old_house_lower =old_house_q1 - (1.5 * old_house_iqr)
    ) |> 
    dplyr::select(
      ends_with("_upper") | ends_with("_lower")
    ) |> 
    dplyr::distinct() |> 
    dplyr::bind_cols(
      df_treatment
    ) |> 
    dplyr::mutate(
      children_check =dplyr::between(children, children_lower, children_upper), 
      own_check = dplyr::between(own, own_lower, own_upper), 
      workforce_check = dplyr::between(workforce, workforce_lower, workforce_upper), 
      student_check = dplyr::between(student, student_lower, student_upper), 
      old_house_check = dplyr::between(old_house, old_house_lower, old_house_upper) 
    ) |> 
    dplyr::select(
      city_id,
      treatment,
      children_check,
      own_check,
      workforce_check,
      student_check,
      old_house_check
    ) |> 
    tidyr::pivot_longer(
      cols = -c("city_id", "treatment"),
      names_to = "category",
      values_to = "check"
    ) 
  
  
  df_based <- dplyr::bind_rows(df_based, df_iqr)
    # dplyr::filter(
    #   check == FALSE
    # )
  
}

