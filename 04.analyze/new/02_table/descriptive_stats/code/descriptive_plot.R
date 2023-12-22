# read_data ---------------------------------------------------------------
df_master <- read_df_csv("master", "master")

list_control <- read_df_csv("geometry_base", "df_control_city") |> 
  dplyr::distinct(city_id) |> 
  dplyr::pull()

df_master <- df_master |> 
  dplyr::filter(
    treatment == 1 | city_id %in% list_control,
    year <= 2019
  ) 

aa <- df_master |> 
  dplyr::filter(
    between(year, 2001, 2002)
  ) |> 
  group_by(city_id) |> 
  dplyr::mutate(
    diff = total - dplyr::lag(total),
    .after = total
  )


df_master |> 
  dplyr::filter(
    is.na(line_name)
  ) |> 
  distinct(city_name)



# clean_data --------------------------------------------------------------

list_col_stats <- c(
  "total",
  "household",
  "moving_in",
  "moving_out",
  "social",
  "social_rate"
)

df_plot_based <- df_master %>%
  dplyr::summarise(
    across(list_col_stats, ~mean(., na.rm = TRUE)),
    # n = dplyr::n(),
    .by = c("treatment", "year")
  ) %>% 
  mutate(across(everything(), ~round(., digits = 2)),
         treatment = as.character(treatment)) |> 
  pivot_longer(
    cols = -c(treatment, year),
    names_to = 'category',
    values_to = 'value') 
  

ggplot(df_plot_based, aes(x = year, y =value, color = treatment)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ category,
             scales = "free") 
  

