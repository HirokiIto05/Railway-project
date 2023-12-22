
# setup -------------------------------------------------------------------

library(kableExtra)
library(dplyr)
library(tidyr)


# read_Data ---------------------------------------------------------------
df_master <- read.csv("03.build/master/data/master.csv", 
                      fileEncoding = "CP932") 

df_control <- read.csv("03.build/geometry_base/data/df_control_city.csv",
                       fileEncoding = "CP932") 

df_pop <- read.csv("03.build/master/data/master_all_municipalities.csv",
                   fileEncoding = "CP932") 


# clean_data --------------------------------------------------------------
list_control <- df_control |>
  dplyr::distinct(city_id) |>
  dplyr::pull()

# list_omitted <- c(1233, 4505, 7213, 11381, 12421, 43348)

df_master <- df_master |>
  dplyr::filter(
    treatment == 1 | city_id %in% list_control
  ) |> 
  dplyr::filter(
    between(year, 1995, 2019)
  )

list_col_stats <- c(
  "total",
  "household",
  "moving_in",
  "moving_out",
  "social",
  "social_rate",
  "workforce_rate",
  "household_with_children_rate",
  "own_household_rate",
  "workforce_rate",
  "student_rate"
)

df_master <- df_master |> 
  select(
    append(list_col_stats, c("year", "treatment", "city_id"))
  ) |> 
  mutate(
    treatment = as.character(treatment)
  )

df_pop <- df_pop |> 
  select(
    append(list_col_stats, c("year", "city_id"))
  ) |> 
  dplyr::filter(
    between(year, 1996, 2019)
  ) |> 
  mutate(
    treatment = "all"
  )


df_table_based <- df_master |> 
  bind_rows(df_pop)
  


# create_table ------------------------------------------------------------  
table_output <- df_table_based %>%
  dplyr::summarise(
    across(list_col_stats, ~mean(., na.rm = TRUE)),
    n = length(unique(city_id)),
    .by = treatment
  ) %>%
  mutate(across(list_col_stats, ~round(., digits = 2))) |> 
  mutate(n = round(n)) |> 
  pivot_longer(
    cols = -treatment,
    names_to = 'category',
    values_to = 'value'
  ) |> 
  pivot_wider(
    names_from = treatment,
    values_from = 'value'
  ) |> 
  dplyr::rename(treatment = "1", control = "0") |> 
  select(
    category, treatment, control, all
  )

table_output |> 
  kable(format = 'latex') |> 
  kable_styling()

library(kableExtra)

table_output |> 
  kable(format = 'latex')



