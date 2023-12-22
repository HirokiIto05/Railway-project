install.packages("Synth")
install.packages("tidysynth")
install.packages("tictoc")
install.packages("here")
install.packages("openxlsx")

library(dplyr)
library(tidysynth)
library(Synth)
library(tictoc)

library(tidyr)


# df_master <- read.csv("/content/drive/MyDrive/Colab Notebooks/Railway_Project/data/master_2.csv", fileEncoding = "cp932")
df_master <- read.csv("/content/drive/MyDrive/Colab Notebooks/Railway_Project/data/master_2.csv", fileEncoding = "cp932")
# df_master <- openxlsx::read.("/content/drive/MyDrive/Colab Notebooks/Railway_Project/data/master_2.csv")
# df_control <- openxlsx::read.xlsx("/content/drive/MyDrive/Colab Notebooks/Railway_Project/data/df_control_city.xlsx")

# list_control <- df_control |>
#   dplyr::distinct(city_id) |>
#   dplyr::pull()

# list_omitted <- c(1233, 4505, 7213, 11381, 12421, 43348)

# df_master <- df_master |>
#   dplyr::filter(
#     treatment == 1 | city_id %in% list_control
#   )

df_master <- df_master |>
  dplyr::filter(
    # !city_id %in% list_omitted,
    year >= 1996 & year <= 2019
  ) |>
    dplyr::group_by(city_id) |>
    dplyr::mutate(
    cum_social = cumsum(replace_na(social_rate, 0))
    )

head(df_master)



df_master |>
  dplyr::filter(
  treatment == 1
  ) |>
    distinct(city_id)

nrow(df_master)

df_master |>
  dplyr::group_by(city_id) |>
    dplyr::mutate(
      cum_social = cumsum(replace_na(social_rate, 0))
      ) |>
  dplyr::filter(city_id == "1549") |>
  dplyr::select(year, cum_social)

df_master |>
  distinct(city_name) |>
  nrow()

list_treatment_id <- df_master |>
  dplyr::filter(
    treatment == 1
    ) |>
    dplyr::distinct(
       city_id
      )|>
        dplyr::pull()

list_treatment_id = list_treatment_id[1:3]

list_treatment_id

id_n = 1549

synth_data <- df_master

  treatment_one <- synth_data |>
    dplyr::filter(city_id == id_n)

  year_end_n <- unique(treatment_one$year_end)

  print(id_n)

  ss <- synth_data |>
    dplyr::ungroup() |>
    dplyr::filter(treatment == 0 | city_id == id_n) |>
    distinct() 
    # dplyr::filter(!city_id  %in% c(7367, 39303, 39304, 40231, 4216))


  df_nan_check <- ss |>
    dplyr::filter(
      year >= 1996 & year <= year_end_n
    ) |>
    dplyr::summarise(
      children = mean(household_with_children_rate, na.rm = TRUE),
      own = mean(own_household_rate, na.rm = TRUE),
      workforce = mean(workforce_rate, na.rm = TRUE),
      student = mean(student_rate, na.rm = TRUE),
      old_house = mean(old_house_rate, na.rm = TRUE),
      population = mean(cum_social, na.rm = TRUE),
      .by = city_id
    )

  list_in <- df_nan_check |>
    tidyr::drop_na() |>
    dplyr::distinct(city_id) |>
    dplyr::pull()

  ss <- ss |>
    dplyr::filter(city_id %in% list_in) |>
    dplyr::select(
      -c(destroyed_all, destroyed_half)
    ) |>
    distinct() |>
    dplyr::group_by(city_id) |>
    # dplyr::mutate(
    #   cum_social = cumsum(replace_na(social_rate, 0))
    # ) |>
    dplyr::mutate(rep_outcome = cum_social) |>
    dplyr::ungroup()
