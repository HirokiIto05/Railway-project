
# setup -------------------------------------------------------------------
# from google.colab import drive
# drive.mount('/content/drive')

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


# read_data ---------------------------------------------------------------


# df_master <- read.csv("/content/drive/MyDrive/Colab Notebooks/Railway_Project/data/master_2.csv", fileEncoding = "cp932")
df_master <- read.csv("/content/drive/MyDrive/Colab Notebooks/Railway_Project/data/master_921.csv", fileEncoding = "cp932")
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



# clean_data --------------------------------------------------------------
df_master <- df_master |>
  dplyr::group_by(city_id) |>
  dplyr::mutate(
    cum_social = cumsum(replace_na(social_rate, 0))
  )

df_master |>
  dplyr::group_by(city_id) |>
  dplyr::filter(city_name == "上北郡六戸町") |>
  dplyr::select(year, cum_social, social_rate, social, total)

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

list_treatment_id = list_treatment_id[9:16]
list_treatment_id 

# list_control_id = df_master |>
#   dplyr::filter(
#     treatment == 0
#   ) |>
#   distinct(city_id) |>
#   pull()

# list_control_id

# df_master |>
#   dplyr::filter(
#   city_id == 2206
# )

# id_n = 2206

# Run_main_scm ------------------------------------------------------------

for (id_n in list_treatment_id) {
  
  tictoc::tic()
  
  synth_data <- df_master
  
  treatment_one <- synth_data |>
    dplyr::filter(city_id == id_n)
  
  year_end_n <- unique(treatment_one$year_end)
  
  print(id_n)
  
  ss <- synth_data |>
    dplyr::ungroup() |>
    dplyr::filter(treatment == 0 | city_id == id_n) |>
    distinct()
  # dplyr::filter(!city_id  %in% c(4505, 11381, 43348, 40231))
  # dplyr::filter(!city_id  %in% c(7367, 39303, 39304, 40231, 4216))
  
  
  df_nan_check <- ss |>
    dplyr::filter(
      year >= 1995 & year <= year_end_n
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
    dplyr::mutate(rep_outcome = cum_social) |>
    dplyr::ungroup()
  
  output_synth <- ss |>
    
    synthetic_control(
      outcome = cum_social,
      unit = city_id,
      time = year,
      i_unit = id_n,
      i_time = year_end_n,
      generate_placebos = T
    ) |>
    
    generate_predictor(
      time_window = 1995:year_end_n,
      children = mean(household_with_children_rate, na.rm = TRUE),
      own = mean(own_household_rate, na.rm = TRUE),
      workforce = mean(workforce_rate, na.rm = TRUE),
      student = mean(student_rate, na.rm = TRUE),
      old_house = mean(old_house_rate, na.rm = TRUE),
      population = mean(rep_outcome, na.rm = TRUE)
    ) |>
    
    # generate_predictor(
    #   time_window = year_end_n - 1,
    #   pre_year_outcome = rep_outcome) |>
    
    generate_weights(
      optimization_window = 1995:year_end_n,
      margin_ipop = .02, sigf_ipop = 7, bound_ipop = 6
    ) |>
    generate_control()
  
  print(id_n)
  
  city_name_t <- ss |>
    dplyr::filter(
      treatment == 1
    ) |>
    dplyr::distinct(city_name) |>
    dplyr::pull()
  
  table_name <- paste0(city_name_t,'.rds')
  
  file_name_table <- paste0(here::here('/content/drive/MyDrive/Colab Notebooks/Railway_Project/main_921/',
                                       table_name))
  
  
  saveRDS(object =  output_synth,
          file = file_name_table)
  
  tictoc::toc()
  
}

