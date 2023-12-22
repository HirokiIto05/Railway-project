
library(readxl)

df_area <- clean_area_data()
df_fci <- clean_fci_data()
df_placebo <- clean_placebo_data() |> 
# df_placebo <- placebo_df |> 
  mutate(
    city_id = as.character(city_id)
  ) |> 
  mutate(
    after = time_unit - year_end
  )
df_distance <- clean_distance_main_city()
df_length <- clean_length_data()

df_cross_based <- joint_cross_data(df_area, 
                                   df_fci, 
                                   df_placebo, 
                                   df_distance,
                                   df_length)

# write.csv(df_cross_based, "03.build/cross/data/cross_based_pre_year.csv")

clean_area_data <- function(){
  
  df_area <- read.csv("02.raw/area_space/R1_R5_all_mencho.csv", fileEncoding = "cp932") |> 
    dplyr::select(1, 5)
    
  colnames(df_area) <- c(
    "city_id",
    "area"
  )
  
  df_area <- df_area |> 
    mutate(
      area = as.numeric(area)
    ) |> 
    dplyr::filter(
      !city_id %in% c("", "全国面積")
    ) |> 
    drop_na()
  
  
  write.csv(df_area, "03.build/cross/data/area.csv")
  
  return(df_area)
}

clean_fci_data <- function() {
  
  df_fci <- read.csv("03.build/power/data/fci_data.csv", fileEncoding = "cp932") |> 
    dplyr::select(
      city_id, 
      fci,
      year
    ) |> 
    mutate(
      city_id = as.character(city_id)
    )
  
  # 2005 - 2019
  df_fci <- df_fci |> 
    dplyr::ungroup() |> 
    dplyr::summarise(fci_mean = mean(fci, na.rm = TRUE),
                     .by = city_id)
  
  write.csv(df_fci, "03.build/cross/data/fci.csv")
  
  return(df_fci)
}


clean_placebo_data <- function() {
  
  df_placebo <- read.csv(here::here("04.analyze/new/03_plot/03_placebo/data/placebo_table_pre_year.csv"), fileEncoding = "cp932") |> 
    mutate(
      city_id = as.character(city_id)
    ) |> 
    mutate(
      after = time_unit - year_end
    )
  
  write.csv(df_placebo, "03.build/cross/data/placebo.csv")
  
  return(df_placebo)
  
}


clean_distance_main_city <- function() {
  df_master <- read.csv('03.build/master/data/master.csv', fileEncoding = 'cp932')
  df_treatment <- df_master |> 
    dplyr::filter(
      treatment == 1
    ) |> 
    dplyr::select(city_name, city_id) |> 
    distinct()

  df_distance <- readxl::read_excel("02.raw/main_city_distance.xlsx") |> 
    right_join(df_treatment) |> 
    dplyr::select(
      city_id, distance
    ) |> 
    mutate(
      city_id = as.character(city_id)
    )
  
  
  write.csv(df_distance, "03.build/cross/data/distance.csv")
  
  return(df_distance)
  
}


clean_length_data <- function() {
  
  df_master <- read.csv('03.build/master/data/master.csv', fileEncoding = 'cp932')
  df_treatment <- df_master |> 
    dplyr::filter(
      treatment == 1
    ) |> 
    dplyr::select(city_name, city_id, line_name) |> 
    distinct()
  
  df_length <- tribble(
    ~"line_name", ~"track_length",
    "江差線", 42.1,
    "ふるさと銀河線", 140.0,
    "南部縦貫鉄道線", 20.9,
    "十和田観光電鉄線", 14.7,
    "七尾線", 20.4,
    "八百津線", 7.3,
    "可部線", 46.2,
    "島原鉄道線", 35.3,
    "高千穂線", 29.1,
    "揖斐線", 5.6
  ) |> 
    dplyr::filter(
      line_name != "南部縦貫鉄道線"
    )
  
  df_length <- df_length |> 
    right_join(df_treatment) |> 
    dplyr::select(city_id, track_length) |> 
    mutate(
      city_id = as.character(city_id),
      track_length = as.numeric(track_length)
    )
  
  return(df_length)
  
  
  
  
}

joint_cross_data <- function(df_area, df_fci, df_placebo, df_distance,
                             df_length) {
  
  # df_five <- df_placebo |> 
  #   dplyr::select(
  #     city_id, diff, after, .placebo
  #   ) |> 
  #   dplyr::filter(
  #     after == 5,
  #     .placebo == 0
  #   ) |> 
  #   distinct() |> 
  #   rename(
  #     diff_five = diff
  #   ) |> 
  #   dplyr::select(
  #     -after
  #   )
  # 
  # df_ten <- df_placebo |> 
  #   dplyr::select(
  #     city_id, diff, after, .placebo
  #   ) |> 
  #   dplyr::filter(
  #     after == 10,
  #     .placebo == 0
  #   ) |> 
  #   distinct() |> 
  #   rename(
  #     diff_ten = diff
  #   ) |> 
  #   dplyr::select(
  #     -after
  #   )
  
  
  df_output <- df_placebo |>
    dplyr::filter(.placebo == 0,
                  after >= 1
                  ) |> 
    dplyr::summarise(
      diff_mean = mean(diff, na.rm = TRUE),
      .by = c(city_name, city_id)
    ) |> 
    distinct() |> 
    # left_join(df_five) |> 
    # left_join(df_ten) |> 
    # dplyr::select(
    #   -.placebo
    # ) |> 
    # pivot_longer(
    #   cols = c(diff_mean, diff_ten, diff_five),
    #   names_to = "category",
    #   values_to = "diff"
    # ) |> 
    # dplyr::mutate(
    #   index_gun = regexpr("郡", city_name)
    #   # city_name = stringr::str_replace_all(city_name, "群", "a")
    # ) |> 
    # mutate(
    #   city_name = if_else(index_gun != -1,
    #                       str_sub(city_name, start = index_gun + 1, end = -1),
    #                       city_name)
    # ) |> 
    left_join(df_area) |> 
    left_join(df_fci) |> 
    left_join(df_distance) |> 
    left_join(df_length) 
    # mutate(
    #   category = as.factor(category),
    #   category = fct_relevel(category, 
    #                          "diff_mean",
    #                          "diff_five",
    #                          "diff_ten"))
  
  return(df_output)
  
}

library(forcats)


