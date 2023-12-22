
# read_data ----------------------------------------------------------------

df_master <- read_df_csv("master", "master") 

df_placebo <- read.csv(here::here("04.analyze/new/03_plot/03_placebo/data/placebo_table_pre_year.csv"),
                       fileEncoding = "CP932") |> 
  dplyr::mutate(after = time_unit - year_end)

list_treatment_files <- df_master |> 
  dplyr::filter(
    treatment == 1
  ) |> 
  arrange(city_id) |> 
  distinct(city_name) |> 
  pull() %>%
  paste0(., ".rds")

# list_ten_city <- df_placebo |> 
#   dplyr::filter(
#     after >= 10
#   ) |> 
#   distinct(city_name) |> 
#   pull()

df_pvalue_table <- purrr::map(list_treatment_files, create_placebo_table, df_placebo, list_ten_city) |> 
  dplyr::bind_rows() |> 
  distinct()

# write.csv(df_pvalue_table, "04.analyze/new/03_plot/03_placebo/data/pvalue_table.csv")

create_placebo_table <- function(file_path, df_placebo, list_ten_city){
  
  print(file_path)
  # 
  city_name_t <- stringr::str_sub(file_path, start = 1, end = -5)
  # 
  # df_five <- df_placebo |> 
  #   dplyr::filter(after == 5) |> 
  #   dplyr::filter(
  #     which_placebo == city_name_t
  #   )
  # 
  # treated_rank <- df_five |> 
  #   mutate(
  #     diff = abs(diff)
  #   ) |> 
  #   arrange(desc(diff)) |> 
  #   mutate(
  #     rank = row_number()
  #   ) |>  
  #   dplyr::filter(
  #     .placebo == 0
  #   ) |> 
  #   pull(rank)
  # 
  # treated_diff <- df_five |> 
  #   dplyr::filter(
  #     .placebo == 0
  #   ) |> 
  #   pull(diff)
  # 
  # p_value_five = round((treated_rank / length(unique(df_five$city_id))), digits = 2)
  # 
  # df_output_five <- tibble(
  #   "city_name" = df_five |> 
  #     distinct(which_placebo) |> 
  #     pull(),
  #   # "rank" = rank_treatment,
  #   "p_value" = p_value_five,
  #   "diff" = treated_diff,
  #   "n" =  length(unique(df_five$city_id)),
  #   "category" = "five"
  # )
  # 
  # if (city_name_t %in% list_ten_city) {
  #   
  #   df_ten <- df_placebo |> 
  #     dplyr::filter(after == 10) |> 
  #     dplyr::filter(
  #       which_placebo == city_name_t
  #     )
  #   
  #   treated_rank <- df_ten |> 
  #     mutate(
  #       diff = abs(diff)
  #     ) |> 
  #     arrange(desc(diff)) |> 
  #     mutate(
  #       rank = row_number()
  #     ) |>  
  #     dplyr::filter(
  #       .placebo == 0
  #     ) |> 
  #     pull(rank)
  #   
  #   treated_diff <- df_ten |> 
  #     dplyr::filter(
  #       .placebo == 0
  #     ) |> 
  #     pull(diff)
  #   
  #   p_value_ten = round((treated_rank / length(unique(df_ten$city_id))), digits = 2)
  #   
  #   df_output_ten <- tibble(
  #     "city_name" = df_ten |> 
  #       distinct(which_placebo) |> 
  #       pull(),
  #     # "rank" = rank_treatment,
  #     "p_value" = p_value_ten,
  #     "diff" = treated_diff,
  #     "n" =  length(unique(df_five$city_id)),
  #     "category" = "ten"
  #   )
  #   
  #   df_output_lag <- bind_rows(df_output_five, df_output_ten)
  #   
  # } else {
  #   
  #   df_output_lag <- df_output_five
  #   
  # }
  # 
  
  df_mean <- df_placebo |> 
    dplyr::filter(after >= 1) |> 
    dplyr::filter(
      which_placebo == city_name_t
    ) |> 
    summarise(
      diff = mean(diff, na.rm = TRUE),
      .by = c(city_id, .placebo, which_placebo)
    ) |> 
    distinct()
  
  treated_rank <- df_mean |> 
    mutate(
      diff = abs(diff)
    ) |> 
    arrange(desc(diff)) |> 
    mutate(
      rank = row_number()
    ) |>  
    dplyr::filter(
      .placebo == 0
    ) |> 
    pull(rank)
  
  treated_diff <- df_mean |> 
    dplyr::filter(
      .placebo == 0
    ) |> 
    pull(diff) %>%
    round(., digits = 2)
  
  p_value_mean = round((treated_rank / length(unique(df_mean$city_id))), digits = 2)
  
  df_output <- tibble(
    "city_name" = df_mean |> 
      distinct(which_placebo) |> 
      pull(),
    # "rank" = rank_treatment,
    "p_value" = p_value_mean,
    "diff" = treated_diff,
    "n" =  length(unique(df_mean$city_id))
    # "category" = "mean"
  )
  
  
  # df_output <- bind_rows(df_output_mean, df_output_lag)
  
  return(df_output)
  
}


output_pvalue <- df_pvalue_table |> 
  # dplyr::select(
  #   -category
  # ) |> 
  kbl(format = 'latex', booktabs = TRUE) |> 
  kable_styling() 
  # add_header_above(c(" " = 1, 
  #                    var_1 = 3,
  #                    var_2 = 3))

output_pvalue


