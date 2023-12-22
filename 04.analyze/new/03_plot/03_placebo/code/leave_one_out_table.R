
# leave_one_out_table -----------------------------------------------------


# read_data --------------------------------------------------------------
list_files <- list.files("04.analyze/new/01_data/data/leave_one_out/output/")

df_master |> 
  dplyr::filter(
    treatment == 1
  ) |> 
  distinct(city_name, city_id)


file_path = "2206.csv"

file_path = list_files[]

df_based = tibble()

for (file_path in list_files) {
  
test <- function(file_path, df_master){
  df_loo_based <- read.csv(paste0("04.analyze/new/01_data/data/leave_one_out/output/", file_path))
  
  treatment_name <- df_master |> 
    dplyr::filter(
      city_id == str_remove(file_path, pattern = ".csv")
    ) |> 
    dplyr::distinct(city_name) |> 
    pull()
  
  df_plot_based <- df_loo_based |> 
    dplyr::filter(
      after >= 1
    ) |> 
    dplyr::summarise(
      diff_mean = mean(diff, na.rm = TRUE),
      .by = c(city_id, placebo)
    ) |> 
    arrange(desc(diff_mean)) %>%
    mutate(
      rank = row_number()
    ) 
  
  sd_n = sd(df_plot_based$diff_mean, na.rm = TRUE)
  
  df_output <- tribble(
    ~"city_id", ~"city_name", ~"sd",
    str_remove(file_path, pattern = ".csv"),treatment_name, sd_n
  )

  return(df_output)
    
}

df_sd <- purrr::map(list_files, test, df_master) |> 
  bind_rows()
  
}

sd(df_plot_based$diff_mean)

