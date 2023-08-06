main <- function() {
  
  df_density <- change_jr_data() |> 
    dplyr::mutate(density = as.numeric(density)) |> 
    dplyr::relocate(company_name, .before = line_name) |> 
    dplyr::mutate(line_name = str_replace_all(line_name, " ",""))
  
  save_df_csv(df_density, folder_name_n = "density", "jr")
  
}


change_jr_data <- function() {
  
  df_jr_est <- readxl::read_xlsx("02.raw/density/JR_excel/東日本.xlsx") |> 
    dplyr::select(2, 3, 4, 7)
  
  colnames(df_jr_est) <- c("line_name", "station_name", 
                           "line_distance", "density")
  
  jr_est <- df_jr_est |> 
    tidyr::drop_na(line_name) |>
    dplyr::filter(line_name != "線  名") |> 
    dplyr::select(-station_name) |> 
    dplyr::mutate(line_distance = as.numeric(line_distance))   |> 
    dplyr::mutate(line_distance = round(line_distance, digits = 1)) |> 
    dplyr::mutate(company_name = "東日本旅客鉄道") |> 
    list()
  
  df_jr_kyu_1 <- readxl::read_xlsx("02.raw/density/JR_excel/九州.xlsx", sheet = 1) |> 
    dplyr::select(1, 4, 6)
  df_jr_kyu_2 <- readxl::read_xlsx("02.raw/density/JR_excel/九州.xlsx", sheet = 2) |> 
    dplyr::select(1, 3, 5)
  
  colnames(df_jr_kyu_1) <- c("line_name", "line_distance", "density")
  colnames(df_jr_kyu_2) <- c("line_name", "line_distance", "density")
  
  df_jr_kyu <- bind_rows(df_jr_kyu_1, df_jr_kyu_2)
  
  jr_kyu <- df_jr_kyu |> 
    tidyr::drop_na(line_name, density) |> 
    dplyr::mutate(line_distance = as.numeric(line_distance))   |> 
    dplyr::mutate(line_distance = round(line_distance, digits = 1)) |> 
    dplyr::mutate(company_name = "九州旅客鉄道") |> 
    list()
  
  df_jr_shi <- readxl::read_xlsx("02.raw/density/JR_excel/四国.xlsx", sheet = 2) |> 
    dplyr::select(1, 3, 7)
  colnames(df_jr_shi) <- c("line_name", "line_distance", "density")
  
  jr_shi <- df_jr_shi |> 
    tidyr::drop_na(line_name) |> 
    dplyr::mutate(line_distance = as.numeric(line_distance))   |> 
    dplyr::mutate(line_distance = round(line_distance, digits = 1)) |> 
    dplyr::mutate(company_name = "四国旅客鉄道") |> 
    list()
  
  df_jr_hok <- readxl::read_xlsx("02.raw/density/JR_excel/北海道.xlsx",
                                 col_names = FALSE) |> 
    dplyr::select(2, 26) 
  
  colnames(df_jr_hok) <- c("line_name", "density")
  
  jr_hok <- df_jr_hok |> 
    mutate(line_distance = NA, .before = density) |> 
    tidyr::drop_na(line_name) |> 
    dplyr::mutate(line_distance = as.numeric(line_distance))   |> 
    dplyr::mutate(line_distance = round(line_distance, digits = 1)) |> 
    dplyr::mutate(company_name = "北海道旅客鉄道") |> 
    list()
  
  df_jr_wst <- readxl::read_xlsx("02.raw/density/JR_excel/西日本.xlsx",
                              col_names = FALSE) |> 
    dplyr::select(1, 4, 6) 
  colnames(df_jr_wst) <- c("line_name", "line_distance", "density")
  
  jr_wst <- df_jr_wst |> 
    tidyr::drop_na(line_name) |> 
    dplyr::mutate(line_distance = as.numeric(line_distance))   |> 
    dplyr::mutate(line_distance = round(line_distance, digits = 1)) |> 
    dplyr::mutate(company_name = "西日本旅客鉄道") |> 
    list()
  
  density_df <- c(jr_est, jr_wst, jr_hok, jr_shi, jr_kyu) |> 
    bind_rows() 
  
    return(density_df)
  
}

