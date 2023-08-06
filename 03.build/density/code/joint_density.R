main <- function() {
  
  df_density_both <- joint_df_jr_local()
  
  df_c <- read_df_csv("geometry_base", "df_control_city") 
  
  df_jr <- df_density_both |> 
    dplyr::filter(jr == 1)
  df_local <- df_density_both |> 
    dplyr::filter(jr == 0) |> 
    dplyr::select(-line_name) 
  
  df_c_jr <- df_c |> 
    dplyr::left_join(df_jr) |> 
    dplyr::filter(jr == 1)
  
  df_c_local <- df_c |> 
    dplyr::filter(company_name != "北海道旅客鉄道",
                  company_name != "東日本旅客鉄道",
                  company_name != "東海旅客鉄道",
                  company_name != "西日本旅客鉄道",
                  company_name != "九州旅客鉄道",
                  company_name != "四国旅客鉄道") |> 
    dplyr::left_join(df_local, by = "company_name") 
  
  df_density_all <- bind_rows(df_c_local, df_c_jr)
  
  save_df_csv(df_density_all, "density", "master_density")
  
}


joint_df_jr_local <- function() {
  
  df_local <- read_df_csv("density", "local") |> 
    mutate(line_name = NA) |> 
    dplyr::select(company_name, line_name, density) |> 
    dplyr::mutate(jr = 0) |> 
    dplyr::filter(company_name != "北海道旅客鉄道",
                  company_name != "東日本旅客鉄道",
                  company_name != "東海旅客鉄道",
                  company_name != "西日本旅客鉄道",
                  company_name != "九州旅客鉄道",
                  company_name != "四国旅客鉄道")
  
  
  df_jr <- read_df_csv("density", "jr") |> 
    dplyr::select(company_name, line_name, density) |> 
    dplyr::mutate(jr = 1)
  
  df_density_both <- bind_rows(df_local, df_jr)
  
  return(df_density_both)
  
}
