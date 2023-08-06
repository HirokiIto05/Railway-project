main <- function() {
  
  density_df <- read_data() 

  density_adjust_df <- adjust_line_name(density_df) |> 
    dplyr::rename(company_name = railway_name) |> 
    tidyr::drop_na(company_name) |> 
    # 以下はデータによって異なる路線名を修正している。
    dplyr::mutate(company_name = str_replace_all(company_name, "ＩＧＲいわて銀河鉄道", "アイジーアールいわて銀河鉄道")) |> 
    dplyr::mutate(company_name = str_replace_all(company_name, "万葉線", "万葉")) |> 
    dplyr::mutate(company_name = str_replace_all(company_name, "小湊鉄道", "小湊鐵道")) |> 
    dplyr::mutate(company_name = str_replace_all(company_name, "ＩＲいしかわ鉄道", "IRいしかわ鉄道")) |> 
    dplyr::mutate(company_name = str_replace_all(company_name, "ＷＩＬＬＥＲ　ＴＲＡＩＮＳ", "WILLER　TRAINS"))

  save_df_csv(density_adjust_df, 'density', 'local')  
  
}

read_data <- function() {
  
  density_df <- readxl::read_xlsx(
    here::here('02.raw','density','others','2018.xlsx'),
    col_names = FALSE) |> 
    dplyr::select(1,2,13)
  
  colnames(density_df) <- c("company_name",
                            "line_name",
                            "density")
  
  return(density_df)
}


adjust_line_name <- function(density_df) {
  
  output_df <- density_df |> 
    dplyr::mutate(railway_name = if_else(is.na(company_name),
                                         line_name,
                                         company_name)) |> 
    dplyr::select(railway_name, density) |> 
    dplyr::mutate(year = 2018,
                  density = as.numeric(density)) |> 
    dplyr::relocate(year, .before = density) 
  
  return(output_df)
}

