main <- function() {

  
  df_higashi <- read_higashi_dis() 
  
  df_higashi |> 
    group_by(city_name) %>%
    dplyr::filter(n()>1)
  
  df_kumamoto <- read_kumamoto_dis()
  
  df_kumamoto |> 
    group_by(city_name) %>%
    dplyr::filter(n()>1)
  
  save_df_xlsx(df_higashi, "disaster", "higashi")
  save_df_xlsx(df_kumamoto, "disaster", "kumamoto")
  
  
}

read_higashi_dis <- function() {
  
  higashi <- read_excel_allsheets("02.raw/disaster/excel/higashi_nihon.xlsx") |> 
    dplyr::bind_rows() |> 
    dplyr::select(1, 2, 4, 10, 11)
  
  colnames(higashi) <- c("prefecture_name", "city_name", "victims", "destroyed_all", "destroyed_half")
  
  var_num <- c("victims", "destroyed_all", "destroyed_half")
  
  higashi_output <- higashi |> 
    tidyr::drop_na(city_name) |> 
    dplyr::filter(city_name != "市町村",
                  city_name != "小計") |> 
    tidyr::fill(prefecture_name, .direction = 'down') |> 
    dplyr::mutate_all(as.character()) %>% 
    dplyr::mutate(across(everything(), ~str_remove_all(., fixed(".")))) |> 
    dplyr::mutate_at(dplyr::vars(var_num), as.numeric) |> 
    dplyr::filter(
      !(prefecture_name == "北海道" & city_name == "伊達市"),
      !(prefecture_name == "埼玉県" & city_name == "美里町")
    ) 
  
  return(higashi_output)
  
}


read_kumamoto_dis <- function() {
  
  kumamoto <- read_xlsx("02.raw/disaster/excel/kumamoto_kumamoto.xlsx", sheet = 1,
                        col_names = FALSE) |> 
    dplyr::select(1, 3, 12, 14)
  
  colnames(kumamoto) <- c("city_name", "victims", "destroyed_all", "destroyed_half")
  
  var_num <- c("victims", "destroyed_all", "destroyed_half")
  
  kumamoto_output <- kumamoto |> 
    dplyr::mutate_at(dplyr::vars(var_num), as.numeric)
  
  return(kumamoto_output)
  
}


read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X, col_names = FALSE))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

