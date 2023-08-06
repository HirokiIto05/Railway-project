main <- function(){
  
  year_list <- seq(2005, 2019)
  
  fci_data <- purrr::map(year_list, read_power) |> 
    dplyr::bind_rows()
  
  write.csv(fci_data, 
            here::here('03.build',
                       'power', 'data', 
                       'fci_data.csv'),
            fileEncoding = "CP932", 
            row.names = FALSE)
  
}




read_power <- function(year_n){
  
  if(year_n <= 2007){
    file_name <- here::here('02.raw',
                            'power',
                            paste0(year_n,'.xls'))
    based_data <- readxl::read_xls(file_name, 
                                   col_names = FALSE,
                                   skip = 2) |> 
      dplyr::select(1,2,6)
    
  }else if(year_n <= 2010){
    file_name <- here::here('02.raw',
                            'power',
                            paste0(year_n,'.xls'))
    based_data <- readxl::read_xls(file_name, 
                                   col_names = FALSE,
                                   skip = 2) |>
      dplyr::select(1,2,3)
  
  }
  else if(year_n <= 2015){
    file_name <- here::here('02.raw',
                            'power',
                            paste0(year_n,'.xls'))
    
    based_data <- readxl::read_xls(file_name, 
                                   col_names = FALSE,
                                   skip = 2) |>
      dplyr::select(2,3,4)

  }else if(year_n <= 2019){
    file_name <- here::here('02.raw',
                            'power',
                            paste0(year_n,'.xlsx'))
    based_data <- readxl::read_xlsx(file_name, 
                                    col_names = FALSE,
                                    skip = 2) |>
      dplyr::select(2,3,4) 
  
  }
  
  colnames(based_data) <- c("region_name", "city_name", "FCI")
  
  output_data <- based_data |> 
    dplyr::mutate(year = year_n, .after = city_name) |> 
    dplyr::mutate(FCI = as.numeric(FCI))
  
  return(output_data)
  
}
