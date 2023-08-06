main <- function(){
  
  year_list <- seq(2000,2010,by= 10)
  
  all_transport_data <- purrr::map(year_list, read_transport_csv) |> 
    dplyr::bind_rows()
  
  all_transport_data <- all_transport_data |> 
    dplyr::mutate(train_only_pop = as.numeric(sub(",", "", train_only_pop)),
                  train_bus_pop = as.numeric(sub(",", "", train_bus_pop)),
                  train_commutebus_pop = as.numeric(sub(",", "", train_commutebus_pop)),
                  train_car_pop = as.numeric(sub(",", "", train_car_pop)),
                  train_motorcycle_pop = as.numeric(sub(",", "", train_motorcycle_pop)),
                  train_bicycle_pop = as.numeric(sub(",", "", train_bicycle_pop))
                  ) |> 
    dplyr::relocate(year, .after = city_name)

  save_df_csv(all_transport_data, "covariates", "transport")
  
}

read_transport_csv <- function(year_n){
  
  file_name <- paste0(year_n, ".csv")
  
  transport_data <- read.csv(here::here('02.raw', 'covariates', 'transport', file_name),
                             fileEncoding = "CP932") 
  
  if(year_n == 2000){
    
    output_data <- transport_data |> 
      dplyr::select(2, 5, 6, 8) |> 
      dplyr::mutate(year = year_n)
    
    colnames(output_data) <- c("transport_type", "city_id", "city_name", "total", "year")
    
    transport_type_list <- c("２　鉄道・電車", "１０　鉄道・電車及び乗合バス",
                             "１１　鉄道・電車及び勤め先・学校のバス",
                             "１２　鉄道・電車及び自家用車",
                             "１３　鉄道・電車及びオートバイ",
                             "１４　鉄道・電車及び自転車")
    
    train_only <- output_data |> 
      dplyr::filter(transport_type == "２　鉄道・電車") |> 
      dplyr::rename(train_only_pop = total) |> 
      dplyr::select(-transport_type)
    
    train_and_bus <-  output_data |> 
      dplyr::filter(transport_type == "１０　鉄道・電車及び乗合バス") |> 
      dplyr::rename(train_bus_pop = total) |> 
      dplyr::select(-transport_type)
    
    train_and_commutebus <-  output_data |> 
      dplyr::filter(transport_type == "１１　鉄道・電車及び勤め先・学校のバス") |> 
      dplyr::rename(train_commutebus_pop = total) |> 
      dplyr::select(-transport_type)
    
    train_and_car <-  output_data |> 
      dplyr::filter(transport_type == "１２　鉄道・電車及び自家用車") |> 
      dplyr::rename(train_car_pop = total) |> 
      dplyr::select(-transport_type)
    
    train_and_motorcycle <-  output_data |> 
      dplyr::filter(transport_type == "１３　鉄道・電車及びオートバイ") |> 
      dplyr::rename(train_motorcycle_pop = total) |> 
      dplyr::select(-transport_type)
    
    train_and_bicycle <-  output_data |> 
      dplyr::filter(transport_type == "１４　鉄道・電車及び自転車") |> 
      dplyr::rename(train_bicycle_pop = total) |> 
      dplyr::select(-transport_type)
    
    all_type_data <- train_only |> 
      dplyr::left_join(train_and_bus) |> 
      dplyr::left_join(train_and_commutebus) |>
      dplyr::left_join(train_and_car) |>
      dplyr::left_join(train_and_motorcycle) |>
      dplyr::left_join(train_and_bicycle) 
    
  } else if(year_n == 2010){
    
    output_data <- transport_data |> 
      dplyr::select(3, 4, 8, 10) |> 
      dplyr::mutate(year = year_n)
    
    colnames(output_data) <- c("city_id", "city_name", "transport_type", "total", "year")
    
    transport_type_list <- c("２　鉄道・電車", "１０　鉄道・電車及び乗合バス",
                             "１１　鉄道・電車及び勤め先・学校のバス",
                             "１２　鉄道・電車及び自家用車",
                             "１３　鉄道・電車及びオートバイ",
                             "１４　鉄道・電車及び自転車")
    
    train_only <- output_data |> 
      dplyr::filter(transport_type == "鉄道・電車") |> 
      dplyr::rename(train_only_pop = total) |> 
      dplyr::select(-transport_type)
    
    train_and_bus <-  output_data |> 
      dplyr::filter(transport_type == "鉄道・電車及び乗合バス") |> 
      dplyr::rename(train_bus_pop = total) |> 
      dplyr::select(-transport_type)
    
    train_and_commutebus <-  output_data |> 
      dplyr::filter(transport_type == "鉄道・電車及び勤め先・学校のバス") |> 
      dplyr::rename(train_commutebus_pop = total) |> 
      dplyr::select(-transport_type)
    
    train_and_car <-  output_data |> 
      dplyr::filter(transport_type == "鉄道・電車及び自家用車") |> 
      dplyr::rename(train_car_pop = total) |> 
      dplyr::select(-transport_type)
    
    train_and_motorcycle <-  output_data |> 
      dplyr::filter(transport_type == "鉄道・電車及びオートバイ") |> 
      dplyr::rename(train_motorcycle_pop = total) |> 
      dplyr::select(-transport_type)
    
    train_and_bicycle <-  output_data |> 
      dplyr::filter(transport_type == "鉄道・電車及び自転車") |> 
      dplyr::rename(train_bicycle_pop = total) |> 
      dplyr::select(-transport_type)
    
    all_type_data <- train_only |> 
      dplyr::left_join(train_and_bus) |> 
      dplyr::left_join(train_and_commutebus) |>
      dplyr::left_join(train_and_car) |>
      dplyr::left_join(train_and_motorcycle) |>
      dplyr::left_join(train_and_bicycle) 
  }
  
  all_type_data <- all_type_data |> 
    dplyr::mutate(city_id = as.character(city_id)) 
  
  return(all_type_data)
  
}

