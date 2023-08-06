main <- function(){
  
  
  
  
}


View(plot_based_data)

five_ten_df <- plot_based_data |> 
  dplyr::select(city_name, five_diff, ten_diff) |> 
  dplyr::mutate(five_diff = round(five_diff, digits = 3),
                ten_diff = round(ten_diff, digits = 3))

write.csv(five_ten_df, 
          here::here('04.analyze',
                     'synthetic_control',
                     'add_outcome_predictor',
                     'five_ten_table', 
                     'five_ten_table.csv'),
          fileEncoding = "CP932",
          row.names = FALSE)

balance_all_table <- purrr::map(treatment_name_lists, 
                                create_balance_table) |> 
  dplyr::bind_cols()

write.csv(balance_all_table, here::here('04.analyze',
                               'synthetic_control',
                               'add_outcome_predictor',
                               'balance_table','all_df.csv'),
          fileEncoding = "CP932", row.names = FALSE)


create_balance_table <- function(city_name_t){
  
  print(city_name_t)
  
  file_name <- paste0(city_name_t, ".rds")
  
  synth_based <- readRDS(here::here('04.analyze', 'synthetic_control',
                                    'add_outcome_predictor', 'table', file_name))
  
  table_df <- synth_based |> 
    grab_balance_table() 
  
  colnames(table_df) <- c("Variable", "Treatment",
                          "Synthetic", "Average") 
  
  output_table <- table_df |>
    dplyr::select(-Average) |> 
    dplyr::mutate(Treatment = round(Treatment, 2),
                  Synthetic = round(Synthetic, 2)) |> 
                  # Average = round(Average, 2)) |> 
    # dplyr::mutate(city_name = city_name_t, .before = Variable) |> 
    dplyr::mutate(Variable = str_to_title(Variable)) |> 
    dplyr::mutate(Variable = sub("_20", "", Variable))
  
  
  return(output_table)
  # write.csv(output_table, here::here('04.analyze', 
  #                                'synthetic_control',
  #                                'add_outcome_predictor',
  #                                'balance_table','intrude.csv'),
  #           fileEncoding = "CP932", row.names = FALSE)
  
  
}
