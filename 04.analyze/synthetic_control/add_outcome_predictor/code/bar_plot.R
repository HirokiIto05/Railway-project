main <- function(){
  
  # treatment_data <- load_csv('complete', 'treatment_data.csv') |> 
  df_master <- read.csv("03.build/master/data/master.csv", 
                             fileEncoding = "CP932")
  
  treatment_name_lists <- df_master |> 
    dplyr::filter(
      treatment == 1
    ) |> 
    dplyr::distinct(city_name) |> 
    dplyr::pull()

  diff_all_data <- purrr::map(treatment_name_lists, read_plot,
                              treatment_data) |> 
    dplyr::bind_rows()
  
  year_end <- 2005
  
  df_okito <- diff_all_data |> 
    dplyr::filter(
      city_name == "常呂郡置戸町",
      time_unit <=  year_end
    ) |>
    dplyr::mutate(
      pe = diff ^ 2,
      pe_inverse = diff_inverse ^ 2
    )
  
  
  # data_okito <- readRDS(here::here('04.analyze','synthetic_control',
  #                                 'add_outcome_predictor',
  #                                 'table', 'except_train',
  #                                 '常呂郡置戸町.rds'))
  
  
  mean(df_okito$pe)
  
  data_okito |> 
    grab_signficance()
  

  
  
}

read_plot <- function(city_name_t, df_master){
  
  print(city_name_t)
  
  file_name <- paste0(city_name_t, ".rds")
  
  base_plot <- readRDS(here::here('04.analyze','synthetic_control',
                                  'add_outcome_predictor',
                                  'table', 'except_train',
                                  file_name))
  
  title_id <- as.character(city_name_t)
  
  treatment_one <- df_master |> 
    dplyr::filter(city_name == city_name_t)
  
  int_year = unique(treatment_one$year_end)
  
  bar_base_data <- base_plot |> 
    grab_synthetic_control() |> 
    dplyr::mutate(diff = real_y - synth_y) |> 
    dplyr::mutate(diff_inverse = synth_y - real_y) |> 
    dplyr::mutate(treatment_year = int_year,
                  .after = time_unit) |> 
    dplyr::mutate(city_name = city_name_t)
  
  return(bar_base_data)
  
}

create_bar_plot <- function(year_i, diff_all_data){
  
  five_year_bar_df <- diff_all_data |>
    mutate(after = time_unit - treatment_year + 1) |>
    dplyr::filter(after == 5) |> 
    dplyr::rename(five_diff = diff)

  # colnames(cross_plot_base)
  
  # diff_all_data <- five_year_bar_df
  
  mean_five <- round(mean(five_year_bar_df$five_diff, na.rm = TRUE), 4)
  
  output_plot_five <- ggplot(five_year_bar_df, aes(x = reorder(city_name, five_diff), 
                                                y = five_diff)) +
    geom_bar(stat = "identity", width = 0.8) +
    geom_hline(yintercept = mean_five, linetype = "longdash") +
    coord_flip(clip = "off") +
    labs(title = "5年後の差分",
         y = "") +
    theme_bw(base_family = "HiraKakuPro-W3") +
    theme(axis.title.y=element_blank(),
          axis.title.x=element_blank(),
          axis.ticks.x=element_blank(), 
          plot.title = element_text(size = 11),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 9)) +
    ylim(c(-0.1, 0.1)) +
    annotate("text", x = 16, y = 0, label = "平均", vjust = -0.5,
             family="HiraKakuPro-W3") 
    
  output_plot_five
  
  file_name_five <- paste0(here::here('04.analyze', 'synthetic_control',
                                      'add_outcome_predictor', 'bar_chart',
                                      "Five_year_later_bar.png"))
  
  ggsave(output_plot_five, filename = file_name_five, width = 4, height = 2.7)
  
  
  ten_year_bar_df <- diff_all_data |>
    mutate(after = time_unit - treatment_year + 1) |>
    dplyr::filter(after == 10) |> 
    dplyr::rename(ten_diff = diff)
  
  diff_all_data_ten <- diff_all_data |> 
    drop_na()
  
  mean_ten <- round(mean(ten_year_bar_df$ten_diff), 4)
  
  # mean_five
  
  output_plot_ten <- ggplot(ten_year_bar_df, aes(x = reorder(city_name, ten_diff), y = ten_diff)) +
    geom_bar(stat = "identity", width = 0.8) +
    geom_hline(yintercept = mean_ten, linetype = "longdash") +
    coord_flip(clip = "off") +
    labs(title = "10年後の差分",
         y = "") +
    theme_bw(base_family = "HiraKakuPro-W3") +
    theme(axis.title.y=element_blank(),
          axis.title.x=element_blank(),
          axis.ticks.x=element_blank(), 
          plot.title = element_text(size = 11),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 9)) +
    ylim(c(-0.1, 0.1)) +
    annotate("text", x = 13, y = mean_ten, label = "平均", vjust = -0.5,
             family="HiraKakuPro-W3") 
  
  output_plot_ten
  
  file_name_ten <- paste0(here::here('04.analyze','synthetic_control',
                                     'add_outcome_predictor', 'bar_chart',
                                     "Ten_year_later_bar.png"))
  
  ggsave(output_plot_ten, filename = file_name_ten, width = 4, height = 2.7 * 0.85)
  
}

output_plot_15 <- output_plot_five + output_plot_ten +
  plot_layout(ncol = 1)

output_plot_15

ggsave(output_plot_15,
       filename = here::here('04.analyze','synthetic_control',
                             'add_outcome_predictor', 'bar_chart',
                             "five_ten_bar.png"),
       width = 4.7, height = 5.5)

theme_gray(base_family = "HiraKakuPro-W3")

five_ten_df <- cross_plot_base |> 
  dplyr::select(city_name, five_diff, ten_diff)

five_ten_df <- five_ten_df |> 
  dplyr::mutate(five_diff = round(five_diff, 4),
                ten_diff = round(ten_diff, 4))

write.csv(five_ten_df, 
          file = here::here('04.analyze','synthetic_control',
                                'add_outcome_predictor',
                                'five_ten_table','five_ten_table.csv'),
          row.names = FALSE, fileEncoding = "CP932")
