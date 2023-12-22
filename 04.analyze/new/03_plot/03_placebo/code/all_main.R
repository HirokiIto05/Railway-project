main <- function(){
  
  df_master <- read_df_csv("master", "master")
  
  list_data <- list.files("04.analyze/new/01_data/data/all_municipalities/") 
  
}

list_main <- purrr::map(list_data, read_plot, df_master)


read_plot <- function(file_path, df_master){
  
  print(file_path)
  
  city_name_t <- stringr::str_sub(file_path, start = 1, end = -5)
  
  # file_name <- paste0(city_name_t, ".rds")
  # 
  # base_plot <- readRDS(here::here('04.analyze','synthetic_control',
  #                                 'add_outcome_predictor','table', file_name))
  
  base_plot <- readRDS(paste0("04.analyze/new/01_data/data/all_municipalities/", file_path))
  
  
  df_plot_based <- base_plot |> 
    grab_synthetic_control() 
  
  df_treatment <- df_master |> 
    dplyr::filter(city_name == city_name_t)
  
  # region_name_t <- unique(df_treatment$prefe)
  
  # title_name <- paste0(city_name_t,':', region_name_t)
  title_name <- paste0(city_name_t)
  
  int_year <- unique(df_treatment$year_end)
  
  output_plot <- ggplot(df_plot_based) +
    geom_line(aes(x = time_unit ,y = real_y,  linetype = "Treatment")) +
    geom_line(aes(x = time_unit, y = synth_y, linetype = "Synthetic"), size = 0.5) +
    scale_linetype_manual(name = "" ,values = c("Treatment" = "solid",
                                                "Synthetic" = "dashed")) +
    scale_color_manual(values = c("Treatment" = "black",
                                  "Synthetic" = "black")) +
    geom_point(aes(x = time_unit, y = real_y), size = 1.1)+
    geom_vline(xintercept = int_year, linetype = "solid",
               size = 0.8, colour = "gray") +
    labs(title = title_name,
         y = "population") +
    theme_bw(base_family = "HiraKakuPro-W3") +
    theme(plot.title = element_text(size = 12),
          axis.text.x = element_text(size = 13),
          axis.text.y = element_text(size = 13),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(size=15)) +
    # ylim(0.6, 1.2) +
    # scale_y_continuous(breaks = seq(0.6,1.4, 0.2)) +
    scale_x_continuous(breaks = c(1995,　int_year)) 
  
  
  output_plot
  
  pdf_name <- paste0(city_name_t, ".png")
  
  # file_name_figure <- paste0(here::here('04.analyze','synthetic_control',
  #                                       'add_outcome_predictor',
  #                                       'figure', pdf_name))
  
  file_name_figure <- paste0(here::here("04.analyze/new/03_plot/03_placebo/figure/all_municipalities/", pdf_name))
  
  file_name_figure
  
  
  ggsave(output_plot, filename = file_name_figure)
  
  
  return(output_plot)
  
}



library(patchwork)


# aggregate_plots ---------------------------------------------------------

plot_nine_agg <- 
  list_main[[1]] + list_main[[2]] + list_main[[3]] +
  list_main[[4]] + list_main[[5]] + list_main[[6]] + 
  list_main[[7]] + list_main[[8]] + list_main[[9]] +
  list_main[[10]] + list_main[[11]] + list_main[[12]] +
  list_main[[13]] + list_main[[14]] + plot_spacer() +
  plot_layout(ncol = 3,
              nrow = 5,
              guides = 'collect') & theme(
                legend.position = 'bottom'
              )

ggsave(plot_nine_agg,
       filename = '04.analyze/new/03_plot/03_placebo/figure/all_municipalities/all.png',
       height = 10,
       width = 8)





