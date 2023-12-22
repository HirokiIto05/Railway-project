main <- function() {
  
  df_master <- read_df_csv("master", "master") 
  
  df_placebo <- read.csv(here::here("04.analyze/new/03_plot/03_placebo/data/placebo_table_pre_year.csv"),
                         fileEncoding = "CP932")
  
  df_placebo <- df_placebo |> 
    dplyr::mutate(after = time_unit - year_end)
  
  list_treatment_files <- df_master |> 
    dplyr::filter(
      treatment == 1
    ) |> 
    arrange(city_id) |> 
    distinct(city_name) |> 
    pull() %>%
    paste0(., ".rds")
  
  list_placebo_plots <- purrr::map(list_treatment_files, create_placebo_plot, df_placebo) 
  
  plot_nine_placebo <- 
    list_placebo_plots[[1]] + list_placebo_plots[[2]] + list_placebo_plots[[3]] +
    list_placebo_plots[[4]] + list_placebo_plots[[5]] + list_placebo_plots[[6]] + 
    list_placebo_plots[[7]] + list_placebo_plots[[8]] + list_placebo_plots[[9]] +
    list_placebo_plots[[10]] + list_placebo_plots[[11]] + list_placebo_plots[[12]] +
    list_placebo_plots[[13]] + list_placebo_plots[[14]] + list_placebo_plots[[15]] +
    list_placebo_plots[[16]] + plot_spacer() + plot_spacer() +
    plot_layout(ncol = 3,
                nrow = 6,
                guides = 'collect') & theme(
                  legend.position = 'bottom',
                  legend.title = element_blank()
                ) &
    scale_color_manual(labels = c("Synthetic", "Treatment"),
                       values = c('gray', 'black'))
  
  plot_nine_placebo
  
  ggsave(plot_nine_placebo,
         filename = '04.analyze/new/03_plot/03_placebo/figure/pre_year/placebo_all.png',
         height = 12,
         width = 8)
  
  
}

create_placebo_plot <- function(file_path, df_placebo){
  
  print(file_path)
  
  city_name_t <- stringr::str_sub(file_path, start = 1, end = -5)
  base_plot <- readRDS(paste0("04.analyze/new/01_data/data/pre_year/", file_path))
  
  based_data <- df_placebo |> 
    dplyr::filter(which_placebo == city_name_t) |> 
    dplyr::arrange(-city_id) |> 
    dplyr::mutate(city_id = as.character(city_id),
                  .placebo = as.character(.placebo)) 
  
  title_name <- unique(based_data$city_name)
  
  treatment_unit <- based_data |> 
    dplyr::filter(.placebo == 0) 
  
  control_unit <- based_data |> 
    dplyr::filter(.placebo == 1) 
  
  
  int_year <- unique(treatment_unit$year_end) 
  
  # control_unit |>
  #   mutate(
  #     .placebo = if_else(.placebo == 1,
  #                        "Treatment",
  #                        "Synthetic")
  #   )
  # 
  
  output_plot <- ggplot() +
    geom_line(control_unit, mapping = aes(x = time_unit, 
                                          y = diff,
                                          group = city_id,
                                          colour = .placebo)) +
    
    theme_bw(base_family = "HiraKakuPro-W3") +
    theme(legend.position = "none") +
    scale_color_manual(values = c("gray","black")) +
    geom_line(treatment_unit,mapping = aes(x = time_unit, 
                                           y = diff,
                                           group = city_id,
                                           colour = "black"),
              size = 1) +
    theme_bw(base_family = "HiraKakuPro-W3") +
    geom_vline(xintercept = int_year, linetype = "dashed", size = 0.8) +
    theme(plot.title = element_text(size = 13),
          axis.text.x = element_text(size = 11),
          # angle = 45,
          # hjust = 0.5,
          # vjust = 0.5),
          axis.text.y = element_text(size = 11),
          axis.title.x = element_blank(),
          # axis.title.y = element_blank(),
          legend.position = "none",
          legend.text = element_text(size=11)) +
    labs(title = title_name,
         y = "処置効果",
         x = "Year") +
    scale_size_manual(values = c(1, 0.5)) +
    scale_x_continuous(breaks = c(1995,int_year)) 
  
  output_plot
  
  file_name <- paste0(city_name_t, ".png")
  # file_name <- "example_placebo.png"
  
  ggsave(output_plot, file = here::here("04.analyze/new/03_plot/03_placebo/figure/pre_year/", file_name))
  
  return(output_plot)
}


