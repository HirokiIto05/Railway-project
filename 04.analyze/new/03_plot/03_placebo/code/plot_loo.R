library(patchwork)

list_filepath <- df_master |> 
  dplyr::filter(
    treatment == 1
  ) |> 
  dplyr::arrange(city_id) |> 
  dplyr::distinct(city_id) |>
  dplyr::pull()

file_path = "1361"

create_loo_plot <- function(file_path, df_master) {
  
  df_based <- read.csv(here::here("04.analyze/new/01_data/data/leave_one_out/output", paste0(file_path, '.csv')),
                       fileEncoding = "cp932")
  
  id_treatment <- stringr::str_remove(file_path, pattern = ".csv")
  
  name_treatment <- df_master |> 
    dplyr::filter(
      city_id == id_treatment
    ) |> 
    dplyr::distinct(city_name) |> 
    pull()
  
  year_end_i <- df_master |>
    dplyr::filter(
      city_id == id_treatment
    ) |> 
    dplyr::distinct(year_end) |> 
    pull()
  
  plot_output <- ggplot() +
    # geom_point(data = df_input |> dplyr::filter(placebo == "1"), aes(x = time_unit, y = diff, group = city_id), color = "gray") +
    geom_line(data = df_based |> 
                dplyr::filter(placebo == "1"), 
              aes(x = time_unit, y = diff, group = city_id), 
              color = "gray", 
              linewidth = 0.4) +
    # geom_point(data = df_input |> dplyr::filter(placebo == "0"), aes(x = time_unit, y = diff), color = "black") + 
    geom_vline(xintercept = year_end_i, color = 'lightgray') +
    geom_line(data = df_based |> 
                dplyr::filter(placebo == "0"), 
              aes(x = time_unit, y = diff), 
              color = "black", 
              linewidth = 0.6) +
    theme_bw(base_family = "HiraKakuPro-W3") +
    theme(
      axis.title.y = element_blank(),
      axis.title.x = element_blank()
    ) +
    labs(
      title = name_treatment
    ) +
    scale_x_continuous(breaks = c(2000, 2010, year_end_i)) 
  
  plot_output <- ggplot() +
    # geom_point(data = df_input |> dplyr::filter(placebo == "1"), aes(x = time_unit, y = diff, group = city_id), color = "gray") +
    geom_line(data = df_based |> 
                dplyr::filter(placebo == "1"), 
              aes(x = time_unit, y = synth_y, group = city_id), 
              color = "gray", 
              linewidth = 0.4) +
    # geom_point(data = df_input |> dplyr::filter(placebo == "0"), aes(x = time_unit, y = diff), color = "black") + 
    geom_vline(xintercept = year_end_i, color = 'lightgray') +
    geom_line(data = df_based |> 
                dplyr::filter(placebo == "0"), 
              aes(x = time_unit, y = real_y), 
              color = "black", 
              linewidth = 0.6) +
    geom_line(data = df_based |> 
                dplyr::filter(placebo == "0"), 
              aes(x = time_unit, y = synth_y, group = city_id), 
              color = "black",
              linetype = 'dashed',
              linewidth = 0.6) +
    theme_bw(base_family = "HiraKakuPro-W3") +
    theme(
      axis.title.y = element_blank(),
      axis.title.x = element_blank()
    ) +
    labs(
      title = name_treatment
    ) +
    scale_x_continuous(breaks = c(2000, 2010, year_end_i)) 
  
  plot_output
  # save_path <- paste0("04.analyze/new/03_plot/03_placebo/figure/leave_one_out/", id_treatment, ".png")
  
  # ggsave(plot_output, filename = save_path)
  
  return(plot_output)
  
}

list_loo <- purrr::map(list_filepath, create_loo_plot, df_master)


plot_loo <- 
  list_loo[[1]] + list_loo[[2]] + list_loo[[3]] +
  list_loo[[4]] + list_loo[[5]] + list_loo[[6]] + 
  list_loo[[7]] + list_loo[[8]] + list_loo[[9]] +
  list_loo[[10]] + list_loo[[11]] + list_loo[[12]] +
  list_loo[[13]] + list_loo[[14]] + list_loo[[15]] +
  list_loo[[16]] + plot_spacer() + plot_spacer() +
  plot_layout(ncol = 3,
              nrow = 6,
              guides = 'collect') & theme(
                legend.position = 'bottom'
              )

plot_loo


ggsave(plot_loo,
       filename = '04.analyze/new/03_plot/03_placebo/figure/leave_one_out/all.png',
       height = 12,
       width = 8)


