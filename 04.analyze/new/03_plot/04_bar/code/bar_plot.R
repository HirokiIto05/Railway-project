# install.packages('Synth')
# install.packages('tidysynth')
# install.packages('kableExtra')
# library('Synth')
# library('tidysynth')
# library('kableExtra')

main <- function(){
  
  # treatment_data <- load_csv('complete', 'treatment_data.csv') |> 
  df_master <- read.csv("03.build/master/data/master.csv", 
                        fileEncoding = "CP932")
  
  list_treatment_files <- df_master |> 
    dplyr::filter(
      treatment == 1
    ) |> 
    arrange(city_id) |> 
    distinct(city_name) |> 
    pull() %>%
    paste0(., ".rds")
  
  df_bar_based <- purrr::map(list_treatment_files, read_plot,
                              df_master) |> 
    dplyr::bind_rows()
  
  create_bar_plot(df_bar_based)
  
  
  df_pop <- read_df_csv("city_adjust", "pop")
  
  aaa <- df_master |> 
    dplyr::group_by(city_id) |> 
    dplyr::mutate(
      cum_social = cumsum(replace_na(social_rate, 0))
    ) |> 
    dplyr::select(
      social, social_rate, cum_social, year, everything()
    ) |> 
    dplyr::filter(
      city_name == "輪島市"
    )
  
}



read_plot <- function(file_path, df_master){
  
  print(file_path)
  
  city_name_t <- stringr::str_sub(file_path, start = 1, end = -5)
  
  base_plot <- readRDS(paste0("04.analyze/new/01_data/data/pre_year/", file_path))
  
  title_id <- as.character(city_name_t)
  
  treatment_one <- df_master |> 
    dplyr::filter(city_name == city_name_t)
  
  int_year = unique(treatment_one$year_end)
  
  bar_base_data <- base_plot |> 
    grab_synthetic_control() |> 
    dplyr::mutate(diff = real_y - synth_y,
                  diff_inverse = synth_y - real_y) |> 
    dplyr::mutate(year_end = int_year,
                  .after = time_unit) |> 
    dplyr::mutate(city_name = city_name_t)
  
  return(bar_base_data)
  
}

create_bar_plot <- function(df_bar_based){
  
  # five_year_bar_df <- df_bar_based |>
  #   mutate(after = time_unit - year_end) |>
  #   dplyr::filter(after == 5) |> 
  #   dplyr::rename(five_diff = diff)
  # 
  # mean_five <- round(mean(five_year_bar_df$five_diff, na.rm = TRUE), 4)
  # 
  # output_plot_five <- ggplot(five_year_bar_df, aes(x = reorder(city_name, five_diff),
  # # ggplot(five_year_bar_df, aes(x = reorder(city_name, five_diff), 
  #                              y = five_diff)) +
  #   geom_bar(stat = "identity", width = 0.8) +
  #   geom_hline(yintercept = mean_five, linetype = "longdash") +
  #   coord_flip(clip = "off") +
  #   labs(title = "5年後の差分",
  #        y = "") +
  #   theme_bw(base_family = "HiraKakuPro-W3") +
  #   theme(axis.title.y=element_blank(),
  #         axis.title.x=element_blank(),
  #         axis.ticks.x=element_blank(), 
  #         plot.title = element_text(size = 11),
  #         axis.text.x = element_text(size = 10),
  #         axis.text.y = element_text(size = 9)) +
  #   scale_y_continuous(
  #     limits = c(-6.5, 9.5),
  #     breaks = c(seq(-6, 8, 2))
  #   )
  #   # ylim(c(-0.1, 0.1)) +
  #   # annotate("text", x = 15, y = 0, 
  #   #          label = paste0("平均","(", round(mean_five, 2),")"),
  #   #          vjust = -5.5,
  #   #          hjust = 0.5,
  #   #          family="HiraKakuPro-W3") 
  #   # scale_y_continuous(
  #   #   limits = c(-4, 10),
  #   #   breaks = c(seq(-4, 4, 2))
  #   # )
  # 
  # output_plot_five
  # 
  # ggsave(output_plot_five, filename = "04.analyze/new/03_plot/04_bar/figure/five.png", 
  #        width = 8, height = 6)
  # 
  # 
  # ten_year_bar_df <- df_bar_based |>
  #   mutate(after = time_unit - year_end) |>
  #   dplyr::filter(after == 10) |> 
  #   dplyr::rename(ten_diff = diff)
  # 
  # df_bar_based_ten <- df_bar_based |> 
  #   drop_na()
  # 
  # mean_ten <- round(mean(ten_year_bar_df$ten_diff), 4)
  # 
  # # mean_five
  # 
  # output_plot_ten <- ggplot(ten_year_bar_df, aes(x = reorder(city_name, ten_diff), y = ten_diff)) +
  # # ggplot(ten_year_bar_df, aes(x = reorder(city_name, ten_diff), y = ten_diff)) +
  #   geom_bar(stat = "identity", width = 0.8) +
  #   geom_hline(yintercept = mean_ten, linetype = "longdash") +
  #   coord_flip(clip = "off") +
  #   labs(title = "10年後の差分",
  #        y = "") +
  #   theme_bw(base_family = "HiraKakuPro-W3") +
  #   theme(axis.title.y=element_blank(),
  #         axis.title.x=element_blank(),
  #         axis.ticks.x=element_blank(), 
  #         plot.title = element_text(size = 11),
  #         axis.text.x = element_text(size = 10),
  #         axis.text.y = element_text(size = 9)) +
  #   scale_y_continuous(
  #     limits = c(-6.5, 9.5),
  #     breaks = c(seq(-6, 8, 2))
  #   )
  #   # ylim(c(-0.1, 0.1)) +
  #   # annotate("text", x = 14.2, y = mean_ten, 
  #   #          label = paste0("平均","(", round(mean_ten, 2),")"),
  #   #          vjust = -2,
  #   #          hjust = 0.3,
  #   #          family="HiraKakuPro-W3") 
  #   # annotate("text", x = 0, y = mean_ten, label = round(mean_ten, 4), 
  #   #          vjust = 1.5,
  #   #          size = 3,
  #   #          family="HiraKakuPro-W3") 
  #   # # scale_y_continuous(
  #   #   limits = c(-5,5),
  #   #   breaks = c(seq(-5, 5, 2.5))
  #   # )
  # 
  # output_plot_ten
  # 
  # ggsave(output_plot_ten, filename = "04.analyze/new/03_plot/04_bar/figure/ten.png", width = 8, height = 6)
  
  df_mean_bar <- df_bar_based |>
    mutate(after = time_unit - year_end) |> 
    dplyr::filter(after >= 1) |> 
    dplyr::rename(mean_diff = diff) |> 
    summarise(
      diff_mean = mean(mean_diff, na.rm = TRUE),
      .by = c(city_name)
    ) |> 
    distinct()
  
  # df_mean_bar <- df_bar_based |> 
  #   drop_na()
  
  mean_mean <- round(mean(df_mean_bar$diff_mean), 4)
  
  # mean_five
  
  output_plot_mean <- ggplot(df_mean_bar, aes(x = reorder(city_name, diff_mean), y = diff_mean)) +
    # ggplot(ten_year_bar_df, aes(x = reorder(city_name, ten_diff), y = ten_diff)) +
    geom_bar(stat = "identity", width = 0.8) +
    geom_hline(yintercept = mean_mean, linetype = "longdash") +
    coord_flip(clip = "off") +
    labs(title = "平均",
         y = "") +
    theme_bw(base_family = "HiraKakuPro-W3") +
    theme(axis.title.y=element_blank(),
          axis.title.x=element_blank(),
          axis.ticks.x=element_blank(), 
          plot.title = element_text(size = 14),
          # axis.text.x = element_text(size = 10),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 10)) +
    scale_y_continuous(
      limits = c(-6.5, 9),
      breaks = c(seq(-6, 8, 2))
    )
  
  output_plot_mean
  
  ggsave(output_plot_mean,
         filename = "04.analyze/new/03_plot/04_bar/figure/pre_year.png",
         width = 8,
         height = 6)
  
}


joint_bar_plots <- function() {
  
  output_bar <- output_plot_five + output_plot_ten + output_plot_mean + plot_spacer() +
    plot_layout(ncol = 2,
                nrow = 2) &
    theme(axis.title.y=element_blank(),
          axis.title.x=element_blank(),
          axis.ticks.x=element_blank(), 
          plot.title = element_text(size = 11),
          # axis.text.x = element_text(size = 10),
          axis.text.x = element_text(size = 10),
          # axis.text.y = element_text(size = 9),
          axis.text.y = element_blank())
  
  output_bar
  
  ggsave(output_bar, 
         filename = "04.analyze/new/03_plot/04_bar/figure/five_ten.png",
         width = 8,
         height = 7)
  
}
 

create_average_bar_chart <- function(df_bar_based) {
  
  df_mean_bar <- df_bar_based |>
    mutate(after = time_unit - year_end) |> 
    dplyr::filter(after >= 1) |> 
    dplyr::rename(mean_diff = diff) |> 
    summarise(
      diff_mean = mean(mean_diff, na.rm = TRUE),
      .by = c(city_name)
    ) |> 
    distinct()
  
  # df_mean_bar <- df_bar_based |> 
  #   drop_na()
  
  mean_num <- round(mean(df_mean_bar$diff_mean), 4)
  
  # mean_five
  
  output_plot_mean <- ggplot(df_mean_bar, aes(x = reorder(city_name, diff_mean), y = diff_mean)) +
    # ggplot(ten_year_bar_df, aes(x = reorder(city_name, ten_diff), y = ten_diff)) +
    geom_bar(stat = "identity", width = 0.8) +
    geom_hline(yintercept = mean_num, linetype = "longdash") +
    coord_flip(clip = "off") +
    labs(title = "平均",
         y = "") +
    theme_bw(base_family = "HiraKakuPro-W3") +
    theme(axis.title.y=element_blank(),
          axis.title.x=element_blank(),
          axis.ticks.x=element_blank(), 
          plot.title = element_text(size = 14),
          # axis.text.x = element_text(size = 10),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 10)) +
    scale_y_continuous(
      limits = c(-6.5, 9),
      breaks = c(seq(-6, 8, 2))
    )
  
  output_plot_mean
  # 
  ggsave(output_plot_mean,
         filename = "04.analyze/new/03_plot/04_bar/figure/pre_year.png",
         width = 8,
         height = 6)
  
}


# joint_bar_plots <- function() {
#   
#   output_bar <- output_plot_five + output_plot_ten + output_plot_mean + plot_spacer() +
#     plot_layout(ncol = 2,
#                 nrow = 2) &
#     theme(axis.title.y=element_blank(),
#           axis.title.x=element_blank(),
#           axis.ticks.x=element_blank(), 
#           plot.title = element_text(size = 11),
#           # axis.text.x = element_text(size = 10),
#           axis.text.x = element_text(size = 10),
#           # axis.text.y = element_text(size = 9),
#           axis.text.y = element_blank())
#   
#   output_bar
#   
#   ggsave(output_bar, 
#          filename = "04.analyze/new/03_plot/04_bar/figure/five_ten.png",
#          width = 8,
#          height = 7)
#   
# }
# 
#   
# }
