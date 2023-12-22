df_mean_bar <- df_bar_based |>
  mutate(after = time_unit - year_end) |> 
  dplyr::filter(after >= 1) |> 
  dplyr::rename(mean_diff = diff) |> 
  summarise(
    diff_mean = mean(mean_diff, na.rm = TRUE),
    .by = c(city_name)
  ) |> 
  distinct()

plot_output <- ggplot(data = df_mean_bar, aes(x = '0', y = diff_mean)) +
  geom_boxplot(width = 0.5) +
  geom_point(data = df_mean_bar, aes(x = '0', y = diff_mean)) +
  coord_flip() +
  theme_bw(base_family = 'HiraKakuPro-W3') +
  labs(
    y = '平均処置効果',
    title = '全16自治体の処置効果'
  ) +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  scale_y_continuous(
    breaks = seq(-2, 6, 2)
  ) 

ggsave(plot_output, filename = '04.analyze/new/99_cover/31.png')

mean_mean <- round(mean(df_mean_bar$diff_mean), 4)

output_plot_mean <- ggplot(df_mean_bar, aes(x = reorder(city_name, diff_mean), y = diff_mean, n=seq(1,16))) +
  # ggplot(ten_year_bar_df, aes(x = reorder(city_name, ten_diff), y = ten_diff)) +
  geom_bar(stat = "identity", width = 0.8) +
  geom_hline(yintercept = mean_mean, linetype = "longdash") +
  geom_hline(yintercept = 0, linetype = "solid") +
  annotate("text", x = -0.1 , y = mean_mean,
           label = round(mean_mean, 1), family = "HiraKakuProN-W3",
           size = 6) +
  coord_cartesian(xlim = c(1,16), clip = "off") +
  labs(title = "16自治体の平均処置効果",
       y = "") +
  theme_bw(base_family = "HiraKakuPro-W3") +
  theme(
    axis.title.y=element_blank(),
    axis.text.x=element_blank(),
    axis.title.x=element_blank(),
    axis.ticks.x=element_blank(), 
    plot.title = element_text(size = 20),
    # axis.text.x = element_text(size = 10),
    # axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 19)
    ) +
  scale_y_continuous(
    # limits = c(-6.5, 6.5),
    breaks = c(seq(-2, 6, 2))
  )

output_plot_mean

ggsave(output_plot_mean, filename = '04.analyze/new/99_cover/31.png',
       width = 7, height = 4)


  

