


print(city_name_t)

file_name <- paste0(city_name_t, ".rds")

base_plot <- readRDS(here::here('04.analyze','synthetic_control',
                                'add_outcome_predictor','table', file_name))

title_id <- as.character(city_name_t)

diff_plot_df <- base_plot %>% 
  tidysynth::grab_synthetic_control() %>%
  dplyr::mutate(diff = real_y - synth_y) %>% 
  dplyr::mutate(city_name = city_name_t)

diff_plot <- ggplot(diff_plot_df) +
  geom_line(aes(x = time_unit, y = diff))+
  labs(title = title_id,
       y = "処置効果") +
  geom_vline(xintercept = int_year - 1, linetype = "solid", size = 0.8,
             colour = "gray") +
  theme_bw(base_family = "HiraKakuPro-W3") +
  theme(plot.title = element_text(size = 17),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 15),
        legend.position = "bottom",
        legend.text = element_text(size=18)) +
  # scale_y_continuous(breaks = seq(0.6,1.4, 0.2)) +
  scale_x_continuous(breaks = c(1995,int_year - 1)) 

diff_plot


file_name_figure <- paste0(here::here('04.analyze','synthetic_control',
                                      'add_outcome_predictor',
                                      'diff_plot', 'diff_plot.png'))

ggsave(diff_plot, filename = file_name_figure)

