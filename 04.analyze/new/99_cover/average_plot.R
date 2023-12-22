df_master <- read_df_csv("master", "master") |> 
  dplyr::group_by(city_id) |>
  dplyr::mutate(
    cum_social = cumsum(replace_na(social_rate, 0))
  ) |> 
  ungroup()

df_treatment <- df_master |> 
  dplyr::filter(city_name == city_name_t) |> 
  dplyr::select(
    year, cum_social
  )

df_control <- df_master |> 
  dplyr::filter(
    treatment == 0
  ) |> 
  dplyr::summarise(
    mean_control = mean(cum_social),
    .by = year
  )

df_plot_based <- df_treatment |> 
  dplyr::left_join(df_control) 

colnames(df_plot_based) <- c('year', 'treatment', 'control')


# region_name_t <- unique(df_treatment$prefe)

# title_name <- paste0(city_name_t,':', region_name_t)
title_name <- paste0(city_name_t)

int_year <- df_master |> 
  dplyr::filter(
    city_name == title_name
  ) |> 
  dplyr::distinct(year_end) |> 
  pull()

output_plot <- ggplot(df_plot_based) +
  geom_line(aes(x = year, y = treatment,  linetype = "Treatment")) +
  geom_line(aes(x = year, y = control, linetype = "Control"), size = 0.5) +
  scale_linetype_manual(name = "" ,values = c("Treatment" = "solid",
                                              "Control" = "dashed")) +
  scale_color_manual(values = c("Treatment" = "black",
                                "Control" = "black")) +
  geom_point(aes(x = year, y = treatment), size = 1.1)+
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
  scale_y_continuous(breaks = seq(-10, 1, 2),
                     limits = c(-11, 1)) +
  scale_x_continuous(breaks = c(1995,　int_year)) 


output_plot

pdf_name <- paste0(city_name_t,'_Control', ".png")

# file_name_figure <- paste0(here::here('04.analyze','synthetic_control',
#                                       'add_outcome_predictor',
#                                       'figure', pdf_name))

file_name_figure <- paste0(here::here("04.analyze/new/03_plot/01_main_plot/figure/pre_year/", pdf_name))

ggsave(output_plot, filename = file_name_figure)
