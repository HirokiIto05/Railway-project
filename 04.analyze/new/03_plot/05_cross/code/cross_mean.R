library(ggpmisc)

# read_data ---------------------------------------------------------------
df_master <- read_df_csv("master", "master")

df_plot_based <- read.csv("03.build/cross/data/cross_based_pre_year.csv") |> 
  dplyr::rename(
    diff = diff_mean
  )
  # dplyr::filter(
  #   category == "diff_mean"
  # ) |> 
  # mutate(
  #   category = case_when(
  #     category == "diff_mean" ~ "平均",
  #     category == "diff_five" ~ "5年後",
  #     category == "diff_ten" ~ "10年後"
  #   )
  # )
# category = as.factor(category),
# category = fct_relevel(category, 
#                        "平均",
#                        "5年後",
#                        "10年後"))


df_plot_based <- df_plot_based |> 
  select(
    -category, -diff
  ) |> 
  left_join(df_test) |> 
  distinct()


# plots -------------------------------------------------------------------

plot_area <- create_plots(df_plot_based, area, "面積", base_setting) 
plot_area
plot_fci <- create_plots(df_plot_based, fci_mean, "財政力指数", base_setting)
plot_distance <- create_plots(df_plot_based, distance, "中核都市との距離", base_setting)
plot_length <- create_plots(df_plot_based, track_length, "廃線距離", base_setting)

plot_cross_1 <- plot_area + plot_fci + plot_distance + plot_length + 
  plot_layout(ncol = 1) &
  theme_bw(base_family = "HiraKakuPro-W3") &
  base_setting &
  geom_smooth(method = "lm",
              formula = y ~ x,
              se = FALSE,
              color = "#3C8DAD",
              linewidth = 1.0)

plot_cross_1


plot_cross_2 <- plot_distance + plot_length + 
  plot_layout(ncol = 1) &
  theme_bw(base_family = "HiraKakuPro-W3") &
  base_setting &
  geom_smooth(method = "lm",
              formula = y ~ x,
              se = FALSE,
              color = "#3C8DAD",
              linewidth = 1.0)

plot_cross_1 + plot_cross_2



test <- cowplot::plot_grid(
  plot_area,
  plot_fci,
  plot_distance,
  plot_length,
  ncol = 2, nrow = 2) 

test



ggsave(test,
       filename = "04.analyze/new/03_plot/05_cross/figure/cross_all_muni.png",
       width = 6,
       height = 8)


create_plots <- function(df_plot_based, var_x, lab_x, base_setting) {
  
  var_x <- rlang::enquo(var_x)
  
  plot_output <- ggplot(df_plot_based, aes(x = !!var_x, y = diff)) +
    geom_point() +
    geom_smooth(method = "lm",
                se = FALSE,
                color = "#3C8DAD",
                linewidth = 1.0) +
    theme_bw() +
    # facet_wrap(~ category) +
    labs(y = "処置効果",
         title = lab_x) +
    theme_bw(base_family = "HiraKakuPro-W3") +
    theme(
      plot.title = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      # axis.text.y = element_text(size = 10),
      # axis.title.x = element_text(size = 12),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 12),
      # axis.title.y = element_blank(),
      panel.grid.major = element_line(color = "lightgray"),
      # panel.border       = element_blank(),
      axis.line.x.bottom = element_line(color = 'black'),
      axis.line.y.left   = element_line(color = 'black'),
      # axis.line.y.right  = element_line(color = 'black'),
      axis.text.y.right  = element_blank()
    ) 
  # stat_poly_eq(formula = y ~ x,
  #              aes(label = paste(..eq.label.., ..rr.label.., ..p.value.label.., sep = "*`,`~")), 
  #              parse = TRUE,
  #              label.x.npc = "right",
  #              vstep = 0.05)
  # aes(label = paste(stat(eq.label))),
  # parse = TRUE)
  
  return(plot_output)
  
}



# lm ------------------------------------------------------------------

library(gt)
library(modelsummary)

list_lm <- create_lm_mean(df_plot_based)

test_1 = append(list_lm_area, list_lm_fci)
test_2 = append(list_lm_distance, list_lm_length)

list_based <- list_lm

create_latex(lm_list)

create_latex <- function(list_based) {
  
  lm_output <- msummary(list_based,
                        output  = "data.frame",
                        # output  = "kableExtra",
                        coef_omit = "Intercept",
                        coef_rename = c("fci_mean" = "Coef",
                                        "area" = "Coef",
                                        "distance" = "Coef",
                                        "track_length" = "Coef"),
                        gof_map = c("nobs", "r.squared"),
                        statistic = c("std.error", "p.value")) |> 
    dplyr::select(
      statistic:廃線距離
    ) |>
    as_tibble() |> 
    kbl(format = 'latex', booktabs = TRUE) 
  
  lm_output
  
    # kable_styling(latex_options = c("repeat_header")) 
    # add_header_above(c(" " = 1, 
    #                    var_1 = 3,
    #                    var_2 = 3))
  
  return(lm_output)
}

aa <- create_latex(test_1, "面積", "財政力指数")
bb <- create_latex(test_2, "中核都市との距離", "廃線距離")
aa

bb

# save_kable(file = "04.analyze/new/03_plot/05_cross/figure/lm_table.pdf")

library(estimatr)

df_plot_based


create_lm_mean <- function(df_plot_based) {
  
  lm_area <- lm_robust(data = df_plot_based,
                       formula = diff ~ area)
  
  lm_fci <- lm_robust(data = df_plot_based,
                       formula = diff ~ fci_mean)
  
  lm_distance <- lm_robust(data = df_plot_based,
                       formula = diff ~ distance)
  
  lm_length <- lm_robust(data = df_plot_based,
                       formula = diff ~ track_length)
  
  
  lm_list <- list("面積" = lm_area,
                  "財政力指数" = lm_fci,
                  "中核都市との距離" = lm_distance,
                  "廃線距離" = lm_length)
  
  return(lm_list)
  
  msummary(lm_list,
           coef_omit = "Intercept",
           gof_map = c("nobs", "r.squared"),
           statistic = c("std.error", "p.value"))
  
}

