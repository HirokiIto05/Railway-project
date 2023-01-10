city_name_list <- unique(treatment_data$city_name)
print(city_name_list)

treatment_data <- load_csv("complete", "treatment_data.csv")

plot_2 <- read_plot("常呂郡置戸町")
plot_3 <- read_plot("中川郡本別町")
plot_4 <- read_plot("足寄郡足寄町")   
plot_5 <- read_plot("足寄郡陸別町", treatment_data)
plot_6 <- read_plot("上北郡七戸町", treatment_data)
plot_7 <- read_plot("行方市")
plot_8 <- read_plot("輪島市")
plot_9 <- read_plot("珠洲市")
plot_10 <- read_plot("鳳珠郡能登町")
plot_11 <- read_plot("加茂郡八百津町")
plot_12 <- read_plot("山県郡安芸太田町")
plot_13 <- read_plot("南島原市")
plot_14 <- read_plot("西臼杵郡高千穂町")
plot_15 <- read_plot("西臼杵郡日之影町")
plot_1 <- read_plot("常呂郡訓子府町")


purrr::map(treatment_name_lists, read_plot)

city_name_t <- "足寄郡陸別町"

read_plot <- function(city_name_t, treatment_data){
  
  print(city_name_t)
  
  file_name <- paste0(city_name_t, ".rds")
  
  base_plot <- readRDS(here::here('04.analyze','synthetic_control',
                                  'after_2015','table', file_name))
  
  title_id <- as.character(city_name_t)
  
  tt <- base_plot %>% grab_synthetic_control() 
  
  int_year <- treatment_data %>% 
    dplyr::filter(city_name == city_name_t)
  
  int_year <- unique(int_year$treatment_year)
  
  output_plot <- ggplot(tt, aes(time_unit)) +
    geom_line(aes(y = real_y)) +
    geom_line(aes(y = synth_y), linetype="dashed", size = 0.5) +
    geom_point(aes(x = time_unit, y = real_y), size = 1.1)+
    geom_vline(xintercept = int_year - 0.5, linetype = "dotted", size = 0.8) +
    labs(title = title_id,
         y = "population") +
    theme_gray(base_family = "HiraKakuPro-W3") +
    theme(plot.title = element_text(size = 13),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    scale_y_continuous(breaks = seq(0.6,1.4, 0.2)) +
    scale_x_continuous(breaks = seq(1995,2015,5))
  
  
  # pdf_name <- paste0(city_name_t, ".png")
  # 
  # file_name_figure <- paste0(here::here('04.analyze','synthetic_control',
  #                                       'after_2015',
  #                                       'figure', pdf_name))
  
  # ggsave(output_plot, filename = file_name_figure)
  
  
  return(output_plot)
  
}

first_group <-  (plot_1 | plot_2) / (plot_3 | plot_4)

library(patchwork)

first_group <-  (plot_6 | plot_5)
first_group

second_group <- (plot_4 | plot_5 | plot_6) 
third_group <- (plot_7 | plot_8 | plot_9) 
four_group <- (plot_10 | plot_11 | plot_12)
five_group <- (plot_13 | plot_14 | plot_15)


second_group <- (plot_5 | plot_6) / (plot_7 | plot_8)
third_group <- (plot_9 | plot_10) / (plot_11| plot_12)

ggsave(first_group, filename = here::here('04.analyze','synthetic_control', 
                                          'after_2015', 'two_plot','two_graph.pdf'),
       device = cairo_pdf,  width = 12, height = 6)

install.packages("Cairo")
library(Cairo)

# cairo

# CairoPDF(file="plot-cars-CairoPDF", width=8, height=6)


ggsave(second_group, filename = here::here('04.analyze','synthetic_control', 'figure',
                                           'synth_cov', 'density_1000', 'second.png'),
       device = cairo_pdf,   width = 12, height = 4)

ggsave(third_group, filename = here::here('04.analyze','synthetic_control', 'figure',
                                          'synth_cov', 'density_1000', 'third.png'),
       device = "png",   width = 12, height = 4)

ggsave(four_group, filename = here::here('04.analyze','synthetic_control', 'figure',
                                         'synth_cov', 'density_1000','four.png'),
       device = "png",   width = 12, height = 4)

ggsave(five_group, filename = here::here('04.analyze','synthetic_control', 'figure',
                                         'synth_cov', 'density_1000','five.png'),
       device = "png",   width = 12, height = 4)



four_group <- (plot_13 | plot_14) / (plot_15 | plot_spacer())

four_group

second_group

install.packages("patchwork")
library(patchwork)


