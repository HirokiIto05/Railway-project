
plot_2 <- read_plot("常呂郡置戸町")
plot_3 <- read_plot("中川郡本別町")
plot_4 <- read_plot("足寄郡足寄町")   
plot_5 <- read_plot("足寄郡陸別町")
plot_6 <- read_plot("上北郡七戸町")
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

plot_1

city_name_t <- "常呂郡訓子府町"

read_plot <- function(city_name_t){
  
  file_name <- paste0(city_name_t, ".rds")
  
  base_plot <- readRDS(here::here('04.analyze','synthetic_control', 'figure',
                                  'synth_cov', 'density_1000','table', file_name))
  
  title_id <- as.character(city_name_t)
  
  tt <- base_plot %>% tidysynth::plot_differences() 
  
  tt_df <- tt$data 
  # output_plot
  
  output_plot <- ggplot(tt_df) + 
    geom_line(aes(x = time_unit, y = diff)) +
    geom_point(aes(x = time_unit, y = diff), size = 0.8)+
    geom_hline(yintercept = 0, linetype = "dotted", size = 0.8) +
    labs(title = title_id,
         y = "difference") +
    theme_gray(base_family = "HiraKakuPro-W3") +
    theme(plot.title = element_text(size = 13),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) 

  output_plot
  
  pdf_name <- paste0(city_name_t,'.png')
  
  file_name_figure <- paste0(here::here('04.analyze','synthetic_control', 'figure',
                                        'synth_cov','density_1000',
                                        'diff', pdf_name))
  
  ggsave(output_plot, filename = file_name_figure)
  
  return(output_plot)
  
}
