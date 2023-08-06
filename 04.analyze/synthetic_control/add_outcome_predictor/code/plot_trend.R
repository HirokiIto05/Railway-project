main <- function(){
  
  treatment_data <- load_csv('complete', 'treatment_data.csv') |> 
    # dplyr::filter(treatment_year <= 2015,
    #               city_name != "揖斐郡大野町",
    #               city_name != "本巣郡北方町",
    #               city_name != "珠洲市",
    #               city_name != "鳳珠郡能登町",
    #               city_name != "十和田市",
    #               city_name != "行方市"
    # )   
  
  treatment_name_lists <- unique(treatment_data$city_name)
  
  
  treatment_name_lists

  
  
    
}

purrr::map(treatment_name_lists, read_plot, treatment_data)


city_name_t <- "常呂郡訓子府町"

read_plot <- function(city_name_t, treatment_data){
  
  print(city_name_t)
  
  file_name <- paste0(city_name_t, ".rds")
  
  base_plot <- readRDS(here::here('04.analyze','synthetic_control',
                                  'add_outcome_predictor','table', file_name))
  
  
  tt <- base_plot |> grab_synthetic_control() 
  
  int_year <- treatment_data |> 
    dplyr::filter(city_name == city_name_t)
  
  region_name_t <- unique(int_year$region_name)
  
  title_name <- paste0(city_name_t,':', region_name_t)
  
  int_year <- unique(int_year$treatment_year)
  
  output_plot <- ggplot(tt) +
    geom_line(aes(x = time_unit ,y = real_y,  linetype = "Treatment")) +
    geom_line(aes(x = time_unit, y = synth_y, linetype = "Synthetic"), size = 0.5) +
    scale_linetype_manual(name = "" ,values = c("Treatment" = "solid",
                                                "Synthetic" = "dashed")) +
    scale_color_manual(values = c("Treatment" = "black",
                                  "Synthetic" = "black")) +
    geom_point(aes(x = time_unit, y = real_y), size = 1.1)+
    geom_vline(xintercept = int_year - 1, linetype = "solid",
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
    scale_x_continuous(breaks = c(1995,int_year -1)) 
  
  
  output_plot
  
  pdf_name <- paste0(city_name_t, ".png")

  file_name_figure <- paste0(here::here('04.analyze','synthetic_control',
                                        'add_outcome_predictor',
                                        'figure', pdf_name))

  ggsave(output_plot, filename = file_name_figure)
  
  
  return(output_plot)
  
}


create_simple <- function(){
  
  two_df <- synth_data |> 
    dplyr::filter(city_name == city_name_t|dummy == 0)
  
  two_smry <- two_df |> 
    dplyr::ungroup() |> 
    dplyr::group_by(dummy, year) |> 
    dplyr::summarise(simple_mean = mean(outcome_percent)) |> 
    dplyr::ungroup()
  
  t_df <- synth_data |> 
    dplyr::ungroup() |> 
    dplyr::filter(dummy == 1) |> 
    dplyr::group_by(dummy, year) |> 
    dplyr::summarise(simple_mean = mean(outcome_percent)) |> 
    dplyr::rename(real_y = simple_mean) |> 
    dplyr::ungroup() |> 
    dplyr::select(year, real_y)
  
  title_name <- paste0(city_name_t, ':', "北海道")
  
  plot_df <- dplyr::left_join(two_smry, t_df) |> 
    dplyr::filter(dummy == 0)
  
  plot_df <- plot_df |> 
    dplyr::select(-dummy) |> 
    dplyr::ungroup()
  str(plot_df)
  
  str(tt)
  
  simple_plot <- ggplot(plot_df) +
    geom_line(aes(x = year, y = real_y, linetype = "Treatment")) +
    geom_line(aes(x = year, y = simple_mean, linetype = "Control Average"), 
              size = 0.5) +
    scale_linetype_manual(name = "" ,values = c("Treatment" = "solid",
                                                "Control Average" = "dashed")) +
    scale_color_manual(values = c("Treatment" = "black",
                                  "Control Average" = "black")) +
    geom_point(aes(x = year, y = real_y), size = 1.1)+
    geom_vline(xintercept = int_year - 1, linetype = "solid",
               size = 0.8, colour = "gray") +
    labs(title = title_name) +
    theme_bw(base_family = "HiraKakuPro-W3") +
    theme(plot.title = element_text(size = 15),
          axis.text.x = element_text(size = 13),
          axis.text.y = element_text(size = 13),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(size=15),
          plot.tag = element_text(size = 11),
          plot.tag.position = c(0.51, 0.98)) +
    ylim(0.6, 1.2) +
    # scale_y_continuous(breaks = seq(0.6,1.4, 0.2)) +
    scale_x_continuous(breaks = c(1995,int_year - 1)) 
    # coord_cartesian(clip = "off") +
    # annotate("text", x = 12.5, y = 3.5, label = "Arbitrary text") +
    
  
  simple_plot
  
}


# library(patchwork)

compare_plot <- simple_plot + output_plot 

compare_plot


ggsave(compare_plot, filename = here::here('04.analyze',
                                        'synthetic_control', 
                                        'add_outcome_predictor',
                                        'two_compare',
                                        'two_graph.png'),
       device = "png",  width = 12, height = 6)




install.packages("patchwork")
library(patchwork)

treatment_name_lists

plot_1 <- read_plot("檜山郡江差町", treatment_data)
plot_2 <- read_plot("檜山郡上ノ国町", treatment_data)
plot_3 <- read_plot("常呂郡訓子府町", treatment_data)
plot_4 <- read_plot("常呂郡置戸町", treatment_data)
plot_5 <- read_plot("中川郡本別町", treatment_data)
plot_6 <- read_plot("足寄郡足寄町", treatment_data)   
plot_7 <- read_plot("足寄郡陸別町", treatment_data)
plot_8 <- read_plot("上北郡七戸町", treatment_data)
plot_9 <- read_plot("上北郡六戸町", treatment_data)
plot_10 <- read_plot("輪島市", treatment_data)
plot_11 <- read_plot("加茂郡八百津町", treatment_data)
plot_12 <- read_plot("山県郡安芸太田町", treatment_data)
plot_13 <- read_plot("南島原市", treatment_data)
plot_14 <- read_plot("西臼杵郡高千穂町", treatment_data)
plot_15 <- read_plot("西臼杵郡日之影町", treatment_data)

p_sum <- (plot_1 + plot_2 +plot_3+
  plot_4 + plot_5 +plot_6+
  plot_7 + plot_8 +plot_9+
  plot_10 + plot_11 +plot_12+
  plot_13 + plot_14 +plot_15)+
  plot_layout(nrow = 5,
              ncol = 3,
              widths = c(1,1,1),
              heights = c(1,1,1,1,1),
              guides = "collect")
    # design = s,
              # guides = "collect")



p_sum

ggsave(p_sum, filename = here::here('04.analyze','synthetic_control', 
                                        'add_outcome_predictor', 'appendix','treand_plot.png'),
       device = "png",  width = 7.5, height = 7.5*1.41421356)


ggsave(p_sum, filename = here::here('04.analyze','synthetic_control', 
                                    'add_outcome_predictor', 'appendix','treand_plot_legend.png'),
       device = "png",  width = 7, height = 7*1.41421356)


p_sum


library(patchwork)
