main <- function(){
  
  treatment_data <- load_csv('complete', 'treatment_data.csv') |> 
    dplyr::filter(treatment_year <= 2015,
                  city_name != "揖斐郡大野町",
                  city_name != "本巣郡北方町",
                  city_name != "珠洲市",
                  city_name != "鳳珠郡能登町",
                  city_name != "十和田市",
                  city_name != "行方市"
    )   
  
  treatment_name_lists <- unique(treatment_data$city_name)

  
  purrr::map(treatment_name_lists, create_mspe_plot,
            folder_name = "after_2015")
    
}



create_mspe_plot <- function(city_name_t, folder_name){
  
  file_name <- paste0(city_name_t, ".rds")
  syn_df <- readRDS(here::here('04.analyze','synthetic_control',
                               folder_name,'table',
                               file_name))
  
  p <- syn_df |> plot_mspe_ratio()
  
  p_data <- p$data
  
  p_plot <- ggplot(p_data, aes(x = reorder(unit_name, mspe_ratio),
                               y = mspe_ratio, fill = type)) +
    geom_bar(stat = "identity", width = 0.8) +
    scale_fill_manual(values=c("gray","black"))+
    # geom_hline(yintercept = mean_five, linetype = "longdash") +
    coord_flip() +
    labs(title = "MSPE ratio",
         y = "Pre MSPE / Post MSPE") +
    # theme_gray(base_family = "HiraKakuPro-W3") +
    theme(axis.title.y = element_blank())
          # plot.title = element_text(size = 13),
          # axis.text.x = element_text(size = 10),
          # axis.text.y = element_text(size = 10)) 
    # ylim(c(-0.1, 0.1))
  
  p_plot
  
  png_name <- paste0(city_name_t, ".png")
  
  ggsave(p_plot, filename = here::here('04.analyze','synthetic_control',
                                   folder_name,'mspe',
                                   png_name))
  
  
}
