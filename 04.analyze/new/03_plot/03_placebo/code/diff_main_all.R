

# read_data ---------------------------------------------------------------
df_master <- read_df_csv("master", "master")

list_file_path <- list.files("04.analyze/new/01_data/data/all_municipalities/")


# create_data -------------------------------------------------------------

df_diff_plot <- purrr::map(list_file_path, read_plot) |> 
  bind_rows()

# create_plot -------------------------------------------------------------

purrr::map(list_file_path, create_diff_plot, df_diff_plot, df_master)

xread_plot <- function(file_path){
  
  city_name_t <- stringr::str_sub(file_path, start = 1, end = -5)
  
  base_plot_main <- readRDS(paste0("04.analyze/new/01_data/data/main/", file_path))
  
  base_plot_all <- readRDS(paste0("04.analyze/new/01_data/data/all_municipalities/", file_path))
  
  df_diff_main <- base_plot_main |> 
    tidysynth::grab_synthetic_control() |>
    dplyr::mutate(diff = real_y - synth_y) |> 
    dplyr::mutate(city_name = city_name_t) |> 
    rename(
      diff_main = diff
    ) |> 
    select(-c(real_y, synth_y))
  
  df_diff_all <- base_plot_all |> 
    tidysynth::grab_synthetic_control() |>
    dplyr::mutate(diff = real_y - synth_y) |> 
    dplyr::mutate(city_name = city_name_t) |> 
    rename(
      diff_all = diff
    ) |> 
    select(-c(real_y, synth_y))
  
  df_diff <- df_diff_main |> 
    left_join(df_diff_all) |> 
    pivot_longer(
      cols = c(diff_main, diff_all),
      names_to = "category",
      values_to = "value"
    )
  
  return(df_diff)
  
}

# file_path


create_diff_plot <- function(file_path, df_diff_plot, df_master) {
  
  city_name_t <- stringr::str_sub(file_path, start = 1, end = -5)
  
  df_plot_based <- df_diff_plot |> 
    dplyr::filter(
      city_name == city_name_t
    )
  
  int_year <- df_master |> 
    dplyr::filter(
      city_name == city_name_t
    ) |> 
    dplyr::distinct(year_end) |> 
    pull()
  
  diff_plot <- ggplot(df_plot_based) +
    geom_line(aes(x = time_unit, y = value, linetype = category)) +
    scale_linetype_manual(values=c("longdash", "solid")) +  
    # scale_linetype_manual(name = "" ,values = c("Treatment" = "solid",
    #                                             "Synthetic" = "dashed")) +
    labs(title = city_name_t,
         y = "処置効果") +
    geom_vline(xintercept = int_year, linetype = "solid", size = 0.8,
               colour = "gray") +
    theme_bw(base_family = "HiraKakuPro-W3") +
    theme(plot.title = element_text(size = 17),
          axis.text.x = element_text(size = 13),
          axis.text.y = element_text(size = 13),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 15),
          legend.position = "bottom",
          legend.text = element_text(size=18))
  
  
  ggsave(diff_plot, 
         filename = paste0(
           "04.analyze/new/03_plot/03_placebo/figure/diff_main_all/", city_name_t, ".png")
         )
  
}


