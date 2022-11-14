main <- function(){
  
  treatment_data <- load_data("base_treatment.csv")
  control_data <- load_data("base_control.csv")
  
  synth_base <- integrate_data(treatment_data, control_data)
  
  out <- gsynth_func(synth_base)
  
}

load_data <- function(file_name){
  
  output_data <- read.csv(here::here('03.build','base_percent','data',file_name),
                             fileEncoding = "CP932") 
  return(output_data)
  
}


integrate_data <- function(treatment_data, control_data){
  
  all_data <- bind_rows(treatment_data, control_data) 
  
  
  all_data <- all_data %>% 
    dplyr::mutate(treatment = ifelse((treatment_year != 0), 1,0),
                  .after = city_name) %>% 
    ungroup()
  
  all_data <- all_data %>%
    ungroup() %>% 
    group_by(city_id) %>% 
    dplyr::mutate(cut_off = ifelse((treatment == 1 & treatment_year <= year), 1, 0),
                  .after = treatment) %>% 
    tidyr::replace_na(list(cut_off = 0, treatment = 0, treatment_year = 0, 
                           city_name = "NNN"))
  
  
  output_data <- all_data %>% 
    filter(city_id != "17204",
           city_id != "21505",
           city_id != "12441",
           city_id != "31325",
           city_id != "2206",
           city_id != "43433") %>% 
    filter(year != 1995)
  
  
  return(output_data)
}

gsynth_func <- function(synth_base){
  
  gsynth.out <- gsynth(
    formula =  percent ~ cut_off,
    data = synth_base,
    index = c("city_id", "year"),
    force = "two-way",
    CV = TRUE,
    EM = TRUE,
    r = c(0, 5),
    se = TRUE,
    inference = "parametric",
    nboots = 1000,
    parallel = F # TRUE
  )
  
  plot(gsynth.out, type = "counterfactual", raw = "all")
  
  return(gsynth.out)
  
}

plot(out, type = "loadings")

save()

file_name <- paste0(here::here('04.analyze','synthetic_control', 'figure',
                               'all','gsynth.pdf'))

ggsave(p, filename = file_name)

p <- plot(out, type = "counterfactual", raw = "all")

sort(out$wgt.implied[,8])

library(gsynth)
