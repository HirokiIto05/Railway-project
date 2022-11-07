all_average <- timing_data %>% 
  ungroup() %>% 
  group_by(year) %>% 
  summarise(mean = mean(percentage, na.rm = TRUE))


treatment_data <- read.csv(here::here('03.build','integrate','data','treatment_data.csv'),
                           fileEncoding = "CP932")

control_data <- read.csv(here::here('03.build','integrate','data','control_data.csv'),
                                   fileEncoding = "CP932")

all_data <- bind_rows(treatment_data, control_data)

all_data <- all_data %>% 
  dplyr::mutate(treatment = ifelse((treatment_year != 0), 1,0),
                .after = city_name) %>% 
  ungroup()

all_data <- all_data %>%

  ungroup() %>% 
  group_by(city_id) %>% 
  dplyr::mutate(cut_off = ifelse((treatment == 1 & treatment_year <= year), 1, 0),
                .after = treatment) 

average_treatment <- timing_data %>% 
  group_by(timing) %>% 
  summarise(mean = mean(percentage, na.rm = TRUE))




colnames(all_data)

omit_data <- all_data %>% 
  drop_na() %>% 
  filter(city_id != "17204",
         city_id != "21505")

gsynth.out <- gsynth(
  formula =  percentage ~ cut_off,
  data = omit_data,
  index = c("city_id", "year"),
  force = "two-way",
  CV = TRUE,
  r = c(0, 5),
  se = TRUE,
  inference = "parametric",
  nboots = 1000,
  parallel = F # TRUE
)

plot(gsynth.out, type = "counterfactual", raw = "all")



install.packages("gsynth")

library(gsynth)


str(all_data)






dataprep.out <-
  dataprep(
    all_data,
    predictors = c("X1", "X2"),
    dependent     = "Y",
    unit.variable = "state.num",
    time.variable = "year",
    unit.names.variable = "state",
    treatment.identifier  = 1,
    controls.identifier   = c(2:10),
    time.predictors.prior = c(1:14),
    time.optimize.ssr     = c(1:14),
    time.plot             = c(1:30)
  )
