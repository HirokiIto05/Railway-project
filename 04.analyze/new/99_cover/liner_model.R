library(fixest)
library(dplyr)
library(tidyr)
library(tidyr)
library(ggplot2)



df_master <- read.csv(here::here('03.build/master/data/master.csv'), fileEncoding = 'cp932')

df_master <- df_master |> 
  dplyr::mutate(
    relative_year =  if_else(!is.na(year_end),
            year - year_end,
            -1000
  )
  ) |> 
  dplyr::group_by(city_id) |>
  dplyr::mutate(
    cum_social = cumsum(replace_na(social_rate, 0))
  ) |> 
  ungroup() |> 
  dplyr::filter(
    between(relative_year, -6, 5)
  )


# df_master |> 
#   dplyr::summarise(
#     dplyr::n(),
#     .by = relative_year
#   ) |> 
#   View()

est_twfe <- feols(social_rate ~ i(relative_year, ref = 0) | city_id + year, data = df_master)

etable(est_twfe)

iplot(est_twfe)
