# ---------------------------------------------------------------------------- #
#                                     Setup                                    #
# ---------------------------------------------------------------------------- #

library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(tidyr)
library(stringr)
library(stringi)
library(kableExtra)
library(gtExtras)
library(fixest)
library(cobalt)
library(fastDummies)
library(data.table)
library(dtplyr)
library(patchwork)

# ---------------------------------------------------------------------------- #
#                                     Main                                     #
# ---------------------------------------------------------------------------- #

# データの読み込み
df <- readr::read_csv(here::here('01.data', 'problem1_dataset', 'master.csv'))

df_master <- read.csv(here::here("03.build", "master", "data", "master.csv"),
                      fileEncoding = "cp932")

df_master <- add_vars(df_master)

df_master |> View()


# ---------------------------------------------------------------------------- #
#                                     Functions                                #
# ---------------------------------------------------------------------------- #


add_vars <- function(df){

    df_output <- df |> 
        dplyr::mutate(
            rel_year = year - year_end,
            rel_year = dplyr::if_else(is.na(rel_year), -1000, rel_year),
            dummy = dplyr::if_else(!is.na(year_end) & year > year_end, 1, 0)) |> 
        dplyr::relocate(rel_year, .after = year) |> 
        dplyr::group_by("city_id") |>
        dplyr::mutate(         
            mean_household_with_children_rate = mean(household_with_children_rate, na.rm = TRUE),
            mean_own_household_rate = mean(own_household_rate, na.rm = TRUE),
            mean_workforce_rate = mean(workforce_rate, na.rm = TRUE),
            mean_student_rate = mean(student_rate, na.rm = TRUE),
            mean_old_house_rate = mean(old_house_rate, na.rm = TRUE)
        ) |> 
        dplyr::ungroup()

    return(df_output)

}


estimate_twfe <- function(df) {

    estimate_simple <- estimatr::lm_robust(
        social_rate ~ household_with_children_rate + own_household_rate + workforce_rate + student_rate + old_house_rate + ,
        weights = ~ meansize,
        data = df)

    df <- df_master |> 
        dplyr::mutate(
            year_end = dplyr::if_else(is.na(year_end), 10000, year_end),
        )

    result <- fixest::feols(
        # social_rate ~ dummy | year + city_id,
        # change_rate ~ dummy | year + city_id,
        # social_rate ~ dummy + mean_household_with_children_rate + mean_own_household_rate + mean_workforce_rate + mean_student_rate + mean_old_house_rate| year + city_id,
        change_rate ~ i(rel_year, ref = c(-1, -1000))| year + city_id,
        # social_rate ~ sunab(year, year_end)| year + city_id,
        data = df,
        cluster = 'city_id') 

    df_master |> 
        summarise(
            n = dplyr::n(),
            .by = 'rel_year'
        ) |> 
        View()
df_master |> 
    filter(!is.na(year_end)) |> 
    distinct(line_name)

df_master |> 
    filter(!is.na(year_end)) |> 
    distinct(city_name)

    result |> etable()
    coefplot(result)
    colnames(df_master)
    
    return(model_output)
}






df_pop <- read.csv(here::here("03.build", "city_adjust", "data", "pop.csv"),
                   fileEncoding = "cp932")

list_treatment <- df_master |> 
    filter(!is.na(year_end)) |> 
    distinct(city_id) |> 
    pull()

df_pop |> 
    filter(year >= 2012) |> 
    summarise(
        rate = mean(social_rate, na.rm = TRUE),
        .by = c(city_id, city_name, prefecture_name)
    ) |> 
    arrange(rate) |> 
    mutate(
        rank = row_number(),
        quantile = rank / 1741) |>
    dplyr::filter(city_id %in% list_treatment) |>
    View()


df_pop |> 
    dplyr::filter(str_detect(city_name, "六戸町")) |> 
    select(city_id, city_name)

