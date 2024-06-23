main <- function() {

    # outcome data
    df_population <- read.csv(here::here("01_data", "intermediate", "outcome", "population.csv"), fileEncoding = "cp932")

    # covariates data
    # children
    df_children <- read.csv(here::here("01_data", "intermediate", "covariates", "children.csv"), fileEncoding = "cp932")

    # age
    df_age <- read.csv(here::here::("01_data", "intermediate", "covariates", "age.csv"), fileEncoding = "cp932")

    # housetype
    df_housetype <- read.csv(here::here::("01_data", "intermediate", "covariates", "housetype.csv"), fileEncoding = "cp932")

    # residence_year
    df_residence_year <- read.csv(here::here::("01_data", "intermediate", "covariates", "residence_year.csv"), fileEncoding = "cp932")

    # worker
    df_worker <- read.csv(here::here::("01_data", "intermediate", "covariates", "worker.csv"), fileEncoding = "cp932")

    # sample
    # station
    df_station <- read.csv(here::here::("01_data", "intermediate", "covariates"), fileEncoding = "cp932")

    # mesh
    df_mesh <- read.csv(here::here::("01_data", "intermediate", "covariates"), fileEncoding = "cp932")
    # density
    df_density <- read.csv(here::here::("01_data", "intermediate", "covariates"), fileEncoding = "cp932")

    # disaster
    df_disaster <- read.csv(here::here::("01_data", "intermediate", "covariates"), fileEncoding = "cp932")

    # df_geometry_station <- read.csv(here::here::("01_data", "intermediate", "covariates"), fileEncoding = "cp932")
}


df_population |>
    dplyr