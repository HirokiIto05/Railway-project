#### Execute following instractions before run the code  
# 1. `QGIS`を開く
# 2. `01_data/raw/geometry_shapefiles/N05-22_GML/N05-22_Station2.shp`と`01_data/raw/geometry_data/shape_files/N03-190101_GML/N03-19_190101.shp`をQGISにドラック&ドロップ
# 3. Vector -> Geoproccessing Tools -> Interactionからデータを合わせる。
# 4. InteractionされたレイヤーをExport -> csvから、`01_data/raw/geometry_data/geometry_base.csv`という名前で保存。

main <- function() {

    # read station and municipality data
    df_geometry <- read_geometry_data()

    line_control <- readxl::read_xlsx(here::here("01_data", "raw", "station_data", "control.xlsx")) |>
        dplyr::distinct(line_name) |>
        dplyr::pull()

    line_treatment <- readxl::read_xlsx(here::here("01_data", "raw", "station_data", "treatment.xlsx")) |>
        dplyr::filter(
            !year >= 2020,
            line_name != "〃"
            ) |>
        dplyr::distinct(line_name) |>
        dplyr::pull() 

    df_treatment <- df_geometry |>
        dplyr::filter(line_name %in% line_treatment)

    df_control <- df_geometry |>
        dplyr::filter(line_name %in% line_control)

    df_geometry_master |>
        dplyr::filter(!line_name %in% list_end)

}

read_geometry_data <- function() {

    df_geometry <- read.csv(here::here("01_data", "raw", "geometry_data", "geometry_base.csv"), encoding = "co932") |> 
        dplyr::select(
            "line_name" = N05_002,
            "company_name" = N05_003,
            "year_start"   = N05_005b,
            "year_end"     = N05_005e,
            "station_name" = N05_011,
            "prefecture_name" = N03_001,
            "city_id" = N03_007
        )
}



df_geometry_master |> 
    # dplyr::distinct(line_name, station_name) |>
    dplyr::summarise(
    n = n(),
    .by = c(line_name, station_name)
    ) |> 
    dplyr::filter(n > 1) |>
    View()



a <- readxl::read_xlsx("01_data", "raw", "control.xlsx") |>
    dplyr::filter(jr == 0)
    
unique(a$company_name)



