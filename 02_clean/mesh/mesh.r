main <- function() {

    # use population data to adjust the mesh data
    df_code <- readxl::read_xls(here::here("01_data", "raw", "jumin_kihon_daicho", "1907nsjin.xls")) |>
        dplyr::select(
            city_id = 1,
            prefecture_name = 2,
            city_name = 3
        ) |>
        dplyr::mutate(
            city_id = stringr::str_sub(city_id, start = 1, end = 5),
            city_id = as.numeric(city_id)
        )
    # 政令指定都市
    list_ordinance_designated_city <- df_code |>
        dplyr::filter(
            city_name %in% c(
                "大阪市", 
                "名古屋市", 
                "京都市", 
                "横浜市",
                "神戸市", 
                "北九州市", 
                "札幌市", 
                "川崎市",
                "福岡市", 
                "広島市", 
                "仙台市", 
                "千葉市",
                "さいたま市", 
                "静岡市", 
                "堺市", 
                "新潟市",
                "浜松市", 
                "岡山市", 
                "相模原市", 
                "熊本市")
                )

    df_mesh <- purrr::map(1:47, read_mesh) |>
        dplyr::bind_rows()

    df_mesh |>
        dplyr::mutate(
            city_name_mod = case_when(
                stringr::str_detect(city_name, "大阪市") ~ "大阪市",
                stringr::str_detect(city_name, "名古屋市") ~ "名古屋市",
                stringr::str_detect(city_name, "京都市") ~ "京都市",
                stringr::str_detect(city_name, "横浜市") ~ "横浜市",
                stringr::str_detect(city_name, "神戸市") ~ "神戸市",
                stringr::str_detect(city_name, "北九州市") ~ "北九州市",
                stringr::str_detect(city_name, "札幌市") ~ "札幌市",
                stringr::str_detect(city_name, "川崎市") ~ "川崎市",
                stringr::str_detect(city_name, "福岡市") ~ "福岡市",
                stringr::str_detect(city_name, "広島市") ~ "広島市",
                stringr::str_detect(city_name, "仙台市") ~ "仙台市",
                stringr::str_detect(city_name, "千葉市") ~ "千葉市",
                stringr::str_detect(city_name, "さいたま市") ~ "さいたま市",
                stringr::str_detect(city_name, "静岡市") ~ "静岡市",
                stringr::str_detect(city_name, "堺市") ~ "堺市",
                stringr::str_detect(city_name, "新潟市") ~ "新潟市",
                stringr::str_detect(city_name, "浜松市") ~ "浜松市",
                stringr::str_detect(city_name, "岡山市") ~ "岡山市",
                stringr::str_detect(city_name, "相模原市") ~ "相模原市",
                stringr::str_detect(city_name, "熊本市") ~ "熊本市",
                .default = city_name
            )
        ) |> dplyr::filter(city_name_mod == "さいたま市") |> distinct(city_name) |> View()

    df_mesh |> distinct(city_id)
    
}

read_mesh <- function(prefecture_i) {

    if(prefecture_i < 10){
        file_name_i <- paste0("0", prefecture_i, ".csv")
    } else {
        file_name_i <- paste0(prefecture_i, ".csv")
    }

    df_raw <- read.csv(
        here::here("01_data", "raw", "mesh", file_name_i), 
        fileEncoding = "cp932"
        ) |>
        dplyr::select(
            city_id = 1,
            city_name = 2,
            mesh = 3
        )

    print(paste0(prefecture_i,":", df_raw |> distinct(city_id) |> nrow()))

}


read.csv("01_data/intermediate/population/population_master.csv", fileEncoding = "cp932") |>
    dplyr::filter(city_name == "仙台市", year == 2018) |>  View()
    dplyr::summarise(
        n = n_distinct(city_id),
        .by  = prefecture_name
    ) 





