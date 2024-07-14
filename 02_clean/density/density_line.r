main <- function(){

    list_year <- seq(2012, 2019)

    df <- purrr::map(
        list_year,
        read_density
      ) |>
      bind_rows()

    # adjust line name
    df <- clean_density(df)

    df_jr <- readxl::read_xlsx(here::here("01_data", "raw", "density", "jr", "density_jr.xlsx")) |>
        clean_jr_density()

    df_density <- df |>
        dplyr::bind_rows(df_jr) |>
        mutate(
            across(c(year, density), as.numeric)
        )

    # Treatment denesity data
    df_density_treatment <- readxl::read_xlsx(here::here("01_data", "raw", "density", "treatment.xlsx")) |>
        clean_density_treatment()

    df_density_treatment |>
        mutate(across(c(year_end, density_end), as.numeric)) |>
        dplyr::filter(
            continue == 0,
            year_end == max(year_end),
            .by = line_name) 
        # summarise(
        #     a = mean(density_end, na.rm = TRUE)
        #     )
    
    write.csv(
        df_density, 
        here::here("01_data", "intermediate", "railway", "density.csv"), 
        row.names = FALSE,
        fileEncoding = "cp932")

}


read_density <- function(year_i) {
  
  df_raw <- readxl::read_xlsx(
    here::here("01_data", "raw", "density", paste0(year_i, ".xlsx")),
    col_names = FALSE) |> 
    dplyr::select(
        "company_name" = 1,
        "line_name" = 2,
        "density" = 13) |>
    mutate(
        year = year_i, .before = density
    ) |>
    dplyr::filter(
        !if_all(everything(), is.na)
        ) |> 
    tidyr::fill(company_name, .direction = "down")
  
  return(df_raw)
}


clean_jr_density <- function(df_jr) {

    df_jr <- df_jr |>
      select(
            line_name = 1,
            everything()
        ) 

    df_jr <- df_jr |>
    dplyr::mutate(
        across(-line_name, as.numeric)
        ) |>
    tidyr::pivot_longer(
        cols = -line_name,
        names_to = "year",
        values_to = "density"
    )  |> 
    mutate(
        company_name = if_else(
            stringr::str_detect(line_name, "旅客鉄道"),
            line_name,
            as.character(NA)
            ),
        .before = line_name
        ) |> 
    fill(company_name, .direction = "down") |>
    dplyr::filter(line_name != company_name) |>
    mutate(
        across(everything(), as.character),
        jr = 1
        ) |>
    mutate(across(everything(), as.character))

    return(df_jr)


}


clean_density <- function(df) {

    df <- df |>
      dplyr::mutate(
            company_name = stringr::str_replace_all(company_name, "ＩＧＲいわて銀河鉄道", "アイジーアールいわて銀河鉄道"),
            company_name = stringr::str_replace_all(company_name, "小湊鉄道", "小湊鐵道"),
            company_name = stringr::str_replace_all(company_name, "土佐電気鉄道", "土佐電氣鐵道"),
            company_name = stringr::str_replace_all(company_name, "ＩＲいしかわ鉄道", "IRいしかわ鉄道"),
            company_name = stringr::str_replace_all(company_name, "青函トンネル記念館", "一般財団法人青函トンネル記念館"),
            company_name = stringr::str_replace_all(company_name, "ＷＩＬＬＥＲ　ＴＲＡＩＮＳ", "WILLER　TRAINS")
        ) |>
      tidyr::drop_na(company_name, density) |> 
      dplyr::filter(
        !stringr::str_detect(
            company_name, 
            "事業者|合計|公営計|運輸局|中小計|大手計|日本鉄道|旅客鉄道|前年度|事務局|臨海鉄道"),
        !stringr::str_detect(
            density, 
            "-|0")
        ) |>
    mutate(across(everything(), as.character)) %>%
    mutate(
        across(everything(), ~str_replace_all(., "　", "")),
        jr = "0"
    )

    return(df)

}

clean_density_treatment <- function(df){

    df <- df |>
        mutate(
            across(everything(), as.character),
        ) |>
        tidyr::pivot_longer(
            cols = -c(company_name, line_name, year, date, length, continue),
            names_to = "year_end",
            values_to = "density_end"
        ) |> 
        dplyr::filter(!is.na(density_end))

    return(df)

}
