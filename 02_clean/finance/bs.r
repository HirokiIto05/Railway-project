main <- function() {

    list_year <- seq(2012, 2019)
    # read data

    df_bs <- purrr::map(list_year, read_bs) |>
        dplyr::bind_rows()

    df_bs <- clean_bs(df_bs)

    write.csv(
        df_bs, 
        here::here("01_data", "intermediate", "railway", "bs.csv"),
        row.names = FALSE,
        fileEncoding = "cp932"
    )
}


read_bs <- function(year_i) {
    print(year_i)

    df_raw <- readxl::read_xlsx(here::here("01_data", "raw", "bs", paste0(year_i, ".xlsx")))


    if(year_i <= 2013) {
        df_output <- df_raw |>
        dplyr::select(
            company_name = 1,
            current_asset = 13,
            fixed_asset = 39,
            total_asset = 資産合計,
            current_debt = 53,
            fixed_debt = 64,
            total_debt = 負債合計,
            total_capital = 資本合計
        )   
    } else if(year_i <= 2019) {
        df_output <- df_raw |>
        dplyr::select(
            company_name = 1,
            current_asset = 12,
            fixed_asset = 36,
            total_asset = 資産合計,
            current_debt = 48,
            fixed_debt = 58,
            total_debt = 負債合計,
            total_capital = 資本合計
        )  
    }

    df_output <- df_output |>
        dplyr::mutate(
            year = year_i,
            .after = company_name
        )
    
    return(df_output)

}

clean_bs <- function(df) {

    df_output <- df |>
        mutate(
            company_name = str_replace_all(company_name, " ", ""),
            company_name = str_replace_all(company_name, "　", ""),
            company_name = dplyr::na_if(company_name, "")
        ) |>
        dplyr::filter(
            !str_detect(company_name, "合計"),
            !str_detect(company_name, "運輸局"),
            !str_detect(company_name, "公営"),
            !str_detect(company_name, "民営"),
            !str_detect(company_name, "事業所名"),
            !str_detect(company_name, "（中小）"),
            !str_detect(company_name, "事業所名"),
            !dplyr::if_all(-year, is.na)
        )

    return(df_output)

}


main()
