main <- function() {

    list_year <- seq(2012, 2019)

    # aggregate data
    df_pl <- purrr::map(list_year, read_pl) |>
        dplyr::bind_rows()
    
    df_pl <- clean_pl(df_pl)

    write.csv(
        df_pl, 
        here::here("01_data", "intermediate", "railway", "pl.csv"),
        row.names = FALSE,
        fileEncoding = "cp932"
    )


}


read_pl <- function(year_i) {
    print(year_i)

    df_raw <- readxl::read_xlsx(here::here("01_data", "raw", "pl", paste0(year_i, ".xlsx")))

    if(year_i <= 2013) {
        df_output <- df_raw |>
    # df_raw |> 
        dplyr::select(
            company_name = 1,
            railway_revenue = 2,
            railway_pl = 4,
            operation_revenue = 15,
            operation_pl = 17,
            all_sector_pl = 全事業経常損益,
            total_pl = 当期損益
        ) 
    } else if(year_i %in% c(2016, 2017)) {
        df_output <- df_raw |>
        dplyr::select(
            company_name = 1,
            railway_revenue = 2,
            railway_pl = 4,
            operation_revenue = 14,
            operation_pl = 16,
            all_sector_pl = 全事業経常損益,
            total_pl = 当期損益
        ) 
    } else if(year_i <= 2020) {
        df_output <- df_raw |>
        dplyr::select(
            company_name = 1,
            railway_revenue = 2,
            railway_pl = 4,
            operation_revenue = 14,
            operation_pl = 16,
            all_sector_pl = 全事業経常損益,
            total_pl = 当期損益
            # all_sector_pl = 21,
            # total_pl = 29
        ) 
    }

    df_output <- df_output |>
        dplyr::mutate(
            year = year_i,
            .after = company_name
        )
    
    return(df_output)

}


clean_pl <- function(df) {

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
