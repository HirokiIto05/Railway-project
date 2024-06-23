main <- function(){

    list_year <- seq(2012, 2019)

    df <- purrr::map(
        list_year,
        read_density
    ) |>
    dplyr::bind_rows()

    # adjust line name
    df <- df |>
        dplyr::mutate(
            company_name = stringr::str_replace_all(company_name, "ＩＧＲいわて銀河鉄道", "アイジーアールいわて銀河鉄道"),
            company_name = stringr::str_replace_all(company_name, "万葉線", "万葉"),
            company_name = stringr::str_replace_all(company_name, "小湊鉄道", "小湊鐵道"),
            company_name = stringr::str_replace_all(company_name, "ＩＲいしかわ鉄道", "IRいしかわ鉄道"),
            company_name = stringr::str_replace_all(company_name, "ＷＩＬＬＥＲ　ＴＲＡＩＮＳ", "WILLER　TRAINS")
        )

    df_jr <- readxl::read_xlsx(here::here("01_data", "raw", "density_line", "jr", "density_jr.xlsx")) |>
        dplyr::select(
            "line_name" = 1,
            everything()
        ) |>
	dplyr::rename_with(
        ~ stringr::str_sub(.x, start = 1, end = 4),
        cols = starts_with("20") 
        ) |>
    dplyr::rename(
        line_name = line
    ) |>
    dplyr::mutate(
        across(-line_name, as.numeric)
        ) |>
    tidyr::pivot_longer(
        cols = -line_name,
        names_to = "year",
        values_to = "density"
    )

}


read_density <- function(year_i) {
  
  df_raw <- readxl::read_xlsx(
    here::here("01_data", "raw", "density_line", paste0(year_i, ".xlsx")),
    col_names = FALSE) |> 
    dplyr::select(
        "company_name" = 1,
        "line_name" = 2,
        "density" = 13) |>
    mutate(
        year = year_i
    ) |>
    dplyr::filter(
        !if_all(everything(), is.na)
        ) |> 
    tidyr::fill(company_name, .direction = "down")
  
  return(df_raw)
}


df |> 
filter(year == 2018) |> 
dplyr::mutate(density = as.numeric(density)) |>  
arrange(density) |>
mutate(
    rank = row_number()
) |> View()

df_jr |> 
filter(year == 2018) |> 
dplyr::mutate(density = as.numeric(density)) |>
arrange(density) |>
mutate(
    rank = row_number()
) |> View()

