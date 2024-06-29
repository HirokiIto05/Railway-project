main <- function() {

    # --------------------------
    # Treatment data
    # --------------------------
    # 01_data/raw/not_table_data/abandon.pdfから転記
    df_treatment <- readxl::read_xlsx(here::here("01_data", "raw", "station_data", "treatment.xlsx"))

    df_treatment <- clean_treatment(df_treatment)

    # --------------------------
    # Control data
    # --------------------------
    # 01_data/raw/not_table_data/continue.pdf
    # 01_data/raw/not_table_data/jr
    # 上記から転記
    # df_control <- readxl::read_xlsx(here::here("01_data", "raw", "station_data", "control.xlsx"))

    # df_control <- clean_control(df_control)

    list_line_control <- df_control |>
        dplyr::select(line_name) |>
        dplyr::distinct() |>
        dplyr::pull()

    df_density <- read.csv(
      here::here("01_data", "intermediate", "railway", "density.csv"),
      fileEncoding = "cp932")

    df_density |> View()

    list_local_density <- df_density |>
      summarise(
        density = mean(density, na.rm = TRUE),
        .by = c(company_name, line_name)
      ) |>
      dplyr::filter(
        density <= 1000
      ) |>  
      distinct(company_name) |>
      pull()
    
    list_jr_density <- df_density |>
      summarise(
        density = mean(density, na.rm = TRUE),
        .by = c(company_name, line_name)
      ) |>
      dplyr::filter(
        density <= 1000
      ) |>  
      distinct(line_name) |>
      pull()

        # df_control <- df_geometry
    df_geometry |>  
        dplyr::filter(
          year_end %in% c(9999, 2019, 2020, 2021, 2022, 2023)
        ) |> 
        dplyr::mutate(
          line_n = dplyr::n_distinct(line_name),
          .by = city_id
        ) |> 
        dplyr::mutate(
          end_n = dplyr::n_distinct(year_end),
          .by = city_id
        ) |> 
        dplyr::filter(
          line_n == 1
        ) |> 
        dplyr::filter(
          (company_name  %in% list_local_density)|(line_name %in% list_jr_density)
        ) |> 
        distinct(city_id) 

    list_jr_density

    



    # --------------------------
    # Geometry data
    # --------------------------
    df_geometry <- read.csv(here::here("01_data", "raw", "geometry_data", "geometry_base.csv"))

    df_geometry <- clean_geometry(df_geometry)
}

clean_treatment <- function(df) {

    df <- df |> 
      mutate(
        company_name = na_if(company_name, "〃"),
        line_name = na_if(line_name, "〃")
        ) |> 
    # fill company name
    tidyr::fill(company_name, .direction = 'down') |> 
    tidyr::fill(line_name, .direction = 'down') |> 
    # change date
    mutate(
        across(year:day, as.character),
        date = lubridate::ymd(paste(year, month, day, sep = "-"))
    )  |> 
    relocate(
        date, .before = company_name
    ) |>
    dplyr::filter(
        year <= 2014
    )

    return(df)
}


clean_control <- function(df) {

    df <- df |> 
    # adjust line name
      mutate(
        line_name = case_when(
            line_name == "日高本線" ~ "日高線",
            line_name == "筑豊本線" ~ "筑豊線",
            line_name == "久大本線" ~ "久大線",
            line_name == "豊肥本線" ~ "豊肥線",
            .default = line_name
        )
        )

    return(df)
}


clean_geometry <- function(df) {

    df <- df |> 
      select(
        line_name = N05_002,
        company_name = N05_003,
        station_name = N05_011,
        city_name = N03_004,
        city_id = N03_007,
        year_start = N05_005b,
        year_end = N05_005e,
        station_id = N05_006
      )

    return(df)
}


extract_treated_municipalities <- function(df_geometry, df_treatment) {

    # list treatment railway lines
    list_line_treatment <- df_treatment |>
      dplyr::select(line_name) |>
      dplyr::distinct() |>
      dplyr::pull()

    # list control railway lines
    list_line_control <- df_control |>
        dplyr::select(line_name) |>
        dplyr::distinct() |>
        dplyr::pull()

      # 各自治体に何本の路線が通っているか数える。(過去も含む) 
      # 1のもののみを抽出
      # その中で、廃線している路線を抽出
    df_treatment <- df_geometry |> 
        dplyr::filter(
          between(year_end, 1999, 2014)| year_end %in% c(9999, 2019, 2020, 2021, 2022, 2023)
        ) |> 
        dplyr::mutate(
          line_n = dplyr::n_distinct(line_name),
          .by = city_id
        ) |> 
        dplyr::mutate(
          year_end_n = dplyr::n_distinct(year_end),
          .by = city_id
        ) |> 
        dplyr::mutate(
          continue = any(year_end %in% c(9999, 2019, 2020, 2021, 2022, 2023)),
          .by = city_id
        ) |> 
        dplyr::filter(
          year_end != 9999,
          line_n == 1,
          continue == FALSE
          # year_end_n == 1
        ) |> 
        dplyr::filter(
            line_name %in% list_line_treatment
        ) |> 
        distinct(city_name, line_name, year_end) |>  View()
    

    # df_control <- df_geometry
    df_geometry |>  
        dplyr::filter(
          year_end %in% c(9999, 2019, 2020, 2021, 2022, 2023)
        ) |> 
        dplyr::mutate(
          line_n = dplyr::n_distinct(line_name),
          .by = city_id
        ) |> 
        dplyr::mutate(
          end_n = dplyr::n_distinct(year_end),
          .by = city_id
        ) |> 
        dplyr::filter(
          line_n == 1
        ) |> 



        # distinct(line_name, city_name, city_id) |> View()

        dplyr::mutate(
          year_end_n = dplyr::n_distinct(year_end),
          .by = city_id
        )

        distinct(line_name, city_name, city_id, company_name) |>  
        group_by(city_name) |>
        dplyr::mutate(
            line_n = dplyr::n_distinct(line_name)
        ) |> 
        dplyr::ungroup() |> 
        dplyr::filter(
            line_n == 1 # one line for each municipality
        ) |>
        dplyr::filter(
          line_name %in% list_line_control
        ) |>
        dplyr::filter(
            # controlに含まれない"本線"を分ける。
            # 以下はControlに含まれる"本線"であり、それ以外を除外する。
            !(line_name == "本線" & !company_name %in% c("黒部峡谷鉄道", "近江鉄道", "広島電鉄", "長崎電気軌道"))
            # !(line_name == "日光線" & company_name == "東武鉄道"),
            # !(line_name == "大月線" & company_name == "富士急行") # change company in 2021(duplicate)
        )

df_control_city_new |> View()

df_geometry |>
  dplyr::filter(year_end  %in% c(9999, 2019, 2020, 2021, 2022, 2023)) |>
  dplyr::filter(city_name %in% list_a) |>
  group_by(city_name) |>
  mutate(
    n = dplyr::n_distinct(company_name)
  )



    df_geometry |>
        distinct(line_name, city_name, city_id) |>  View()

    list_one_city_one_line <- df_geometry |>
        dplyr::summarise(
            n = dplyr::n_distinct(line_name),
            .by = c(city_id, city_name)
        ) |> 
        dplyr::filter(n == 1) |> 
        pull(city_id)
  
  df_current_name_id <- df_current |> 
    select(city_name, city_id) |> 
    dplyr::mutate(check = 1) |> 
    distinct() 
  
  df_treat_name_id <- df_treatment |> 
    select(city_name, city_id) |> 
    distinct() 
  
  df_check <- dplyr::left_join(df_treat_name_id, df_current_name_id) |> 
    distinct() |> 
    dplyr::filter(is.na(check)) 
  
  list_current_city <- unique(df_check$city_name)
  
  df_treatment_one <- df_treatment |> 
    dplyr::filter(city_name %in% list_current_city,
                  year_end >= 1999) 

  list_city_one_end <- df_treatment_one |> 
    dplyr::distinct(year_end, city_name) |>
    group_by(city_name) |> 
    dplyr::filter(n() == 1) |> 
    distinct(city_name) |> 
    unlist() |> 
    as.character()

  df_output <- df_treatment |> 
    dplyr::filter(city_name %in% list_city_one_end)

  return(df_output)

}


list_low_density <- df_density |> 
  summarise(
    density = mean(density, na.rm = TRUE),
    .by = c(company_name, line_name)
  ) |>
  dplyr::filter(
    density <= 2000
  )




list_line_unique <- df_geometry |>
  dplyr::filter(year_end  %in% c(9999, 2019, 2020, 2021, 2022, 2023)) |>
  distinct(line_name, city_name, city_id) |>  
  group_by(city_name) |>
  dplyr::mutate(
      line_n = dplyr::n_distinct(line_name)
  ) |> 
  dplyr::ungroup() |> 
  dplyr::filter(
      line_n == 1 # one line for each municipality
  ) |>
  dplyr::filter(
    line_name %in% list_line_control
  ) |>
  dplyr::distinct(city_name) |>
  pull()

  dplyr::filter(
      # controlに含まれない"本線"を分ける。
      # 以下はControlに含まれる"本線"であり、それ以外を除外する。
      # !(line_name == "本線" & !company_name %in% c("黒部峡谷鉄道", "近江鉄道", "広島電鉄", "長崎電気軌道"))
      # !(line_name == "日光線" & company_name == "東武鉄道"),
      # !(line_name == "大月線" & company_name == "富士急行") # change company in 2021(duplicate)
  )


list_company_unique<- df_geometry |>
  dplyr::filter(year_end %in% c(9999, 2019, 2020, 2021, 2022, 2023)) |>
  distinct(city_name, city_id, company_name, line_name) |>  
  group_by(city_name) |>
  dplyr::mutate(
      line_n = dplyr::n_distinct(company_name)
  ) |> 
  dplyr::ungroup() |> 
  dplyr::filter(
      line_n == 1 # one line for each municipality
  ) |>
  dplyr::filter(
    line_name %in% list_line_control
  ) |>
  dplyr::distinct(city_name) |>
  pull()


setdiff(list_line_unique, list_company_unique)
setdiff(list_company_unique, list_line_unique)


  dplyr::filter(
      # controlに含まれない"本線"を分ける。
      # 以下はControlに含まれる"本線"であり、それ以外を除外する。
      # !(line_name == "本線" & !company_name %in% c("黒部峡谷鉄道", "近江鉄道", "広島電鉄", "長崎電気軌道"))
      # !(line_name == "日光線" & company_name == "東武鉄道"),
      # !(line_name == "大月線" & company_name == "富士急行") # change company in 2021(duplicate)
  )


list_line_treatment <- df_treatment |>
  distinct(line_name) |>
  pull()

a <- df_geometry |>
  dplyr::filter((between(year_end, 1999, 2013))) |> 
  dplyr::filter(line_name %in% list_line_treatment)   

list_a <- df_geometry |> dplyr::filter(
  between(year_end, 2000, 2014)) |> 
  dplyr::distinct(station_name) |>
  pull()

list_not_end = df_geometry |>
  dplyr::filter(
    station_name %in% list_a
  ) |> 
  group_by(station_name) |>
  mutate(
    n = dplyr::n_distinct(year_end),
    end_dummy = if_else(year_end == 9999, 1, 0)
  ) |>
  ungroup() |> 
  dplyr::filter(
    end_dummy == 1
  ) |>
  dplyr::distinct(station_name) |>
  pull()

list_end <- setdiff(list_a, list_not_end)

length(list_a)
length(list_not_end)
length(list_end)


df_geometry |>
  dplyr::filter(
    station_name %in% list_end
  ) |> 
  group_by(station_name) |>
  mutate(
    n = dplyr::n_distinct(year_end)
  ) |>
  ungroup()  |> View()





list_lost_station_city <- 
df_geometry |> dplyr::filter(
  between(year_end, 2000, 2014)
  ) |> 
  distinct(city_id) |>
  pull()

df_geometry |> 
  dplyr::filter(
      city_id %in% list_lost_station_city
    ) |> 
    dplyr::filter(
      year_end  %in% c(9999, 2019, 2020, 2021, 2022, 2023)
    ) |>
    summarise(
      n = dplyr::n_distinct(line_name),
      .by = c(city_id, city_name)
    ) |> View()



  distinct(city_name, city_id) |> 
  arrange(city_name) |> 
  View()



df_geometry |> dplyr::filter(
  between(year_end, 2000, 2014)) |> 
  group_by(station_id) |>
  mutate(
    # n = dplyr::n_distinct(year_end)
    n = dplyr::n()
  ) |>
  ungroup() |> 
  group_by(station_name) |>
  mutate(
  # n = dplyr::n_distinct(year_end)
  n_station = dplyr::n()
  ) |>
  ungroup() |>  View()


df_treatment |> View()


  a <- df_treatment |> 
      mutate(
        railway_revenue = NA,
        railway_pl = NA,
        operation_revenue = NA,
        operation_pl = NA,
        all_sector_pl = NA,
        total_pl = NA,
        total_asset = NA,
        total_debt = NA,
        total_capital = NA
      ) |>
    select(
      company_name, line_name, date, length, railway_revenue, railway_pl, operation_revenue, operation_pl, all_sector_pl, total_pl, total_asset, total_debt, total_capital
    )
    tidyr::pivot_longer(
      cols = -c(date, company_name, line_name, length),
      names_to = "variable",
      values_to = "value"
    ) 





