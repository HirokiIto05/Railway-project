main <- function() {


    # density data --------------------------
    df_density <- read.csv(
      here::here("01_data", "intermediate", "railway", "density.csv"),
      fileEncoding = "cp932") |>
      clean_density()


    # Geometry data --------------------------
    df_geometry <- read.csv(here::here("01_data", "raw", "geometry_data", "geometry_base.csv")) |>
      clean_geometry()

    # Treatment data --------------------------
    # 01_data/raw/not_table_data/abandon.pdfから転記
    df_treatment <- readxl::read_xlsx(here::here("01_data", "raw", "station_data", "treatment.xlsx"))

    df_treatment <- clean_treatment(df_treatment)

    df_density_treatment <- readxl::read_xlsx(here::here("01_data", "raw", "density", "treatment.xlsx")) |>
        clean_density_treatment()

    df_density_treatment |>
        mutate(across(c(year_end, density_end), as.numeric)) |>
        dplyr::filter(
            continue == 0,
            year_end == max(year_end),
            .by = line_name)


    df_treatment <- extract_treated_municipalities(df_geometry, df_treatment, "scm")
    # control data --------------------------
    df_control_line <- extract_control_line(df_geometry, df_density, 3000)
    df_control <- extract_control_municipalities(df_control_line)

  
  df_main <- df_treatment |>
      bind_rows(df_control)

  write.csv(df_main, here("01_data", "intermediate", "railway", "main.csv"), row.names = FALSE, fileEncoding = "cp932")

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


clean_density <- function(df){

  df <- df |>
    mutate(
      company_name = stringr::str_replace_all(company_name, "（旧国鉄）", ""),
      jr = if_else(str_detect(company_name, "旅客鉄道"), 1, 0),
      company_name = stringr::str_replace_all(company_name, "WILLERTRAINS", "WILLER　TRAINS"),
      line_name = stringr::str_replace_all(line_name, "叡山鋼索線", "鋼索線"),
      line_name = stringr::str_replace_all(line_name, "ケ", "ヶ"),
      line_name = stringr::str_replace_all(line_name, "十国峠鋼索線", "十国鋼索線"),
      line_name = stringr::str_replace_all(line_name, "久大本線", "久大線"),
      line_name = stringr::str_replace_all(line_name, "豊肥本線", "豊肥線"),
      across(c(year, density), as.numeric)
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
      ) |>
      mutate(
        company_name = stringr::str_replace_all(company_name, "（旧国鉄）", ""),
        jr = if_else(str_detect(company_name, "旅客鉄道"), 1, 0 )
      )

    return(df)
}


extract_control_line <- function(df_geometry, df_density, cutoff_density, df_) {
  # jr以外の鉄道
  # 企業単位で輸送密度が3000人以下
  list_company_control <- df_density |>
      dplyr::filter(
        year == 2019,
        density <= cutoff_density,
        jr == 0,
        is.na(line_name)
        ) |> 
      distinct(company_name) |> 
      pull()

  df_control_company_base <- df_geometry |>
    dplyr::filter(
      jr == 0,
      company_name %in% list_company_control
    )

  # check
  # length(list_company_control)
  # df_control_company_base |> distinct(company_name) |> nrow()

  # 路線単位で輸送密度が3000人以下
  # remove duplicates
  df_line_control <- df_density |>
      dplyr::filter(
        year == 2019,
        density <= cutoff_density,
        jr == 0,
        !is.na(line_name),
        !company_name %in% list_company_control
        ) |> 
      distinct(company_name, line_name)

  # lineだけで抽出すると、line nameが同じの関係のない会社も含まれるため、
  # lineとcompanyを別でリストで作成して、両方に合致するものを抽出
  # remove same line name, but different company
  list_company_line <- df_line_control |>
    mutate(
      company_line = paste0(company_name, line_name)
      ) |> 
    distinct(company_line) |>
    pull()

  df_control_line_base <- df_geometry |>
    dplyr::filter(
      jr == 0,
      paste0(company_name, line_name) %in% list_company_line
    ) 

  df_control_local <- df_control_company_base |>
    bind_rows(df_control_line_base)

  # jr
  # jrの幹線（主要な路線）は廃線しない可能性が高いため、削除
  # 幹線以外にの路線を用いる。
  df_not_main_line_jr <- readxl::read_xlsx(here::here("01_data", "raw", "station_data", "control.xlsx")) |>
    dplyr::filter(jr == 1) |>
    mutate(
      line_name = stringr::str_replace_all(line_name, "久大本線", "久大線"),
      line_name = stringr::str_replace_all(line_name, "豊肥本線", "豊肥線"),
      line_name = stringr::str_replace_all(line_name, "日高本線", "日高線"),
    )


  list_not_main_line <- df_not_main_line_jr |> 
    distinct(line_name) |> 
    pull()

  df_control_density_jr <- df_density |>
    dplyr::filter(
      year == 2019,
      density <= cutoff_density,
      jr == 1
      ) |>  
      distinct(line_name, company_name)

  list_company_line_jr <- df_control_density_jr |>
    mutate(
      company_line = paste0(company_name, line_name)
      ) |> 
    distinct(company_line) |>
    pull()


  df_control_jr <- df_geometry |>
    dplyr::filter(
      jr == 1,
      paste0(company_name, line_name) %in% list_company_line_jr
    ) |>
    right_join(df_control_density_jr)

  df_control_jr <- df_control_jr |>
    dplyr::filter(line_name %in% list_not_main_line)

  # check (cutoff_density not included)
  # length(list_not_main_line)
  # df_control_jr |> distinct(line_name) |> nrow()

  df_control <- df_control_local |>
    bind_rows(df_control_jr) |>
    dplyr::filter(
      year_end %in% c(9999, 2019, 2020, 2021, 2022, 2023)
    )

  return(df_control)
}


extract_control_municipalities <- function(df_control_line) {

  list_control_company_line <- df_control_line |>
    mutate(
      company_line = paste0(company_name, line_name)
      ) |>
    distinct(company_line) |>
    pull()

  list_control_company <- df_control_line |>
    dplyr::distinct(company_name) |>
    dplyr::pull()

  df_control <- df_geometry |>
    dplyr::filter(
      year_end %in% c(9999, 2019, 2020, 2021, 2022, 2023)
    ) |>  
    dplyr::mutate(
      line_n = dplyr::n_distinct(line_name),
      .by = city_id
    ) |>  
    # identify same line name, but different company
    dplyr::filter(
      paste0(company_name, line_name) %in% list_control_company_line
    ) |>
    mutate(
      treated = FALSE
    ) |>
    select(
      city_id,
      city_name,
      treated,
      year_end,
      line_name,
      company_name,
      jr,
      station_name)
    # distinct(city_id, city_name, line_name, company_name) 

    return(df_control)

}


extract_treated_municipalities <- function(df_geometry, df_treatment, method_i = "scm") {

    # list treatment railway lines
    list_line_treatment <- df_treatment |>
      dplyr::select(line_name) |>
      dplyr::distinct() |>
      dplyr::pull()

    list_company_treatment <- df_treatment |>
      dplyr::select(company_name) |>
      dplyr::distinct() |>
      dplyr::pull()

    if(method_i == "scm"){

    # count the number of lines in municipality
    # extract only line in per municipality
    # then, extract only abandoned line
    df_treatment <- df_geometry |> 
        dplyr::filter(
          between(year_end, 1999, 2014)| year_end %in% c(9999, 2019, 2020, 2021, 2022, 2023)
        ) |> 
        dplyr::mutate(
          line_n = dplyr::n_distinct(line_name),
          .by = city_id
        ) |> 
        # duplicate end year
        dplyr::mutate(
          year_end_n = dplyr::n_distinct(year_end),
          .by = city_id
        ) |> 
        dplyr::mutate(
          continue = any(year_end %in% c(9999, 2019, 2020, 2021, 2022, 2023)),
          .by = city_id
        ) |> 
        dplyr::filter(
          line_n == 1,
          continue == FALSE
          # year_end_n == 1
        ) |> # distinct(company_name, line_name, city_name, year_end) |> View()
        dplyr::filter(
            line_name %in% list_line_treatment
        ) |>
        mutate(
          treated = TRUE
        ) |>
        select(
          city_id,
          city_name,
          treated,
          year_end,
          line_name,
          company_name,
          jr,
          station_name)

    } else if(method_i == "linear"){
      
      df_treatment <- df_geometry |> 
        # dplyr::filter(
        #   between(year_end, 1999, 2014)| year_end %in% c(9999, 2019, 2020, 2021, 2022, 2023),
        # ) |> 
        dplyr::filter(
          company_name %in% list_company_treatment & line_name %in% list_line_treatment,
          between(year_end, 1999, 2014)
        )

        setdiff(ab,a)
        # duplicate end year
        dplyr::mutate(
          year_end_n = dplyr::n_distinct(year_end),
          .by = c(station_name, city_id)
        ) |> 
        dplyr::mutate(
          continue = any(year_end %in% c(9999, 2019, 2020, 2021, 2022, 2023)),
          .by = city_id
        ) |> 
        dplyr::filter(
          # continue == FALSE,
          between(year_end, 1999, 2014),
          line_name %in% list_line_treatment
        ) |>
        dplyr::mutate(
          line_n = dplyr::n_distinct(line_name),
          .by = city_id
        ) |>  
        dplyr::filter(
        ) |> 
        distinct(company_name, line_name, city_name, year_end) |> View()

      
    }

  return(df_treatment)

}


clean_density_treatment <- function(df){

    df <- df |>
        mutate(
            across(everything(), as.character)
        ) |>
        tidyr::pivot_longer(
            cols = -c(company_name, line_name, year, date, length, continue),
            names_to = "year_end",
            values_to = "density_end"
        ) |> 
        dplyr::filter(!is.na(density_end))

    return(df)

}


main()
