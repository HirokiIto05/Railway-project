library(estatapi)

appid <- "12604ebf628834b46b3867721893644141fe9a34"


list_data <- estat_getStatsList(appId = appid, 
                                searchWord = "住居の種類")

a = list_data |> 
  dplyr::filter(
    # grepl("12", STATISTICS_NAME),
    # grepl("都道府県", STATISTICS_NAME),
    # grepl("市区町村", TITLE),
    # grepl("住居の種類・住宅の所有の関係", TITLE)
    # grepl("50", TITLE)
  ) |> 
  dplyr::mutate(
    year = as.numeric(stringr::str_sub(OPEN_DATE, start = 1, end = 4))
  ) |> 
  dplyr::arrange(
    year
  )


data_main <- estat_getStatsData(
  appId = appid,
  statsDataId = "0000032999"
)

write.xlsx(data_main, file = "02.raw/covariates/housetype/2000_large.xlsx")

