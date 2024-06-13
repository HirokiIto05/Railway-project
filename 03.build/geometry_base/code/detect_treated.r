# package
library(dplyr)
library(tidyr)
library(here)
library(readxl)
library(stringr)
library(lubridate)

# read_data

df <- readxl::read_xlsx(here::here('02.raw', '廃線・存続表', 'list_abandoned.xlsx'))


# data cleaning
df <- df |> 
# df |> 
    select(
        company_name = "事業者名",
        line_name = "路線名",
        section_name = "区間",
        length = "営業キロ",
        year,
        month,
        day
    ) |> 
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
        date = ymd(paste(year, month, day, sep = "-"))
    )  |> 
    relocate(
        date, .before = company_name
    )
# save data

df |> 
    write.csv(
        here::here('03.build', 'geometry_base', 'data', 'df_abandoned.csv'), 
        fileEncoding = "CP932",
        row.names = FALSE)

