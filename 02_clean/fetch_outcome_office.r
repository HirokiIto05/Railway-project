appid <- "12604ebf628834b46b3867721893644141fe9a34"

fetch_private_companies <- function(appid) {
    # 事業所・企業統計調査
    # 1986; 0000040355
    # 1991; 0000040728
    # 1994; 0000040882 # only a total num, must estimate the number of private companies.
    # 1996; 0000041027 # contain only private companies.
    # 1999; 0000041313 # contain only private companies.
    # 2001; 0000041455
    # 2004; 0000041658 # contain only private companies.
    # 2006; 0003000452 # ?
    
    # 経済センサス - 基礎調査
    # 2009; 0003032532 # not correct
    # 2014; 0003117639
    # 2019_a; 0003422213 - private
    # 2019_b; 0003414253 - public

    # 経済センサス - 活動調査
    # 2012; 0003085552 # contain only private companies.
    # 2016; 0003218500 # contain only private companies.



    # 1986;
    df_raw <- estatapi::estat_getStatsData(
        appId = appid,
        statsDataId = "0000040355")
    write.csv(df_raw, here::here("01_data", "raw", "private_companies", "1986.csv"), fileEncoding = "CP932", row.names = FALSE)

    # 1991;
    df_raw <- estatapi::estat_getStatsData(
        appId = appid,
        statsDataId = "0000040728")
    write.csv(df_raw, here::here("01_data", "raw", "private_companies", "1991.csv"), fileEncoding = "CP932", row.names = FALSE)

    # 1994;
    df_raw <- estatapi::estat_getStatsData(
        appId = appid,
        statsDataId = "0000040882")
    write.csv(df_raw, here::here("01_data", "raw", "private_companies", "1994.csv"), fileEncoding = "CP932", row.names = FALSE)

    # 1996;
    df_raw <- estatapi::estat_getStatsData(
        appId = appid,
        statsDataId = "0000041027")
    write.csv(df_raw, here::here("01_data", "raw", "private_companies", "1996.csv"), fileEncoding = "CP932", row.names = FALSE)

    # 1999;
    df_raw <- estatapi::estat_getStatsData(
        appId = appid,
        statsDataId = "0000041313")
    write.csv(df_raw, here::here("01_data", "raw", "private_companies", "1999.csv"), fileEncoding = "CP932", row.names = FALSE)

    # 2001;
    df_raw <- estatapi::estat_getStatsData(
        appId = appid,
        statsDataId = "0000041455")
    write.csv(df_raw, here::here("01_data", "raw", "private_companies", "2001.csv"), fileEncoding = "CP932", row.names = FALSE)

    # 2004;
    df_raw <- estatapi::estat_getStatsData(
        appId = appid,
        statsDataId = "0000041658")
    write.csv(df_raw, here::here("01_data", "raw", "private_companies", "2004.csv"), fileEncoding = "CP932", row.names = FALSE)

    # 2006;
    df_raw <- estatapi::estat_getStatsData(
        appId = appid,
        statsDataId = "0003000452")
    write.csv(df_raw, here::here("01_data", "raw", "private_companies", "2006.csv"), fileEncoding = "CP932", row.names = FALSE)

    # 2009;
    df_raw <- estatapi::estat_getStatsData(
        appId = appid,
        statsDataId = "0003032532")

    df_raw |>  View()
    write.csv(df_raw, here::here("01_data", "raw", "private_companies", "2009.csv"), fileEncoding = "CP932", row.names = FALSE)

    # 経済センサス - 基礎調査
    # 2014;
    df_raw <- estatapi::estat_getStatsData(
        appId = appid,
        statsDataId = "0003117639")
    write.csv(df_raw, here::here("01_data", "raw", "private_companies", "2014.csv"), fileEncoding = "CP932", row.names = FALSE)

    # 2019;
    df_raw <- estatapi::estat_getStatsData(
        appId = appid,
        statsDataId = "0003422213")
    write.csv(df_raw, here::here("01_data", "raw", "private_companies", "2019.csv"), fileEncoding = "CP932", row.names = FALSE)

    # 経済センサス - 活動調査
    # 2012;
    df_raw <- estatapi::estat_getStatsData(
        appId = appid,
        statsDataId = "0003085552")
    write.csv(df_raw, here::here("01_data", "raw", "private_companies", "2012.csv"), fileEncoding = "CP932", row.names = FALSE)

    # 2016;
    df_raw <- estatapi::estat_getStatsData(
        appId = appid,
        statsDataId = "0003218500")
    write.csv(df_raw, here::here("01_data", "raw", "private_companies", "2016.csv"), fileEncoding = "CP932", row.names = FALSE)

}
