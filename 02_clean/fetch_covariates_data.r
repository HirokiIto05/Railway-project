appid <- "12604ebf628834b46b3867721893644141fe9a34"

fetch_children <- function(appid) {
  # 1995; "0000032240"
  # 2000; "0000032980"
  # 2005; "0000033816"
  # 2010; "0003038598"

  # 1995;
  df_raw <- estatapi::estat_getStatsData(
    appId = appid,
    statsDataId = "0000032240")
  write.csv(df_raw, here::here("01_data", "raw", "children", "1995.csv"), fileEncoding = "CP932", row.names = FALSE)

  # 2000;
  df_raw <- estatapi::estat_getStatsData(
    appId = appid,
    statsDataId = "0000032980")
  write.csv(df_raw, here::here("01_data", "raw", "children", "2000.csv"), fileEncoding = "CP932", row.names = FALSE)

  # 2005;
  df_raw <- estatapi::estat_getStatsData(
    appId = appid,
    statsDataId = "0000033816")
  write.csv(df_raw, here::here("01_data", "raw", "children", "2005.csv"), fileEncoding = "CP932", row.names = FALSE)

  # 2010;
  df_raw <- estatapi::estat_getStatsData(
    appId = appid,
    statsDataId = "0003038598")
  write.csv(df_raw, here::here("01_data", "raw", "children", "2010.csv"), fileEncoding = "CP932", row.names = FALSE)

}


fetch_housetype <- function(appid) {
  # 1995  ; "0000032264"
  # 2000_a; "0000032999" over  30 mill
  # 2000_b; "0000033001" under 30 mill
  # 2005  ; "0000033837"
  # 2010  ; "0003038618"
  
  # 1995;
    df_raw <- estatapi::estat_getStatsData(
      appId = appid,
      statsDataId = "0000032264")
    
    write.csv(df_raw, here::here("01_data", "raw", "housetype", "1995.csv"), fileEncoding = "CP932", row.names = FALSE)
  # 2000_a;
    df_raw <- estatapi::estat_getStatsData(
      appId = appid,
      statsDataId = "0000032999")
    write.csv(df_raw, here::here("01_data", "raw", "housetype", "2000_a.csv"), fileEncoding = "CP932", row.names = FALSE)
  # 2000_b;
    df_raw <- estatapi::estat_getStatsData(
      appId = appid,
      statsDataId = "0000033001")
    write.csv(df_raw, here::here("01_data", "raw", "housetype", "2000_b.csv"), fileEncoding = "CP932", row.names = FALSE)
  # 2005;
    df_raw <- estatapi::estat_getStatsData(
      appId = appid,
      statsDataId = "0000033837")
    write.csv(df_raw, here::here("01_data", "raw", "housetype", "2005.csv"), fileEncoding = "CP932", row.names = FALSE)
  # 2010;
    df_raw <- estatapi::estat_getStatsData(
      appId = appid,
      statsDataId = "0003038618")
    write.csv(df_raw, here::here("01_data", "raw", "housetype", "2010.csv"), fileEncoding = "CP932", row.names = FALSE)

}


fetch_residence_year <- function(appid) {

    # 2000; 0000033277
    # 2010; 0003067226


  # 2000;
    df_raw <- estatapi::estat_getStatsData(
      appId = appid,
      statsDataId = "0000033277")
    write.csv(df_raw, here::here("01_data", "raw", "residence_year", "2000.csv"), fileEncoding = "CP932", row.names = FALSE)
  # 2010;
    df_raw <- estatapi::estat_getStatsData(
      appId = appid,
      statsDataId = "0003067226")
    write.csv(df_raw, here::here("01_data", "raw", "residence_year", "2010.csv"), fileEncoding = "CP932", row.names = FALSE)    

}


fetch_working <- function(appid) {
    # 1995; 0000032371 over 5 billion
    # 1995; 0000032373
    # 2000; 0000033134
    # 2005; 0000033948
    # 2010; 0003052121


    df_raw <- estatapi::estat_getStatsData(
      appId = appid,
      statsDataId = "0000032371")

    write.csv(df_raw, here::here("01_data", "raw", "working", "1995_a.csv"), fileEncoding = "CP932", row.names = FALSE)    

    df_raw <- estatapi::estat_getStatsData(
      appId = appid,
      statsDataId = "0000032373")

    write.csv(df_raw, here::here("01_data", "raw", "working", "1995_b.csv"), fileEncoding = "CP932", row.names = FALSE)    

    df_raw <- estatapi::estat_getStatsData(
      appId = appid,
      statsDataId = "0000033134")

    write.csv(df_raw, here::here("01_data", "raw", "working", "2000.csv"), fileEncoding = "CP932", row.names = FALSE)    

    df_raw <- estatapi::estat_getStatsData(
      appId = appid,
      statsDataId = "0000033948")

    write.csv(df_raw, here::here("01_data", "raw", "working", "2005.csv"), fileEncoding = "CP932", row.names = FALSE)    

    df_raw <- estatapi::estat_getStatsData(
      appId = appid,
      statsDataId = "0003052121")

    write.csv(df_raw, here::here("01_data", "raw", "working", "2010.csv"), fileEncoding = "CP932", row.names = FALSE)    



}


fetch_student <- function(appid) {
    # 1995; 
    # 2000; 
    # 2005; 
    # 2010; 
    
}

# fetch_children(appid)
# fetch_housetype(appid)
# fetch_residence_year(appid)
fetch_working(appid)
