main <- function() {
  
  df_higashi <- read_df_xlsx("disaster", "higashi") %>% 
    list()
  df_hanshin <- read_df_xlsx("disaster", "hanshin") %>% 
    list()
  df_oita <- read_df_xlsx("disaster", "oita") %>% 
    list()
  df_kumamoto <- read_df_xlsx("disaster", "kumamoto") %>% 
    list()
  
  list_disaster <- c(df_higashi, df_hanshin, df_kumamoto, df_oita)

  df_disaster <- bind_rows(list_disaster)
  
  save_df_csv(df_disaster, "disaster", "disaster_all")
  
  
}


