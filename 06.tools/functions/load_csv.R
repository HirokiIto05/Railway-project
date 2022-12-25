load_csv <- function(folder_name, file_name){
  
  output_data <- read.csv(here::here('03.build',folder_name,'data', file_name),
                          fileEncoding = "CP932")
  return(output_data)
  
}
