main <- function(){
  
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(stringr)
  library(tidysynth)
  library(rlang)
  
  function_lists <- list.files(here::here('06.tools', 'functions'), full.names = TRUE)
  lapply(function_lists, source) 
  
}


main()

