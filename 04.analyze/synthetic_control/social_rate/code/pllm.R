install.packages("plm")

library(plm)


city_id_list <- unique(master_data$city_id)

colnames(master_data)

based_data <- master_data %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(city_id) %>% 
  dplyr::select(city_id, cut_off,
                city_name, region_name,
                year, total,
                middle, dummy,
                children_household_percent, 
                own_household_percent,
                workforce_percent,
                student_percent, 
                houseyear_pop_percent,
                train_pop_percent)  

plm_based <- purrr::map(city_id_list, add_relative, 
                              based_data) %>% 
  dplyr::bind_rows()


id_n <- 1549
  
add_relative <- function(id_n, based_data){
  
  interim_data <- based_data %>% 
    dplyr::filter(city_id == id_n)
  
  num_data <- interim_data %>% 
    dplyr::filter(year == 1995) %>% 
    dplyr::distinct(middle) 
  
  base_num <- unique(num_data$middle)
  
  output_data <- interim_data %>% 
    dplyr::mutate(middle_relative = middle/base_num,
                  .after = middle)
  
  return(output_data)
  
}

print(colnames(plm_based))


install.packages("did")
library(did)

plm_based <- plm_based %>%
  dplyr::relocate(did, .after = city_id)


plm_result <-  plm(middle_relative ~ cut_off +
                   + children_household_percent
                   + own_household_percent
                   + workforce_percent
                   + student_percent
                   + houseyear_pop_percent
                   + train_pop_percent,
                     data = plm_based, 
                     index = c("city_id", "year"), 
                     model = "within", 
                     effect = "twoways")
summary(plm_result)

fixed <- pgmm(plm_based, middle_relative ~ children_household_percent 
              + own_household_percent
              + workforce_percent
              + student_percent
              + houseyear_pop_percent
              + train_pop_percent
           +                index=c('city_id','year'), # 個体と時間変数の指定
           +                effect = "twoways",   # 固定効果の設定
           +                transformation = 'd'  # 'ld'ならシステムGMM、'd'なら一階階差GMM
           +            )