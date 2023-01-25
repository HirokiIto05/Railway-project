hist_average <- ggplot(placebo_average_df , aes(x = placebo_average),fill="gray",
                 colour="black") +
  geom_histogram(bins = 30,fill="lightgray",
                 colour="black") +
  labs(x = "Placebo effect",
       y = "Frequency") +
  # scale_fill_manual(values=c("gray","black")) +
  # stat_function(fun = dnorm, 
  #               args = list(mean = mean(ten_after_df$diff),
  #                           sd = sd(ten_after_df$diff))) +
  # geom_line()+
  geom_vline(data = average_t,
             aes(xintercept = placebo_average),
             linetype = "longdash", colour = "black") +
  # geom_vline(data = q,
  #            aes(xintercept = quantile),
  #            linetype = "solid", colour = "black") +
  
  theme_bw()

hist_average

ggsave(hist_average, filename = here::here('04.analyze','synthetic_control',
                                      'add_outcome_predictor',
                                      'normal', 'normal_ando.png'))


average_t <- placebo_average_df %>% 
  dplyr::filter(.placebo == 0)

placebo

placebo_average_df$placebo_average


q_average <- data.frame(quantile = quantile(placebo_average_df$placebo_average,
                                            c(.025, .05, .95, .975)))

q_average

treatment_name_lists

q_list <- c(unlist(q_average))



for(i in treatment_name_lists){
  
  print(i)
}

for(i in treatment_name_lists){

  i_df <- average_t %>% 
    dplyr::filter(city_name == i) 
  
  i_num <- i_df$placebo_average

  if(i_num < q_list[2] | i_num > q_list[3]){
    
    print(i)
    
  }
  
  
}


sig_df <- average_t %>% 
  dplyr::filter(city_name %in%  c("檜山郡江差町",
                                  "檜山郡上ノ国町",
                                  "常呂郡訓子府町",
                                  "中川郡本別町",
                                  "足寄郡陸別町",
                                  "上北郡七戸町",
                                  "上北郡六戸町",
                                  "山県郡安芸太田町",
                                  "西臼杵郡高千穂町",
                                  "西臼杵郡日之影町"))




file_name <- paste0(city_name_t, ".rds")

base_plot <- readRDS(here::here('04.analyze','synthetic_control',
                                'add_outcome_predictor',
                                'table', file_name))
 

balance_table <-  
  base_plot %>% grab_balance_table() 


colnames(balance_table) <- c("variable",
                             "treated_city",
                             "synthetic_city",
                             "mean_control_group")

colnames(balance_table)

balance_table_4 <- balance_table %>% 
  dplyr::mutate(treated_city = round(treated_city, 4),
                synthetic_city= round(synthetic_city, 4),
                mean_control_group= round(mean_control_group, 4))

write.csv(balance_table_4, here::here('04.analyze','synthetic_control',
                                    'add_outcome_predictor',
                                    'balance_table', 'balance_table.csv'),
          fileEncoding = "CP932", row.names = FALSE)


p_hist <- ggplot(ten_after_df, aes(x = diff),fill="gray",
                 colour="black") +
  geom_histogram(bins = 30,fill="lightgray",
                 colour="black") +
  labs(x = "Placebo effect",
       y = "Frequency") +
  # scale_fill_manual(values=c("gray","black")) +
  # stat_function(fun = dnorm, 
  #               args = list(mean = mean(ten_after_df$diff),
  #                           sd = sd(ten_after_df$diff))) +
  # geom_line()+
  geom_vline(data = hist_df_t,
             aes(xintercept = diff),
             linetype = "longdash", colour = "black") +
  # geom_vline(data = q,
  #            aes(xintercept = quantile),
  #            linetype = "solid", colour = "black") +
  
  theme_bw()

p_hist

g <- hist(ten_after_df$diff)

f <- g$density

cc <- ten_after_df %>% 
  dplyr::arrange(diff) %>% 
  dplyr::mutate(percent = prop.table(diff)) %>% 
  dplyr::mutate(cum_percent = cumsum(percent))


ggsave(p_hist, filename = here::here('04.analyze','synthetic_control',
                                     'add_outcome_predictor',
                                     'normal', 'normal_add_bar.png'))

