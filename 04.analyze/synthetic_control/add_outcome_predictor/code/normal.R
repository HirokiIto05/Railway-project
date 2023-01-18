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

ks.test(placebo_average_df$placebo_average, "pnorm", 
        mean = mean(placebo_average_df$placebo_average),
        sd = sd(placebo_average_df$placebo_average))




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

cdf_p <-  ggplot(ten_after_df, aes(x = diff))+
  stat_ecdf(pad = FALSE)

h <- hist(ten_after_df$diff)
cum.prob <- data.frame(value=h$mids, prob=cumsum(h$counts)/sum(h$counts))


cdf_p$data

cdf_df <- ecdf(ten_after_df$diff)


cdf_df(-0.050157501)


q <- data.frame(quantile = quantile(ten_after_df$diff,c(.05, .10, .90, .95)))

class(q)

class(cdf_df)

nn <- as.data.frame(rnorm(10000, mean = 5, sd = 10))
colnames(nn) <- "N"

ks.test(nn$N, "pnorm", mean = 5, sd = 10)

# bb <- tibble(B = rnorm(945, mean = mean(ten_after_df$diff),
                       # sd = sd(ten_after_df$diff)))

ks.test(bb$B, ten_after_df$diff)

ggplot(bb, aes(x = B))+
  geom_histogram()



ks.test(ten_after_df$diff, 
        "pnorm", 
        mean = mean(ten_after_df$diff),
        sd = sd(ten_after_df$diff))


ggplot(ten_after_df, aes(x = diff))+
  geom_histogram()


ks.test(ten_after_df$diff, "pnorm", mean)

ks.test(ten_after_df$diff, "pnorm", mean=mean(ten_after_df$diff), sd=sd(ten_after_df$diff),
        alternative = "two.sided")


tapply(ten_after_df$diff, bb$B, shapiro.test)

ks.test(normal_df$diff, "pnorm", alternative = "two.sided")

help(ks.test)

p_hist

t.test(ten_after_df$diff, mu =  mean_p, alternative = "two.side")

mean_p = mean(ten_after_df$diff)
sd_p = sd(ten_after_df$diff)

mean_p - 1.65*sd_p
mean_p + 1.65*sd_p

normal_df <- ten_after_df %>% 
  mutate(normal_diff =((diff - mean_p)/sd_p))

n_hist <- ggplot(normal_df, aes(x = normal_diff)) +
  geom_histogram(mapping = aes(y=..density..), 
                 fill="lightgray",
                 colour="lightgray",
                 bins = 50) +
  # scale_fill_manual(values=c("gray","black")) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(normal_df$normal_diff),
                            sd = sd(normal_df$normal_diff))) +
  # geom_vline(data = hist_df_t,               
  #            aes(xintercept = diff),  
  #            linetype = "longdash", colour = "black") +
  theme_bw()

n_hist

mean(normal_df$normal_diff)
sd(normal_df$normal_diff)

treated_diff_list

cum_p <-  ggplot(ten_after_df, aes(x = diff))+
  stat_ecdf(pad = FALSE) +
  geom_vline(data = hist_df_t,               
             aes(xintercept = diff),  
             linetype = "longdash", colour = "black") +
  theme_bw()

cum_df <- cum_p$data
  

t.test(ten_after_df$diff, mu=mean(ten_after_df$diff), alternative="two.sided")

p_hist

hist_df_t <- ten_after_df %>% 
  dplyr::filter(.placebo == 0)

treated_diff_list <- hist_df_t$diff

# hist_df_c <- ten_after_df %>% 
#   dplyr::filter(.placebo == 1)
# 
# p_hist <- ggplot() +
#   geom_histogram(data = hist_df_t, aes(x = diff), fill="red") +
#   geom_histogram(data = hist_df_c,aes(x = diff),  fill="gray") +
#   # stat_function(fun = dnorm, args = list(mean = mean(ten_after_df$diff), sd = sd(ten_after_df$diff)))+
#   theme_bw()

p_hist

p_hist_a <- ggplot(ten_after_df, aes(x = diff)) +
  geom_histogram()
  

p_hist_a + p_hist

diff_alone <- ten_after_df %>% 
  distinct(diff)

ks.test(diff_alone, "pnorm", alternative = "two.sided")

