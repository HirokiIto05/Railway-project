

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
             linetype = "solid", colour = "gray") +
  geom_vline(data = q,
             aes(xintercept = quantile),
             linetype = "solid", colour = "black") +
  
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

bb <- tibble(B = rnorm(945, mean = mean(ten_after_df$diff),
                       sd = sd(ten_after_df$diff)))

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

