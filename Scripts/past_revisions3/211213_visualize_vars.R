

x <-readRDS('./Data/211214_df.rds') %>% 
  filter(AGE1B_XRND > 0 &
           !racenew %in% "Other")

fp <- ggplot(x, aes(x = AGE1B_XRND, group = racenew, fill = racenew)) + 
  geom_density(alpha = 0.4) +
  ylab("Density") +
#  xlab("Age at first birth") +
  ggtitle("Women") +
  scale_x_continuous(limits = c(16,55)) +
  scale_fill_manual(values = c("Red","#FFAA00","Green"),
                    name = "") +
  theme_ipsum(axis_title_size = 15,
              plot_title_size = 15,
              strip_text_size = 15, 
              base_size = 10,
              plot_margin = margin(10,10,10,10)) +
  theme(legend.position = c(0.75, .55),
        legend.text = element_text(size = 15))



x <-readRDS('./Data/211214_dm.rds') %>% 
  filter(AGE1B_XRND > 0 &
           !racenew %in% "Other")

mp <- ggplot(x, aes(x = AGE1B_XRND, group = racenew, fill = racenew)) + 
  geom_density(alpha = 0.4) +
  # ylab("Density") +
  xlab("Age at first birth") +
  ggtitle("Men") +
  scale_x_continuous(limits = c(16,55)) +
  scale_fill_manual(values = c("Red","#FFAA00","Green"),
                    name = "") +
  theme_ipsum(axis_title_size = 15,
              plot_title_size = 15,
              strip_text_size = 15, 
              base_size = 10,
              plot_margin = margin(10,10,10,10)) +
  theme(legend.position = c(0.75, .55),
        legend.text = element_text(size = 15))