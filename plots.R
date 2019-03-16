### Some plots

library(ggplot2)
library(ggthemes)
library(dplyr)

d$weekday <- strftime(d$Date, "%A")

d %>% mutate(miss = abs((Actual.TMR..lbs.)-as.numeric(TMR.Target))) %>%
  filter(Date > "2019-02-19") %>%
  mutate(weekday = factor(weekday,
                          levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))) %>%
  ggplot(aes(miss, color = weekday))+
  geom_histogram(bins = 20)+
  facet_wrap(~weekday)+
  theme_fivethirtyeight()+
  theme(legend.position = 'none')


d %>% mutate(miss = abs((Actual.TMR..lbs.)-as.numeric(TMR.Target))) %>%
  filter(Date > "2019-02-19") %>%
  group_by(weekday) %>%
  summarise(m = mean(miss, na.rm = T)) %>% 
  ungroup() %>%
  mutate(weekday = factor(weekday,
                          levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))) %>%
  ggplot(aes(weekday,m))+geom_col() + ylab("Average Absolute Feeding Error (lbs)") +
  theme_classic()+
  theme(axis.title.x = element_blank(), text = element_text(size = 14))
