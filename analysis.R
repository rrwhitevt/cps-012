setwd("C:/Users/Doug/Documents/PhD Papers/Tanner/files")

fl <- list.files()

for(i in fl) {
  header1 <- read.DIF(i, nrows = 1, header = F, transpose = TRUE)
  header2 <- read.DIF(i, skip = 1, nrows = 1, header = F, transpose = TRUE)
  dt <- read.DIF(i, skip = 2, header=FALSE, transpose = TRUE)
  names(dt) <- trimws(paste(t(header1), t(header2)))
  
  dt$Date <- paste(substring(i, 16, 17), "-", substring(i, 13, 14), "-", substring(i, 19, 22), sep="")
  
  if( i == fl[1]) {out <- dt} else {out <- rbind(out, dt)}
  
}

names(out) <- c("Index", "Animal.ID", "Gyn", "Lact.no", "DIM","DDO", "MY.m1", "MY.m2", "MY", "Date")

out <- subset(out, is.na(out$Index)==FALSE)
out$Date <- as.Date(out$Date, "%m-%d-%Y")


mysheets <- gs_ls()

gsd1 <- (gs_title("E012 - Data"))

t <- data.frame(gs_read(gsd1, ws="Intakes"))
t$Date <- as.Date(t$Date, "%m-%d-%Y")


d <- merge(out, t, by=c("Date", "Animal.ID"))


### Some plots

library(ggplot2)
library(ggthemes)
library(dplyr)

d %>% mutate(miss = abs((Actual.TMR..lbs.)-as.numeric(TMR.Target))) %>%
  filter(Date > "2019-02-19") %>%
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
