# setwd("C:/Users/Doug/Documents/PhD Papers/Tanner/cps-012")

library(googlesheets)
library(ggplot2)
library(ggthemes)
library(dplyr)


fl <- list.files("./files",full.names = T)

for(i in fl) {
  header1 <- read.DIF(i, nrows = 1, header = F, transpose = TRUE)
  header2 <- read.DIF(i, skip = 1, nrows = 1, header = F, transpose = TRUE)
  dt <- read.DIF(i, skip = 2, header=FALSE, transpose = TRUE)
  names(dt) <- trimws(paste(t(header1), t(header2)))
  
  dt$Date <- paste(substring(i, 16+8, 17+8), "-", substring(i, 13+8, 14+8), "-", substring(i, 19+8, 22+8), sep="")
  
  if( i == fl[1]) {out <- dt} else {out <- rbind(out, dt)}
  
}

names(out) <- c("Index", "Animal.ID", "Gyn", "Lact.no", "DIM","DDO", "MY.m1", "MY.m2", "MY", "Date")

out <- subset(out, is.na(out$Index)==FALSE)
out$Date <- as.Date(out$Date, "%m-%d-%Y")



mysheets <- gs_ls()

gsd1 <- (gs_title("E012 - Data"))

t <- data.frame(gs_read(gsd1, ws="Intakes"))
t$Date <- as.Date(t$Date, "%m-%d-%Y")
# t$Animal.ID <- as.factor(t$Animal.ID)


d <- merge(out, t, by=c("Date", "Animal.ID"))


### Add cow info 
cowAtts <- read.csv('cowAtts.csv')
d <- merge(d, cowAtts[,c("ID","LacNO", "BW", "X.Fat", "X.Prt")], by.x = "Animal.ID", by.y = "ID")



d2 <- d %>% 
  mutate(DIM = as.numeric(DIM),
         MY.m1 = as.numeric(as.character(MY.m1)),
         MY.m2 = as.numeric(as.character(MY.m2)),
         MY = as.numeric(as.character(MY)),
         Period = as.factor(Period),
         Top.Dress = as.factor(Top.Dress),
         TMR.Adjustment = as.numeric(TMR.Adjustment),
         NEl.Target = as.numeric(NEl.Target),
         TMR.Target = as.numeric(TMR.Target),
         CG.Target = as.numeric(CG.Target),
         SBM.Target = as.numeric(SBM.Target),
         GH.Target = as.numeric(GH.Target),
         Top.Dress.pct = (CG.Target+SBM.Target+GH.Target)/TMR.Target) %>%
  # 2x single milk measurements
  mutate(MY.adj = ifelse(is.na(MY.m1+MY.m2),
                         (ifelse(is.na(MY.m1),0,MY.m1)+ifelse(is.na(MY.m2),0,MY.m2))*2, MY),
         MY.adj = replace(MY.adj, MY.adj == 0, NA)) %>% 
  group_by(Animal.ID) %>%
  mutate(MY.shift = lead(MY.adj, 1),
         Refusal.shift= lead(Refusal..lbs.,1),
         Eff = (Total.Actual-Refusal.shift)/MY.shift,
         Eff.max = Eff/max(Eff,na.rm = T)) %>%
  data.frame() %>%
  group_by(Animal.ID) %>%
  mutate(MY.pct.max = MY.adj/max(MY.adj, na.rm = T)) %>%
  ggplot(aes(Date, Eff.max, color = Top.Dress)) +
  geom_point() + 
  facet_wrap(~Animal.ID)

library(lme4)
library(lmerTest)

m1 <- lmer(Eff.max ~ TMR.Target+ CG.Target + SBM.Target + GH.Target + as.factor(LacNO) + (1|Animal.ID), data = d2)
anova(m1)


  
