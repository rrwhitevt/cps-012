# setwd("C:/Users/Doug/Documents/PhD Papers/Tanner/cps-012")
# setwd("C:/Users/rrwhite/Dropbox/Tanners Cows")

library(googlesheets)
library(ggplot2)
library(ggthemes)
library(dplyr)


fl <- list.files("./files",full.names = T)
difs <- subset(fl, substring(fl, nchar(fl)-3,nchar(fl))==".dif")
txts <- subset(fl, substring(fl, nchar(fl)-3, nchar(fl))==".Txt")

for(i in txts) {
  dt <- read.delim(i)
  dt$Animal.ID <- substring(i, 9,12)
  if(i == txts[1]) {nout <- dt} else {nout <- rbind(nout, dt)}
}

nout$Date <- as.Date(nout$Date, "%d/%m/%Y") # paste(substring(nout$Date, 4,5), "-", substring(nout$Date, 1,2), "-", substring(nout$Date, 7,10), sep="")

for(i in difs) {
  header1 <- read.DIF(i, nrows = 1, header = F, transpose = TRUE)
  header2 <- read.DIF(i, skip = 1, nrows = 1, header = F, transpose = TRUE)
  dt <- read.DIF(i, skip = 2, header=FALSE, transpose = TRUE)
  names(dt) <- trimws(paste(t(header1), t(header2)))
  
  dt$Date <- paste(substring(i, 16+8, 17+8), "-", substring(i, 13+8, 14+8), "-", substring(i, 19+8, 22+8), sep="")
  
  if( i == difs[1]) {
    out <- dt
  } else if(length(header1)<15) {
      out <- rbind(out, dt)
  } else if(i == "./files/C-D 3 G-8 - 19-03-2019 14-22.dif") {
        lout <- dt
  } else {
          lout<-rbind(lout, dt)
          }
  
}

names(out) <- c("Index", "Animal.ID", "Gyn", "Lact.no", "DIM","DDO", "MY.m1", "MY.m2", "MY", "Date")
out <- subset(out, is.na(out$Index)==FALSE)
out$Date <- as.Date(out$Date, "%m-%d-%Y")
nout$Date <- as.Date(nout$Date, "%m-%d-%Y")
out <- merge(out, nout, by=c("Animal.ID", "Date"))

names(lout) <- c("Index", "Animal.ID", "Gyn", "Lact.no", "DIM", "DDO", "MY.m1", "MY.m2", "MY", "Protein", "Fat", "Blood", "Lactose", "Activity", "X", "Date")
lout$Days <- NA
lout$Milk <- as.numeric(lout$MY.m1)+as.numeric(lout$MY.m2)
lout$Weight <- NA
lout$Date <- as.Date(lout$Date, "%m-%d-%Y")
out <- rbind(out, lout)

mysheets <- gs_ls()

gsd1 <- (gs_title("E012 - Data"))

t <- data.frame(gs_read(gsd1, ws="Intakes"))
t$Date <- as.Date(t$Date, "%m-%d-%Y")
# t$Animal.ID <- as.factor(t$Animal.ID)


d <- merge(out, t, by=c("Date", "Animal.ID"))

head(d)

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
         Eff = MY.shift/(Total.Actual-Refusal.shift),
         EffR = (Eff-mean(Eff, na.rm=T))/sd(Eff, na.rm=T)
         )
d2 <- data.frame(d2)
d2 <- subset(d2, d2$EffR<2.0 & d2$EffR>-2.0 & d2$Eff>0.2 & d2$Eff<1.2)

d2 <- d2 %>%
  group_by(Animal.ID) %>%
  mutate(
  Eff.max= Eff/max(Eff, na.rm=T),
  MY.pct.max = MY.adj/max(MY.adj, na.rm = T)) %>%
  ggplot(aes(Date, Eff.max, color = Top.Dress)) +
  geom_point() + 
  facet_wrap(~Animal.ID)

d3 <- data.frame(d2$data)
ggplot(d3, aes(Date, Eff.max, color = Top.Dress)) +
  geom_point() + 
  facet_wrap(~Animal.ID)
  
library(lme4)
library(lmerTest)

m1 <- lmer(Eff.max ~ TMR.Target+ CG.Target + SBM.Target + GH.Target + as.factor(LacNO) + (1|Animal.ID), data = d2)
anova(m1)


  
