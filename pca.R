library(factoextra)
library(reshape2)
library(tidyverse)
library(cluster)
library(fpc)

cowxday <- d2 %>%
  filter(Day >= 0, Day < 8) %>%
  group_by(Animal.ID) %>%
  mutate(Eff.max = ifelse(is.na(Eff.max), lag(Eff.max,1), Eff.max)) %>%
  dcast(Animal.ID + Top.Dress ~ Day, mean, value.var = 'Eff.max') 


td = "TMR"
data.pca <- na.omit(cowxday %>% filter(Top.Dress == td) %>%
                      column_to_rownames('Animal.ID') %>% select(-Top.Dress))

gh.pca <- prcomp(data.pca)

mydata <- gh.pca$x[,c(1,2)]
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:8) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:8, wss, type="b", xlab="Number of Clusters",
     ylab="WGSS")

result = kmeans(mydata,centers = 4)$cluster
r = kmeans(mydata,centers = 4)$cluster %>%
  data.frame() %>%
  rownames_to_column("Animal.ID") %>%
  rename(TMR.group = '.') %>%
  mutate(Animal.ID = as.factor(Animal.ID))

# plot(mydata,col = result, pch = 19,
#      grid = FALSE, frame = FALSE)


fviz_pca_biplot(gh.pca,
              col.ind = as.factor(result), # Color by the quality of representation
              repel = TRUE,     # Avoid text overlapping
              title = paste("9-day PCA for ", td, sep = ""),
             legend.title = 'Groups'
             )
# d2 <- d2 %>%
#   mutate(Animal.ID = as.factor(Animal.ID))
d2 <- d2 %>%
  merge(r, by = "Animal.ID", all.x = T)

#######################################


td = "SBM"
data.pca <- na.omit(cowxday %>% filter(Top.Dress == td) %>%
                      column_to_rownames('Animal.ID') %>% select(-Top.Dress))

gh.pca <- prcomp(data.pca)

mydata <- gh.pca$x[,c(1,2)]
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:8) wss[i] <- sum(kmeans(mydata,
                                    centers=i)$withinss)
plot(1:8, wss, type="b", xlab="Number of Clusters",
     ylab="WGSS")

result = kmeans(mydata,centers = 3)$cluster
r = kmeans(mydata,centers = 3)$cluster %>%
  data.frame() %>%
  rownames_to_column("Animal.ID") %>%
  rename(SBM.group = '.') %>%
  mutate(Animal.ID = as.factor(Animal.ID))

# plot(mydata,col = result, pch = 19,
#      grid = FALSE, frame = FALSE)


fviz_pca_biplot(gh.pca,
                col.ind = as.factor(result), # Color by the quality of representation
                repel = TRUE,     # Avoid text overlapping
                title = paste("9-day PCA for ", td, sep = ""),
                legend.title = 'Groups'
)
# d2 <- d2 %>%
#   mutate(Animal.ID = as.factor(Animal.ID))
d2 <- d2 %>%
  merge(r, by = "Animal.ID", all.x = T)

#######################################
 
td = "CG"
data.pca <- na.omit(cowxday %>% filter(Top.Dress == td) %>%
                      column_to_rownames('Animal.ID') %>% select(-Top.Dress))

gh.pca <- prcomp(data.pca)

mydata <- gh.pca$x[,c(1,2)]
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:8) wss[i] <- sum(kmeans(mydata,
                                    centers=i)$withinss)
plot(1:8, wss, type="b", xlab="Number of Clusters",
     ylab="WGSS")

result = kmeans(mydata,centers = 3)$cluster
r = kmeans(mydata,centers = 3)$cluster %>%
  data.frame() %>%
  rownames_to_column("Animal.ID") %>%
  rename(CG.group = '.') %>%
  mutate(Animal.ID = as.factor(Animal.ID))

# plot(mydata,col = result, pch = 19,
#      grid = FALSE, frame = FALSE)


fviz_pca_biplot(gh.pca,
                col.ind = as.factor(result), # Color by the quality of representation
                repel = TRUE,     # Avoid text overlapping
                title = paste("9-day PCA for ", td, sep = ""),
                legend.title = 'Groups'
)
# d2 <- d2 %>%
#   mutate(Animal.ID = as.factor(Animal.ID))
d2 <- d2 %>%
  merge(r, by = "Animal.ID", all.x = T)

#################################################

td = "GH"
data.pca <- na.omit(cowxday %>% filter(Top.Dress == td) %>%
                      column_to_rownames('Animal.ID') %>% select(-Top.Dress))

gh.pca <- prcomp(data.pca)

mydata <- gh.pca$x[,c(1,2)]
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:8) wss[i] <- sum(kmeans(mydata,
                                    centers=i)$withinss)
plot(1:8, wss, type="b", xlab="Number of Clusters",
     ylab="WGSS")

result = kmeans(mydata,centers = 3)$cluster
r = kmeans(mydata,centers = 3)$cluster %>%
  data.frame() %>%
  rownames_to_column("Animal.ID") %>%
  rename(GH.group = '.') %>%
  mutate(Animal.ID = as.factor(Animal.ID))

# plot(mydata,col = result, pch = 19,
#      grid = FALSE, frame = FALSE)


fviz_pca_biplot(gh.pca,
                col.ind = as.factor(result), # Color by the quality of representation
                repel = TRUE,     # Avoid text overlapping
                title = paste("9-day PCA for ", td, sep = ""),
                legend.title = 'Groups'
)
# d2 <- d2 %>%
#   mutate(Animal.ID = as.factor(Animal.ID))
d2 <- d2 %>%
  merge(r, by = "Animal.ID", all.x = T)


## Get mean efficiency by group
d3 <- d2 %>%
  group_by(CG.group) %>%
  mutate(CG.mean.eff = ifelse(is.na(CG.group),NA,mean(Eff.max,na.rm = T))) %>%
  ungroup() %>%
  group_by(SBM.group) %>%
  mutate(SBM.mean.eff = ifelse(is.na(SBM.group),NA,mean(Eff.max,na.rm = T))) %>%
  ungroup() %>%
  group_by(GH.group) %>%
  mutate(GH.mean.eff = ifelse(is.na(GH.group),NA,mean(Eff.max,na.rm = T))) %>%
  ungroup() %>%
  group_by(TMR.group) %>%
  mutate(TMR.mean.eff = ifelse(is.na(TMR.group),NA,mean(Eff.max,na.rm = T))) %>%
  ungroup() %>%
  data.frame() 

d3$max <- max.col(-(replace(d3[,c("CG.mean.eff","SBM.mean.eff",'GH.mean.eff',"TMR.mean.eff")],
                          is.na(d3[,c("CG.mean.eff","SBM.mean.eff",'GH.mean.eff',"TMR.mean.eff")]),Inf)))
ggplot(d3, aes(max, color = max))+geom_histogram()
