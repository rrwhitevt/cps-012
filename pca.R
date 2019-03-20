library(factoextra)
library(reshape2)
library(tidyverse)
library(cluster)
library(fpc)

cowxday <- d2 %>%
  filter(Day >= 0) %>%
  group_by(Animal.ID) %>%
  mutate(Eff.max = ifelse(is.na(Eff.max), lag(Eff.max,1), Eff.max)) %>%
  dcast(Animal.ID + Top.Dress ~ Day, mean, value.var = 'Eff.max') 


td = "GH"
data.pca <- na.omit(cowxday %>% filter(Top.Dress == td) %>%
                      column_to_rownames('Animal.ID') %>% select(-Top.Dress))

gh.pca <- prcomp(data.pca)

mydata <- gh.pca$x[,c(1,2)]
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
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
d2 <- base::merge(d2, r, by = "Animal.ID")
