#0. Identificacion base de datos---
#ICTWSS procesada y analisis en "Analisis descriptivo- Artículo 2"

#1. Cargar librerías y base de datos ------

#Librerias
pacman::p_load(dplyr, #manip
               tidyverse,#manip
               car, #manip
               summarytools, #descr
               VIM,
               ggpubr,
               sjmisc,
               sjPlot,
               corrplot,
               factoextra, #clusteranalysis
               NbClust) #clusteranalysis

#Bases de datos

load(file = "../Input/Data_proc/2019/data_naomit2019.RData")

df <- df1; remove(df1)


#2. Explorar base de datos -----

names(df)
dim(df)

# 3. K means -----

# 3.1 Cluster for first year -----

df1 <- df %>% filter(year == 2002) %>% tibble::column_to_rownames(var = "country")

df1 <- scale(df)

# 3.1.1 Define n° cluster

#One fundamental question is: How to choose the right number of expected clusters (k)?

#Elbow method
 
fviz_nbclust(df1, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)+
  labs(subtitle = "Elbow method")

#Knee in 3

# Silhouette method
fviz_nbclust(df1, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")
# In 3

#3 cluster

# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)

fviz_nbclust(df1, kmeans, nstart = 25,  method = "gap_stat", nboot = 500)+
  labs(subtitle = "Gap statistic method")

# 4. NBclust

set.seed(1)

a <- NbClust(data = df1, diss = NULL, distance = "euclidean",
        min.nc = 2, max.nc = 15, method = "kmeans")


### Compte k-means ----

# Compute k-means with k = 3
set.seed(123)
km <- kmeans(df1, 3, nstart = 25)
print(km)


#Aggregate

aggregate(df1, by=list(cluster=km$cluster), mean)


#Visualize

fviz_cluster(km, data = df1, geom = "point",
            stand = FALSE, frame.type = "norm") + theme_bw()


##### Two indicators

df2 <- scale(df1[,c(5,12)])

# 3.1.1 Define n° cluster

#One fundamental question is: How to choose the right number of expected clusters (k)?

#Elbow mwthod
fviz_nbclust(df2, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)+
  labs(subtitle = "Elbow method")

#Knee in 3

# Silhouette method
fviz_nbclust(df2, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")
# In 7

#3 cluster

# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)

fviz_nbclust(df1, kmeans, nstart = 25,  method = "gap_stat", nboot = 500)+
  labs(subtitle = "Gap statistic method")

# 4. NBclust

set.seed(1)

a <- NbClust(data = df2, diss = NULL, distance = "euclidean",
             min.nc = 2, max.nc = 15, method = "kmeans")

#3

### Compte k-means ----

# Compute k-means with k = 3
set.seed(123)
km2 <- kmeans(df2, 3, nstart = 25)
print(km2)


#Aggregate

aggregate(df2, by=list(cluster=km2$cluster), mean)


#Visualize

g200A <- fviz_cluster(km2, data = df2, geom = "point",
             stand = FALSE, frame.type = "norm") + theme_bw


#Con km1 y cluster con MWS - abudsex
g2002B <- fviz_cluster(object = km, data = df1, geom = "point",
             choose.vars = c("MWS", "absudsex"), stand = FALSE, 
             ellipse.type = "norm") + theme_bw() + labs(title = "2002")


g2002B
