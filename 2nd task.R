library(dplyr)
library(cluster)
library(factoextra)
library(NbClust)
set.seed(12345)
df=read.csv("Iris.csv",sep=",",header=TRUE)
sum(is.na(df))
colnames(df)
new_df<-df%>%
  dplyr::select(SepalLengthCm,SepalWidthCm,PetalLengthCm,PetalWidthCm)
#no null values
summary(new_df)
# no outliers
# here all the variables are numeric, so no need of dummy variables
# scaling 
new_df1 <- scale(new_df, center=TRUE, scale=TRUE)
head(new_df1)

# Elbow method
fviz_nbclust(new_df1, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")
# here we can say that the optimum number of clusters is 3, since bend is at 3.

#calculate gap statistic based on number of clusters
gap_stat <- clusGap(new_df1,
                    FUN = kmeans,
                    nstart = 25,
                    K.max = 10,
                    B = 50)

#plot number of clusters vs. gap statistic
fviz_gap_stat(gap_stat)
# here we can see that gap statistic is highest at k=3

# here we can say that the optimum number of clusters is 3

# representing it visually
fviz_cluster(kmeans(new_df1, centers = 3), geom = "point", data = new_df)
