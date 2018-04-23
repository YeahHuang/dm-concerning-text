
# Problem 1
> setwd(“~/Desktop")                    #change the working place
> wine <- read.csv(“wine.csv”, header=TRUE)     #read in the data and save it in wine
> standardWine<-scale(wine)             #normalize

# Problem 2
> kmeansWine=kmeans(standardWine,3)
> library(cluster)
> daisyWine=daisy(standardWine)         #calculate the relative distance in data
> silhouetteWine=silhouette(kmeansWine$cluster,daisyWine) #calculate silhouette score of data
> tmp <- summary(silhouetteWine)            #The ‘mean’ value of summary() is the answer.
> tmp                           #print tmp

# Problem 3 
# This part should be iterated for several times to avoid random initialized status. 
>install.packages(“fpc”)
>library(fpc)   #With the help of package”fpc”, below is another approach for calculating the silhouette score of the k-clustering method.
> begin=2;
> end=2;
> result=c();
> result[begin:end]=-1;
> for(i in begin:end){                          #“i” is the number of clustering
+    tmpKmeansWine=kmeans(standardWine,i);
+    tmpStats=cluster.stats(dist(standardWine),tmpKmeansWine$cluster)
+    result[i]=tmpStats$avg.silwidth                #result[i] is the silhouette score of i
+ }
> plot(result, type="o",xlab="Number of Cluster",ylab="Silhouette Coefficient”);

#Problem 4 
> wine$Wine=0                         #To eliminate the influence of wine$Wine
> standardWine1=scale(wine)             
> distWine1=dist(as.matrix(standardWine1))
> hclustComplete1<-hclust(distWine1,method="complete")
> cutree(hclustComplete1,k=3)
> hclustAverage=hclust(distWine1, method=“average")
> cutree(hclustAverage,k=3)
> hclustSingle=hclust(distWine1, method="single")
> cutree(hclustSingle, k=3)

# Problem 5
> plot(hclustComplete1,hang=-1);            # use hang=-1 to avoid the overlap in image
> plot(hclustAverage,hang=-1);
> plot(hclustSingle, hang=-1)