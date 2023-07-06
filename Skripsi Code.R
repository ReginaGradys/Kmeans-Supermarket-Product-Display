#Descriptive Statistics
library(dplyr)
library(amap)
library(fpc)
library(readxl)

data_tr <- read_excel("data_gatra.xlsx")
data_tr

boxplot(data_tr, xlab="variabel")

#KMO Test
kmo<-function(x)
{
  x<-subset(x,complete.cases(x))
  r<-cor(x)
  r2<-r^2
  i<-solve(r)
  d<-diag(i)
  p2<-(-i/sqrt(outer(d,d)))^2
  diag(r2)<-diag(p2)<-0
  KMO<-sum(r2)/(sum(r2)+sum(p2))
  MSA<-colSums(r2)/(colSums(r2)+colSums(p2))
  return(list(KMO=KMO, MSA=MSA))
}
kmo(data_tr)

#Correlation 
cor(data_tr, round(data_tr,3))

#Kmeans Euclidean
#Kmeans using euclidean distance
set.seed(14)
k_euc<-Kmeans(data_tr, 3, iter.max=15, nstart=1, method="euclidean")
k_euc

#dunn index and silhouette coefficient
datatr<-as.matrix(data_tr)
rownames(datatr)<-datatr[1:130]
vale<-clValid(datatr,3,clMethods="kmeans",validation="internal",metric="euclidean")
summary(vale)


#Kmeans using manhattan distance
set.seed(14)
k_man<-Kmeans(data_tr,3, iter.max=15, nstart=1, method="manhattan")
k_man

#dunn index and silhouette coefficient
valm<-clValid(datatr,3,clMethods="kmeans",validation="internal",metric="manhattan")
summary(valm)

#cluster interpretation
data_int<-cbind(Data_transaksi, k_euc$cluster)
data_in<-cbind(Data_transaksi,k_man$cluster)
options(max.print=3000)
data_int
result<-as_tibble(data_int)
res<-as_tibble(data_in)
print(result, n=130)
c1=as.matrix(filter(result,k_euc$cluster==1))
c1
c2=as.matrix(filter(result,k_euc$cluster==2))
c2
c3=as.matrix(filter(result,k_euc$cluster==3))
c3
c3$TRANSAKSI
print(summary(c1[,1:11]))
print(summary(c2[,1:11]))
print(summary(c3[,1:11]))

c1=as.matrix(filter(res,k_man$cluster==1))
c1
c2=as.matrix(filter(res,k_man$cluster==2))
c2
c3=as.matrix(filter(res,k_man$cluster==3))
c3

summary(data_tr)
