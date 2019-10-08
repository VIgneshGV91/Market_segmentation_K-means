library(readxl)
BathSoap_Data <- read_excel("C:/Users/vigne/Documents/BathSoap_Data.xls",sheet = "DM_Sheet")
View(BathSoap_Data)

data <- BathSoap_Data

bd <- BathSoap_Data

#We need to normalize the data before clustering:

#Creating a Function to perform normalization

Norm_data <- function(a){
  numerator <- a - min(a,na.rm = TRUE)
  denominator <- max(a,na.rm = TRUE) - min(a,na.rm = TRUE)
  return(numerator/denominator)
}

bd_normalized <- as.data.frame(lapply(bd[1:46],Norm_data))

View(bd_normalized)

#To find Maximum Brand Loyalty we need to find Max to one brand which can be found by calculating the Max of all the brands except Others999

brandsdf <- as.data.frame(lapply(bd[23:30],Norm_data))

View(brandsdf)

#To find Max to one brand

MtOne <- as.data.frame(apply(brandsdf, 1, max))

View(MtOne)


pur_beh_vars <- bd_normalized[,c(12,13,14,15,16,19,31)]

View(pur_beh_vars)

pur_beh_vars <- cbind(pur_beh_vars,MtOne)

#Renaming the column name:

colnames(pur_beh_vars)[colnames(pur_beh_vars)=="apply(brandsdf, 1, max)"] <- "max_to_one_brand"



table(is.na.data.frame(pur_beh_vars))
pur_beh_vars <- na.omit(pur_beh_vars)


#Check for the optimal number of clusters given the data

wss <- (nrow(pur_beh_vars)-1)*sum(apply(pur_beh_vars,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(pur_beh_vars, centers=i)$withinss)
wss
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method", pch=20, cex=2)


#k=10
set.seed(30)
km10 = kmeans(pur_beh_vars, 10, nstart=100)
km10


col =(km10$cluster +1)

plot(pur_beh_vars, col = col , main="K-Means result with 3 clusters", pch=20, cex=2)




#k=2
set.seed(30)
km2 = kmeans(pur_beh_vars, 2, nstart=100)
km2

col =(km2$cluster +1)

plot(pur_beh_vars, col = col , main="K-Means result with 2 clusters", pch=20, cex=2)


#k=3
set.seed(30)
km3 = kmeans(pur_beh_vars, 3, nstart=100)
km3


pur_beh_vars <- cbind(pur_beh_vars,as.data.frame(km3$cluster))

View(pur_beh_vars)

cluster.stats(pur_beh_vars, km3$cluster)

col =(km3$cluster +1)

plot(pur_beh_vars, col = col , main="K-Means result with 3 clusters", pch=20, cex=2)


#k=4
set.seed(30)
km4 = kmeans(pur_beh_vars, 4, nstart=100)
km4

pur_beh_vars <- cbind(pur_beh_vars,as.data.frame(km4$cluster))

cluster.stats(pur_beh_vars, km4$cluster)

col =(km4$cluster +1)

plot(pur_beh_vars, col = col , main="K-Means result with 4 clusters", pch=20, cex=2)


#k=5
set.seed(30)
km5 = kmeans(pur_beh_vars, 5, nstart=100)
km5

col =(km5$cluster +1)

plot(pur_beh_vars, col = col , main="K-Means result with 5 clusters", pch=20, cex=2)



#k=6
set.seed(30)
km6 = kmeans(pur_beh_vars, 6, nstart=100)
km6


col =(km6$cluster +1)

plot(pur_beh_vars, col = col , main="K-Means result with 6 clusters", pch=20, cex=2)


#k=7
set.seed(30)
km7 = kmeans(pur_beh_vars, 7, nstart=100)
km7


col =(km7$cluster +1)

plot(pur_beh_vars, col = col , main="K-Means result with 6 clusters", pch=20, cex=2)




#1.b)The variables that describe basis-for-purchase.
#Pur-vol-no-promo, Pur-vol-promo-6, Pur-vol-other, all price categories, selling propositions]

bop_vars <- bd_normalized[,c(20,21,22,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46)]
View(bop_vars)

table(is.na.data.frame(bop_vars))
bop_vars <- na.omit(bop_vars)

#Check for the optimal number of clusters given the data

wss <- (nrow(bop_vars)-1)*sum(apply(bop_vars,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(bop_vars, centers=i)$withinss)
wss
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method", pch=20, cex=2)


dev.off()
par(mfrow=c(4,3))
plot(bop_vars$PropCat.5)
plot(bop_vars$PropCat.6)
plot(bop_vars$PropCat.7)
plot(bop_vars$PropCat.8)
plot(bop_vars$PropCat.9)
plot(bop_vars$PropCat.10)
plot(bop_vars$PropCat.11)
plot(bop_vars$PropCat.12)
plot(bop_vars$PropCat.13)
plot(bop_vars$PropCat.14)
plot(bop_vars$PropCat.15)


dev.off()
par(mfrow=c(4,3))
hist(bop_vars$PropCat.5)
hist(bop_vars$PropCat.6)
hist(bop_vars$PropCat.7)
hist(bop_vars$PropCat.8)
hist(bop_vars$PropCat.9)
hist(bop_vars$PropCat.10)
hist(bop_vars$PropCat.11)
hist(bop_vars$PropCat.12)
hist(bop_vars$PropCat.13)
hist(bop_vars$PropCat.14)
hist(bop_vars$PropCat.15)

#Neglecting PropCat 10, PropCat 11, PropCat 12, PropCat 13 and PropCat 15 

bop_vars_filtered <- bd_normalized[,c(20,21,22,32,33,34,35,36,37,38,39,40,45)]
View(bop_vars_filtered)

str(bop_vars_filtered)

table(is.na.data.frame(bop_vars_filtered))
bop_vars_filtered <- na.omit(bop_vars_filtered)

#Check for the optimal number of clusters given the data

wss <- (nrow(bop_vars_filtered)-1)*sum(apply(bop_vars_filtered,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(bop_vars_filtered, centers=i)$withinss)
wss
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method", pch=20, cex=2)

#k=9
set.seed(30)
km9 = kmeans(bop_vars_filtered, 9, nstart=100)
km9


col =(km9$cluster +1)

plot(bop_vars_filtered, col = col , main="K-Means result with 9 clusters", pch=20, cex=2)

#k=2
set.seed(30)
km2 = kmeans(bop_vars_filtered, 2, nstart=100)
km2



col =(km2$cluster +1)

plot(bop_vars_filtered, col = col , main="K-Means result with 2 clusters", pch=20, cex=2)


#k=3
set.seed(30)
km3 = kmeans(bop_vars_filtered, 3, nstart=100)
km3



col =(km3$cluster +1)

plot(bop_vars_filtered, col = col , main="K-Means result with 3 clusters", pch=20, cex=2)


#k=4
set.seed(30)
km4 = kmeans(bop_vars_filtered, 4, nstart=100)
km4



col =(km4$cluster +1)

plot(bop_vars_filtered, col = col , main="K-Means result with 4 clusters", pch=20, cex=2)


#k=5
set.seed(30)
km5 = kmeans(bop_vars_filtered, 5, nstart=100)
km5



col =(km5$cluster +1)

plot(bop_vars_filtered, col = col , main="K-Means result with 5 clusters", pch=20, cex=2)


#k=6
set.seed(30)
km6 = kmeans(bop_vars_filtered, 6, nstart=100)
km6


col =(km6$cluster +1)

plot(bop_vars_filtered, col = col , main="K-Means result with 6 clusters", pch=20, cex=2)


#k=7
set.seed(30)
km7 = kmeans(bop_vars_filtered, 7, nstart=100)
km7


col =(km7$cluster +1)

plot(bop_vars_filtered, col = col , main="K-Means result with 7 clusters", pch=20, cex=2)

str(bop_vars_filtered)



#1.c)

allvars <- cbind(pur_beh_vars,bop_vars_filtered)
str(allvars)
View(allvars)

table(is.na.data.frame(allvars))
allvars <- na.omit(allvars)


#Check for the optimal number of clusters given the data

wss <- (nrow(allvars)-1)*sum(apply(allvars,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(allvars, centers=i)$withinss)
wss
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method", pch=20, cex=2)


#k=13
set.seed(30)
km13 = kmeans(allvars, 13, nstart=100)
km13


col =(km13$cluster +1)

plot(allvars, col = col , main="K-Means result with 13 clusters", pch=20, cex=2)


#k=2
set.seed(30)
km2 = kmeans(allvars, 2, nstart=100)
km2


col =(km2$cluster +1)

plot(allvars, col = col , main="K-Means result with 2 clusters", pch=20, cex=2)


#k=3
set.seed(30)
km3 = kmeans(allvars, 3, nstart=100)
km3



col =(km3$cluster +1)

plot(allvars, col = col , main="K-Means result with 3 clusters", pch=20, cex=2)



#k=4
set.seed(30)
km4 = kmeans(allvars, 4, nstart=100)
km4



col =(km4$cluster +1)

plot(allvars, col = col , main="K-Means result with 4 clusters", pch=20, cex=2)



#k=5
set.seed(30)
km5 = kmeans(allvars, 5, nstart=100)
km5



col =(km5$cluster +1)

plot(allvars, col = col , main="K-Means result with 5 clusters", pch=20, cex=2)



#2.b)

#creating dataframe of demographics
demo <- bd[,c(2,3,4,5,6,7,8,9,10,11)]
View(demo)


table(is.na.data.frame(demo))
demo <- na.omit(demo)

km5$cluster
allvars <- cbind(allvars,as.data.frame(km5$cluster))

allvars <- cbind(allvars,as.data.frame(demo))



#SEC Characteristics
dev.off()
allvars_cluster1 <-allvars[allvars$`km5$cluster` == 1, ]
par(mfrow=c(3,2))
ptab<-table(allvars_cluster1$SEC)
ptab<-prop.table(ptab)
ptab<-ptab*100 # Convert to percentages 
barplot(ptab, main = "Cluster-1", xlab = "SEC", ylab = "% of #Records", col=c("orange", "steelblue","green","purple"), ylim=c(0,50))



allvars_cluster2 <-allvars[allvars$`km5$cluster` == 2, ]
ptab<-table(allvars_cluster2$SEC)
ptab<-prop.table(ptab)
ptab<-ptab*100 # Convert to percentages 
barplot(ptab, main = "Cluster-2", xlab = "SEC", ylab = "% of #Records", col=c("orange", "steelblue","green","purple"), ylim=c(0,50))


allvars_cluster3 <-allvars[allvars$`km5$cluster` == 3, ]
ptab<-table(allvars_cluster3$SEC)
ptab<-prop.table(ptab)
ptab<-ptab*100 # Convert to percentages 
barplot(ptab, main = "Cluster-3", xlab = "SEC", ylab = "% of #Records", col=c("orange", "steelblue","green","purple"), ylim=c(0,50))



allvars_cluster4 <-allvars[allvars$`km5$cluster` == 4, ]
ptab<-table(pur_beh_vars_cluster4$SEC)
ptab<-prop.table(ptab)
ptab<-ptab*100 # Convert to percentages 
barplot(ptab, main = "Cluster-4", xlab = "SEC", ylab = "% of #Records", col=c("orange", "steelblue","green","purple"), ylim=c(0,50))



allvars_cluster5 <-allvars[allvars$`km5$cluster` == 5, ]
ptab<-table(allvars_cluster5$SEC)
ptab<-prop.table(ptab)
ptab<-ptab*100 # Convert to percentages 
barplot(ptab, main = "Cluster-5", xlab = "SEC", ylab = "% of #Records", col=c("orange", "steelblue","green","purple"), ylim=c(0,50))


#FEH Characteristics
dev.off()
par(mfrow=c(3,2))
ptab<-table(allvars_cluster1$FEH)
ptab<-prop.table(ptab)
ptab<-ptab*100 # Convert to percentages 
barplot(ptab, main = "Cluster-1", xlab = "FEH", ylab = "% of #Records", col=c("orange", "steelblue","green","purple"), ylim=c(0,60))


ptab<-table(allvars_cluster2$FEH)
ptab<-prop.table(ptab)
ptab<-ptab*100 # Convert to percentages 
barplot(ptab, main = "Cluster-2", xlab = "FEH", ylab = "% of #Records", col=c("orange", "steelblue","green","purple"), ylim=c(0,50))


ptab<-table(allvars_cluster3$FEH)
ptab<-prop.table(ptab)
ptab<-ptab*100 # Convert to percentages 
barplot(ptab, main = "Cluster-3", xlab = "FEH", ylab = "% of #Records", col=c("orange", "steelblue","green","purple"), ylim=c(0,50))


ptab<-table(allvars_cluster4$FEH)
ptab<-prop.table(ptab)
ptab<-ptab*100 # Convert to percentages 
barplot(ptab, main = "Cluster-4", xlab = "FEH", ylab = "% of #Records", col=c("orange", "steelblue","green","purple"), ylim=c(0,70))

ptab<-table(allvars_cluster5$FEH)
ptab<-prop.table(ptab)
ptab<-ptab*100 # Convert to percentages 
barplot(ptab, main = "Cluster-4", xlab = "FEH", ylab = "% of #Records", col=c("orange", "steelblue","green","purple"), ylim=c(0,70))



#MT Characteristics
dev.off()
par(mfrow=c(3,2))
ptab<-table(allvars_cluster1$MT)
ptab<-prop.table(ptab)
ptab<-ptab*100 # Convert to percentages 
barplot(ptab, main = "Cluster-1", xlab = "MT", ylab = "% of #Records", col=c("orange", "steelblue","green","purple"), ylim=c(0,60))

ptab<-table(allvars_cluster2$MT)
ptab<-prop.table(ptab)
ptab<-ptab*100 # Convert to percentages 
barplot(ptab, main = "Cluster-2", xlab = "MT", ylab = "% of #Records", col=c("orange", "steelblue","green","purple"), ylim=c(0,60))

ptab<-table(allvars_cluster3$MT)
ptab<-prop.table(ptab)
ptab<-ptab*100 # Convert to percentages 
barplot(ptab, main = "Cluster-3", xlab = "MT", ylab = "% of #Records", col=c("orange", "steelblue","green","purple"), ylim=c(0,50))

ptab<-table(allvars_cluster4$MT)
ptab<-prop.table(ptab)
ptab<-ptab*100 # Convert to percentages 
barplot(ptab, main = "Cluster-4", xlab = "MT", ylab = "% of #Records", col=c("orange", "steelblue","green","purple"), ylim=c(0,70))

ptab<-table(allvars_cluster5$MT)
ptab<-prop.table(ptab)
ptab<-ptab*100 # Convert to percentages 
barplot(ptab, main = "Cluster-5", xlab = "MT", ylab = "% of #Records", col=c("orange", "steelblue","green","purple"), ylim=c(0,50))


#Gender Characteristics
dev.off()
par(mfrow=c(3,2))
ptab<-table(allvars_cluster1$SEX)
ptab<-prop.table(ptab)
ptab<-ptab*100 # Convert to percentages 
barplot(ptab, main = "Cluster-1", xlab = "SEX", ylab = "% of #Records", col=c("orange", "steelblue","green","purple"), ylim=c(0,100))

ptab<-table(allvars_cluster2$SEX)
ptab<-prop.table(ptab)
ptab<-ptab*100 # Convert to percentages 
barplot(ptab, main = "Cluster-2", xlab = "SEX", ylab = "% of #Records", col=c("orange", "steelblue","green","purple"), ylim=c(0,100))

ptab<-table(allvars_cluster3$SEX)
ptab<-prop.table(ptab)
ptab<-ptab*100 # Convert to percentages 
barplot(ptab, main = "Cluster-3", xlab = "SEX", ylab = "% of #Records", col=c("orange", "steelblue","green","purple"), ylim=c(0,100))

ptab<-table(allvars_cluster4$SEX)
ptab<-prop.table(ptab)
ptab<-ptab*100 # Convert to percentages 
barplot(ptab, main = "Cluster-4", xlab = "SEX", ylab = "% of #Records", col=c("orange", "steelblue","green","purple"), ylim=c(0,100))

ptab<-table(allvars_cluster5$SEX)
ptab<-prop.table(ptab)
ptab<-ptab*100 # Convert to percentages 
barplot(ptab, main = "Cluster-5", xlab = "SEX", ylab = "% of #Records", col=c("orange", "steelblue","green","purple"), ylim=c(0,100))



#AGE Characteristics
dev.off()
par(mfrow=c(3,2))
ptab<-table(allvars_cluster1$AGE)
ptab<-prop.table(ptab)
ptab<-ptab*100 # Convert to percentages 
barplot(ptab, main = "Cluster-1", xlab = "AGE", ylab = "% of #Records", col=c("orange", "steelblue","green","purple"), ylim=c(0,60))

ptab<-table(allvars_cluster2$AGE)
ptab<-prop.table(ptab)
ptab<-ptab*100 # Convert to percentages 
barplot(ptab, main = "Cluster-2", xlab = "AGE", ylab = "% of #Records", col=c("orange", "steelblue","green","purple"), ylim=c(0,60))

ptab<-table(allvars_cluster3$AGE)
ptab<-prop.table(ptab)
ptab<-ptab*100 # Convert to percentages 
barplot(ptab, main = "Cluster-3", xlab = "AGE", ylab = "% of #Records", col=c("orange", "steelblue","green","purple"), ylim=c(0,60))

ptab<-table(allvars_cluster4$AGE)
ptab<-prop.table(ptab)
ptab<-ptab*100 # Convert to percentages 
barplot(ptab, main = "Cluster-4", xlab = "AGE", ylab = "% of #Records", col=c("orange", "steelblue","green","purple"), ylim=c(0,60))

ptab<-table(allvars_cluster5$AGE)
ptab<-prop.table(ptab)
ptab<-ptab*100 # Convert to percentages 
barplot(ptab, main = "Cluster-5", xlab = "AGE", ylab = "% of #Records", col=c("orange", "steelblue","green","purple"), ylim=c(0,60))



#EDU Characteristics
dev.off()
par(mfrow=c(3,2))
ptab<-table(allvars_cluster1$EDU)
ptab<-prop.table(ptab)
ptab<-ptab*100 # Convert to percentages 
barplot(ptab, main = "Cluster-1", xlab = "EDU", ylab = "% of #Records", col=c("orange", "steelblue","green","purple"), ylim=c(0,50))

ptab<-table(allvars_cluster2$EDU)
ptab<-prop.table(ptab)
ptab<-ptab*100 # Convert to percentages 
barplot(ptab, main = "Cluster-2", xlab = "EDU", ylab = "% of #Records", col=c("orange", "steelblue","green","purple"), ylim=c(0,50))

ptab<-table(allvars_cluster3$EDU)
ptab<-prop.table(ptab)
ptab<-ptab*100 # Convert to percentages 
barplot(ptab, main = "Cluster-3", xlab = "EDU", ylab = "% of #Records", col=c("orange", "steelblue","green","purple"), ylim=c(0,50))

ptab<-table(allvars_cluster4$EDU)
ptab<-prop.table(ptab)
ptab<-ptab*100 # Convert to percentages 
barplot(ptab, main = "Cluster-4", xlab = "EDU", ylab = "% of #Records", col=c("orange", "steelblue","green","purple"), ylim=c(0,50))

ptab<-table(allvars_cluster5$EDU)
ptab<-prop.table(ptab)
ptab<-ptab*100 # Convert to percentages 
barplot(ptab, main = "Cluster-5", xlab = "EDU", ylab = "% of #Records", col=c("orange", "steelblue","green","purple"), ylim=c(0,50))


#HS Characteristics
dev.off()
par(mfrow=c(3,2))
ptab<-table(allvars_cluster1$HS)
ptab<-prop.table(ptab)
ptab<-ptab*100 # Convert to percentages 
barplot(ptab, main = "Cluster-1", xlab = "HS", ylab = "% of #Records", col=c("orange", "steelblue","green","purple"), ylim=c(0,30))

ptab<-table(allvars_cluster2$HS)
ptab<-prop.table(ptab)
ptab<-ptab*100 # Convert to percentages 
barplot(ptab, main = "Cluster-2", xlab = "HS", ylab = "% of #Records", col=c("orange", "steelblue","green","purple"), ylim=c(0,30))

ptab<-table(allvars_cluster3$HS)
ptab<-prop.table(ptab)
ptab<-ptab*100 # Convert to percentages 
barplot(ptab, main = "Cluster-3", xlab = "HS", ylab = "% of #Records", col=c("orange", "steelblue","green","purple"), ylim=c(0,30))

ptab<-table(allvars_cluster4$HS)
ptab<-prop.table(ptab)
ptab<-ptab*100 # Convert to percentages 
barplot(ptab, main = "Cluster-4", xlab = "HS", ylab = "% of #Records", col=c("orange", "steelblue","green","purple"), ylim=c(0,30))

ptab<-table(allvars_cluster5$HS)
ptab<-prop.table(ptab)
ptab<-ptab*100 # Convert to percentages 
barplot(ptab, main = "Cluster-5", xlab = "HS", ylab = "% of #Records", col=c("orange", "steelblue","green","purple"), ylim=c(0,30))


#CHILD Characteristics
dev.off()
par(mfrow=c(3,2))
ptab<-table(allvars_cluster1$CHILD)
ptab<-prop.table(ptab)
ptab<-ptab*100 # Convert to percentages 
barplot(ptab, main = "Cluster-1", xlab = "CHILD", ylab = "% of #Records", col=c("orange", "steelblue","green","purple"), ylim=c(0,50))

ptab<-table(allvars_cluster2$CHILD)
ptab<-prop.table(ptab)
ptab<-ptab*100 # Convert to percentages 
barplot(ptab, main = "Cluster-2", xlab = "CHILD", ylab = "% of #Records", col=c("orange", "steelblue","green","purple"), ylim=c(0,50))

ptab<-table(allvars_cluster3$CHILD)
ptab<-prop.table(ptab)
ptab<-ptab*100 # Convert to percentages 
barplot(ptab, main = "Cluster-3", xlab = "CHILD", ylab = "% of #Records", col=c("orange", "steelblue","green","purple"), ylim=c(0,50))

ptab<-table(allvars_cluster4$CHILD)
ptab<-prop.table(ptab)
ptab<-ptab*100 # Convert to percentages 
barplot(ptab, main = "Cluster-4", xlab = "CHILD", ylab = "% of #Records", col=c("orange", "steelblue","green","purple"), ylim=c(0,50))

ptab<-table(allvars_cluster5$CHILD)
ptab<-prop.table(ptab)
ptab<-ptab*100 # Convert to percentages 
barplot(ptab, main = "Cluster-5", xlab = "CHILD", ylab = "% of #Records", col=c("orange", "steelblue","green","purple"), ylim=c(0,50))


#CS Characteristics
dev.off()
par(mfrow=c(3,2))
ptab<-table(allvars_cluster1$CS)
ptab<-prop.table(ptab)
ptab<-ptab*100 # Convert to percentages 
barplot(ptab, main = "Cluster-1", xlab = "CS", ylab = "% of #Records", col=c("orange", "steelblue","green","purple"), ylim=c(0,100))

ptab<-table(allvars_cluster2$CS)
ptab<-prop.table(ptab)
ptab<-ptab*100 # Convert to percentages 
barplot(ptab, main = "Cluster-2", xlab = "CS", ylab = "% of #Records", col=c("orange", "steelblue","green","purple"), ylim=c(0,100))

ptab<-table(allvars_cluster3$CS)
ptab<-prop.table(ptab)
ptab<-ptab*100 # Convert to percentages 
barplot(ptab, main = "Cluster-3", xlab = "CS", ylab = "% of #Records", col=c("orange", "steelblue","green","purple"), ylim=c(0,100))

ptab<-table(allvars_cluster4$CS)
ptab<-prop.table(ptab)
ptab<-ptab*100 # Convert to percentages 
barplot(ptab, main = "Cluster-4", xlab = "CS", ylab = "% of #Records", col=c("orange", "steelblue","green","purple"), ylim=c(0,100))

ptab<-table(allvars_cluster5$CS)
ptab<-prop.table(ptab)
ptab<-ptab*100 # Convert to percentages 
barplot(ptab, main = "Cluster-5", xlab = "CS", ylab = "% of #Records", col=c("orange", "steelblue","green","purple"), ylim=c(0,100))


dev.off()
par(mfrow=c(3,2))
plot(allvars_cluster1$max_to_one_brand,main = "Cluster-1", xlab = "max_to_one_brand", ylab = "% of #Records") #has max brand loyalty
plot(allvars_cluster2$max_to_one_brand,main = "Cluster-2", xlab = "max_to_one_brand", ylab = "% of #Records") #has max brand loyalty
plot(allvars_cluster3$max_to_one_brand,main = "Cluster-3", xlab = "max_to_one_brand", ylab = "% of #Records")
plot(allvars_cluster4$max_to_one_brand,main = "Cluster-4", xlab = "max_to_one_brand", ylab = "% of #Records") #has max brand loyalty
plot(allvars_cluster5$max_to_one_brand,main = "Cluster-5", xlab = "max_to_one_brand", ylab = "% of #Records")


dev.off()
par(mfrow=c(5,3))
plot(allvars_cluster1$Pur.Vol.No.Promo....,main = "Cluster-1", xlab = "Pur.Vol.No.Promo....", ylab = "% of #Records") 
plot(allvars_cluster2$Pur.Vol.No.Promo....,main = "Cluster-2", xlab = "Pur.Vol.No.Promo....", ylab = "% of #Records") 
plot(allvars_cluster3$Pur.Vol.No.Promo....,main = "Cluster-3", xlab = "Pur.Vol.No.Promo....", ylab = "% of #Records")
plot(allvars_cluster4$Pur.Vol.No.Promo....,main = "Cluster-4", xlab = "Pur.Vol.No.Promo....", ylab = "% of #Records") 
plot(allvars_cluster5$Pur.Vol.No.Promo....,main = "Cluster-5", xlab = "Pur.Vol.No.Promo....", ylab = "% of #Records")

plot(allvars_cluster1$Pur.Vol.Promo.6..,main = "Cluster-1", xlab = "Pur.Vol.Promo.6..", ylab = "% of #Records") 
plot(allvars_cluster2$Pur.Vol.Promo.6..,main = "Cluster-2", xlab = "Pur.Vol.Promo.6..", ylab = "% of #Records") 
plot(allvars_cluster3$Pur.Vol.Promo.6..,main = "Cluster-3", xlab = "Pur.Vol.Promo.6..", ylab = "% of #Records")
plot(allvars_cluster4$Pur.Vol.Promo.6..,main = "Cluster-4", xlab = "Pur.Vol.Promo.6..", ylab = "% of #Records") 
plot(allvars_cluster5$Pur.Vol.Promo.6..,main = "Cluster-5", xlab = "Pur.Vol.Promo.6..", ylab = "% of #Records")

plot(allvars_cluster1$Pur.Vol.Other.Promo..,main = "Cluster-1", xlab = "Pur.Vol.Other.Promo..", ylab = "% of #Records") 
plot(allvars_cluster2$Pur.Vol.Other.Promo..,main = "Cluster-2", xlab = "Pur.Vol.Other.Promo..", ylab = "% of #Records") 
plot(allvars_cluster3$Pur.Vol.Other.Promo..,main = "Cluster-3", xlab = "Pur.Vol.Other.Promo..", ylab = "% of #Records")
plot(allvars_cluster4$Pur.Vol.Other.Promo..,main = "Cluster-4", xlab = "Pur.Vol.Other.Promo..", ylab = "% of #Records") 
plot(allvars_cluster5$Pur.Vol.Other.Promo..,main = "Cluster-5", xlab = "Pur.Vol.Other.Promo..", ylab = "% of #Records")



dev.off()
par(mfrow=c(3,2))
plot(allvars_cluster1$Pr.Cat.1) 
plot(allvars_cluster2$Pr.Cat.1) 
plot(allvars_cluster3$Pr.Cat.1)
plot(allvars_cluster4$Pr.Cat.1) 
plot(allvars_cluster5$Pr.Cat.1)

dev.off()
par(mfrow=c(3,2))
plot(allvars_cluster1$Pr.Cat.2) 
plot(allvars_cluster2$Pr.Cat.2) 
plot(allvars_cluster3$Pr.Cat.2)
plot(allvars_cluster4$Pr.Cat.2) 
plot(allvars_cluster5$Pr.Cat.2)

dev.off()
par(mfrow=c(3,2))
plot(allvars_cluster1$Pr.Cat.3) 
plot(allvars_cluster2$Pr.Cat.3) 
plot(allvars_cluster3$Pr.Cat.3)
plot(allvars_cluster4$Pr.Cat.3) 
plot(allvars_cluster5$Pr.Cat.3)


dev.off()
par(mfrow=c(3,2))
plot(allvars_cluster1$Pr.Cat.4) 
plot(allvars_cluster2$Pr.Cat.4) 
plot(allvars_cluster3$Pr.Cat.4)
plot(allvars_cluster4$Pr.Cat.4) 
plot(allvars_cluster5$Pr.Cat.4)


dev.off()
par(mfrow=c(3,2))
plot(allvars_cluster1$PropCat.5) 
plot(allvars_cluster2$PropCat.5) 
plot(allvars_cluster3$PropCat.5)
plot(allvars_cluster4$PropCat.5) 
plot(allvars_cluster5$PropCat.5)

dev.off()
par(mfrow=c(3,2))
plot(allvars_cluster1$PropCat.6) 
plot(allvars_cluster2$PropCat.6) 
plot(allvars_cluster3$PropCat.6)
plot(allvars_cluster4$PropCat.6) 
plot(allvars_cluster5$PropCat.6)

dev.off()
par(mfrow=c(3,2))
plot(allvars_cluster1$PropCat.7) 
plot(allvars_cluster2$PropCat.7) 
plot(allvars_cluster3$PropCat.7)
plot(allvars_cluster4$PropCat.7) 
plot(allvars_cluster5$PropCat.7)


dev.off()
par(mfrow=c(3,2))
plot(allvars_cluster1$PropCat.8) 
plot(allvars_cluster2$PropCat.8) 
plot(allvars_cluster3$PropCat.8)
plot(allvars_cluster4$PropCat.8) 
plot(allvars_cluster5$PropCat.8)


dev.off()
par(mfrow=c(3,2))
plot(allvars_cluster1$PropCat.9) 
plot(allvars_cluster2$PropCat.9) 
plot(allvars_cluster3$PropCat.9)
plot(allvars_cluster4$PropCat.9) 
plot(allvars_cluster5$PropCat.9)


dev.off()
par(mfrow=c(3,2))
plot(allvars_cluster1$PropCat.14) 
plot(allvars_cluster2$PropCat.14) 
plot(allvars_cluster3$PropCat.14)
plot(allvars_cluster4$PropCat.14) 
plot(allvars_cluster5$PropCat.14)







