#preprocess
library(readxl)
globalterrorismdb_0617dist <- read_excel("D:/5th sem/Data Analytics/project/globalterrorismdb_0617dist.xlsx")
View(globalterrorismdb_0617dist)

df <- globalterrorismdb_0617dist
#excluding the columns that are half filled/(incomplete info) because they were added 
# during the second phase of maintaining the database.
#Our model doesnt concern with these columns

df_necessary <- subset(df,select=-c(5,7,12,19,26,37,38,39,40,44:48,50,52:56,58,60,62,64,65,69,74,77,80,98,100,103,107,109,112,119,121,122,124,126:133,135))

#Removing categorical variables which have their respective dummy vaiables in the database
gtd <- subset(df_necessary, select=-c(7,9,10,21,25,31,33,39,40,42,43,54,56,58,60,62,64,66,68))

#treating various missing valued columns differently 
#based on their intented meaning and contribution to dataset 
#filling the following columns with the means of the weapon used for attack,
#since weapons used cause different extents of damage.

#nkill(No of persons killed(civilians)), 
#nkillter(No of terrorist dead during the attack),
#nwound(No of civilians wounded by attack),
#nwoundte(No of terrorists dead during attack),
#propextent(Extent of Property damaged),
#propvalue(Value of property damaged)
n <- c()
weap_means <- setNames(data.frame(matrix(ncol = 7, nrow = 0)),c("1","2","3","4","5","6","7"))
for(i in c(1:13)){
  w <- data.frame(subset(globalterrorismdb_0617dist,globalterrorismdb_0617dist$weaptype1==i))
  n <- c(n,as.character(w[1,"weaptype1_txt"]))
  mean_nkill <- round(colMeans(w["nkill"],na.rm = TRUE))
  mean_nkillter <- round(colMeans(w["nkillter"],na.rm = TRUE))
  mean_nwound <- round(colMeans(w["nwound"],na.rm = TRUE))
  mean_nwoundte <- round(colMeans(w["nwoundte"],na.rm = TRUE))
  w <- subset(w,w$property==1)
  w <- subset(w,w$propvalue>=0)
  mean_propextent <- round(colMeans(w["propextent"],na.rm = TRUE))
  mean_propvalue <- round(colMeans(w["propvalue"],na.rm = TRUE))
  weap_means <- rbind(weap_means,c(mean_nkill,mean_nkillter,mean_nwound,mean_nwoundte,mean_propvalue,mean_propextent))
}
colnames(weap_means) <- c("nkill","nkillter","nwound","nwoundte","propvalue","propextent")
weap_means$weapon <- n
#------------------------------------------------------------------------------------------------------------------------------------
# filling now...
glo <- globalterrorismdb_0617dist
gtd$gno <- as.numeric(as.factor(gtd$gname))
for(i in range(1:13)){
  g <- subset(gtd,gtd$weaptype1==i)
  gtd <- subset(gtd, gtd$weaptype1!=i)
  g[,"nkill"][is.na(g[,"nkill"]) ] <- weap_means[i,"nkill"]
  g[,"nkillter"][is.na(g[,"nkillter"])] <- weap_means[i,"nkillter"]
  g[,"nwound"][is.na(g[,"nwound"])] <- weap_means[i,"nwound"]
  g[,"nwoundte"][is.na(g[,"nwoundte"])] <- weap_means[i,"nwoundte"]
  g[,"propvalue"][is.na(g[,"propvalue"])] <- weap_means[i,"propvalue"]
  g[,"propextent"][is.na(g[,"propextent"])] <- weap_means[i,"propextent"]
  gtd <- rbind(gtd,g)
}
#gtd <- gtd[complete.cases(gtd), ]
gtd[is.na(gtd)] <- 0

summary(gtd)

#building a non character dataset
gtd$propvalue <- as.numeric(as.factor(gtd$propvalue))
gtd$ransompaid <- as.numeric(as.factor(gtd$ransompaid))
gtd$divert <- as.numeric(as.factor(gtd$divert))
gtd$kidhijcountry <- as.numeric(as.factor(gtd$kidhijcountry))


non_char <- subset(gtd, select=-c(12,22,24,31))

#-----------------------------------------------------------------------------------------------------
#Predicting terrorist group
#install.packages("rpart")

#clustering terrorist groups

#perform PCA without scaling, because normalising the data without knowing the population of 
#the country and various other attributes isnt correct. Because for example, 
#nkill(range - 0 to thousands --> people killed) & 
#propextent(range - 0 to billions of dollars --> money) 
#when normalised means we are valuing number of people killed in dollars
#which is incorrect.
require(stats)
prin_comp <- prcomp(non_char[,-65])
std_dev <- prin_comp$sdev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#chose 10 principal componenets since the graph becomes fla at 10
new_frame <- data.frame(prin_comp$x[,1:10])
new_frame$gno <- non_char$gno

#Performing elbow joint method to figure out the optimum number of clusters
data <- new_frame
wss <- (nrow(data)-1)*sum(apply(data,2,var))
for (i in 1:10) wss[i] <- sum(kmeans(data,
                                     centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)

#50 is decided as the optimal number of clusters 
#based on elbow joint and to make sure each cluster 
#wont become too bulky wrt terrorist groups

#for reproducing results
set.seed(100)


km.res <- kmeans(new_frame, 50, nstart = 25,iter.max = 200)
km.res

#dataframe contains digit and the cluster it belongs to
gno_cluster <- data.frame(km.res$cluster, non_char$gno)
mydata <- non_char

mydata <- cbind(non_char,km.res$cluster)

#---------------------------------------------------------------------------------------------------------------------------
#Building a decision tree model, using clustered data only for predicitng the group 
#responsible for attack. For rest normal data used
library(rpart)

#dataset should contain only numeric data
#predicting whether the incident was a terrorist attack
dbt_Ter <- subset(mydata,select = -c(65))

smp_size <- floor(0.70*nrow(dbt_Ter))
set.seed(100)
train_ind <- sample(seq_len(nrow(dbt_Ter)),size=smp_size)
train <- dbt_Ter[train_ind, ]
test <- dbt_Ter[-train_ind, ]
fit <- rpart( train$`doubtterr` ~ ., data=train, method='class')
plotcp(fit)
printcp(fit)
fit
preds <- predict(fit, test, type = 'class')
preds
confMat <- table(test$`doubtterr`,preds)
accuracy <- sum(diag(confMat))/sum(confMat)
accuracy

#-------------------------------------------------------------------------------
#if not terrosrism who launched the attack?
  
alt <- subset(mydata,doubtterr==1,select = -c(65))

smp_size <- floor(0.70*nrow(alt))
set.seed(100)
train_ind <- sample(seq_len(nrow(alt)),size=smp_size)
train <- alt[train_ind, ]
test <- alt[-train_ind, ]
fit <- rpart( train$`alternative` ~ ., data=train, method='class')
plotcp(fit)
printcp(fit)
fit
preds <- predict(fit, test, type = 'class')
preds
confMat <- table(test$`alternative`,preds)
accuracy <- sum(diag(confMat))/sum(confMat)
accuracy

#------------------------------------------------------------------------------------
#predicting which cluster launched the 'terrorist attack'
df <- subset(mydata,select = -c(65))

smp_size <- floor(0.70*nrow(df))
set.seed(100)
train_ind <- sample(seq_len(nrow(df)),size=smp_size)
train <- df[train_ind, ]
test <- df[-train_ind, ]
fit <- rpart( train$`km.res$cluster` ~ ., data=train, method='class')
plotcp(fit)
printcp(fit)
fit
preds <- predict(fit, test, type = 'class')
preds
confMat <- table(test$`km.res$cluster`,preds)
accuracy <- sum(diag(confMat))/sum(confMat)
accuracy
#-----------------------------------------------------------------------------------
#predict number of people that migh get killed

nkilldf <- subset(mydata,attacktype1!=9 & attacktype2!=9 & attacktype3!=9 & weaptype1!=13 & weaptype2!=13 & weaptype3!=13 & weaptype4!=13 & weapsubtype1!=27 & weapsubtype2!=27 & weapsubtype3!=27 & weapsubtype4!=27,select = -c(65))

smp_size <- floor(0.70*nrow(nkilldf))
set.seed(100)
train_ind <- sample(seq_len(nrow(nkilldf)),size=smp_size)
train <- nkilldf[train_ind, ]
test <- nkilldf[-train_ind, ]
fit <- rpart( train$`nkill` ~ ., data=train, method='class')
plotcp(fit)
printcp(fit)
fit
preds <- predict(fit, test, type = 'class')
preds
confMat <- table(test$`nkill`,preds)
accuracy <- sum(diag(confMat))/sum(confMat)
accuracy

#-----------------------------------------------------------------------------------
#if property damage exists, then whats the extent?
prop_d <- subset(mydata,attacktype1!=9 & attacktype2!=9 & attacktype3!=9 & weaptype1!=13 & weaptype2!=13 & weaptype3!=13 & weaptype4!=13 & weapsubtype1!=27 & weapsubtype2!=27 & weapsubtype3!=27 & weapsubtype4!=27 ,select = -c(65))

smp_size <- floor(0.70*nrow(prop_d))
set.seed(100)
train_ind <- sample(seq_len(nrow(prop_d)),size=smp_size)
train <- prop_d[train_ind, ]
test <- prop_d[-train_ind, ]
fit <- rpart( train$`propextent` ~ ., data=train, method='class')
plotcp(fit)
printcp(fit)
fit
preds <- predict(fit, test, type = 'class')
preds
confMat <- table(test$`propextent`,preds)
accuracy <- sum(diag(confMat))/sum(confMat)
accuracy

