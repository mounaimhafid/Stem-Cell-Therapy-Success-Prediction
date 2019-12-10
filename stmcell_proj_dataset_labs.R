#TODO load data into file 
library(ISLR)
library('e1071')
install.packages('BBmisc')
library('BBmisc')
library('graphics')
library("lattice")
library('caTools')
install.packages("rminer")
install.packages("penalizedSVM")
library('rminer')
library('party')
library('penalizedSVM')
library('ggplot2')
library('caret')
library('dplyr')

df.labs <- read.csv("labs_mod.csv")
df.labs <- df.labs[-1]
drop<- c("LBXMPSI")
df.labs <- df.labs[ , !names(df.labs) %in% drop]

Success <- cbind()


for (i in 1:nrow(df.labs)){
  #print(i)
  if(df.labs[i, names(df.labs) %in% c("LBXPLTSI")] >= 300){
    Success <- append(Success, 1)
  }
  else{
    Success <- append(Success, 0)
  }
}

#Success <- as.factor(Success)

platelets <- df.labs[, names(df.labs) %in% "LBXPLTSI"]

drop<- c("LBXPLTSI")
df.labs <- df.labs[ , !names(df.labs) %in% drop]

Success <- as.factor(Success)

df.labs <- cbind(df.labs, Success)
df.labs <- as.data.frame(df.labs)


#df.labs <- factor(df.labs, labels = names(df.labs))

str(df.labs)
summary(df.labs)



#normalized.labs <- normalize(df.labs) 

#labs.pca <- prcomp(df.labs, scale = TRUE, center = TRUE)
#str(labs.pca)
#summary(labs.pca)


#pca.cols <- c(1, 8, 15, 22, 29, 36, 43, 50, 57, 64, 59, 70)

#df.labs[, 50]

#df.labs.pca <- df.labs[, pca.cols]
#splom(normalized.labs, groups = normalized.labs$LBXPLTSI)

split = sample.split(df.labs$Success, SplitRatio = 0.9)


labs.train = subset(df.labs, split == TRUE)
labs.test = subset(df.labs, split == FALSE)
#labs.pca.train = subset(df.labs.pca, split == TRUE)
#labs.pca.test = subset(df.labs.pca, split == FALSE)

labs.svm <- svm(Success ~. , data = labs.train )
#labs.pca.svm <- svm(LBXPLTSI ~ ., data = labs.pca.train)

labs.svm
#labs.pca.svm


#plot(labs.svm)
#plot(labs.pca.svm)

str(labs.svm)
summary(labs.svm)
#str(labs.pca.svm)
#summary(labs.pca.svm)


labs.svm.predictions <- predict(labs.svm, newdata = labs.test)
labs.svm.predictions

#labs.pca.svm.predictions <- predict(labs.pca.svm, newdata = labs.pca.test)
#labs.pca.svm.predictions 



table(labs.svm.predictions, labs.test$Success)
#table(labs.pca.svm.predictions, labs.pca.test$LBXPLTSI)

#plot(labs.pca.svm, df.labs.pca)
plot(labs.svm, df.labs)

#kernlab::plot(labs.pca.svm)


#TUNING

labs.svm.tune.results <- tune(svm, Success ~. , data = labs.train,
                              ranges=list(cost=c(0.1, 1, 10), gamma=c(.5,1,2)))

labs.svm.tune.results$best.parameters
labs.svm.tune.results$performances



tuned.labs.svm <- svm(Success ~. , data = labs.train, kernel = "polynomial",
                      gamma = 0.5, cost = 10)

tuned.labs.svm.predictions <- predict(tuned.labs.svm , newdata = labs.test)

table(tuned.labs.svm.predictions, labs.test$Success)

#plot tuned svm

###########
#Normalized
ndf.labs <-normalize(df.labs)

par(mar= c(5.1, 4.1, 4.1, 2.1))


split = sample.split(ndf.labs$Success, SplitRatio = 0.9)


ndflabs.train = subset(df.labs, split == TRUE)
ndflabs.test = subset(df.labs, split == FALSE)

ndflabs.svm <- svm(ndflabs.train$Success ~ . , data = ndflabs.train, kernel = 'radial')
#ndflabs.svm2 <- svmfs(x = ndflabs.train[-70], y = c(-1,1),
 #                      data = ndflabs.train)
summary(ndflabs.svm)

ndflabs.svm.predictions <- predict(ndflabs.svm , newdata = ndflabs.test)

confusionMatrix(table(ndflabs.svm.predictions, ndflabs.test$Success))

#plot.new()
#points(x = ndflabs.train$LBXSASSI, y = ndflabs.train$Success, type = 'b')

#importance <-varImp(ndflabs.svm)
#importance
plot.new()

Success2 <- as.numeric(as.character(ndflabs.train$Success))
Success2


plot(ndflabs.svm, ndflabs.train, Success2 ~ .)

ndflabs.svm$index

ndf.labs <- cbind(ndf.labs, platelets)

plot((select(ndf.labs, 1:12, 71)))
plot((select(ndf.labs, 13:24, 71)))
plot((select(ndf.labs, 25:36, 71)))
plot((select(ndf.labs, 37:48, 71)))
plot((select(ndf.labs, 49:60, 71)))
plot((select(ndf.labs, 61:70, 71)))



