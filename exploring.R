#Threshhold platelet count that must be met for successful treatment
Threshhold = 1500 #Remember this is in thousands per micro-Liter
Multiplier = 5 #Centrifuge Multiplier

diet = read.csv("C:/Users/wryde/Desktop/Fall 2019/CS Capstone/national-health-and-nutrition-examination-survey/diet.csv")
summary(diet)

labs = read.csv("C:/Users/wryde/Desktop/Fall 2019/CS Capstone/national-health-and-nutrition-examination-survey/labs.csv")
summary(labs)

#LBXPLTSI platelet count variable number
#summary of the platelet count
summary(labs$LBXPLTSI)
#histogram of the platelet count
hist(labs$LBXPLTSI)

#remove row data that has nothing for the labs dataset

#length(labs[!sapply(labs, function(x) mean(is.na(x)))>0.999999, ])
#plsword <-labs[!sapply(labs, function(x) mean(is.na(x)))>0.999999, ]
length(which(rowSums(is.na(labs)) < 422))
slab <- labs[which(rowSums(is.na(labs)) < 422), ]

#join the data together with the diet dataset upon the SEQN
joined_data <- merge(diet, slab, by="SEQN")

#consolidate columns by removing data with more than 75% 
colrem <- joined_data[,!sapply(joined_data, function(x) mean(is.na(x)))>0.75]

#size of those with adequate platelet in the sample
length(which(colrem$LBXPLTSI*Multiplier > Threshhold))

#clear out the null values by replacing them with medians
clear_away=function(x){
  x<-as.numeric(as.character(x)) #first convert each column into numeric if it is from factor
  x[is.na(x)] = median(x, na.rm=TRUE) #convert the item with NA to median value from the column
  x #display the column
}
clean_data = data.frame(apply(colrem,2,clear_away))

sum(is.na(clean_data)) #check they are taken care of.


#add a new attribute to determine whether this person would have successful operation based on platelet count.
clean_data$ThreshReached <- clean_data$LBXPLTSI*Multiplier > Threshhold

#now to try and do some modeling
library(rpart.plot)
library(caret)


#For replicability's sake
set.seed(102)

#create 10 folds of precious data
folds <- createFolds(clean_data$ThreshReached, k = 10, list = TRUE)

decideTree=function(i, foldem, dt){
  if(i == 1)
  {
    ind <- unlist(foldem[2:10])
  }
  else if(i == 10)
  {
    ind <- unlist(foldem[1:9])
  }
  else
  {
    ind <- unlist(c(foldem[1:i-1], foldem[i+1:10]))
  }
  train_dt <- dt[ind,]
  test_dt <- dt[foldem[[i]],]
  
  fit <- rpart(ThreshReached ~ . - SEQN - LBXPLTSI - LBXMPSI,
               method="class", data=train_dt)
  
  pred <- predict(fit, test_dt, type="class")
  CM <- confusionMatrix(pred, as.factor(test_dt$ThreshReached))
  
  result <- list(CM, fit)
  return(result)
}

result_dt <- lapply(1:10, decideTree, foldem = folds, dt = clean_data)

dtree1 <- result_dt[[1]]
dtree1_CM <- dtree1[[1]]
dtree1_fit <- dtree1[[2]]
rpart.plot(dtree1_fit)
dtree1_CM

dtree2 <- result_dt[[2]]
dtree2_CM <- dtree2[[1]]
dtree2_fit <- dtree2[[2]]
rpart.plot(dtree2_fit)
dtree2_CM

dtree3 <- result_dt[[3]]
dtree3_CM <- dtree3[[1]]
dtree3_fit <- dtree3[[2]]
rpart.plot(dtree3_fit)
dtree3_CM

dtree4 <- result_dt[[4]]
dtree4_CM <- dtree4[[1]]
dtree4_fit <- dtree4[[2]]
rpart.plot(dtree4_fit)
dtree4_CM

dtree5 <- result_dt[[5]]
dtree5_CM <- dtree5[[1]]
dtree5_fit <- dtree5[[2]]
rpart.plot(dtree5_fit)
dtree5_CM

dtree6 <- result_dt[[6]]
dtree6_CM <- dtree6[[1]]
dtree6_fit <- dtree6[[2]]
rpart.plot(dtree6_fit)
dtree6_CM

dtree7 <- result_dt[[7]]
dtree7_CM <- dtree7[[1]]
dtree7_fit <- dtree7[[2]]
rpart.plot(dtree7_fit)
dtree7_CM

dtree8 <- result_dt[[8]]
dtree8_CM <- dtree8[[1]]
dtree8_fit <- dtree8[[2]]
rpart.plot(dtree8_fit)
dtree8_CM

dtree9 <- result_dt[[9]]
dtree9_CM <- dtree9[[1]]
dtree9_fit <- dtree9[[2]]
rpart.plot(dtree9_fit)
dtree9_CM

dtree10 <- result_dt[[10]]
dtree10_CM <- dtree10[[1]]
dtree10_fit <- dtree10[[2]]
rpart.plot(dtree10_fit)
dtree10_CM


#rpart.plot(fit)
#summary(fit)

#printcp(fit)
#plotcp(fit)

library(pROC)
roc(CM)



library(neuralnet)

nn=neuralnet(ThreshReached ~ . - SEQN, data=df, hidden=5,act.fct = "logistic",
             linear.output = FALSE)




