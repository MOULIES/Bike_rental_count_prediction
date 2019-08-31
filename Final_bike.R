rm(list =  ls())

setwd("G:/Data science/Statistics/R Code")
getwd()
library(xlsx)

#load library
x = c('ggplot2','corrgram','DMwR','caret','randomForest','unbalanced','C50','dummies','e1071','Information',
      'MASS', 'rpart','gbm', 'ROSE','sampling', 'DataCombine', 'inTrees')

lapply(x, require, character.only = TRUE)
# 
# install.packages(c("dplyr","plyr","reshape","ggplot2","data.table","GGally"))
# install.packages("GGally")
# 
# Install  Require libraries
library("dplyr")
library("plyr")
library("ggplot2")
library("data.table")
library("GGally")

#load the data
df = read.csv("day - Copy.xlsx.csv",header = TRUE)
df = df[,2:16]
#info
str(df)

#verify summary of data
summary(df)

#convert cATEGORIC VARIABLE TYPE from int to factor
df$season = as.factor(df$season)
df$yr = as.factor(df$yr)
df$mnth = as.factor(df$holiday)
df$weekday = as.factor(df$weekday)
df$holiday = as.factor(df$holiday)
df$workingday = as.factor(df$workingday)
df$weathersit = as.factor(df$weathersit)
# df$cnt  = as.numeric(df$cnt)

# df$registered  = as.numeric(df$registered)
################################# Analysing variables by Visulaization ##########

##univariate dist. of numeric variables

dist_numeric = function(num_x){
  
  ggplot(df)+
    geom_histogram(aes(x=num_x,y=..density..),
                   fill= "grey",binwidth = 500,bins = 50)+
    geom_density(aes(x=num_x,y=..density..))
  
}

# analyze the distribution of  target variable 'cnt'
dist_numeric(df$cnt)

dist1 = function(x1){
  ggplot(df, aes(x1)) +
    geom_histogram(fill= "grey",bins = 50)+
    geom_density(aes(x=x1,y=..density..))
}

# analyse the distrubution of  independence variable 'temp'
dist1(df$temp)

# analyse the distrubution of  independence variable 'atemp'
dist1(df$atemp)


# analyse the distrubution of  independence variable 'hum'
dist1(df$hum)


# analyse the distrubution of  independence variable 'windspeed'
dist1(df$windspeed)

# analyse the distrubution of  independence variable 'casual'
dist1(df$casual)


##################################Missing Values Analysis###############################################
missing_val = data.frame(apply(df,2,function(x){sum(is.na(x))}))
missing_val

# No missing value is present in the data


################################## Outlier Analysis ###############################################

# ## BoxPlots - Distribution and Outlier Check
numeric_index = sapply(df,is.integer) #selecting only integer
numeric_data = df[,numeric_index]
cnames  = colnames(numeric_data)

for( i in 1:length(cnames)) {
  print(i)
  assign(paste0('gn',i), ggplot( data = df,aes_string( y= cnames[i], x = 'cnt'), )+ 
                      stat_boxplot(geom = "errorbar", width = 0.5) +
                      geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                                   outlier.size=1, notch=FALSE) +
                      theme(legend.position="bottom")+
                      labs(y=cnames[i],x="cnt")+
                      ggtitle(paste("Box plot of cnt for",cnames[i])))
  }

# ## Plotting plots together
gridExtra::grid.arrange(gn1,gn2,gn3,ncol=3)

#outlier is present in casual variable as per boxplot
#remove outliers

for( i in cnames){
  print(i)
  val = df[,i][df[,i] %in% boxplot.stats(df[,i])$out]
  df = df[ which(! df[,i] %in% val),]
}


##################################Feature Selection################################################
## Correlation Plot

corrgram(df[,c('temp','atemp','hum','windspeed','cnt')],order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")



# As per correlation matrix, 'temp' and 'atemp' having strong relationship
# there is no  relationship between 'hum' and 'cnt'

#  dimensionality  reduction

df = subset(df,select=-c(atemp,hum))



##################################Feature Scaling################################################
#Normality check
qqnorm(df$casual)
hist(df$casual)

#Normalisation
cnames = c("casual","registered")
for( i in cnames){
  df[,i] = (df[,i] - min(df[,i])) / ( max(df[,i]) -min( df[,i]))
 }

###################################Model Development#######################################
#Clean the environment
rmExcept("df")
set.seed(1234)
train.index = createDataPartition(df$cnt,p= 0.8,list = F)
train = df[train.index,2:13]
test = df[ -train.index, 2:13]


################################### Evaluation matrics#######################################
EVM = function( y_actual, y_predict) {
  print("MAPE")
  m = mean(abs( (y_actual - y_predict) / y_actual) )
  print(m  )
  difference = y_actual - y_predict
  root_mean_square = sqrt(mean(difference^2))
  print("RMSE")
  print(root_mean_square)
  }

################################## DECISION TREE MODEL #########################################

DT = rpart(cnt ~ ., data = train, method = 'anova')

predictions_DT = predict( DT, test[,-12])
print(DT)

EVM(test[,12], predictions_DT)


# "MAPE" = 0.1018411
# "RMSE" = 414.7741


################################## RANDOM FOREST #########################################

RF = randomForest(cnt ~ .,train , importance = TRUE, ntree = 500)

predictions_RF = predict( RF, test[,-12])
# plot(RF)

EVM(test[,12], predictions_RF)

# "MAPE" = 0.0591
# "RMSE" = 210.2734

# # Extract rules fromn random forest
# # transform rf object to an inTrees' format
# treeList = RF2List(RF)
# 
# #Extract rules
# exec = extractRules(treeList, train[,-12])  # R-executable conditions
# 
# #Visualize some rules
# exec[1:2,]
# 
# #Make rules more readable:
# readableRules = presentRules(exec, colnames(train))
# readableRules[1:2,]
# 
# #Get rule metrics
# ruleMetric = getRuleMetric(exec, train[,-12], train$cnt)  # get rule metrics
# 
# #evaulate few rules
# ruleMetric[1:2,]
# 


# ##################### ##############  Parameter Tuning for random forest################################
# 
# RF_2=randomForest(cnt ~ . , data = train, mtry =7, ntree=500 ,nodesize =10 ,importance =TRUE)
# #Predict for new test cases
# predictions_RF_2 = predict(RF_2, test[,-12])
# EVM(test[,12], predictions_RF_2)

# # "MAPE" = 0.02413833
# # "RMSE" = 101.5057


################################## KNN #######################################
KNN_prediction = knnregTrain(train[, 1:11] , test[, 1:11] , train$cnt , k = 3)
EVM(test[,12], KNN_prediction)

# # "MAPE" = 0.1602449
# # "RMSE" = 757.5562

# 
# ################################## NaiveBayes #######################################
# 
# NB = naiveBayes( cnt ~ . , data = train)
# NB_prediction = predict(NB , test[,-12],type = "")
# EVM(test[,12], NB_prediction)

################################## LOGISTIC REGRESSION #######################################
# logit_model = glm( cnt ~ ., data = train )
# Predictions_Logit = predict( logit_model,test[,-12] )
# 
# EVM(test[,12], Predictions_Logit)
# summary(logit_model)

# "MAPE" = 1.593383e-15
# "RMSE" = 5.141082e-12

################################## LINEAR REGRESSION #########################################
#run regression model
LM = lm(cnt ~., data = train)

predictions_LM = predict( LM , test[,-12])
# plot(RF)

EVM(test[,12], predictions_LM)
summary(LM)

# "MAPE" = 1.593383e-15
# "RMSE" = 5.141082e-12










