# Project-2: Retail
# Date: 19-01-2020

setwd("J:/Edvancer R/Project 2 Retail/")
getwd()

# read the train and test datasets
st_train = read.csv("store_train.csv", stringsAsFactors = F)
st_test = read.csv("store_test.csv", stringsAsFactors = F)

# what is the total sales (sum of all sales) of Supermarket Type1 in area Kennebec County, ME?
sort(table(st_train$countyname), decreasing = T)[30:40]
sort(table(st_train$Areaname), decreasing = T) # area name consists of state and county name
unique(st_train$countyname)
names(st_train) # names of columns
library(dplyr)
st_train_sls = st_train %>% 
  filter(Areaname == "Kennebec County, ME") %>%
  group_by(store_Type) %>%
  summarise(Sum_sales0 = sum(sales0, na.rm = T), Sum_sales1 = sum(sales1, na.rm = T),
            Sum_sales2 = sum(sales2, na.rm = T), Sum_sales3 = sum(sales3, na.rm = T),
            Sum_sales4 = sum(sales4, na.rm = T))
totalSales = sum(st_train_sls[st_train_sls$store_Type == "Supermarket Type1", -1])
# Ans: total sales = 38680

# Should storecode be included in building models?
sort(table(st_train$storecode), decreasing = T)
unique(st_train$storecode) # 1891 unique values
# Ans : No, this should not be used as is but can be source of a feature

# should country be treated as numeric type or character?
sort(table(st_train$country), decreasing = T)
unique(st_train$country) # 274 unique categories
# Ans : character

# Find out number of unique categories of variable Areaname.
unq_Areaname = unique(st_train$Areaname) # 1891 unique categories
# Ans : 1891 unique categories

# For store type grocery store what is the response rate ? 
# [ what % of obs have response value as 1 ]  Round off to two decimal digits.
unique(st_train$store_Type) # 4 unique categories
st_grcry = st_train$store[st_train$store_Type == "Grocery Store"] # 432 observations
tab_grcry = table(st_grcry) # in counts
round(prop.table(tab_grcry)*100, 2) # in percentage round off to two digits after decimal
# Ans: 42.13

# Do all the sales variable follow normal distribution?
library(ggplot2)
# sales0
ggplot(st_train,aes(x = sales0)) + geom_histogram()
ggplot(st_train,aes(x = sales0)) + geom_density(color = "red") + 
  stat_function(fun=dnorm, 
                args=list(mean=mean(st_train$sales0),sd=sd(st_train$sales0)), 
                color="green")
# sales1
ggplot(st_train,aes(x = sales1)) + geom_histogram()
ggplot(st_train,aes(x = sales1)) + geom_density(color = "red") + 
  stat_function(fun=dnorm, 
                args=list(mean=mean(st_train$sales1),sd=sd(st_train$sales1)), 
                color="green")
# sales2
ggplot(st_train,aes(x = sales2)) + geom_histogram()
ggplot(st_train,aes(x = sales2)) + geom_density(color = "red") +
  stat_function(fun=dnorm, 
                args=list(mean=mean(st_train$sales2),sd=sd(st_train$sales2)), 
                color="green")
# sales3
ggplot(st_train,aes(x = sales3)) + geom_histogram()
ggplot(st_train,aes(x = sales3)) + geom_density(color = "red") +
  stat_function(fun=dnorm, 
                args=list(mean=mean(st_train$sales3),sd=sd(st_train$sales3)), 
                color="green")
# sales4
ggplot(st_train,aes(x = sales4)) + geom_histogram()
ggplot(st_train,aes(x = sales4)) + geom_density(color = "red") +
  stat_function(fun=dnorm, 
                args=list(mean=mean(st_train$sales4),sd=sd(st_train$sales4)), 
                color="green")
# Ans: No, they are all positively skewed distributions

# Number of outliers for total sales based on following limits (q1-1.5*IQR, q3+1.5*IQR)? 
ts = st_train[, c(2,3,4,5,6)]
ts$totalSales = rowSums(ts[,c(1:5)])
ts.IQR = IQR(ts$totalSales)
ts.min = min(ts$totalSales)
ts.max = max(ts$totalSales)
summary(ts$totalSales)
ts.Q1 = as.vector(summary(ts$totalSales)[2])
ts.Q3 = as.vector(summary(ts$totalSales)[5])
boxplot(ts$totalSales)
ts.uw = ts.Q3+(1.5*ts.IQR) # ts.uw = min(ts.max, ts.Q3+(1.5*ts.IQR))
ts.lw = ts.Q1-(1.5*ts.IQR) # ts.lw = max(ts.min,ts.Q1-(1.5*ts.IQR))
out = ts %>%  filter((totalSales > ts.uw) | (totalSales < ts.lw))
num_out = nrow(out)
# Ans: 140
# Aliter
OutVals = boxplot(ts$totalSales)$out
which(ts$totalSales %in% OutVals)

# which store type has maximum variance in total sales?
ts$store_Type = st_train$store_Type
ts_vars = ts %>%
  group_by(store_Type) %>%
  summarise(var.totalSales = var(totalSales, na.rm = T))
ts_vars$store_Type[ts_vars$var.totalSales == max(ts_vars$var.totalSales)]
# Ans: "Grocery Store"

# How many dummies will you create for variable state_alpha?
table(st_train$State,st_train$state_alpha)
unique(st_train$state_alpha)
unique(st_train$State)
tab_state = sort(table(st_train$state_alpha), decreasing = T)
sort(table(st_train$State), decreasing = T)
# state_alpha & State are identical data, remove anyone while building the model
which(tab_state > 100)
# Ans : 24 categories having frequency above 50

# What should be the type of categorical variable when using the function randomForest?
# Ans: factor

glimpse(st_train)
# combine test & train data
library(dplyr)
st_test$store = NA
st_train$data = 'train'
st_test$data = 'test'
st_all = rbind(st_train, st_test)
glimpse(st_all)

# convert the response variable to factor from character datatype for classification problem
st_all_dp = st_all
st_all_dp$store = as.factor(st_all_dp$store)
glimpse(st_all_dp)

# write function to create dummy for categorical variables
CreateDummies=function(data,var,freq_cutoff=10){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub(",","",name)
    name=gsub("\\+","",name)
    name=gsub("-","_",name)
    name=gsub("/","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

# the variable State & stae_alpha represents same data hence we can drop State 
st_all_dp = st_all_dp %>%
  select(-State)

## convert some character variable to factor as decision tress and random forests can take factors

# variable country is provided in numeric form 
unique(st_all_dp$country) # 330 unique values
sort(table(st_all_dp$country), decreasing = T)
# it can be converted to dummy variables by having a cut-off of 100 
# but now we will just convert country into factor variable
# st_all_dp$country = as.factor(st_all_dp$country)
# convert to dummy
st_all_dp = CreateDummies(st_all_dp, "country", 100)
glimpse(st_all_dp)

# variable countyname
unique(st_all_dp$countyname) # 1962 unique values
sort(table(st_all_dp$countyname), decreasing = T)[1:30]
# it can be converted to dummy variables by having a cut-off of 40
# but now we will just convert countyname into factor variable
# st_all_dp$countyname = as.factor(st_all_dp$countyname)
# convert to dummy
st_all_dp = CreateDummies(st_all_dp, "countyname", 40)
glimpse(st_all_dp)

# variable storecode
unique(st_all_dp$storecode) # 2572 unique values
sort(table(st_all_dp$storecode), decreasing = T)[1:30]
# it can be converted to dummy variables by having a cut-off of 30 or 
# a feature categorical variable having value such as METRO and NCNTY is also possible
# but now we will just drop it as it has too many unique values
st_all_dp = st_all_dp %>%
  select(-storecode)
glimpse(st_all_dp)

# variable Areaname 
unique(st_train$Areaname) # 1891 unique categories
# however Areaname is basically combinations of variable countyname & variable state_alpha
# hence, we will just drop it
st_all_dp = st_all_dp %>%
  select(-Areaname)
glimpse(st_all_dp)

# variable Id would be drop as it will have no impact on response variable store
st_all_dp = st_all_dp %>%
  select(-Id)
glimpse(st_all_dp)

# variable countytownname
unique(st_all_dp$countytownname) # 3176 unique values
sort(table(st_all_dp$countytownname), decreasing = T)[1:30]
# it can be converted to dummy variables by having a cut-off of 10 or
# it could be converted to factor variable
# but now we will just drop countytown name as it has too many unique values
st_all_dp = st_all_dp %>%
  select(-countytownname)
glimpse(st_all_dp)

# variable state_alpha
unique(st_all_dp$state_alpha) # 54 unique values
sort(table(st_all_dp$state_alpha), decreasing = T)[1:30]
# it can be converted to dummy variables by having a cut-off of 60
# but now we will just convert state_alpha into factor variable
# st_all_dp$state_alpha = as.factor(st_all_dp$state_alpha)
# convert to dummy
st_all_dp = CreateDummies(st_all_dp, "state_alpha", 40)
glimpse(st_all_dp)

# variable store_Type
unique(st_all_dp$store_Type) # 4 unique values
sort(table(st_all_dp$store_Type), decreasing = T)
# it can be converted to dummy variables 
# but now we will just convert store_Type into factor variable
st_all_dp$store_Type = as.factor(st_all_dp$store_Type)
glimpse(st_all_dp)
# now all the character variable has been converted to factor except data

# check for na
sum(is.na(st_all_dp$sales0)) # No NA
sum(is.na(st_all_dp$sales1)) # No NA
sum(is.na(st_all_dp$sales2)) # No NA
sum(is.na(st_all_dp$sales3)) # No NA
sum(is.na(st_all_dp$sales4)) # No NA
# sum(is.na(st_all_dp$country)) # 1 NA
sum(is.na(st_all_dp$CouSub)) #  No NA
# sum(is.na(st_all_dp$countyname)) # No NA
sum(is.na(st_all_dp$population)) # 2 NA
# sum(is.na(st_all_dp$state_alpha)) # No NA
sum(is.na(st_all_dp$store_Type)) # No NA
sum(is.na(st_all_dp$store)) # 1431 NA

# total na
sum(is.na(st_all_dp)) # 3 NA

# check for NA in variable store for train data
sum(is.na(st_train$store)) # 0 NA

# replace the observation with mean value from train data having NA except store
# total observations = 4769
rowindex = which(is.na(st_all_dp[,c(11:68)]), arr.ind=TRUE)[,1]

for(col in names(st_all_dp)){
  
  if(sum(is.na(st_all_dp[,col]))>0 & !(col %in% c("data","store"))){
    
    st_all_dp[is.na(st_all_dp[,col]),col]=mean(st_all_dp[st_all_dp$data=='train',col],na.rm=T)
  }
  
}

# st_all_p = st_all_dp[-rowindex,]
sum(is.na(st_all_dp))

# separate train and test data
st_train_p = st_all_dp %>% filter(data == "train") %>% select(-data)
st_test_p = st_all_dp %>% filter(data == "test") %>% select(-data,-store)

# again check for NA in train and test data
sum(is.na(st_train_p)) # 0 NA
sum(is.na(st_test_p)) # 0 NA

# divide the train data into two parts for validation
set.seed(2)
s = sample(1:nrow(st_train_p),0.8*nrow(st_train_p))
st_train1_p = st_train_p[s,]
st_train2_p = st_train_p[-s,]

# decision trees
library(tree)
tree.fit = tree(store~., data = st_train1_p, na.action = na.exclude)
# Error in tree(store ~ ., data = st_train1_p, na.action = na.exclude) : 
# factor predictors must have at most 32 levels
# Error was removed by further data processing
summary(tree.fit)

# tree in text form
tree.fit

# tree in visual format
plot(tree.fit)
text(tree.fit)

# Performance on validation set
val.score = predict(tree.fit, newdata = st_train2_p, type='vector')[,2]
pROC::roc(st_train2_p$store,val.score)$auc
# AUC Score is 0.7922 go for random forests

# random forests
library(randomForest)
glimpse(st_train1_p)
rf.fit = randomForest(store~., data = st_train1_p, na.action = na.exclude, do.trace = T)
# Error in randomForest.default(m, y, ...) : 
# Can not handle categorical predictors with more than 53 categories.
# Variables having more than 53 unique values needs to converted to dummies
summary(rf.fit)
rf.fit

# Performance on validation set
rf.val.score = predict(rf.fit, newdata = st_train2_p, type='prob')[,2]
pROC::roc(st_train2_p$store,rf.val.score)$auc
# AUC Score is 0.8285

# predict the response for val data
rf.val.score.class = predict(rf.fit, newdata = st_train2_p) # default type is class prediction

# draw a table to find confusion matrix
table(st_train2_p$store, rf.val.score.class)

# Build the model on full train data set
rf.fit.fnl = randomForest(store~., data = st_train_p, na.action = na.exclude, do.trace = T)
summary(rf.fit.fnl)
rf.fit.fnl

# predict the probabability score for each observations
fnl.score = predict(rf.fit.fnl, newdata = st_test_p , type = "prob")[,2]

# ensure no NA values in test.pred
sum(is.na(fnl.score)) # no NA

# write into the folder
getwd()
write.csv(fnl.score, "P.Akash_Pattanaik_P2_part2.csv", row.names = F)

################














