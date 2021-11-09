# Uploading CSV to R 
Data <- read.csv(file.choose(),header = T) 
nrow(Data) # 1000 rows 
dim(Data) # 1000 rows and 101 columns 
names(Data) # 1 Response Variable Y and 100 predictors 

# Data Preprocessing 

func1 <- function(x){
sum(is.na(x))}
apply(Data,2,func1)  # check if there are NAs in variables 
Data <- Data[,-56] # Remove X55 

# Splitting Data Set into Training and Test Set 
set.seed(4211) # ensures reproduciblity of results 
x <- model.matrix(Y~.,Data)[,-1]
ind <- sample(2,nrow(x),replace = TRUE,prob = c(0.8,0.2))
# Split into training,test set 
x.train <- x[ind == 1,]
x.test <- x[ind == 2,]
y.train <- Data$Y[ind == 1]
y.test <- Data$Y[ind == 2]

nrow(x.train) 
[1] 799
nrow(x.test) 
[1] 201

# LASSO Coefficient Plot 
library(glmnet) 
grid <- 10^seq(10,-2,length = 100) 
lasso.mod <- glmnet(x.train,y.train,alpha = 1,lambda = grid) 
plot(lasso.mod) 

# Plotting Cross-Validation Error as a function of lambda 
cv.out <- cv.glmnet(x.train,y.train,alpha = 1)
plot(cv.out) 
# Generate the lambda that globally minimizes CV Error 
bestlam <- cv.out$lambda.min 
bestlam 
[1] 0.1885761

# Fitting LASSO model with optimal lambda 
lasso.pred <- predict(lasso.mod,s = bestlam,newx= x.test)
# Calculating test MSE and R^2 
mean((lasso.pred-y.test)^2) 
[1] 24.25108
# Computing R^2 
Rsquared <- function(x,y){
1-sum((x-y)^2)/sum((y-mean(y))^2)
}
Rsquared(lasso.pred,y.test) 
[1] 0.607644

# Test MSE and R^2 of Least Squares Fit  
linear.pred <- predict(lasso.mod,s = 0,newx = x.test) 
mean((linear.pred - y.test)^2)
[1] 25.43509
Rsquared(linear.pred,y.test) 
[1] 0.5884881


# Extracing Coefficients of LASSO Model 
# Fit the LASSO model on the entire Data Set 
output <- glmnet(x,Data$Y,alpha = 1,lambda = grid) 
lasso.coef <- predict(output,type = "coefficients",s=bestlam)[1:100,]
lasso.coef[lasso.coef!=0]



# Elastic Net Regression # 
# Plotting Coefficient Plots for Different alpha 
alpha0 <- glmnet(x.train,y.train,alpha = 0,lambda = grid) 
alpha1 <- glmnet(x.train,y.train,alpha = 0.1,lambda = grid) 
alpha2 <- glmnet(x.train,y.train,alpha = 0.2,lambda = grid) 
alpha3 <- glmnet(x.train,y.train,alpha = 0.3,lambda = grid) 
alpha4 <- glmnet(x.train,y.train,alpha = 0.4,lambda = grid) 
alpha5 <- glmnet(x.train,y.train,alpha = 0.5,lambda = grid) 
alpha6 <- glmnet(x.train,y.train,alpha = 0.6,lambda = grid) 
alpha7 <- glmnet(x.train,y.train,alpha = 0.7,lambda = grid) 
alpha8 <- glmnet(x.train,y.train,alpha = 0.8,lambda = grid) 
alpha9 <- glmnet(x.train,y.train,alpha = 0.9,lambda = grid) 

par(mfrow = c(2,5)) 
plot(alpha0,sub = "alpha = 0")
plot(alpha1,sub = "alpha = 0.1")
plot(alpha2,sub = "alpha = 0.2")
plot(alpha3,sub = "alpha = 0.3") 
plot(alpha4,sub = "alpha = 0.4") 
plot(alpha5,sub = "alpha = 0.5") 
plot(alpha6,sub = "alpha = 0.6") 
plot(alpha7,sub = "alpha = 0.7") 
plot(alpha8,sub = "alpha = 0.8") 
plot(alpha9,sub = "alpha = 0.9") 

# Plotting CV Error as a function of lambda 
cv.out0 <- cv.glmnet(x.train,y.train,alpha = 0)
cv.out1 <- cv.glmnet(x.train,y.train,alpha = 0.1)
cv.out2 <- cv.glmnet(x.train,y.train,alpha = 0.2)
cv.out3 <- cv.glmnet(x.train,y.train,alpha = 0.3)
cv.out4 <- cv.glmnet(x.train,y.train,alpha = 0.4)
cv.out5 <- cv.glmnet(x.train,y.train,alpha = 0.5)
cv.out6 <- cv.glmnet(x.train,y.train,alpha = 0.6)
cv.out7 <- cv.glmnet(x.train,y.train,alpha = 0.7)
cv.out8 <- cv.glmnet(x.train,y.train,alpha = 0.8)
cv.out9 <- cv.glmnet(x.train,y.train,alpha = 0.9)

par(mfrow=c(2,5))
plot(cv.out0,sub = "alpha = 0")
plot(cv.out1,sub = "alpha = 0.1")  
plot(cv.out2,sub = "alpha = 0.2") 
plot(cv.out3,sub = "alpha = 0.3") 
plot(cv.out4,sub = "alpha = 0.4") 
plot(cv.out5,sub = "alpha = 0.5") 
plot(cv.out6,sub = "alpha = 0.6") 
plot(cv.out7,sub = "alpha = 0.7") 
plot(cv.out8,sub = "alpha = 0.8") 
plot(cv.out9,sub = "alpha = 0.9")

# Finding optimal lambda for each alpha value 
result <- data.frame(alpha = 0,lambda = cv.out0$lambda.min) 
result <- rbind(result,data.frame(alpha = 0.1,lambda = cv.out1$lambda.min)) 
result <- rbind(result,data.frame(alpha = 0.2,lambda = cv.out2$lambda.min))
result <- rbind(result,data.frame(alpha = 0.3,lambda = cv.out3$lambda.min))
result <- rbind(result,data.frame(alpha = 0.4,lambda = cv.out4$lambda.min))
result <- rbind(result,data.frame(alpha = 0.5,lambda = cv.out5$lambda.min))
result <- rbind(result,data.frame(alpha = 0.6,lambda = cv.out6$lambda.min))
result <- rbind(result,data.frame(alpha = 0.7,lambda = cv.out7$lambda.min))
result <- rbind(result,data.frame(alpha = 0.8,lambda = cv.out8$lambda.min))
result <- rbind(result,data.frame(alpha = 0.9,lambda = cv.out9$lambda.min))
result 


# Computing Test MSE and R^2 Value 

# Making Prediction using test Data 
Elastic.pred0 <- predict(alpha0,s = cv.out0$lambda.min,newx = x.test) 
Elastic.pred1 <- predict(alpha1,s = cv.out1$lambda.min,newx = x.test) 
Elastic.pred2 <- predict(alpha2,s = cv.out2$lambda.min,newx = x.test) 
Elastic.pred3 <- predict(alpha3,s = cv.out3$lambda.min,newx = x.test) 
Elastic.pred4 <- predict(alpha4,s = cv.out4$lambda.min,newx = x.test) 
Elastic.pred5 <- predict(alpha5,s = cv.out5$lambda.min,newx = x.test) 
Elastic.pred6 <- predict(alpha6,s = cv.out6$lambda.min,newx = x.test) 
Elastic.pred7 <- predict(alpha7,s = cv.out7$lambda.min,newx = x.test) 
Elastic.pred8 <- predict(alpha8,s = cv.out8$lambda.min,newx = x.test) 
Elastic.pred9 <- predict(alpha9,s = cv.out9$lambda.min,newx = x.test) 

# Calculating Test MSE 

test0 <- mean((Elastic.pred0-y.test)^2) 
test1 <- mean((Elastic.pred1-y.test)^2) 
test2 <- mean((Elastic.pred2-y.test)^2) 
test3 <- mean((Elastic.pred3-y.test)^2) 
test4 <- mean((Elastic.pred4-y.test)^2) 
test5 <- mean((Elastic.pred5-y.test)^2) 
test6 <- mean((Elastic.pred6-y.test)^2) 
test7 <- mean((Elastic.pred7-y.test)^2) 
test8 <- mean((Elastic.pred8-y.test)^2) 
test9 <- mean((Elastic.pred9-y.test)^2) 


# Calculating test R^2 

R0 <- Rsquared(Elastic.pred0,y.test) 
R1 <- Rsquared(Elastic.pred1,y.test) 
R2 <- Rsquared(Elastic.pred2,y.test) 
R3 <- Rsquared(Elastic.pred3,y.test) 
R4 <- Rsquared(Elastic.pred4,y.test) 
R5 <- Rsquared(Elastic.pred5,y.test) 
R6 <- Rsquared(Elastic.pred6,y.test) 
R7 <- Rsquared(Elastic.pred7,y.test) 
R8 <- Rsquared(Elastic.pred8,y.test) 
R9 <- Rsquared(Elastic.pred9,y.test) 

# Summarizing results 
test.results <- data.frame(alpha = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),
test.MSE = c(test0,test1,test2,test3,test4,test5,test6,test7,test8,test9),
test.Rsquared = c(R0,R1,R2,R3,R4,R5,R6,R7,R8,R9))
test.results 

# Extracting the Coefficients of best Elastic Net Model 
output <- glmnet(x,Data$Y,alpha = 0.9,lambda = grid) 
Elastic.coef <- predict(output,type = "coefficients",s=cv.out9$lambda.min)[1:100,]
Elastic.coef[Elastic.coef!=0]

# Principal Component Analysis 

# Plot CV Error as a function of PC Components 
library(pls) 
train <- which(ind  == 1) 
pcr.fit <- pcr(Y~.,data = Data,subset = train,scale = TRUE,validation = "CV") 
validationplot(pcr.fit,val.type = "MSEP") 

# Extracting the optimal value of M 
cverr <- RMSEP(pcr.fit)$val[1,,]
imin <- which.min(cverr) - 1 # Find the optimal number of PC to use 
imin

# Summary of pcr.fit 
summary(pcr.fit) 
train.mse <- 4.994 ** 2 
train.mse 
[1] 24.94004

# Evaluating the test MSE and R^2 of PCR 
pcr.pred <- predict(pcr.fit,x.test,ncomp=9) 
mean((pcr.pred-y.test)^2) 
[1] 40.33388
pcr.Rsquared <- Rsquared(pcr.pred,y.test) 
pcr.Rsquared 
[1] 0.347442

# Partial Least Squares 
# Training the PLS Model 
pls.fit <- plsr(Y~.,data = Data,subset = train,scale = TRUE,validation = "CV") 
summary(pls.fit) 

# Plotting CV Error as a function of PLS Components 
validationplot(pls.fit,val.type="MSEP")

# Extracting Optimal number of PLS Components 

cverr <- RMSEP(pls.fit)$val[1,,]
imin <- which.min(cverr) - 1
imin


# Calculating training MSE and R^2 


training.mse <- 4.995 ** 2 
training.mse 

pls.train.pred <- predict(pls.fit,x.train,ncomp = 8) 
pls.train.Rsquared <- Rsquared(pls.train.pred,y.train) 
pls.train.Rsquared 
[1] 0.6591573


# Calculating the test MSE and R^2 

pls.pred <- predict(pls.fit,x.test,ncomp = 8) 
mean((pls.pred-y.test)^2) 
[1] 25.82683
pls.Rsquared <- Rsquared(pls.pred,y.test) 
pls.Rsquared 
[1] 0.5821501

# Implementing Xgboost 

library(xgboost)
d.train <- xgb.DMatrix(x.train, label = y.train)
d.test <- xgb.DMatrix(x.test, label = y.test)

# Iterate through 200 times to find best parameters 
# to minimize Training MSE 


optimal.parameters <- list() # store the optimal parameters after for loop 
optimal.seed <- 4211
optimal.rmse <- Inf # store minimum root MSE 
optimal.idx <- 0 # Locate the smallest minimum root MSE

for (iter in 1:200){
	parameters <- list(objective = "reg:linear", 
				eval_metric = "rmse",
				max_depth = sample(2:10,1), 
				eta = runif(1,0.01,0.3), 
				subsample = runif(1,0.6,0.9), 
				colsample_bytree = runif(1,0.5,0.8), 
				min_child_weight = sample(1:40,1), 
				max_delta_step = sample(1:10,1)) 
	
	# At each iteration make use of Randomisation to minimize 
	# training MSE 
	
	no.rounds <- 1000 
	cv.folds <- 10 # Performing 10 fold Cross-Validation 
	current.seed <- sample.int(10000,1) # find the best seed that minimizes CV
	set.seed(current.seed) 
	
	model <- xgb.cv(data = d.train,params = parameters,nfold = cv.folds,
				nrounds = no.rounds,verbose = F, 
				early_stopping_rounds = 8,maximise = FALSE) 
	
	current.idx <- model$best_iteration 
	current.rmse <- model$evaluation_log[current.idx]$test_rmse_mean 
	# update the optimal rmse
	if (current.rmse < optimal.rmse){
		optimal.parameters <- parameters 
		optimal.idx <- current.idx 
		optimal.seed <- current.seed 
		optimal.rmse <- current.rmse}
	
}

# Train the model using the best parametrs from the for loop 


set.seed(optimal.seed) 
xg_mod <- xgboost(data = d.train,params = optimal.parameters,nround = optimal.idx,verbose = F)

# Best Tuning Parameters 
result <- data.frame(optimal.parameters,optimal.idx,optimal.rmse,optimal.seed) 
result 


   objective eval_metric max_depth        eta subsample colsample_bytree
1 reg:linear        rmse         2 0.02979434 0.7835935        0.7236754
  min_child_weight max_delta_step optimal.idx optimal.rmse optimal.seed
1               19              3         370     4.837736          575

# Calculate test MSE and R^2 
yhat_xg <- predict(xg_mod,d.test) 
test.mse <- mean((yhat_xg-y.test)^2) 
test.mse 
[1] 25.14059
Rsquared(yhat_xg,y.test) 
[1] 0.5932528


	
# Feature importance 
importance_matrix <- xgb.importance(colnames(x.train), model = xg_mod)
library(Ckmeans.1d.dp) # for xgb.ggplot.importance
xgb.ggplot.importance(importance_matrix, top_n = 10, 
                      measure = "Gain")

# Plot Xgboost Trees 
library(DiagrammeR) 
xgb.plot.tree(model = xg_mod,trees = 1)
xgb.plot.tree(model = xg_mod,trees = 2)
xgb.plot.tree(model = xg_mod,trees = 3)

# LASSO is the best performing model on test data 
# we shall use LASSO to predict values of y from an independent set 
# Upload Data Set to R 
test <- read.csv(file.choose(),header = T) 
test <- test[,-55] # remove X55 
edited <- data.frame(Y = rep(1,nrow(test)),test) 
final.test <- model.matrix(Y~.,edited)[,-1] 
Y <- predict(lasso.mod,s = bestlam,newx = final.test) 
values <- data.frame(Y = Y) 
library(readr) 
setwd("C:\\Users\\Aiman\\Documents\\DSA4211")
write_csv(values,"A0187108B.csv")





