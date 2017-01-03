
library(reshape2)
library(ggplot2)
library(gplots)
library(glmnet)
library(lattice) 

df_full<-read.csv("/Users/sajal/UPENN/independent_study/arranged_data/final_dataset.csv",header=T) 
exclude_list <- c(1,3,17,31,45,52,59,72,82,105,106,129,144,193,245,451,456,1182) #Excluding whose essays are not present.
df_full <- df_full[-exclude_list,]

#Correlation of different human and machine features
sub <- df_full[, c(5,6,7,8,9,10,11,12,13,14,15,16)]
cor_d <- cor(sub)

temp <- cor(df_full$Wharton.GPA_To.Date, df_full$Essay.Rating.Avg)
#melted_cor <- melt(cor_d)
#ggplot(data = melted_cor, aes(x=Var1, y=Var2, fill=value)) + geom_tile()
#qplot(x=Var1, y=Var2, data=melted_cor, fill=value, geom="tile") + scale_fill_gradient2(limits=c(-1, 1))

#dev.off()
#heatmap.2(cor_d,dendrogram = "none", Rowv = FALSE, Colv = FALSE, symm = TRUE,trace='both',nrow=10)

#Code for regression starts

machine_df <-df_full[,c(6,7,8,9,11,12)] #Inserted gender here
human_df <-df_full[,c(13,14,15,16)]

Y <- as.data.frame(df_full$Wharton.GPA_To.Date)

#***Should be common to all the models below ***#
set.seed(runif(1)*100)
row_len<-nrow(df_full)
indices <- sample(row_len,1000,replace = FALSE) #Get 1000 random indices 
#***Should be common to all the models below ***#



mac_train <- machine_df[indices,]
mac_test <- machine_df[-indices,]
human_train <- human_df[indices,]
human_test <- human_df[-indices,]

trainY <- Y[indices,]
testY <- Y[-indices,]


#Ridge Machine

xmat_tr <- data.matrix(mac_train)
ridge_model <- cv.glmnet(xmat_tr,trainY, alpha = 0) 

rr.cv <- cv.glmnet(xmat_tr, trainY, alpha = 0)
plot(rr.cv) #Plotting variation of error with change of lambda (Regularization parameter)
rr.bestlam <- rr.cv$lambda.min
plot(ridge_model$glmnet.fit,xvar="lambda",label=TRUE) #Plot for coffecients of variables vs log lambda
coef(rr.cv,s ="lambda.min")
xmat_test <- data.matrix(mac_test)
predict_mac_ridge <- predict(ridge_model, xmat_test, type ="response", s=rr.bestlam)[,1]
mse_machine_ridge <- sqrt(mean((predict_mac_ridge - testY)^2))

predict_mac_tr_ridge <-  predict(ridge_model, xmat_tr, type ="response", s=rr.bestlam)[,1] #Used for ensembling



#Lasso Machine

xmat_tr <- data.matrix(mac_train)
ridge_model <- cv.glmnet(xmat_tr,trainY, alpha = 1) 

rr.cv <- cv.glmnet(xmat_tr, trainY, alpha = 1)
plot(rr.cv) #Plotting variation of error with change of lambda (Regularization parameter)
rr.bestlam <- rr.cv$lambda.min
plot(ridge_model$glmnet.fit,xvar="lambda",label=TRUE) #Plot for coffecients of variables vs log lambda

xmat_test <- data.matrix(mac_test)
predict_lasso <- predict(ridge_model, xmat_test, type ="response", s=rr.bestlam)[,1]
mse_machine_lasso <- sqrt(mean((predict_lasso - testY)^2))


#Elastic Machine

xmat_tr <- data.matrix(mac_train)
#ridge_model <- cv.glmnet(xmat_tr,trainY, alpha = 0) 

rr.cv <- cv.glmnet(xmat_tr, trainY, alpha = 0.5)

lm_model <- lm(trainY~. , data=as.data.frame(xmat_tr))

plot(rr.cv) #Plotting variation of error with change of lambda (Regularization parameter)
plot(rr.cv$glmnet.fit,xvar ="lambda" , label=TRUE) 
rr.bestlam <- rr.cv$lambda.min
#plot(ridge_model$glmnet.fit,xvar="lambda",label=TRUE) #Plot for coffecients of variables vs log lambda
coef(rr.cv,s="lambda.min")
xmat_test <- data.matrix(mac_test)
predict_elastic <- predict(rr.cv, xmat_test, type ="response", s=rr.bestlam)[,1]
mse_machine_elastic <- sqrt(mean((predict_elastic - testY)^2))


#Only GPA and School Quality (mod_mac)
mod_mac_df = machine_df[,c(1,2)]
mod_mac_train <- mod_mac_df[indices,]
mod_mac_test <- mod_mac_df[-indices,]

xmat_tr <- data.matrix(mod_mac_train)
ridge_model <- cv.glmnet(xmat_tr,trainY, alpha = 0) 

rr.cv <- cv.glmnet(xmat_tr, trainY, alpha = 0)
plot(rr.cv) #Plotting variation of error with change of lambda (Regularization parameter)
rr.bestlam <- rr.cv$lambda.min
plot(ridge_model$glmnet.fit,xvar="lambda",label=TRUE) #Plot for coffecients of variables vs log lambda

xmat_test <- data.matrix(mod_mac_test)
predict_mod_mac_ridge <- predict(ridge_model, xmat_test, type ="response", s=rr.bestlam)[,1]
mse_mod_mac_ridge <- sqrt(mean((predict_mod_mac_ridge - testY)^2))

