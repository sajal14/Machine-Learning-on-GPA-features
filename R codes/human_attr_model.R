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




#Ridge Human

xmat_tr <- data.matrix(human_train)
ridge_model <- cv.glmnet(xmat_tr,trainY, alpha = 1) 

rr.cv <- cv.glmnet(xmat_tr, trainY, alpha = 1)
plot(rr.cv) #Plotting variation of error with change of lambda (Regularization parameter)
rr.bestlam <- rr.cv$lambda.min
plot(ridge_model$glmnet.fit,xvar="lambda",label=TRUE) 

xmat_test <- data.matrix(human_test)
predict_ridge_human <- predict(ridge_model, xmat_test, type ="response", s=rr.bestlam)[,1]
mse_human_ridge <- sqrt(mean((predict_ridge_human - testY)^2))

predict_human_tr_ridge <-  predict(ridge_model, xmat_tr, type ="response", s=rr.bestlam)[,1] #Used for ensembling




