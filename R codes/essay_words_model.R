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




df_words<-read.csv("/Users/sajal/UPENN/independent_study/arranged_data/word_feature.csv",header=T) 
df_words <- df_words[,c(2:2001)] #Removing col of appids
df_words <- df_words[-exclude_list,]
df_mat <- as.matrix(df_words)
df_sqmat <- sqrt(df_mat)
normal_c <- rowSums(df_sqmat)
df_norm <- df_sqmat/normal_c
df_norm<-cbind(df_norm, normal_c) #Adding normalization constant as an extra column.
mod_df_words <- as.data.frame(df_norm)


#data from previously defined train and test indices
word_train <- mod_df_words[indices,]
word_test <- mod_df_words[-indices,]

#Ridge regression in words
xmat_tr <- data.matrix(word_train)
ridge_model_words <- cv.glmnet(xmat_tr,trainY, alpha = 0) 

rr.cv <- cv.glmnet(xmat_tr, trainY, alpha = 0)
plot(rr.cv) #Plotting variation of error with change of lambda (Regularization parameter)
rr.bestlam <- rr.cv$lambda.min
plot(ridge_model_words$glmnet.fit,xvar="lambda",label=TRUE) #Plot for coffecients of variables vs log lambda

xmat_test <- data.matrix(word_test)
predict_ridge_words <- predict(ridge_model_words, xmat_test, type ="response", s=rr.cv$lambda.min)[,1]
mse_words_ridge <- sqrt(mean((predict_ridge_words - testY)^2))

predict_words_tr_ridge <-  predict(ridge_model_words, xmat_tr, type ="response", s=rr.cv$lambda.min)[,1] #Used for ensembling

