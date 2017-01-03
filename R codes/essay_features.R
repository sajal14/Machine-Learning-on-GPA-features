
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




#Essay extra features

df_essayf<-read.csv("/Users/sajal/UPENN/independent_study/arranged_data/essay_features.csv",header=T) 
df_essayf <- df_essayf[,c(2:4)] #Removing col of appids
df_essayf <- df_essayf[-exclude_list,]

essayf_train <- df_essayf[indices,]
essayf_test <- df_essayf[-indices,]

#Ridge regression in words
xmat_tr <- data.matrix(essayf_train)

rr.cv <- cv.glmnet(xmat_tr, trainY, alpha = 0.5)
plot(rr.cv) #Plotting variation of error with change of lambda (Regularization parameter)
rr.bestlam <- rr.cv$lambda.min
plot(rr.cv$glmnet.fit,xvar="lambda",label=TRUE) #Plot for coffecients of variables vs log lambda

xmat_test <- data.matrix(essayf_test)
predict_ridge_words <- predict(rr.cv, xmat_test, type ="response", s=rr.bestlam)[,1]
mse_essayf_ridge <- sqrt(mean((predict_ridge_words - testY)^2))






#Bigram words model

df_bigram<-read.csv("/Users/sajal/UPENN/independent_study/arranged_data/bigram_feature.csv",header=T) 
df_bigram <- df_bigram[,c(2:2000)] #Removing col of appids
df_bigram <- df_bigram[-exclude_list,]
#df_mat <- as.matrix(df_bigram)
#df_sqmat <- sqrt(df_mat)
#normal_c <- rowSums(df_sqmat)
#df_norm <- df_sqmat/normal_c
#df_norm<-cbind(df_norm, normal_c) #Adding normalization constant as an extra column.
#mod_df_bigram <- as.data.frame(df_norm)


bigram_train <- df_bigram[indices,]
bigram_test <- df_bigram[-indices,]

xmat_tr <- data.matrix(bigram_train)
ridge_model_bigrams <- cv.glmnet(xmat_tr,trainY, alpha = 1) 

rr.cv <- cv.glmnet(xmat_tr, trainY, alpha = 1)
plot(rr.cv) #Plotting variation of error with change of lambda (Regularization parameter)
rr.bestlam <- rr.cv$lambda.min
plot(ridge_model_bigrams$glmnet.fit,xvar="lambda",label=TRUE) #Plot for coffecients of variables vs log lambda

xmat_test <- data.matrix(bigram_test)
predict_ridge_bigrams <- predict(ridge_model_bigrams, xmat_test, type ="response", s=rr.bestlam)[,1]
mse_bigrams_ridge <- sqrt(mean((predict_ridge_bigrams - testY)^2))


# Correlation for bigrams

cor_wordY <- cor(df_bigram,Y)

essay_rate <- df_full$Essay.Rating.Avg
cor_word2Y <- cor(df_bigram,df_full$Essay.Rating.Avg)

