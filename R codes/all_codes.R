
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

asian_race = as.character(df_full$Race)
asian_race[asian_race != "Asian"] <- 0
asian_race[asian_race == "Asian"] <- 1
asian_race <- as.data.frame(asian_race)
machine_df["Asian_race"] <- asian_race


#white_race = as.character(df_full$Race)
#white_race[white_race != "White"] <- 0
#white_race[white_race == "White"] <- 1
#machine_df["White_race"] <- as.data.frame(white_race)


black_race = as.character(df_full$Race)
black_race[black_race != "Black"] <- 0
black_race[black_race == "Black"] <- 1
machine_df["Black_race"] <- as.data.frame(black_race)


amind_race = as.character(df_full$Race)
amind_race[amind_race != "American.Indian"] <- 0
amind_race[amind_race == "American.Indian"] <- 1
machine_df["American.Indian_race"] <- as.data.frame(amind_race)


multi_race = as.character(df_full$Race)
multi_race[multi_race != "Multiple"] <- 0
multi_race[multi_race == "Multiple"] <- 1
machine_df["Multiple_race"] <- as.data.frame(multi_race)



unspecified_race = as.character(df_full$Race)
unspecified_race[unspecified_race != "Unspecified"] <- 0
unspecified_race[unspecified_race == "Unspecified"] <- 1
machine_df["Unspecified_race"] <- as.data.frame(unspecified_race)

#Leaving White



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

#BaseLine #1
mse_avg <- sqrt(mean((testY - rep(mean(testY),559))^2))


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




############################################
#HUMAN ATTRIBUTES

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

#Ensemble Human and Machine

xmat_ensemble_tr <- as.data.frame(predict_mac_tr_ridge)
xmat_ensemble_tr["predict_human_tr_ridge"] <-  predict_human_tr_ridge
xmat_ensemble_tr <- data.matrix(xmat_ensemble_tr)

xmat_ensemble_test <- as.data.frame(predict_mac_ridge)
xmat_ensemble_test["predict_human_ridge"] <- predict_ridge_human
xmat_ensemble_test <- data.matrix(xmat_ensemble_test)


#ridge_model_en <- cv.glmnet(xmat_ensemble_tr,trainY, alpha = 0.5) 

rr.cv <- cv.glmnet(xmat_ensemble_tr, trainY, alpha = 0.5)
plot(rr.cv) #Plotting variation of error with change of lambda (Regularization parameter)
rr.bestlam <- rr.cv$lambda.min
plot(rr.cv$glmnet.fit,xvar="lambda",label=TRUE) 
coef(rr.cv,s="lambda.min")

predict_en_hum_mac <- predict(rr.cv, xmat_ensemble_test, type ="response", s=rr.bestlam)[,1]
mse_en_hum_mac <- sqrt(mean((predict_en_hum_mac - testY)^2))


#Random forest ensemble




#GPA ~ machine + human + sex* machine + sex * human

mac <-df_full[,c(6,7,8,11,12)] 
hum <-df_full[,c(13,14,15,16)]

sex <- as.data.frame(df_full[,c(9)])
sex[sex<0.5] <- -1

temp <- as.data.frame(rep(sex,5))
mac_sex = mac*temp

temp2 <- as.data.frame(rep(sex,4))
hum_sex = hum*temp2

comb <- mac
comb <- cbind(comb,hum)
comb <- cbind(comb,sex)
comb <- cbind(comb,mac_sex)
comb <- cbind(comb,hum_sex)


comb_train <- comb[indices,]
comb_test <- comb[-indices,]

xmat_tr <- data.matrix(comb_train)
#ridge_model <- cv.glmnet(xmat_tr,trainY, alpha = 0) 

rr.cv <- cv.glmnet(xmat_tr, trainY, alpha = 0.5)
plot(rr.cv) #Plotting variation of error with change of lambda (Regularization parameter)
rr.bestlam <- rr.cv$lambda.min
plot(rr.cv$glmnet.fit,xvar="lambda",label=TRUE) 

coef(rr.cv,s="lambda.min")

xmat_test <- data.matrix(comb_test)
predict_ridge_comb <- predict(rr.cv, xmat_test, type ="response", s=rr.bestlam)[,1]
mse_comb_ridge <- sqrt(mean((predict_ridge_comb - testY)^2))





#Only essay ratings

essay_df <- as.data.frame(human_df[,c(3)])
essay_train <- as.data.frame(essay_df[indices,])
essay_test <- as.data.frame(essay_df[-indices,])

xmat_tr <- as.matrix(essay_train)
ridge_model <- cv.glmnet(xmat_tr,trainY, alpha = 0)  #NOT WORKING   x should be a matrix with 2 or more columns

rr.cv <- cv.glmnet(xmat_tr, trainY, alpha = 0)
plot(rr.cv) #Plotting variation of error with change of lambda (Regularization parameter)
rr.bestlam <- rr.cv$lambda.min
plot(ridge_model$glmnet.fit,xvar="lambda",label=TRUE) 

xmat_test <- data.matrix(essay_test)
predict_ridge_essay <- predict(ridge_model, xmat_test, type ="response", s=rr.bestlam)[,1]
mse_essay_ridge <- sqrt(mean((predict_ridge_essay - testY)^2))



#Compare
compareTest <- data.frame(TrueLabels = testY, MachinePredict = predict_ridge, HumanPredict = predict_ridge_human)

xyplot(testY[1:100]+rep(mean(testY),100)+predict_ridge[1:100] + predict_ridge_human[1:100] + predict_en_hum_mac[1:100] ~c(1:100),type="l",auto.key = T) #First 100 test X values and prediction



#Prediction through essay words

#Normalization word count by square root of count and adding extra column for normalizing constant
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



#Ensemble predicted with words and predicted with machine attributes

#ensemble_predict <- (predict_ridge_words+predict_ridge)/2
#mse_ensemble <- sqrt(mean((ensemble_predict - testY)^2))

#xmat_ensemble_tr <- as.data.frame(predict_mac_tr_ridge)
#xmat_ensemble_tr["predict_words_tr_ridge"] <-  predict_words_tr_ridge
xmat_ensemble_tr <- as.data.frame(predict_words_tr_ridge)
xmat_ensemble_tr["mac_tr_ridge"] <- predict_mac_tr_ridge

xmat_ensemble_tr <- data.matrix(xmat_ensemble_tr)

#xmat_ensemble_test <- as.data.frame(predict_mac_ridge)
#xmat_ensemble_test["predict_words_ridge"] <- predict_ridge_words
xmat_ensemble_test <- as.data.frame(predict_ridge_words)
xmat_ensemble_test["mac_ridge"] <- predict_mac_ridge
xmat_ensemble_test <- data.matrix(xmat_ensemble_test)


ridge_model_en <- cv.glmnet(xmat_ensemble_tr,trainY, alpha = 0) 

rr.cv <- cv.glmnet(xmat_ensemble_tr, trainY, alpha = 0)
plot(rr.cv) #Plotting variation of error with change of lambda (Regularization parameter)
rr.bestlam <- rr.cv$lambda.min
plot(ridge_model_en$glmnet.fit,xvar="lambda",label=TRUE) 

predict_en_mac_words <- predict(ridge_model_en, xmat_ensemble_test, type ="response", s=rr.bestlam)[,1]
mse_en_mac_words <- sqrt(mean((predict_en_mac_words - testY)^2))

sqrt(mean(((4*predict_mac_ridge + predict_ridge_words)/5 - testY)^2))
sqrt(mean(((5*predict_mac_ridge + 1*predict_ridge_words)/6 - testY)^2))


#Ensemble predicted with human attributes and predicted with machine attributes
ensemble_predict_hum_mac <- (predict_ridge_human+predict_ridge)/2
mse_ensemble_hum_mac <- sqrt(mean((ensemble_predict_hum_mac - testY)^2))




#Essay Ratings only

essay_rate_df = data.frame(machine_df[,c(1)])
essay_rate_df <- cbind(essay_rate_df[1],essay_rate_df[1]);

mod_mac_train <- essay_rate_df[indices,]
mod_mac_test <- essay_rate_df[-indices,]

xmat_tr <- data.matrix(mod_mac_train)
ridge_model <- cv.glmnet(xmat_tr,trainY, alpha = 0.5) 

rr.cv <- cv.glmnet(xmat_tr, trainY, alpha = 0.5)
plot(rr.cv) #Plotting variation of error with change of lambda (Regularization parameter)
rr.bestlam <- rr.cv$lambda.min
plot(ridge_model$glmnet.fit,xvar="lambda",label=TRUE) #Plot for coffecients of variables vs log lambda

xmat_test <- data.matrix(mod_mac_test)
predict_essay_ridge <- predict(ridge_model, xmat_test, type ="response", s=rr.bestlam)[,1]
mse_essay_ridge <- sqrt(mean((predict_essay_ridge - testY)^2))

#Ensemble predicted with essay and ratings
ensemble_predict_rate_words <- (predict_ridge_words+predict_essay_ridge)/2
mse_ensemble_rate_words <- sqrt(mean((ensemble_predict_rate_words - testY)^2))



#Correlations for words
cor_wordY <- cor(mod_df_words,Y)

#cor_wordY <- cor(df_words,df_full$Wharton.GPA_To.Date)

cor_wordY <- cor(mod_df_words,df_full$Gender)


essay_rate <- df_full$Essay.Rating.Avg
cor_word2Y <- cor(mod_df_words,df_full$Essay.Rating.Avg)


#Prediction from correlated words 

cor_data <- as.data.frame(mod_df_words$BCG)
cor_data["Bain"] <- as.data.frame(mod_df_words$Bain)
cor_data["Learning"] <- as.data.frame(mod_df_words$Learning)
cor_data["exciting"] <- as.data.frame(mod_df_words$exciting)
cor_data["analysts"] <- as.data.frame(mod_df_words$analysts)
cor_data["X.s"] <- as.data.frame(mod_df_words$X.s)
cor_data["challenging"] <- as.data.frame(mod_df_words$challenging)
cor_data["team"] <- as.data.frame(mod_df_words$team)
cor_data["problem"] <- as.data.frame(mod_df_words$problem)
cor_data["pharmaceutical"] <- as.data.frame(mod_df_words$pharmaceutical)
cor_data["trips"] <- as.data.frame(mod_df_words$trips)



cor_data["extenuating"] <- as.data.frame(mod_df_words$extenuating)
cor_data["here"] <- as.data.frame(mod_df_words$here)
cor_data["X250"] <- as.data.frame(mod_df_words$X250)
cor_data["recommenders"] <- as.data.frame(mod_df_words$recommenders)
cor_data["e.g."] <- as.data.frame(mod_df_words$e.g.)
cor_data["circumstances"] <- as.data.frame(mod_df_words$circumstances)
cor_data["please"] <- as.data.frame(mod_df_words$please)
cor_data["aware"] <- as.data.frame(mod_df_words$aware)
cor_data["gaps"] <- as.data.frame(mod_df_words$gaps)
cor_data["Committee"] <- as.data.frame(mod_df_words$Committee)


cor_data["Academic"] <- as.data.frame(mod_df_words$Academic)
cor_data["aspire"] <- as.data.frame(mod_df_words$aspire)
cor_data["element"] <- as.data.frame(mod_df_words$element)
cor_data["contributing"] <- as.data.frame(mod_df_words$contributing)
cor_data["professionally"] <- as.data.frame(mod_df_words$professionally)
cor_data["personally"] <- as.data.frame(mod_df_words$personally)
cor_data["yourself"] <- as.data.frame(mod_df_words$yourself)
cor_data["Wharton"] <- as.data.frame(mod_df_words$Wharton)
cor_data["community"] <- as.data.frame(mod_df_words$community)
cor_data["through"] <- as.data.frame(mod_df_words$through)


cor_data["your"] <- as.data.frame(mod_df_words$your)
cor_data["objectives"] <- as.data.frame(mod_df_words$objectives)
cor_data["normal_c"] <- as.data.frame(mod_df_words$normal_c)
cor_data["time"] <- as.data.frame(mod_df_words$time)
cor_data["when"] <- as.data.frame(mod_df_words$when)
cor_data["it"] <- as.data.frame(mod_df_words$it)
cor_data["was"] <- as.data.frame(mod_df_words$was)
cor_data["us"] <- as.data.frame(mod_df_words$us)
cor_data["down"] <- as.data.frame(mod_df_words$down)
cor_data["Robertson"] <- as.data.frame(mod_df_words$Robertson)



word_train <- cor_data[indices,]
word_test <- cor_data[-indices,]

#Ridge regression in words
xmat_tr <- data.matrix(word_train)
ridge_model_words <- cv.glmnet(xmat_tr,trainY, alpha = 0) 

rr.cv <- cv.glmnet(xmat_tr, trainY, alpha = 0)
plot(rr.cv) #Plotting variation of error with change of lambda (Regularization parameter)
rr.bestlam <- rr.cv$lambda.min
plot(ridge_model_words$glmnet.fit,xvar="lambda",label=TRUE) #Plot for coffecients of variables vs log lambda

xmat_test <- data.matrix(word_test)
predict_cor_ridge_words <- predict(ridge_model_words, xmat_test, type ="response", s=rr.bestlam)[,1]
mse_cor_words_ridge <- sqrt(mean((predict_cor_ridge_words - testY)^2))




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




#Regression with test scores

Y2 <- as.data.frame(df_full$Essay.Rating.Avg)

trainY2 <- Y2[indices,]
testY2 <- Y2[-indices,]

xmat_tr <- data.matrix(mac_train)

rr.cv <- cv.glmnet(xmat_tr, trainY2, alpha = 0.5)
plot(rr.cv) #Plotting variation of error with change of lambda (Regularization parameter)
rr.bestlam <- rr.cv$lambda.min
plot(rr.cv$glmnet.fit,xvar="lambda",label=TRUE) #Plot for coffecients of variables vs log lambda
coef(rr.cv,s="lambda.min")
xmat_test <- data.matrix(mac_test)
predict_mac_ridge_tscore <- predict(rr.cv, xmat_test, type ="response", s=rr.bestlam)[,1]
mse_machine_ridge_tscore <- sqrt(mean((predict_mac_ridge_tscore - testY2)^2))



cor(Y2,df_full$Gender)
cor(Y,df_full$Gender)


#Predict Gender And then Correlation with GPA

trainYGen <- as.data.frame(df_full$Gender)[indices,]
testYGen <- as.data.frame(df_full$Gender)[-indices,]

word_train <- mod_df_words[indices,]
word_test <- mod_df_words[-indices,]

xmat_tr <- data.matrix(word_train)

rr.cv <- cv.glmnet(xmat_tr, trainYGen, alpha = 0.5)
plot(rr.cv) #Plotting variation of error with change of lambda (Regularization parameter)
rr.bestlam <- rr.cv$lambda.min
plot(rr.cv$glmnet.fit,xvar="lambda",label=TRUE) #Plot for coffecients of variables vs log lambda

xmat_test <- data.matrix(word_test)
predict_words_gen <- predict(rr.cv, xmat_test, type ="response", s=rr.cv$lambda.min)[,1]

predict_train_words_gen <- predict(rr.cv, xmat_tr, type ="response", s=rr.cv$lambda.min)[,1]

#Correlation between predicted sex and Wharton GPA
gpa = as.data.frame(testY)
predict_words_gen2 = as.data.frame(predict_words_gen)
cor_predictgen_gpa = cor(gpa,predict_words_gen2)



#Prediction with being and appearing female

gender_X_test <- as.data.frame(predict_words_gen)
gender_X_test["actual gender"] <- as.data.frame(testYGen)

gender_X_train <- as.data.frame(predict_train_words_gen)
gender_X_train["actual gender"] <- as.data.frame(trainYGen)

rr.cv <- cv.glmnet(as.matrix(gender_X_train), trainY, alpha = 0)
plot(rr.cv) #Plotting variation of error with change of lambda (Regularization parameter)
rr.bestlam <- rr.cv$lambda.min
plot(rr.cv$glmnet.fit,xvar="lambda",label=TRUE) #Plot for coffecients of variables vs log lambda
coef(rr.cv,s="lambda.min")
predict_GPA_gender <- predict(rr.cv, as.matrix(gender_X_test), type ="response", s=rr.cv$lambda.min)[,1]


lm_model <- lm(trainY~. , data=gender_X_train)




#Correlation between predicted sex and Wharton GPA
gpa = as.data.frame(testY)
predict_words_gen2 = as.data.frame(predict_words_gen)

cor_predictgen_gpa = cor(gpa,predict_words_gen2)

mse_GPA_gen <- sqrt(mean((predict_GPA_gender - testY)^2))



plot(rr.cv) 
plot(rr.cv$glmnet.fit,xvar ="lambda" , label=TRUE) 
rr.bestlam <- rr.cv$lambda.min



