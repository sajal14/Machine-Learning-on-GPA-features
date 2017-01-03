library(reshape2)
library(ggplot2)
library(gplots)
library(glmnet)
library(lattice) 
library(dplyr)
library(wordcloud)

df_words<-read.csv("/Users/sajal/UPENN/independent_study/arranged_data/word_feature.csv",header=T)
df_full<-read.csv("/Users/sajal/UPENN/independent_study/arranged_data/final_dataset.csv",header=T) 

exclude_list <- c(1,3,17,31,45,52,59,72,82,105,106,129,144,193,245,451,456,1182) #Excluding whose essays are not present.
df_words <- df_words[-exclude_list,]


essay_ratings <- as.data.frame(df_full$Essay.Rating.Avg)
essay_ratings <- as.data.frame(essay_ratings[-exclude_list,])


words_n_rating <- as.data.frame(df_words) #essay_ratings]
words_n_rating["Essay Ratings"] = essay_ratings


top_essays <- subset(words_n_rating, words_n_rating$`Essay Ratings` >=3.5)
low_essays <- subset(words_n_rating, words_n_rating$`Essay Ratings` <= 1)

#sum(df_words[,:])

bin_top_essay <- top_essays
bin_top_essay$`Essay Ratings` <- (1)

bin_low_essay <- low_essays
bin_low_essay$`Essay Ratings` <- (0)

#new_set <- rbind(top_essays,low_essays)
bin_new_set <- rbind(bin_top_essay,bin_low_essay) #Concatanating rows

#words_label <- colnames(top_essays)
#words_label = words_label[2:2001]


#word_freq_set <- as.data.frame(colSums(new_set[1000:2001]))
subset_new_set <- bin_new_set[2:2001]
essay_rate <- bin_new_set$`Essay Ratings`



correlation <- cor(subset_new_set,essay_rate)
corr<-na.omit(correlation)
row_words_label <- row.names(corr)
wordcloud(words = row_words_label, freq = corr , random.order = FALSE, max.words = 1983, min.freq = -1, colors = brewer.pal(9, "BuGn") )






#----------------------------------





word_freq_top <- colSums(top_essays[1000:2001])
words_label_top <- colnames(top_essays)
words_label_top = words_label_top[1000:2001]
wordcloud(words = words_label_top, freq = word_freq_top)

test <- as.data.frame(word_freq_top)



word_freq_low <- colSums(low_essays[1000:2001])
wordcloud(words = words_label_top, freq = word_freq_low, min.freq = 100,colorPalette = "RdPu")




