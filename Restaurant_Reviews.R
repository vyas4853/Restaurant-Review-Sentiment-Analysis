# Natural Language Processing

# Importing the dataset
setwd("G:\\udmey machine learing content\\Machine-Learning-A-Z\\Machine Learning A-Z New\\Part 7 - Natural Language Processing\\Section 36 - Natural Language Processing")
dataset_original = read.delim("Restaurant_Reviews.tsv", quote = '', stringsAsFactors = FALSE)

##cleaning the text dataset_original
#install.packages("tm", repos="http://R-Forge.R-project.org")
library(tm)
####
corpus<- VCorpus(VectorSource(dataset_original$Review))
###convert all words of review column in lowercase column.
corpus<- tm_map(corpus, content_transformer(tolower))
###There is no need of numbers in review so removing numbers from reviews
corpus<- tm_map(corpus, removeNumbers)
####remove punctuation from the review
corpus<- tm_map(corpus, removePunctuation)

##removing unwanted words from reviews like is, are , am, this etc.

library(SnowballC)
corpus<- tm_map(corpus, removeWords, stopwords())

#######Steming  means getting roots of every words
corpus<-  tm_map(corpus, stemDocument)

#######Removing extra space##############
corpus<- tm_map(corpus, stripWhitespace)

##creating bag of words
dtm<- DocumentTermMatrix(corpus)


###Removing very less frequent words
dtm<- removeSparseTerms(dtm, 0.999)

##converting sparse matrix into dataframe
dataset<- as.data.frame(as.matrix(dtm))
dataset$Liked<- dataset_original$Liked


##converting dependent variable to the factor

dataset$Liked<- factor(dataset$Liked, levels = c(0,1))

##splitting the dataset into training set and test set
set.seed(123)
x<- sample(1:nrow(dataset), nrow(dataset)/5)
training_set<- dataset[-x, ]
test_set<-dataset[x, ]

########################################################Random Forest###################################################
# Fitting Random Forest Classification to the Training set
# install.packages('randomForest')
library(randomForest)
set.seed(123)
classifier = randomForest(x = training_set[-692],
                          y = training_set$Liked,
                          ntree = 10)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-692])

# Making the Confusion Matrix
cm = table(test_set[, 692], y_pred)
########accuracy is 80%########################

##############################################################Naive Bayes #########################################################
library(e1071)
classifer1<- naiveBayes(x=training_set[-692], y=training_set$Liked)

#predicting the test set resu

# Predicting the Test set results
y_pred1 = predict(classifier, newdata = test_set[-692])

# Making the Confusion Matrix
cm1 = table(test_set[ ,692], y_pred1)


#######################accuracy is 81%###########################################################################################





















