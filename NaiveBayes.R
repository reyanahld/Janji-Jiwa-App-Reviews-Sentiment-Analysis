library(dplyr)
library(tidyr)

datamanual <- read.csv2(file = "D:\\Skripsi\\INI SKRIPSI\\DATA\\labelmanual.csv", header = TRUE)
head(datamanual$text)
konvjum2<-function(x){
  ifelse(x > 0, "yes", ifelse(x == 0, "neutral", "no"))
}

corpus2<-Corpus(VectorSource(datamanual$text))
stop<-readLines("~/id.stopwords.02.01.2016.txt")
corpus2<-tm_map(corpus2,removeWords,c('dan','yg','ini','itu'))
corpus2<-tm_map(corpus2,removeWords,stop)

dtm2<-DocumentTermMatrix(corpus2)
dtm2<-removeSparseTerms(dtm2, 0.98)
dtm2
datasetM<-apply(dtm2, 2, konvjum2)
dataset2<-as.data.frame(as.matrix(datasetM))
dataset2$Class=datamanual$klasifikasi

table(dataset2$Class)

set.seed(447)  # Mengatur seed untuk hasil yang dapat direproduksi
acak=sample(1:nrow(dataset2), 491) #asumsi 90% = data train
train_set2<- dataset2[acak,]
#Kelas Testing
test_set2<- dataset2[-acak,]


control<-trainControl(method='cv', number = 10)
system.time(classifier_nb<-naiveBayes(train_set2,train_set2$Class, laplace =1,
                                      trControl = control, tuneLenght = 7))
nb_pred2 = predict(classifier_nb, type = 'class', newdata = test_set2)
confusionMatrix(nb_pred2, as.factor(test_set2$Class), mode = 'everything')
