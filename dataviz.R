#########################NEGATIF LB##############################
labelnegLB<- read.csv2(file = "D:\\Skripsi\\INI SKRIPSI\\DATA\\labelnegLB.csv", header = TRUE)

negLB<-Corpus(VectorSource(labelnegLB$text))
stop<-readLines("~/id.stopwords.02.01.2016.txt")
negLB<-tm_map(negLB,removeWords,c('aplikasi',"jiwa", 'app','aplikasinya','banget','gak','aja','udah','nya',
                                "pake",'pas','kali','sih','bikin','kalo','mulu','dapet','gitu','klo','udh','dah',
                                'cek','trs','emang','gini','oke','deh','karna'))
negLB<-tm_map(negLB,removeWords,stop)

{
  dtm<-TermDocumentMatrix(negLB)
  m<-as.matrix(dtm)
  v<-sort(rowSums(m),decreasing = TRUE)
  d<-data.frame(word=names(v),freq=v)
}

wordcloud2(d, backgroundColor = "white",
           color = "random-light", size = 0.4)

d%>%
  filter(freq>=15)%>%
  mutate(word=reorder(word,freq))%>%
  ggplot(aes(word,freq,fill=word))+
  geom_bar(stat='identity')+
  xlab('Kata')+
  ylab('Frekuensi')+
  ggtitle(paste("Kata-Kata yang Paling Sering Muncul Pada Label Negatif"))+
  theme(legend.position = "none")+
  coord_flip()
###########################POSITIF LB###############################

labelposLB<- read.csv2(file = "D:\\Skripsi\\INI SKRIPSI\\DATA\\labelposLB.csv", header = TRUE)

posLB<-Corpus(VectorSource(labelposLB$text))
stop<-readLines("~/id.stopwords.02.01.2016.txt")
posLB<-tm_map(posLB,removeWords,c('aplikasi',"jiwa", 'app','aplikasinya','banget','gak','aja','udah','nya',
                                  "pake",'pas','kali','sih','bikin','kalo','mulu','dapet','gitu','klo','udh','dah',
                                  'cek','trs','emang','gini','oke','deh','karna'))
posLB<-tm_map(posLB,removeWords,stop)

{
  dtm<-TermDocumentMatrix(posLB)
  m<-as.matrix(dtm)
  v<-sort(rowSums(m),decreasing = TRUE)
  d<-data.frame(word=names(v),freq=v)
}

wordcloud2(d, backgroundColor = "white",
           color = "random-light", size = 0.4)

d%>%
  filter(freq>=15)%>%
  mutate(word=reorder(word,freq))%>%
  ggplot(aes(word,freq,fill=word))+
  geom_bar(stat='identity')+
  xlab('Kata')+
  ylab('Frekuensi')+
  ggtitle(paste("Kata-Kata yang Paling Sering Muncul Pada Label Positif"))+
  theme(legend.position = "none")+
  coord_flip()

labelnegM<- read.csv2(file = "D:\\Skripsi\\INI SKRIPSI\\DATA\\labelnegM.csv", header = TRUE)

negM<-Corpus(VectorSource(labelnegM$text))
stop<-readLines("~/id.stopwords.02.01.2016.txt")
negM<-tm_map(negLB,removeWords,c('aplikasi',"jiwa", 'app','aplikasinya','banget','gak','aja','udah','nya',
                                  "pake",'pas','kali','sih','bikin','kalo','mulu','dapet','gitu','klo','udh','dah',
                                  'cek','trs','emang','gini','oke','deh','karna'))
negM<-tm_map(negM,removeWords,stop)

{
  dtm<-TermDocumentMatrix(negM)
  m<-as.matrix(dtm)
  v<-sort(rowSums(m),decreasing = TRUE)
  d<-data.frame(word=names(v),freq=v)
}

wordcloud2(d, backgroundColor = "white",
           color = "random-light", size = 0.4)

d%>%
  filter(freq>=15)%>%
  mutate(word=reorder(word,freq))%>%
  ggplot(aes(word,freq,fill=word))+
  geom_bar(stat='identity')+
  xlab('Kata')+
  ylab('Frekuensi')+
  ggtitle(paste("Kata-Kata yang Paling Sering Muncul Pada Label Negatif"))+
  theme(legend.position = "none")+
  coord_flip()
