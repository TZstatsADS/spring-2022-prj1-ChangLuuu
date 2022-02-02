remove(list=ls())
packages.used=c("sentimentr", "ggplot2", "dplyr","tm", "syuzhet", "RColorBrewer","wordcloud")

# check packages that need to be installed.
packages.needed=setdiff(packages.used, 
                        intersect(installed.packages()[,1], 
                                  packages.used))
# install additional packages
if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE)
}

# load packages
library("sentimentr")
library("ggplot2")
library("dplyr")
library("tm")
library("syuzhet")
library("RColorBrewer")
library("wordcloud")

rawdata <- read.csv("~/Desktop/22spring/GR5243 Applied DS/spring-2022-prj1-ChangLuuu/data/philosophy_data.csv")
#head(rawdata,5)
colnames(rawdata)
unique(rawdata$author)
unique(rawdata$school)

s <- Sys.time()
#rawdata <- rawdata[rawdata$original_publication_date>1500,]
data <- rawdata[,c("title","author","school","original_publication_date",
                   "sentence_length","sentence_lowered")]
sentscore <- sentiment(data$sentence_lowered)
sentscore <- sentscore[sentscore$sentence_id==1,"sentiment"]
data$sentscore <- sentscore[[1]]
e <- Sys.time()
e-s

scoreschooldf <- data.frame(school=unique(data$school))
scoreschool <- c()
for(i in 1:length(unique(data$school))){
  scoreschool[i] <- mean(data[data$school==unique(data$school)[i],"sentscore"])
}
scoreschooldf$score <- scoreschool

png("~/Desktop/22spring/GR5243 Applied DS/spring-2022-prj1-ChangLuuu/figs/sentiment_score_school_level.png", units="in", width=8, height=5, res=600)
ggplot(data=scoreschooldf,aes(x=school,y=score,fill=score)) +
  geom_bar(stat="identity") +
  scale_fill_gradient(low = "light blue", high = "dark blue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5), 
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "sentiment score of school level")
dev.off()

scoreauthordf <- data.frame(author=unique(data$author))
scoreauthor <- c()
for(i in 1:length(unique(data$author))){
  scoreauthor[i] <- mean(data[data$author==unique(data$author)[i],"sentscore"])
}
scoreauthordf$score <- scoreauthor

png("~/Desktop/22spring/GR5243 Applied DS/spring-2022-prj1-ChangLuuu/figs/sentiment_score_author_level.png", units="in", width=8, height=5, res=600)
ggplot(data=scoreauthordf,aes(x=author, y=score, fill=score))+
  geom_bar(stat="identity")+
  scale_fill_gradient(low = "light blue", high = "dark blue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
        plot.title = element_text(hjust = 0.5))+
  labs(title = "sentiment score of author level")
dev.off()

gerideal <- data[data$school=="german_idealism","sentence_lowered"]
s <- Sys.time()
nrcsentgi <- get_nrc_sentiment(gerideal)
e <- Sys.time()
e-s

geridatti <- rep(NA,nrow(nrcsentgi))
for(i in 1:nrow(nrcsentgi)){
  if(nrcsentgi[i,"negative"]>nrcsentgi[i,"positive"]){
    geridatti[i] <- "negative"
  }
  if(nrcsentgi[i,"negative"]<nrcsentgi[i,"positive"]){
    geridatti[i] <- "positive"
  }
  if(nrcsentgi[i,"negative"]==nrcsentgi[i,"positive"]){
    geridatti[i] <- "neutral"
  }
}
gidf <- data.frame(attitude=c("positive","neutral","negative"),percentage=c(mean(geridatti=="positive"),mean(geridatti=="neutral"),mean(geridatti=="negative")))
png("~/Desktop/22spring/GR5243 Applied DS/spring-2022-prj1-ChangLuuu/figs/attitude_percentage_GI.png", units="in", width=8, height=5, res=600)
ggplot(data=gidf, aes(x=attitude,y=percentage,fill=percentage))+
  geom_bar(stat="identity")+
  scale_fill_gradient(low = "light blue", high = "dark blue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
        plot.title = element_text(hjust = 0.5))+
  labs(title = "attitude percentage of German Idealism")
dev.off()

percgi <- (colSums(nrcsentgi)/sum(colSums(nrcsentgi[,1:8])))[1:8]
senti <- names(percgi)
GIsent <- data.frame(sentiment=senti,percentage=percgi)

png("~/Desktop/22spring/GR5243 Applied DS/spring-2022-prj1-ChangLuuu/figs/sentiment_label_GI.png", units="in", width=8, height=5, res=600)
ggplot(data=GIsent, aes(x=sentiment,y=percentage,fill=percentage))+
  geom_bar(stat="identity")+
  scale_fill_gradient(low = "light blue", high = "dark blue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
        plot.title = element_text(hjust = 0.5))+
  labs(title = "sentiment label percentage of German Idealism")
dev.off()

continental <- data[data$school=="continental","sentence_lowered"]
s <- Sys.time()
nrcsentcon <- get_nrc_sentiment(continental)
e <- Sys.time()
e-s

conatti <- rep(NA,nrow(nrcsentcon))
for(i in 1:nrow(nrcsentcon)){
  if(nrcsentcon[i,"negative"]>nrcsentcon[i,"positive"]){
    conatti[i] <- "negative"
  }
  if(nrcsentcon[i,"negative"]<nrcsentcon[i,"positive"]){
    conatti[i] <- "positive"
  }
  if(nrcsentcon[i,"negative"]==nrcsentcon[i,"positive"]){
    conatti[i] <- "neutral"
  }
}
condf <- data.frame(attitude=c("positive","neutral","negative"),percentage=c(mean(conatti=="positive"),mean(conatti=="neutral"),mean(conatti=="negative")))

png("~/Desktop/22spring/GR5243 Applied DS/spring-2022-prj1-ChangLuuu/figs/attitude_percentage_Con.png", units="in", width=8, height=5, res=600)
ggplot(data=condf, aes(x=attitude,y=percentage,fill=percentage))+
  geom_bar(stat="identity")+
  scale_fill_gradient(low = "light blue", high = "dark blue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
        plot.title = element_text(hjust = 0.5))+
  labs(title = "attitude percentage of Continental")
dev.off()

perccon <- (colSums(nrcsentcon)/sum(colSums(nrcsentcon[,1:8])))[1:8]
senti <- names(perccon)
Consent <- data.frame(sentiment=senti,percentage=perccon)

png("~/Desktop/22spring/GR5243 Applied DS/spring-2022-prj1-ChangLuuu/figs/sentiment_label_percentage_Con.png", units="in", width=8, height=5, res=600)
ggplot(data=Consent, aes(x=sentiment,y=percentage,fill=percentage))+
  geom_bar(stat="identity")+
  scale_fill_gradient(low = "light blue", high = "dark blue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
        plot.title = element_text(hjust = 0.5))+
  labs(title = "sentiment label percentage of Continental")
dev.off()

Husserl <- data[data$author=="Husserl","sentence_lowered"]
s <- Sys.time()
nrcsentHusserl <- get_nrc_sentiment(Husserl)
e <- Sys.time()
e-s

Husserlatti <- rep(NA,nrow(nrcsentHusserl))
for(i in 1:nrow(nrcsentHusserl)){
  if(nrcsentHusserl[i,"negative"]>nrcsentHusserl[i,"positive"]){
    Husserlatti[i] <- "negative"
  }
  if(nrcsentHusserl[i,"negative"]<nrcsentHusserl[i,"positive"]){
    Husserlatti[i] <- "positive"
  }
  if(nrcsentHusserl[i,"negative"]==nrcsentHusserl[i,"positive"]){
    Husserlatti[i] <- "neutral"
  }
}
Husserldf <- data.frame(attitude=c("positive","neutral","negative"),percentage=c(mean(Husserlatti=="positive"),mean(Husserlatti=="neutral"),mean(Husserlatti=="negative")))

png("~/Desktop/22spring/GR5243 Applied DS/spring-2022-prj1-ChangLuuu/figs/attitude_percentage_Husserl.png", units="in", width=8, height=5, res=600)
ggplot(data=Husserldf, aes(x=attitude,y=percentage,fill=percentage))+
  geom_bar(stat="identity")+
  scale_fill_gradient(low = "light blue", high = "dark blue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
        plot.title = element_text(hjust = 0.5))+
  labs(title = "attitude percentage of Husserl")
dev.off()

percH <- (colSums(nrcsentHusserl)/sum(colSums(nrcsentHusserl[,1:8])))[1:8]
senti <- names(percH)
Hsent <- data.frame(sentiment=senti,percentage=percH)

png("~/Desktop/22spring/GR5243 Applied DS/spring-2022-prj1-ChangLuuu/figs/sentiment_label_percentage_Husserl.png", units="in", width=8, height=5, res=600)
ggplot(data=Hsent, aes(x=sentiment,y=percentage,fill=percentage))+
  geom_bar(stat="identity")+
  scale_fill_gradient(low = "light blue", high = "dark blue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
        plot.title = element_text(hjust = 0.5))+
  labs(title = "sentiment label percentage of Husserl")
dev.off()

Davis <- data[data$author=="Davis","sentence_lowered"]
s <- Sys.time()
nrcsentDavis <- get_nrc_sentiment(Davis)
e <- Sys.time()
e-s

Davisatti <- rep(NA,nrow(nrcsentDavis))
for(i in 1:nrow(nrcsentDavis)){
  if(nrcsentDavis[i,"negative"]>nrcsentDavis[i,"positive"]){
    Davisatti[i] <- "negative"
  }
  if(nrcsentDavis[i,"negative"]<nrcsentDavis[i,"positive"]){
    Davisatti[i] <- "positive"
  }
  if(nrcsentDavis[i,"negative"]==nrcsentDavis[i,"positive"]){
    Davisatti[i] <- "neutral"
  }
}

Davisdf <- data.frame(attitude=c("positive","neutral","negative"),percentage=c(mean(Davisatti=="positive"),mean(Davisatti=="neutral"),mean(Davisatti=="negative")))

png("~/Desktop/22spring/GR5243 Applied DS/spring-2022-prj1-ChangLuuu/figs/attitude_percentage_Davis.png", units="in", width=8, height=5, res=600)
ggplot(data=Davisdf, aes(x=attitude,y=percentage,fill=percentage))+
  geom_bar(stat="identity")+
  scale_fill_gradient(low = "light blue", high = "dark blue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
        plot.title = element_text(hjust = 0.5))+
  labs(title = "attitude percentage of Davis")
dev.off()

percD <- (colSums(nrcsentDavis)/sum(colSums(nrcsentDavis[,1:8])))[1:8]
senti <- names(percD)
Dsent <- data.frame(sentiment=senti,percentage=percD)

png("~/Desktop/22spring/GR5243 Applied DS/spring-2022-prj1-ChangLuuu/figs/sentiment_label_percentage_Davis.png", units="in", width=8, height=5, res=600)
ggplot(data=Dsent, aes(x=sentiment,y=percentage,fill=percentage))+
  geom_bar(stat="identity")+
  scale_fill_gradient(low = "light blue", high = "dark blue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
        plot.title = element_text(hjust = 0.5))+
  labs(title = "sentiment label percentage of Davis")
dev.off()

Foucault <- data[data$author=="Foucault","sentence_lowered"]
s <- Sys.time()
nrcsentFoucault <- get_nrc_sentiment(Foucault)
e <- Sys.time()
e-s

Foucaultatti <- rep(NA,nrow(nrcsentFoucault))
for(i in 1:nrow(nrcsentFoucault)){
  if(nrcsentFoucault[i,"negative"]>nrcsentFoucault[i,"positive"]){
    Foucaultatti[i] <- "negative"
  }
  if(nrcsentFoucault[i,"negative"]<nrcsentFoucault[i,"positive"]){
    Foucaultatti[i] <- "positive"
  }
  if(nrcsentFoucault[i,"negative"]==nrcsentFoucault[i,"positive"]){
    Foucaultatti[i] <- "neutral"
  }
}

Foucaultdf <- data.frame(attitude=c("positive","neutral","negative"),percentage=c(mean(Foucaultatti=="positive"),mean(Foucaultatti=="neutral"),mean(Foucaultatti=="negative")))

png("~/Desktop/22spring/GR5243 Applied DS/spring-2022-prj1-ChangLuuu/figs/attitude_percentage_Foucault.png", units="in", width=8, height=5, res=600)
ggplot(data=Foucaultdf, aes(x=attitude,y=percentage,fill=percentage))+
  geom_bar(stat="identity")+
  scale_fill_gradient(low = "light blue", high = "dark blue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
        plot.title = element_text(hjust = 0.5))+
  labs(title = "attitude percentage of Foucault")
dev.off()

percF <- (colSums(nrcsentFoucault)/sum(colSums(nrcsentFoucault[,1:8])))[1:8]
senti <- names(percF)
Fsent <- data.frame(sentiment=senti,percentage=percF)

png("~/Desktop/22spring/GR5243 Applied DS/spring-2022-prj1-ChangLuuu/figs/sentiment_label_percentage_Foucault.png", units="in", width=8, height=5, res=600)
ggplot(data=Fsent, aes(x=sentiment,y=percentage,fill=percentage))+
  geom_bar(stat="identity")+
  scale_fill_gradient(low = "light blue", high = "dark blue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
        plot.title = element_text(hjust = 0.5))+
  labs(title = "sentiment label percentage of Foucault")
dev.off()

HsentenceCorpus <- Corpus(VectorSource(data[data$author=="Husserl","sentence_lowered"]))
HsentenceCorpus<-tm_map(HsentenceCorpus, removeWords, stopwords("english"))
HsentenceCorpus<-tm_map(HsentenceCorpus, removeWords, character(0))
HsentenceCorpus<-tm_map(HsentenceCorpus, removePunctuation)
HsentenceCorpus<-tm_map(HsentenceCorpus, stripWhitespace)

Htdm <- TermDocumentMatrix(HsentenceCorpus)
Htdm = removeSparseTerms(Htdm, 0.99)
Htdm.tidy = tidytext::tidy(Htdm)
Htdm.overall=summarise(group_by(Htdm.tidy, term), sum(count))

png("~/Desktop/22spring/GR5243 Applied DS/spring-2022-prj1-ChangLuuu/figs/WC_Husserl.png", units="in", width=8, height=5, res=600)
wordcloud(Htdm.overall$term, Htdm.overall$`sum(count)`,
          scale=c(2.5,0.5),
          max.words=100,
          min.freq=10,
          random.order=FALSE,
          rot.per=0.3,
          random.color=FALSE,
          colors=brewer.pal(9,"Blues"))
dev.off()

DsentenceCorpus <- Corpus(VectorSource(data[data$author=="Davis","sentence_lowered"]))
DsentenceCorpus<-tm_map(DsentenceCorpus, removeWords, stopwords("english"))
DsentenceCorpus<-tm_map(DsentenceCorpus, removeWords, character(0))
DsentenceCorpus<-tm_map(DsentenceCorpus, removePunctuation)
DsentenceCorpus<-tm_map(DsentenceCorpus, stripWhitespace)

Dtdm <- TermDocumentMatrix(DsentenceCorpus)
Dtdm = removeSparseTerms(Dtdm, 0.99)
dtdm.tidy = tidytext::tidy(Dtdm)
Dtdm.overall=summarise(group_by(dtdm.tidy, term), sum(count))

png("~/Desktop/22spring/GR5243 Applied DS/spring-2022-prj1-ChangLuuu/figs/WC_Davis.png", units="in", width=8, height=5, res=600)
wordcloud(Dtdm.overall$term, Dtdm.overall$`sum(count)`,
          scale=c(2.5,0.5),
          max.words=100,
          min.freq=1,
          random.order=FALSE,
          rot.per=0.3,
          random.color=FALSE,
          colors=brewer.pal(9,"Blues"))
dev.off()

FsentenceCorpus <- Corpus(VectorSource(data[data$author=="Foucault","sentence_lowered"]))
FsentenceCorpus<-tm_map(FsentenceCorpus, removeWords, stopwords("english"))
FsentenceCorpus<-tm_map(FsentenceCorpus, removeWords, character(0))
FsentenceCorpus<-tm_map(FsentenceCorpus, removePunctuation)
FsentenceCorpus<-tm_map(FsentenceCorpus, stripWhitespace)

Ftdm <- TermDocumentMatrix(FsentenceCorpus)
Ftdm = removeSparseTerms(Ftdm, 0.99)
ftdm.tidy = tidytext::tidy(Ftdm)
Ftdm.overall=summarise(group_by(ftdm.tidy, term), sum(count))

png("~/Desktop/22spring/GR5243 Applied DS/spring-2022-prj1-ChangLuuu/figs/WC_Foucault.png", units="in", width=8, height=5, res=600)
wordcloud(Ftdm.overall$term, Ftdm.overall$`sum(count)`,
          scale=c(2.5,0.5),
          max.words=100,
          min.freq=1,
          random.order=FALSE,
          rot.per=0.3,
          random.color=FALSE,
          colors=brewer.pal(9,"Blues"))
dev.off()

data$publication <- rawdata$original_publication_date
GI <- data[data$school=="german_idealism",]
Phe <- data[data$school=="phenomenology",]
Rat <- data[data$school=="rationalism",]

GIdf <- data.frame(date=unique(GI$publication))
Phedf <- data.frame(date=unique(Phe$publication))
Ratdf <- data.frame(date=unique(Rat$publication))
GIscore <- c()
Phescore <- c()
Ratscore <- c()

for(i in 1:length(unique(GI$publication))){
  GIscore[i] <- mean(GI[GI$publication==unique(GI$publication)[i],"sentscore"])
}
GIdf$score <- GIscore

for(i in 1:length(unique(Phe$publication))){
  Phescore[i] <- mean(Phe[Phe$publication==unique(Phe$publication)[i],"sentscore"])
}
Phedf$score <- Phescore

for(i in 1:length(unique(Rat$publication))){
  Ratscore[i] <- mean(Rat[Rat$publication==unique(Rat$publication)[i],"sentscore"])
}
Ratdf$score <- RatscoreGIdf <- data.frame(date=unique(GI$publication))
Phedf <- data.frame(date=unique(Phe$publication))
Ratdf <- data.frame(date=unique(Rat$publication))
GIscore <- c()
Phescore <- c()
Ratscore <- c()

for(i in 1:length(unique(GI$publication))){
  GIscore[i] <- mean(GI[GI$publication==unique(GI$publication)[i],"sentscore"])
}
GIdf$score <- GIscore

for(i in 1:length(unique(Phe$publication))){
  Phescore[i] <- mean(Phe[Phe$publication==unique(Phe$publication)[i],"sentscore"])
}
Phedf$score <- Phescore

for(i in 1:length(unique(Rat$publication))){
  Ratscore[i] <- mean(Rat[Rat$publication==unique(Rat$publication)[i],"sentscore"])
}
Ratdf$score <- Ratscore

png("~/Desktop/22spring/GR5243 Applied DS/spring-2022-prj1-ChangLuuu/figs/Regression_GI.png", units="in", width=8, height=5, res=600)
ggplot(data = GIdf, aes(x=date, y=score))+
  geom_point()+
  geom_smooth(method=lm)+
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "sentiment development of German Idealism")
dev.off()

png("~/Desktop/22spring/GR5243 Applied DS/spring-2022-prj1-ChangLuuu/figs/Regression_Phe.png", units="in", width=8, height=5, res=600)
ggplot(data = Phedf, aes(x=date, y=score))+
  geom_point()+
  geom_smooth(method=lm)+
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "sentiment development of Phenomenology")
dev.off()

png("~/Desktop/22spring/GR5243 Applied DS/spring-2022-prj1-ChangLuuu/figs/Regression_Rat.png", units="in", width=8, height=5, res=600)
ggplot(data = Ratdf, aes(x=date, y=score))+
  geom_point()+
  geom_smooth(method=lm)+
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "sentiment development of Rationalism")
dev.off()
