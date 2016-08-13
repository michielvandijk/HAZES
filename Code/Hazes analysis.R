# Code to scrape lyrics for Andre Hazes.
# Packages
require(tm)
require(stm)
require(topicmodels)
require(Rmpfr) # needed for optimal topic model determination
require(devtools)
#install_github("kshirley/LDAtools")
#install_github("cpsievert/LDAvis")
#require(LDAviz)
require(LDAvis)

# set working directory
setwd("D:\\dijk158\\Dropbox\\Michiel research\\Text analysis")

# R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

# Load Discography
# all titles changed into lowercase as this is not unform across databases
Albums<-read.csv("Data\\AlbumDisco.csv")
Albums$Album<-tolower(Albums$Album)
Singles<-read.csv("Data\\SinglesDisco.csv")
Singles$Title<-tolower(Singles$Title)

# Select Master set of Albums
Albums.Master<-Albums[Albums$Selection=="Y",]

# Load Lyrics
load("Data\\Hazes.RData")
Lyrics<-unique(Hazes) #removes duplicate lyrics
Lyrics<-Lyrics[which(sapply(Lyrics, function(x) x[[4]]!=""))] # remove songs with empty lyrics
GeenAlbum<-Lyrics[sapply(Lyrics, function(x) (x[[2]]=="Geen album titel"))]

Lyrics.Master<-Lyrics[which(sapply(Lyrics, function(x) tolower(x[[2]]) %in% Albums.Master$Album))] # remove songs with empty lyrics
LyricsNotMaster<-Lyrics[which(sapply(Lyrics, function(x) tolower(x[[2]]) %nin% Albums.Master$Album))]

# Create dataframe with all information to check manually
Master<- ldply (Lyrics, data.frame)
Master$Album<-tolower(Master$Album)
Master$Lyrics<-NULL
Master<-merge(Master, Albums.Master[,c(2,8)], by=c("Album"), all.x=TRUE)
Master<-Master[order(Master$Title, Master$Album),]
write.csv(Master, "Data\\Master.csv", row.names=FALSE)

# Load edited master
MasterEdit<-read.csv("Data\\MasterEdit.csv")
# Remove all songs that are duplicates, in English, Italian, etc for text analysis
MasterFTA<-MasterEdit[MasterEdit$RemoveFTA!="Y",]
MasterFTA<-MasterFTA[order(MasterFTA$NewYear),]

# Analysis
# Word Count
# Create vector of lyrics
Lyrics.TM<-sapply(Lyrics, function(x) x[[4]])
# Correct types and slit "kerst"  words
Lyrics.TM<-gsub("kersfeest", " kerstfeest ", Lyrics.TM, ignore.case=TRUE)
#Lyrics.TM<-gsub("kerst", " kerst ", Lyrics.TM, ignore.case=TRUE)
# Create Corpus for text mining
Lyrics.TM<-Corpus(VectorSource(Lyrics.TM,encoding = "UTF-8"),readerControl = list(language = "nld"))

# Add tags to corpus. Note there are three types of tags (1) doc level: meta(Lyrics, type="corpus"); (2) doc level but index: meta(Lyrics)
# (1) and (2) are stored in a separate database; (3) doc level: meta(Lyrics[[1]]). 
# Change tags at level (2)
meta(Lyrics.TM, "MetaID")<-c(1:length(Lyrics.TM))
meta(Lyrics.TM, "Author")<-"Andre Hazes" 
meta(Lyrics.TM, "Title")<-sapply(Lyrics, function(x) x[[1]])
meta(Lyrics.TM, "Album")<-sapply(Lyrics, function(x) x[[2]])
meta(Lyrics.TM, "Year")<-sapply(Lyrics, function(x) x[[3]])
meta(Lyrics.TM)

# Change tags at level 3
i<-0
Lyrics.TM<-tm_map(Lyrics.TM, function(x) {
  i <<- i +1
  meta(x, "Author") <- "Andre Hazes"
  meta(x, "ID") <-i
  meta(x, "Heading")<-Hazes[[i]][[1]]
  x
})

# View corpus
inspect(Lyrics.TM[1:2])
Meta.Lyrics<-as.data.frame(meta(Lyrics.TM))
Meta.Lyrics$Title<-tolower(Meta.Lyrics$Title)

# Clean corpus: remove dublicates and foreign language lyrics
# Compare MasterFTA with Tags
MasterFTA$Title<-tolower(MasterFTA$Title)
MasterFTA$NewTitle<-tolower(MasterFTA$NewTitle)
MasterFTA$Info<-"YES"
check<-merge(Meta.Lyrics, MasterFTA[,c(2,7,11)], by=c("Title"), all.x=TRUE)
check<-check[is.na(check$Info),]
Meta.Lyrics2<-merge(Meta.Lyrics, MasterFTA[,c(7,11)], by.x=c("Title"), by.y=c("NewTitle"))
Meta.Lyrics2<-Meta.Lyrics2[order(Meta.Lyrics2$MetaID),]
check2<-check[is.na(check$Info),]
# NEED FINAL MANUAL CHECK

# Filter corpus
Lyrics.TM.C<-tm_filter(Lyrics.TM, FUN = sFilter, "id %in% Meta.Lyrics2$MetaID")
Lyrics.TM.C<- tm_map(Lyrics.TM.C, tolower)
stopwords("dutch")
mywords<-c(stopwords("dutch"),"jij", "jou", "andré", "hazes", "(c)", "refrein", "refr",
           "rommedomdom", "ahhaha", "no", "ohoho", "oohh", "oho", "hoho", "ohohohohohohho",
           "ohohohohoh", "ohohoho", "lalala", "hmhmhmhmhm", "ohohohohoh", "yeah", "oho", "hoho",
           "copyright", "lailalailalaila", "lalalalalala", "lalalalalalala", "lalalalalalalala",
           "lalalalalalalalaaah", "oehhh", "oehoehoehoehoe", "ohhh", "ohoh", "ohohoh", "oooh",
           "All", "lyrics", "are", "their", "Complimentary", "tekst en muziek: a.de raaf/j.schutte/j.de cler/e.lopez/a.pola",
           "ahahaha", "wohw", "haha", "achja", "hahahaha", "read", "more", "amor", "la")
#, "laat", "bent", "weer")
Lyrics.TM.C <- tm_map(Lyrics.TM.C, removeWords, mywords )
Lyrics.TM.C <- tm_map(Lyrics.TM.C, removePunctuation)
Lyrics.TM.C <- tm_map(Lyrics.TM.C, removeNumbers)
# stemming of words
require(SnowballC)
#Lyrics.TM.C <- tm_map(doc.Lyrics, stemDocument, language = "dutch")
Lyrics.TM.C <- tm_map(Lyrics.TM.C, stripWhitespace)
# replace kerst* with kerst followed by space.
inspect(Lyrics.TM.C)
# find text with words
FT<-function(input) meta(tm_filter(Lyrics.TM, FUN = function(x) any(grep(input, x))))
SL<-function(input) inspect(tm_filter(Lyrics.TM, FUN=function(x) any(grep(input, x))))

FT2<-function(input) meta(tm_filter(Lyrics.TM.C, FUN = function(x) any(grep(input, x))))
SL2<-function(input) inspect(tm_filter(Lyrics.TM.C, FUN=function(x) any(grep(input, x))))

# create Term Document Matrix without stemming
dtm.control <- list(tolower = TRUE,
                    removePunctuation = TRUE,
                    removeNumbers = TRUE,
                    stopwords = mywords,
                    stemming = FALSE,
                    wordLengths = c(4, Inf),
                    minDocFreq=2, 
                    #minWordLength=2,
                    weighting = weightTf) # CHECK
DTM.Lyrics <- DocumentTermMatrix(Lyrics.TM.C, control = dtm.control)
TDM.Lyrics <- TermDocumentMatrix(Lyrics.TM.C, control = dtm.control)
dim(TDM.Lyrics)

findAssocs(TDM.Lyrics, "papa", 0.5)


# create Term Document Matrix with stemming
dtm.control2 <- list(tolower = TRUE,
                    removePunctuation = TRUE,
                    removeNumbers = TRUE,
                    stopwords = mywords,
                    stemming = TRUE,
                    wordLengths = c(4, Inf),
                    minDocFreq=2, 
                    #minWordLength=2,
                    weighting = weightTf) # CHECK
DTM.Lyrics2 <- DocumentTermMatrix(Lyrics.TM.C, control = dtm.control2)
TDM.Lyrics2 <- TermDocumentMatrix(Lyrics.TM.C, control = dtm.control2)
dim(DTM.Lyrics2)
dimnames(DTM.Lyrics2)

findFreqTerms(TDM.Lyrics, 150, 300)
findFreqTerms(TDM.Lyrics2, 150, 300)

# Write DTM
m <- as.matrix(TDM.Lyrics)
dim(m)
v <- rowSums(m)
l <-rowSums(m>0)
d <- data.frame(word = names(v),freq=v,freq_songs=l, rel_freq_songs=l/ncol(m)*100)
d <- d[order(-d$freq),]
write.csv(d,"WordCount.csv",row.names = FALSE)

m2 <- as.matrix(TDM.Lyrics2)
dim(m2)
v2 <- sort(rowSums(m2),decreasing=TRUE)
l2 <-rowSums(m2>0)
d2 <- data.frame(word = names(v2),freq=v2, freq_songs=l2, rel_freq_songs=l2/ncol(m2)*100)
write.csv(d2,"WordCount_stemmed.csv",row.names = FALSE)




# Remove sparse terms
# dim(DTM.Lyrics)
# DTM.Lyrics.s<-removeSparseTerms(DTM.Lyrics, 0.99)
# dim(DTM.Lyrics.s)
# inspect(DTM.Lyrics.s)
# DTMcheck<-as.matrix(inspect(DTM.Lyrics))
# write.csv(t(DTMcheck),"DTMcheck.csv")




# Wordcloud
require(slam)
require(wordcloud)
library(RColorBrewer)
pal <- brewer.pal(9, "BuGn")
pal <- brewer.pal(8, "Dark2")
#pal <- pal[-(1:2)]
table(d$freq)
wordcloud(d$word,d$freq, scale=c(8,.3),min.freq=2,max.words=100, random.order=T, rot.per=.15, colors=pal, vfont=c("sans serif","plain"))
png("wordcloud.png", width=1280,height=800)

wordcloud(d$word,d$freq, scale=c(8,.2),min.freq=3,max.words=200, random.order=FALSE, rot.per=.15, colors=pal)
dev.off()

png("wordcloud.png", width=1280,height=800)
bmp("wordcloud.bmp")
pdf("wordcloud.pdf")
dev.off()

# Sentiment analysis
# Create cleaned dataframe with all information
SA<- ldply (Lyrics, data.frame)
SA$Title<-tolower(SA$Title)
# Remove foreign lyrics etc
SA<-merge(Meta.Lyrics2, SA, by=c("Title", "Album", "Year"))
require(RCurl)
require(RJSONIO)
sent<-function(x){
  print(x$Title)
  R<-postForm("http://text-processing.com/api/sentiment/", language="dutch",text=x$Lyrics)
  R<-fromJSON(R)
  R<-data.frame(sentiment=R[[2]], neg=R[[1]][1], neutral=R[[1]][2], pos=R[[1]][3])
  return(R)
}

SA.res<-ddply(SA,.(Title, Album, Year, MetaID),sent)
write.csv(SA.res, "Data\\SA.res.csv", row.names=FALSE)
SA.res<-read.csv("Data\\SA.res.csv")
SA.res<-SA.res[order(-SA.res$pos),]
SA.res<-SA.res[order(-SA.res$neg),]
table(SA.res$sentiment)
SA.res$Year2<-as.numeric(SA.res$Year)      
Table<-as.data.frame(xtabs(~Year+sentiment, data=SA.res[SA.res$Year2>100,]))
Table<-ddply(Table,.(sentiment),transform, cumul=cumsum(Freq))      
ggplot()+geom_line(data=Table, aes(x=Year, y=cumul, group=sentiment, colour=sentiment))+theme_bw()
ggsave("sentiment.png")  
ggplot()+geom_bar(data=Table, aes(x=Year, y=Freq), stat="identity")+ facet_wrap(~sentiment)


meta(tm_filter(Lyrics.TM, FUN = function(x) any(grep("kerst", x))))
inspect(tm_filter(Lyrics.TM, FUN=function(x) any(grep("kerst", x))))

# Calc pos/neg per album
Table.Album<-dcast(SA.res, Album+Year2~sentiment, value.var="Title", length)
Table.Album$TotalSongs<-rowSums(Table.Album[,c(3:5)])
Table.Album<-transform(Table.Album, Neg.sh=neg/TotalSongs*100, Neutral.sh=neutral/TotalSongs*100, Pos.sh=pos/TotalSongs*100)
Table.Album<-Table.Album[order(Table.Album$Year2),]

ggplot()+geom_line(data=Table.Album[Table.Album$Year2>100,], aes(x=Year2, y=Pos.sh))+
  stat_smooth(data=Table.Album[Table.Album$Year2>100,], aes(x=Year2, y=Pos.sh), se=FALSE, method="loess")

ggplot()+geom_line(data=Table.Album[Table.Album$Year2>100,], aes(x=Year2, y=Neg.sh))+
  stat_smooth(data=Table.Album[Table.Album$Year2>100,], aes(x=Year2, y=Neg.sh), se=FALSE, method="loess")

ggplot()+geom_line(data=Table.Album[Table.Album$Year2>100,], aes(x=Year2, y=Neutral.sh))+
  stat_smooth(data=Table.Album[Table.Album$Year2>100,], aes(x=Year2, y=Neutral.sh), se=FALSE, method="loess")

inspect(tm_filter(Lyrics.TM, FUN = sFilter, "Album=='Want Ik Hou Van Jou'"))

# Topic analysis with STM
# Run stm package
#install_github("bstewart/stm",dependencies=TRUE)
#load(url("http://goo.gl/91KbfS"))
#labelTopics(poliblogPrevFit, c(1, 7, 10))


require(stm)
out<-readCorpus(DTM.Lyrics, type=c("slam"))
documents<-out$documents
vocab<-out$vocab
meta<-data.frame(year=c(1:270))
stm2<-stm(documents, vocab, 12, init.type=c("LDA"), seed=NULL, 
         max.em.its=200, emtol=0.01 ) # emtal set higher to ensure convergence with 20 iterations =>CHECK
#stm2<-selectModel(out$documents,out$vocab,K=15)
#plotModels(stm2)
#stm2<-stm2$runout[[3]]
# Present topic words
plot(stm2)
labelTopics(stm2, n=7)
# Show documents associated with topics
thoughts1<-findThoughts(stm2, texts=shortdoc, n=2, topics=1)$docs[[1]]
plotQuote(thoughts1, width=40, main="Topic 1")
plot.STM(stm2,type="summary", xlim=c(0,.4))
plot.STM(stm2,type="perspectives", topics=c(11, 15))
mod.out.corr<-topicCorr(stm2)
plot.topicCorr(mod.out.corr)
  
}
wordCheck("Amsterdam")  
  check<-meta(doc.Lyrics[291])
findAssocs(TDM.Lyrics, "bier", 0.99)


# Topic analysis with Topic Models
#Fit models and find an optimal number of topics as suggested by Ben Marmick --# http://stackoverflow.com/questions/21355156/topic-models-cross-validation-with-loglikelihood-or-perplexity/21394092#21394092
# It appears some documents have zeros in all docs in the DTM, giving an error in the LDA.
#rowTotals <- apply(TDM.Lyrics, 1, sum) #Find the sum of words in each Document
#TDM.Lyrics2<- TDM.Lyrics[rowTotals>0,]           #remove all docs without words
#inspect(TDM.Lyrics2)
dim(DTM.Lyrics)
dim(DTM.Lyrics2)
library(slam)
summary(col_sums(DTM.Lyrics))
# Some criteria (See TopicModels package) is used to exclude rare and very frequent terms
term_tfidf <-
  tapply(DTM.Lyrics$v/row_sums(DTM.Lyrics)[DTM.Lyrics$i], DTM.Lyrics$j, mean) *
  log2(nDocs(DTM.Lyrics)/col_sums(DTM.Lyrics > 0))
summary(term_tfidf)

term_tfidf2 <-
  tapply(DTM.Lyrics2$v/row_sums(DTM.Lyrics2)[DTM.Lyrics2$i], DTM.Lyrics2$j, mean) *
  log2(nDocs(DTM.Lyrics2)/col_sums(DTM.Lyrics2 > 0))
summary(term_tfidf)

DTM.Lyrics_X <- DTM.Lyrics[,term_tfidf >= 0.1]
DTM.Lyrics_X <- DTM.Lyrics_X[row_sums(DTM.Lyrics_X) > 0,]
summary(col_sums(DTM.Lyrics_X))

DTM.Lyrics2_X <- DTM.Lyrics2[,term_tfidf2 >= 0.1]
DTM.Lyrics2_X <- DTM.Lyrics2_X[row_sums(DTM.Lyrics2_X) > 0,]
summary(col_sums(DTM.Lyrics2_X))

dim(DTM.Lyrics_X) 
dim(DTM.Lyrics2_X)

LDA.Lyrics_X<-LDA(DTM.Lyrics_X,13)
t_X = terms(LDA.Lyrics_X,10)
t_X

LDA.Lyrics2_X<-LDA(DTM.Lyrics2_X,13)
t2_X = terms(LDA.Lyrics2_X,10)
t2_X


# Link topics with Meta Information
Topics<-data.frame(MetaID=as.numeric(names(topics(LDA.Lyrics2_X,1))), Topics=topics(LDA.Lyrics2_X,1))
Topics<-merge(Meta.Lyrics2, Topics, by=("MetaID"))
table(Topics$Topics)
xtabs(~Album+Topics, data=Topics)
t2_X


LDA.Lyrics2<-LDA(DTM.Lyrics2_X,12)
t2 = terms(LDA.Lyrics2,10)
t2

k <- 30
R> SEED <- 2010
R> jss_TM <-
  + list(VEM = LDA(JSS_dtm, k = k, control = list(seed = SEED)),
         + VEM_fixed = LDA(JSS_dtm, k = k,
                           + control = list(estimate.alpha = FALSE, seed = SEED)),
         + Gibbs = LDA(JSS_dtm, k = k, method = "Gibbs",
                       + control = list(seed = SEED, burnin = 1000,
                                        + thin = 100, iter = 1000)),
         + CTM = CTM(JSS_dtm, k = k,
                     + control = list(seed = SEED,
                                      + var = list(tol = 10^-4), em = list(tol = 10^-3))))


#http://stackoverflow.com/questions/16396090/r-topic-modeling-lda-model-labeling-function
#http://stackoverflow.com/questions/14875493/lda-with-topicmodels-how-can-i-see-which-topics-different-documents-belong-to?rq=1

meta(tm_filter(Lyrics.TM, FUN = function(x) any(grep("refr", x))))
inspect(tm_filter(Lyrics.TM, FUN=function(x) any(grep("plein", x))))
meta(tm_filter(Lyrics.TM, FUN = function(x) any(grep("hoho", x))))
inspect(tm_filter(Lyrics.TM, FUN=function(x) any(grep("hoho", x))))
meta(doc.Lyrics[291])



rowTotals <- apply(TDM.Lyrics.s, 1, sum) #Find the sum of words in each Document
TDM.Lyrics.s2<- TDM.Lyrics.s[rowTotals>0,]           #remove all docs without words
inspect(TDM.Lyrics.s2)
LDA.Lyrics.s<-LDA(TDM.Lyrics.s2,20)
t.s = terms(LDA.Lyrics.s,15)
t.s
topics(LDA.Lyrics.s,2)


rownames(dtm.new)
#
meta(Lyrics[c(34,223,366,438,463)])

harmonicMean <- function(logLikelihoods, precision = 2000L) {
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                       prec = precision) + llMed))))
}
burnin <- 1000
iter <- 1000
keep <- 50
ks <- seq(2, 40, by = 1)
models <- lapply(ks, function(k) LDA(DTM.Lyrics2, k, method = "Gibbs", control = list(burnin = burnin, iter = iter, keep = keep)))
logLiks <- lapply(models, function(L)  L@logLiks[-c(1:(burnin/keep))])
hm <- sapply(logLiks, function(h) harmonicMean(h))

# Find optimal model
plot(ks, hm, type = "l")
opt <- models[which.max(hm)][[1]]
opt
# Extract the 'guts' of the optimal model
doc.id <- opt@wordassignments$i
token.id <- opt@wordassignments$j
topic.id <- opt@wordassignments$v
vocab <- opt@terms

# Get the phi matrix using LDAviz
dat <- getProbs(token.id, doc.id, topic.id, vocab, K = max(topic.id), sort.topics = "byTerms")
phi <- t(dat$phi.hat)
# NOTE TO SELF: these things have to be numeric vectors or else runVis() will break...add a check in check.inputs
token.frequency <- as.numeric(table(token.id))
topic.id <- dat$topic.id
topic.proportion <- as.numeric(table(topic.id)/length(topic.id))

# Run the visualization locally using LDAvis
z <- check.inputs(K=max(topic.id), W=max(token.id), phi, token.frequency, vocab, topic.proportion)
runVis()

# sensitivity analysis

# # following is test for httr and other sentiment api that does not work yet
# require(httr)
# r<-GET("http://api.ai-applied.nl/api/sentiment_api/")
# http_status(r)
# content(r, "text")
# url<-"http://api.ai-applied.nl/api/sentiment_api/"
# body<- list(data=list(api_key="DEMO_ACCOUNT", call=list(data=list(text="sometimes you get nicely surprised from unexpected sources", 
#                                                                 language_iso="eng", id=1))))
# r <- POST(url, body = body, encode = "json")
# r
# R<-POST("http://text-processing.com/api/sentiment/", body=list(text="goed boek", language="dutch"))
# POST("http://api.ai-applied.nl/api/sentiment_api/", body=list(api_key="DEMO_ACCOUNT", data=c("text": "sometimes you get nicely surprised from unexpected sources", "language_iso":"eng")))
# 






# Statistics on number of songs, etc
MasterEdit2<-MasterEdit[MasterEdit$Remove!="Y",]
MasterEdit2<-MasterEdit2[order(MasterEdit2$NewYear),]
table(MasterEdit2$NewYear)
SongsCalc<-as.data.frame(cbind(Year=unique(MasterEdit2$NewYear), Freq=table(MasterEdit2$NewYear), Cumul=cumsum(table(MasterEdit2$NewYear))))
SinglesCalc<-as.data.frame(cbind(Year=unique(Singles$Jaar), Freq=table(Singles$Jaar), Cumul=cumsum(table(Singles$Jaar))))

ggplot()+geom_bar(data=SongsCalc, aes(x=Year, y=Freq), stat="identity")
ggplot()+geom_bar(data=SinglesCalc, aes(x=Year, y=Freq), stat="identity")
ggplot()+geom_line(data=SinglesCalc, aes(x=Year, y=Freq))+theme_bw()
ggsave(file="Singles.png")
unique(sapply(LyricsNotMaster, function(x) (x[[1]])))


sort(unique(sapply(Lyrics.Master, function(x) (x[[2]]))))
sort(Albums.Master$Album)
length(Lyrics[check])
tolower(sapply(Lyrics, function(x) x[[2]]=="geen album titel"))
tolower(Albums.Master$Album)
