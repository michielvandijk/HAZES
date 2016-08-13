# Code to scrape lyrics for Andre Hazes.
# Packages
require(XML)
require(RCurl)
require(stringr)
require(chron)
require(tm)
require(topicmodels)
require(Rmpfr) # needed for optimal topic model determination
require(devtools)
#install_github("kshirley/LDAviz")
#install_github("cpsievert/LDAvis")
#require(LDAviz)
#require(LDAvis)

# set working directory
setwd("D:\\dijk158\\Dropbox\\Michiel research\\Text analysis")

# R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

# Discografie Discogs
# url from lyrics.wikia.com with all albums
AlbumListUrl<-"http://www.discogs.com/artist/282287-Andr�-Hazes?type=Releases&subtype=Albums"
                      
# Encode URL, not sure if this is needed
AlbumListUrl<-URLencode(AlbumListUrl)
# Take xml data with list of all hazes songs
AlbumList<-getURLContent(AlbumListUrl, encoding="UTF-8", useragent = "DiscogsR") # Specify encoding when dealing with non-latin characters
# R formated XML data
AlbumLinksParsed<-htmlParse(AlbumList)
# HTML links to all songs
AlbumLinks<-xpathSApply(AlbumLinksParsed, "//*[@class='title']/a/@href")

# Filter list so it only contains 'master' albums and no duplicate versions
names(AlbumLinks)<-NULL
save(AlbumLinks, file="Data\\AlbumLinks.RData")
AlbumLinks<-AlbumLinks[!str_detect(AlbumLinks, "#")]
AlbumLinksm<-AlbumLinks[str_detect(AlbumLinks, "master")]
AlbumLinksr<-AlbumLinks[!str_detect(AlbumLinks, "master")]


load("Data\\AlbumLinks.RData")


# Function to scrape album content.
AlbumTrackScraperm.f<- function(url){
  url2<-paste("http://www.discogs.com",url, sep="")
  AlbumContent<-getURL(url2,encoding="UTF-8",useragent = "DiscogsR")
  AlbumContentParsed<- htmlParse(AlbumContent)
  AlbumTitle<-xpathSApply(AlbumContentParsed, "//*[@id='profile_title']/span[2]/text()",xmlValue)
  AlbumTitle<-str_trim(gsub("\\n","",AlbumTitle), side = "both")
  TrackName<-xpathSApply(AlbumContentParsed, "//*[@id='tracklist']/div/table/tr/td[1]/span/text()",xmlValue)
  TrackName<-lapply(TrackName,function(x) str_trim(gsub("\\n","",x), side = "both"))
  TrackName<-as.data.frame(TrackName)
  names(TrackName)<-NULL
  TrackTime<-xpathSApply(AlbumContentParsed, "//*[@id='tracklist']/div/table/tr/td[2]/span/text()",xmlValue)
  Tracks<-as.data.frame(t((rbind(AlbumTitle,TrackName,TrackTime))))
  names(Tracks)<-c("Album","Track","Time")
  return(Tracks)
}

Master<-ldply(AlbumLinksm,AlbumTrackScraperm.f)
Table<-cbind(Freq=table(Master$Album), Cumul=cumsum(table(Master$Album)))
Table

# Function to scrape album content.
AlbumTrackScraperr.f<- function(url){
  url2<-paste("http://www.discogs.com",url, sep="")
  AlbumContent<-getURL(url2,encoding="UTF-8",useragent = "DiscogsR")
  AlbumContentParsed<- htmlParse(AlbumContent)
  AlbumTitle<-xpathSApply(AlbumContentParsed, "//*[@id='profile_title']/span[2]/text()",xmlValue)
  AlbumTitle<-str_trim(gsub("\\n","",AlbumTitle), side = "both")
  print(AlbumTitle)
  TrackName<-xpathSApply(AlbumContentParsed, "//*[@id='tracklist']/div/table/tr/td[2]/span/text()",xmlValue)  
  TrackName<-lapply(TrackName,function(x) str_trim(gsub("\\n","",x), side = "both"))
  TrackName<-as.data.frame(TrackName)
  names(TrackName)<-NULL
  TrackTime<-xpathSApply(AlbumContentParsed, "//*[@id='tracklist']/div/table/tr/td[3]/span/text()",xmlValue)
  if (length(TrackTime)==0){TrackTime=0} 
  Tracks<-as.data.frame(t((rbind(AlbumTitle,TrackName,TrackTime))))
  names(Tracks)<-c("Album","Track","Time")
  return(Tracks)
}

NMaster<-ldply(AlbumLinksr,AlbumTrackScraperr.f)
NMaster<-NMaster[NMaster$Time!="0",] # remove all duplicate albums with missing time 
NMaster<-unique(NMaster)
Table<-cbind(Freq=table(Master$Album), Cumul=cumsum(table(Master$Album)))
Table

# Create list of all albums with complete data
# Data from Discogs
AlbumDisco<-read.csv("Data\\AlbumDisco.csv")
NMasterSel<-AlbumDisco[AlbumDisco$DisCogs=="NMaster",]
NMaster2<-subset(NMaster, tolower(Album) %in% tolower(NMasterSel$Album))
Table<-cbind(Freq=table(NMaster$Album), Cumul=cumsum(table(NMaster$Album)))
Table

# Data from other
MissingInfo<-read.csv("Data\\HazesMissingSongs.csv")
MissingInfo$Time<-str_extract(MissingInfo$Track, "(\\([0-9]+[:][0-9]+\\))")
MissingInfo$Time<-gsub("\\(","",MissingInfo$Time)
MissingInfo$Time<-gsub("\\)","",MissingInfo$Time)
MissingInfo$Track<-gsub("(\\([0-9]{1}[:]{1}[0-9]{2}\\))","",MissingInfo$Track)

# Merge Album and track information
TrackDisco<-rbind(Master, NMaster2, MissingInfo)
TrackDisco$Album<-tolower(TrackDisco$Album)
TrackDisco$Track<-tolower(TrackDisco$Track)
AlbumDiscoSel<-subset(AlbumDisco, Selection=="Y")
AlbumDiscoSel$Album<-tolower(AlbumDiscoSel$Album)
TrackDiscoD<-merge(TrackDisco, AlbumDiscoSel,by="Album",all.x=TRUE)
write.csv(TrackDiscoD, "Data\\TrackDiscoD.csv")
TrackDisco2<-subset(TrackDisco, Album %in% tolower(AlbumDiscoSel$Album))
TrackDisco3<-subset(TrackDisco, !(Track %in% TrackDisco2$Track))
Table<-cbind(Freq=table(TrackDisco2$Album), Cumul=cumsum(table(TrackDisco2$Album)))
Table

DupTrack<-TrackDisco[duplicated(TrackDisco$Track) | duplicated(TrackDisco$Track, fromLast = TRUE),]
DupTrack<-DupTrack[order(DupTrack$Track),]
Table<-data.frame(cbind(Freq=table(TrackDisco2$Track), Cumul=cumsum(table(TrackDisco2$Track))))
Table
AllAlbums<-rbind(Master,NMaster)
save(AllAlbums, file="Data\\AllAlbums.RData")
load("Data\\AllAlbums.RData")
AllAlbums2<-AllAlbums
AllAlbums2$Album<-tolower(AllAlbums2$Album)
AllAlbums2$Track<-tolower(AllAlbums2$Track)
AllAlbums2<-unique(AllAlbums2)
AllAlbums2<-AllAlbums2[AllAlbums2$Time!="0",] # remove all duplicate albums with missing time 


Table<-cbind(Freq=table(AllAlbums2$Album), Cumul=cumsum(table(AllAlbums2$Album)),relative=prop.table(table(AllAlbums2$Album)))
Table


AllAlbums2[which(AllAlbums2=='eenzame kerst'),]



# Scrape songs
Hazes<-llply(SongLinks,lyricsScraper.f) 
names(Hazes)<-NULL
# Add appropriate names to SongLinks
save(Hazes, file="Data\\Hazes.RData")
load("Data\\Hazes.RData")
Hazes<-unique(Hazes)




# Access to Discogs API
memory.limit(size=5000)
devtools::install_github("hadley/httr")
library(httr)
key <- 'iajbeYBGAaABhJcVkcZw'
secret <- 'JpTUoePhLuQGsPqxErzgmWWjjKtDgNym'
tokenURL <- 'http://api.discogs.com/oauth/request_token'
accessTokenURL <- 'http://api.discogs.com/oauth/access_token'
authorizeURL <- 'http://www.discogs.com/oauth/authorize'

dcr <- oauth_app('discogs',key,secret)
discogs<- oauth_endpoint(tokenURL, authorizeURL,accessTokenURL)

token <- oauth1.0_token(discogs, dcr)


options(HTTPUserAgent="DiscogsR")
getOption("HTTPUserAgent")

p1 <- GET("http://api.discogs.com/artists/282287/releases?page=1&per_page=100")
p2 <- GET("http://api.discogs.com/artists/282287/releases?page=2&per_page=100")
p1<-content(p1)
p2<-content(p2)


# Scrape Hazes discografie wikipedia
theurl <- "http://nl.wikipedia.org/wiki/Andre_hazes"
tables <- readHTMLTable(theurl)
n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
Albums<-as.data.frame(tables[[4]])
write.csv(Albums,"Data\\AlbumDisco.csv" )
Singles<-as.data.frame(tables[[6]])
write.csv(Singles,"Data\\SinglesDisco.csv" )

# Scrape Hazes songs
# url from lyrics.wikia.com with all hazes songs
SongListUrl<-"http://lyrics.wikia.com/api.php?func=getArtist&artist=Andr%C3%A9_Hazes"
# Taw xml data with list of all hazes songs
SongList<-getURL(SongListUrl,encoding="UTF-8") # Specify encoding when dealing with non-latin characters
# R formated XML data
SongListParsed<-htmlParse(SongList)
# HTML links to all songs
SongLinks<-xpathSApply(SongListParsed, "//ul[@class='songs']/li/a/@href")
Songlinks<-unique(SongLinks) # remove duplicate songs that appear on more than one album

# Browse one of the links. Note that the link must be encoded to transform allocted signs
#browseURL(URLencode(SongLinks[10]))

# function to obtain songtitle
# Obtain songtitle by taken last part of url and removing "-"
Title.f<-function(url){
  Title<-unlist(str_split(URLdecode(url),":"))
  Title<-Title[length(Title)]
  Title<-gsub("_"," ", Title)
  return (Title)
}

# Function to scrape one song.
lyricsScraper.f<- function(url){
  Song<-getURL(url,encoding="UTF-8")
  SongParsed<- htmlParse(Song)
  Title<-Title.f(url)
  Album<-xpathSApply(SongParsed, "//i[1]",xmlValue)
  if (length(Album)==0){
    Album<-"Geen album titel"
    Year<-"Geen Year"
  } else {
    Year<-str_extract(Album, "[0-9]{1,4}")
  }
  print(Album)
  print(Title)
  print(Year)
  Album<-gsub(pattern = "\\s\\([0-9]{1,4}\\)", replacement = "", x = Album) # remove year from title
  # Obtain lyrics. Note that /text() is added to collect text elements. 
  Title<-Title.f(url)
  Lyrics<-xpathSApply(SongParsed, "//div[@class='lyricbox']/text()", xmlValue)
  Lyrics<-paste(Lyrics, collapse=" ") # to create one item in which sentences are separated by spaces.  
  Out<-list(Title=Title, Album=Album, Year=Year, Lyrics=Lyrics)
  #Out<-list(Title=Title, Album=Album, Lyrics=Lyrics)
  return(Out)
}

# Scrape songs
Hazes<-llply(SongLinks,lyricsScraper.f) 
names(Hazes)<-NULL
# Add appropriate names to SongLinks
save(Hazes, file="Data\\Hazes.RData")
load("Data\\Hazes.RData")
Hazes<-unique(Hazes)


# Create Corpus for text mining
# Create vector of lyrics
Lyrics<-sapply(Hazes, function(x) x[[4]])
Lyrics<-Corpus(VectorSource(Lyrics,encoding = "UTF-8"),readerControl = list(language = "nld"))
# Add tags to corpus. Note there are three types of tags (1) doc level: meta(Lyrics, type="corpus"); (2) doc level but index: meta(Lyrics)
# (1) and (2) are stored in a separate database; (3) doc level: meta(Lyrics[[1]]). 
# Change tags at level (2)
meta(Lyrics, "MetaID")<-c(1:length(Lyrics))
meta(Lyrics, "Author")<-"Andre Hazes" 
meta(Lyrics, "Title")<-sapply(Hazes, function(x) x[[1]])
meta(Lyrics, "Album")<-sapply(Hazes, function(x) x[[2]])
meta(Lyrics, "Year")<-sapply(Hazes, function(x) x[[3]])
# Change tags at level 3
i<-0
Lyrics<-tm_map(Lyrics, function(x) {
  i <<- i +1
  meta(x, "Author") <- "André Hazes"
  meta(x, "ID") <-i
  meta(x, "Heading")<-Hazes[[i]][[1]]
  x
})
# View corpus
#inspect(Lyrics[1:2])
Meta.Lyrics<-as.data.frame(meta(Lyrics))
Meta.Lyrics<-unique(Meta.Lyrics[,c(4,5)])

#inspect(tm_filter(Lyrics, FUN = sFilter, "Year=='1981'"))

# Edit corpus
doc.Lyrics <- tm_map(Lyrics, tolower)
#stopwords("dutch")
mywords<-c(stopwords("dutch"),"jij", "jou", "andré", "hazes", "(c)", "refrein")
#doc.Lyrics <- tm_map(doc.Lyrics, removeWords, mywords )
#doc.Lyrics <- tm_map(doc.Lyrics, removePunctuation)
#doc.Lyrics <- tm_map(doc.Lyrics, removeNumbers)
# stemming of words
require(SnowballC)
doc.Lyrics <- tm_map(doc.Lyrics, stemDocument, language = "dutch")
doc.Lyrics <- tm_map(doc.Lyrics, stripWhitespace)
inspect(doc.Lyrics)

# create Term Document Matrix
dtm.control <- list(tolower = TRUE,
                    removePunctuation = TRUE,
                    removeNumbers = TRUE,
                    stopwords = mywords,
                    #stemming = TRUE,
                    wordLengths = c(3, Inf),
                    minDocFreq=2, 
                    #minWordLength=2,
                    weighting = weightTf)
DTM.Lyrics <- DocumentTermMatrix(Lyrics, control = dtm.control)
TDM.Lyrics <- TermDocumentMatrix(Lyrics, control = dtm.control)
dim(TDM.Lyrics)
check<-as.matrix(inspect(TDM.Lyrics))
write.csv(check,"TDM.csv")
Sel.words<-inspect(DocumentTermMatrix(doc.Lyrics, list(dictionary = c("mary"))))
findFreqTerms(TDM.Lyrics, 150, 200)

# find text with words
meta(tm_filter(Lyrics, FUN = function(x) any(grep("Mary", x))))
inspect(tm_filter(Lyrics, FUN=function(x) any(grep("Mary", x))))
meta(doc.Lyrics[213])
findAssocs(TDM.Lyrics, "leven", 0.3)

# Remove sparse terms
dim(TDM.Lyrics)
TDM.Lyrics.s<-removeSparseTerms(TDM.Lyrics, 0.99)
dim(TDM.Lyrics.s)
inspect(TDM.Lyrics.s)

# Topic modelling
#Fit models and find an optimal number of topics as suggested by Ben Marmick --# http://stackoverflow.com/questions/21355156/topic-models-cross-validation-with-loglikelihood-or-perplexity/21394092#21394092
# It appears some documents have zeros in all docs in the DTM, giving an error in the LDA.
rowTotals <- apply(TDM.Lyrics, 1, sum) #Find the sum of words in each Document
TDM.Lyrics2<- TDM.Lyrics[rowTotals>0,]           #remove all docs without words
inspect(TDM.Lyrics2)
LDA.Lyrics<-LDA(TDM.Lyrics2,5)
t = terms(LDA.Lyrics,15)
t

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
models <- lapply(ks, function(k) LDA(TDM.Lyrics2, k, method = "Gibbs", control = list(burnin = burnin, iter = iter, keep = keep)))
logLiks <- lapply(models, function(L)  L@logLiks[-c(1:(burnin/keep))])
hm <- sapply(logLiks, function(h) harmonicMean(h))

# Find optimal model
plot(ks, hm, type = "l")
opt <- models[which.max(hm)][[1]]

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


# Wordcloud
require(slam)
require(wordcloud)
library(RColorBrewer)

TDM.dense <- as.matrix(TDM.common)
head(TDM.dense)
object.size(TDM.common)
object.size(TDM.dense)
palette <- brewer.pal(9,"BuGn")[-(1:4)]
wordcloud(rownames(TDM.dense), TDM.denseDocs, min.freq = 1, color = palette)
