# http://quantifyingmemory.blogspot.co.uk/2014/02/web-scraping-part2-digging-deeper.html

library(RCurl)
library(XML)
web_page <- getURL("http://songteksten.net/lyric/142/80194/andre-hazes/nou-ben-ik-aan-de-beurt.html")
check<- htmlTreeParse(web_page) # do not know what this does but is the only way to visualise with head
summary(check)
substring(check,1,20000)
PARSED <- htmlParse(web_page)
head(check)
xpathApply(PARSED, "///li")
xpathSApply(PARSED, "//h1",xmlValue) 
xpathSApply(PARSED, "//h3",xmlValue)
length(xpathSApply(PARSED, "//a/@href")) 
head(xpathSApply(PARSED, "//ul[@class='lyrics']",xmlValue))
head(xpathSApply(PARSED, "//span[@class='citation news']/a/@href")) 
xpathSApply(PARSED, "//div[@id='body_right']",xmlValue) #  WERKT!!
xpathSApply(PARSED, "//div[@id='body_right']/p",xmlValue) # werkt niet, zou tekst moeten opknippen in verschillende lists.

# Webscraper function
songs1<- getURL("http://songteksten.net/artist/lyrics/142/andre-hazes.html")
PARSED <- htmlParse(songs1)
xpathSApply(PARSED, "//ul[@class='lyrics']",xmlValue, recursive=T)
xpathSApply(PARSED, "//ul[@class='lyrics']",xmlName)
getNodeSet(PARSED, "//ul[@class='lyrics']")
doc<-xmlRoot(check)
xmlName(doc)
xmlSize(doc)
doc[[2]]

node <- xmlNode("foo", "Some text")
xmlValue(node)
xmlAttrs(doc)
