library(tm)
library(wordcloud)
library(RColorBrewer)

getwd()
setwd()

name <- file.path("C:/.../text")
length(dir(name))
dir(name)
docs <- Corpus(DirSource(name)) 
docs

docs <- tm_map(docs, tolower)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, stemDocument, language = "porter")
docs <- tm_map(docs, removeWords, c("applause", "can", "cant", "will", "that",
                                    "weve", "dont","wont", "youll", "youre",
                                    "thats")) 
docs <- tm_map(docs, PlainTextDocument)
dtm <- DocumentTermMatrix(docs)
dim(dtm)
dtm = removeSparseTerms(dtm, 0.75)
dim(dtm)

rownames(dtm) <- c("2010","2011","2012","2013","2014","2015","2016")
inspect(dtm[1:7, 1:5])

freq = colSums(as.matrix(dtm))
ord = order(-freq) #order the frequency
freq[head(ord)]
freq[tail(ord)]

head(table(freq))
tail(table(freq))
findFreqTerms(dtm, 125)
findAssocs(dtm, "jobs", corlimit = 0.85)

wordcloud(names(freq), freq, 
          min.freq = 70, scale = c(3, .5), colors=brewer.pal(6, "Dark2"))
wordcloud(names(freq), freq, max.words = 25)

freq <- sort(colSums(as.matrix(dtm)), decreasing = TRUE)
wf <- data.frame(word = names(freq), freq = freq)
wf <- wf[1:10, ]
barplot(wf$freq, names = wf$word, main = "Word Frequency",
        xlab = "Words", ylab = "Counts", ylim = c(0, 250))
#topic models
library(topicmodels)
set.seed(123)
lda3 <- LDA(dtm, k = 3, method = "Gibbs")
topics(lda3)
set.seed(456)

terms(lda3, 25)

library(qdap)

speech16 <- paste(readLines("sou2016.txt"), collapse=" ")
speech16 <- iconv(speech16, "latin1", "ASCII", "")

prep16 <- qprep(speech16)

prep16 <- replace_contraction(prep16)
prep16 <- rm_stopwords(prep16, Top100Words, separate = F)
prep16 <- strip(prep16, char.keep = c("?", "."))

sent16 <- data.frame(speech = prep16)
sent16 <- sentSplit(sent16, "speech")
sent16$year <- "2016"

speech10 <- paste(readLines("sou2010.txt"), collapse=" ")
speech10 <- iconv(speech10, "latin1", "ASCII", "")
speech10 <- gsub("(Applause.)", "", speech10)
prep10 <- qprep(speech10)
prep10 <- replace_contraction(prep10)
prep10 <- rm_stopwords(prep10, Top100Words, separate = F)
prep10 <- strip(prep10, char.keep = c("?", "."))
sent10 <- data.frame(speech = prep10)
sent10 <- sentSplit(sent10, "speech")
sent10$year <- "2010"

sentences <- data.frame(rbind(sent10, sent16))

plot(freq_terms(sentences$speech))
wordMat <- wfm(sentences$speech, sentences$year)
head(wordMat[order(wordMat[, 1], wordMat[, 2],decreasing = TRUE),])

trans_cloud(sentences$speech, sentences$year, min.freq = 10)

ws <- word_stats(sentences$speech, sentences$year, rm.incomplete = T)
plot(ws, label = T, lab.digits = 2)

pol <- polarity(text.var = sentences$speech, 
                grouping.var = sentences$year)
pol
plot(pol)
pol.df <- pol$all
which.min(pol.df$polarity)
pol.df$text.var[12]

ari <- automated_readability_index(text.var = sentences$speech,
                                  grouping.var = sentences$year)
ari$Readability

form <- formality(sentences$speech, sentences$year)
form
form$form.prop.by
plot(form)

div <- diversity(sentences$speech, sentences$year)
div

dispersion_plot(sentences$speech, 
               rm.vars = sentences$year,
                c("security", "jobs", "economy"),
                color = "black", bg.color = "white")


