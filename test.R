#studying package topicmodels

library(lda)
#install.packages("corpus.JSS.papers",repos = "http://datacube.wu.ac.at/", type = "source")
#data("JSS_papers", package = "corpus.JSS.papers")

#input data
data1<-read.csv("ms_total_delete.csv",header = T, sep = ";")
#data1<-read.table("msdata.txt",header = F)

#install.packages("xlsx")
#library(xlsx)

#data1<-read.table("clipboard",header=T)

#==================================
#library("OAIHarvester")
#x <- oaih_list_records("http://www.jstatsoft.org/oai")
#JSS_papers <- oaih_transform(x[, "metadata"])
#JSS_papers <- oaih_transform(x[, "metadata"])
#JSS_papers <- JSS_papers[order(as.Date(unlist(JSS_papers[, "date"]))), ]
#JSS_papers <- JSS_papers[grep("Abstract:", JSS_papers[, "description"]), ]
#JSS_papers[, "description"] <- sub(".*\nAbstract:\n", "",unlist(JSS_papers[, "description"]))
#==================================

#use only abstracts published up to 2010-08-05
#omit those containing non-ASCII characters in the abstracts
#JSS_papers <- JSS_papers[JSS_papers[,"date"] < "2010-08-05",]
#JSS_papers <- JSS_papers[sapply(JSS_papers[, "description"],Encoding) == "unknown",]

#change into character
data1$AB<-as.character(data1$AB) 
#omit those containing non-ASCII characters in the abstracts
data1 <- data1[sapply(data1[, "AB"],Encoding) == "unknown",]

#===============================================================
library("topicmodels")
library("XML")

#HTML markup in the abstracts for greek letters, subscripting, etc., is removed using package XML (Temple Lang 2010).
remove_HTML_markup <- function(s) 
  {
    doc <- htmlTreeParse(s, asText = TRUE, trim = FALSE)
    xmlValue(xmlRoot(doc))
}
library("tm")
#corpus <- Corpus(VectorSource(sapply(JSS_papers[, "description"], remove_HTML_markup)))
#corpus <- Corpus(VectorSource(sapply(data1[, "AB"], remove_HTML_markup)))
corpus1 <- Corpus(VectorSource(data1[, "AB"]))

#terms are stemmed and the stop words, punctuation, numbers and terms of length less than 3 are removed using the control argument
#(We use a C locale for reproducibility.)
Sys.setlocale("LC_COLLATE", "C")
library("SnowballC")
#JSS_dtm <- DocumentTermMatrix(corpus,
#           control = list(stemming = TRUE, stopwords = TRUE, minWordLength = 3,
#           removeNumbers = TRUE, removePunctuation = TRUE))
data1_dtm <- DocumentTermMatrix(corpus1,
                control = list(stemming = TRUE, stopwords = TRUE, minWordLength = 3,
                        removeNumbers = TRUE, removePunctuation = TRUE))
dim(data1_dtm) #or:4971*8618 //4956*8701
#summary(col_sums(data1_dtm))

#===========tfidf: select the most important words
library(slam)
#term_tfidf <-
#      tapply(JSS_dtm$v/row_sums(JSS_dtm)[JSS_dtm$i], JSS_dtm$j, mean) *
#      log2(nDocs(JSS_dtm)/col_sums(JSS_dtm > 0))
term_tfidf <-
  tapply(data1_dtm$v/row_sums(data1_dtm)[data1_dtm$i], data1_dtm$j, mean) *
  log2(nDocs(data1_dtm)/col_sums(data1_dtm > 0))
#7636words

data1_dtm <- data1_dtm[, term_tfidf >= 0.1] #4922words #ms_total:11355->9688 //9045  #or:4971*6788 //4956*8609
data1_dtm <- data1_dtm[row_sums(data1_dtm) > 0,] #3932->3124 #ms_total:#records:7704->7612 //7039 -> 7036
#summary(col_sums(JSS_dtm))

#=============== LDA =================
k <- 40 #no. of topics
SEED <- 2018
data1_TM <- list(
    #VEM = LDA(data1_dtm, k = k, control = list(seed = SEED)),
    #VEM_fixed = LDA(data1_dtm, k = k, control = list(estimate.alpha = FALSE, seed = SEED)),
    Gibbs = LDA(data1_dtm, k = k, method = "Gibbs", control = list(seed = SEED, burnin = burnin, iter = iter, keep = keep))
    #CTM = CTM(data1_dtm, k = k, control = list(seed = SEED, var = list(tol = 10^-4), em = list(tol = 10^-3)))
    )

#control = list(burnin = burnin, iter = iter, keep = keep)
Topic <- topics(data1_TM[["Gibbs"]], 5)
Terms <- terms(data1_TM[["Gibbs"]], 20)
Terms[, 1:5]

Topic_result <- as.data.frame(Topic)
Terms_result <- as.data.frame(Terms)
#===========================often used
#z=matrix(c("a","b", "c", 1, "a","b","c","d"),nrow = 2 )
#class(data1)
#mode(data2[1,3])
#class(data1$AB)
gamma_list <- as.data.frame(data1_TM[["Gibbs"]]@gamma)
write.table(gamma_list,file="outcome/ms40gamma_list.txt",quote=F,col.name=TRUE,row.names=TRUE)

beta_list <- as.data.frame(data1_TM[["Gibbs"]]@beta)

write.table(Terms_result,file="outcome/ms_test2.txt",quote=F,col.name=TRUE,row.names=TRUE)

