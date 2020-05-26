#correlation

library(text2vec) 

corr_data<-read.csv("corr.csv",header = T, sep = ";")

corr_data1 <- aggregate(x = corr_data[,3:72], by = list(corr_data$word),FUN="sum")

col_sums(corr_data1[,2:71])

#dtm1 = create_dtm(corr_data1, vectorizer)  
#dtm2 = create_dtm(it2, vectorizer) 
#d1_d2_cos_sim = sim2(corr_data1$X2, corr_data1$X3, method = "cosine", norm = "l2")

#heatmap
#source: https://flowingdata.com/2010/01/21/how-to-make-a-heatmap-a-quick-and-easy-solution/
nba <- read.csv("orheat1_data.csv", sep=";")

#nba <- nba[order(nba$PTS),]
row.names(nba) <- nba$PY
nba <- nba[,2:31]

nba_matrix <- data.matrix(nba)
nba_heatmap <- heatmap(nba_matrix, Rowv=NA, Colv=NA, col = brewer.pal(9, "Blues"), scale="column", margins=c(5,10))

nba_heatmap <- heatmap(nba_matrix, Rowv=NA, Colv=NA, scale="column", margins=c(5,10))
