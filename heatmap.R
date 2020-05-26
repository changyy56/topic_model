#julei heatmap

#w=w[,-1]     #剔除日期字段


beta_log_changed <-read.csv("beta_log_changed.csv",header = F, sep = ";") #12982*70: 1-30lie(OR), 31-70lie(MS)

beta_log_changed <-t(beta_log_changed)      #将数据转置

w <- beta_log_changed[1:70,]
rownames(w) <- c("OR 1", "OR 2", "OR 3", "OR 4", "OR 5", "OR 6", "OR 7", "OR 8", "OR 9", "OR 10", "OR 11", "OR 12", "OR 13", "OR 14", "OR 15", "OR 16", "OR 17", "OR 18", "OR 19", "OR 20", "OR 21", "OR 22", "OR 23", "OR 24", "OR 25", "OR 26", "OR 27", "OR 28", "OR 29", "OR 30","MS 1", "MS 2", "MS 3", "MS 4", "MS 5", "MS 6", "MS 7", "MS 8", "MS 9", "MS 10", "MS 11", "MS 12", "MS 13", "MS 14", "MS 15", "MS 16", "MS 17", "MS 18", "MS 19", "MS 20", "MS 21", "MS 22", "MS 23", "MS 24", "MS 25", "MS 26", "MS 27", "MS 28", "MS 29", "MS 30", "MS 31", "MS 32", "MS 33", "MS 34", "MS 35", "MS 36", "MS 37", "MS 38", "MS 39", "MS 40")
#colnames(w) <- c("Topic 1", "Topic 2", "Topic 3", "Topic 4", "Topic 5", "Topic 6", "Topic 7", "Topic 8", "Topic 9", "Topic 10", "Topic 11", "Topic 12", "Topic 13", "Topic 14", "Topic 15", "Topic 16", "Topic 17", "Topic 18", "Topic 19", "Topic 20", "Topic 21", "Topic 22", "Topic 23", "Topic 24", "Topic 25", "Topic 26", "Topic 27", "Topic 28", "Topic 29", "Topic 30", "Topic 31", "Topic 32", "Topic 33", "Topic 34", "Topic 35", "Topic 36", "Topic 37", "Topic 38", "Topic 39", "Topic 40")

dist.e=dist(w, method="manhattan")  #计算其欧氏距离矩阵 "euclidean" "manhattan"

#dist.e <- read.csv("ms_cos.csv",header = F, sep = ";")

a1 <- as.matrix(dist.e)
#rownames(a1) <- c("Topic 1", "Topic 2", "Topic 3", "Topic 4", "Topic 5", "Topic 6", "Topic 7", "Topic 8", "Topic 9", "Topic 10", "Topic 11", "Topic 12", "Topic 13", "Topic 14", "Topic 15", "Topic 16", "Topic 17", "Topic 18", "Topic 19", "Topic 20", "Topic 21", "Topic 22", "Topic 23", "Topic 24", "Topic 25", "Topic 26", "Topic 27", "Topic 28", "Topic 29", "Topic 30", "Topic 31", "Topic 32", "Topic 33", "Topic 34", "Topic 35", "Topic 36", "Topic 37", "Topic 38", "Topic 39", "Topic 40")
#colnames(a1) <- c("Topic 1", "Topic 2", "Topic 3", "Topic 4", "Topic 5", "Topic 6", "Topic 7", "Topic 8", "Topic 9", "Topic 10", "Topic 11", "Topic 12", "Topic 13", "Topic 14", "Topic 15", "Topic 16", "Topic 17", "Topic 18", "Topic 19", "Topic 20", "Topic 21", "Topic 22", "Topic 23", "Topic 24", "Topic 25", "Topic 26", "Topic 27", "Topic 28", "Topic 29", "Topic 30", "Topic 31", "Topic 32", "Topic 33", "Topic 34", "Topic 35", "Topic 36", "Topic 37", "Topic 38", "Topic 39", "Topic 40")
#rownames(dist.e) <- c("Topic 1", "Topic 2", "Topic 3", "Topic 4", "Topic 5", "Topic 6", "Topic 7", "Topic 8", "Topic 9", "Topic 10", "Topic 11", "Topic 12", "Topic 13", "Topic 14", "Topic 15", "Topic 16", "Topic 17", "Topic 18", "Topic 19", "Topic 20", "Topic 21", "Topic 22", "Topic 23", "Topic 24", "Topic 25", "Topic 26", "Topic 27", "Topic 28", "Topic 29", "Topic 30", "Topic 31", "Topic 32", "Topic 33", "Topic 34", "Topic 35", "Topic 36", "Topic 37", "Topic 38", "Topic 39", "Topic 40")

#heatmap
heatmap(a1)  #将矩阵绘制热图

#Hierarchical Clustering
model1=hclust(dist.e,method="single")   #建立系统聚类模型
plot(model1, labels=NULL, main='Hierarchical clustering on Management Science', xlab = "")          #画聚类树图
