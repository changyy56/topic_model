#Reference: https://stackoverflow.com/questions/21355156/topic-models-cross-validation-with-loglikelihood-or-perplexity
# load up some R packages including a few we'll need later
library(topicmodels)
library(doParallel)
library(parallel)
library(ggplot2)
library(scales)

full_data <- data1_dtm
folds <- 10
n <- nrow(full_data)
#splitfolds <- sample(1:folds, n, replace = TRUE)
#candidate_k <- c(2,5,10,15,20,25,30,40,50,100) # candidates for how many topics
#candidate_k <- c(60,75,80,150)
#candidate_k <- c(2,5,10,15,20,25,30,40,50,60,75,80,100,150)
candidate_k <- c(2,5,10,20,30,40,50,100)
burnin = 1000
iter = 2000
keep = 50

# export all the needed R objects to the parallel sessions
#clusterExport(cluster, c("full_data", "burnin", "iter", "keep", "splitfolds", "folds", "candidate_k"))

# we parallelize by the different number of topics.  A processor is allocated a value
# of k, and does the cross-validation serially.  This is because it is assumed there
# are more candidate values of k than there are cross-validation folds, hence it
# will be more efficient to parallelise
#system.time({
#  results <- foreach(j = 1:length(candidate_k), .combine = rbind) %dopar%{
results_1k <- matrix(0, nrow = folds*length(candidate_k), ncol = 2)
results_1l <- matrix(0, nrow = folds*length(candidate_k), ncol = 2)
for (j in 1:length(candidate_k)) {
    colnames(results_1k) <- c("k", "perplexity")
    for(i in 1:folds){
      train_set <- full_data[splitfolds != i , ]
      valid_set <- full_data[splitfolds == i, ]
      
      fitted <- LDA(train_set, k = candidate_k[j], method = "Gibbs",
                    control = list(burnin = burnin, iter = iter, keep = keep) )
      results_1k[(j-1)*folds + i,] <- c(candidate_k[j], perplexity(fitted, newdata = valid_set))
      results_1l[(j-1)*folds + i,] <- c(candidate_k[j], logLik(fitted, newdata = valid_set))
    }
}
    #return(results_1k)
#  }
#})
#stopCluster(cluster)

library(splines)

results_df <- as.data.frame(results_1l)

ggplot(results_df, aes(x = V1, y = V2)) +
  geom_point() +
  geom_smooth(se = FALSE,method="glm", #"gam"
              formula=y~ns(x,10),
              family=gaussian(link="log"),
              show.legend = FALSE,lwd=1) +
  #theme(legend.position=c(.2,0.8)) +
  ggtitle("10-fold cross-validation of topic modelling with the OR dataset") +
  labs(x = "k (number of topics)", y = "loglikelihood")

ggplot(results_df, aes(x = k, y = perplexity)) +
  geom_point() +
  geom_smooth(se = FALSE,method="glm",formula=y~ns(x,9),family=gaussian(link="log"),show.legend = FALSE) +
  ggtitle("5-fold cross-validation of topic modelling with the 'Associated Press' dataset",
          "(ie five different models fit for each candidate number of topics)") +
  labs(x = "Candidate number of topics", y = "Perplexity when fitting the trained model to the hold-out set")
