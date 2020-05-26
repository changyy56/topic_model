#selection for k

#Simulation using 10-fold cross-validation
set.seed(0908)
topics <- 10 * c(1:5, 10, 20)
SEED <- 20080809
library("topicmodels")
#library(doParallel)
library(ggplot2)
library(scales)

#data("AssociatedPress", package = "topicmodels")
D <- nrow(data1_dtm)
#folding <- sample(rep(seq_len(10), ceiling(D))[seq_len(D)])
for (k in topics) {
  for (chain in seq_len(10)) {
    #FILE <- paste("VEM_", k, "_", chain, ".rda", sep = "")
    #training <- LDA(data1_dtm[folding != chain,], k = k,
    #                control = list(seed = SEED))
    #testing <- LDA(data1_dtm[folding == chain,], model = training,
    #               control = list(estimate.beta = FALSE, seed = SEED))
    #save(training, testing, file = file.path("results", FILE))
    #FILE <- paste("VEM_fixed_", k, "_", chain, ".rda", sep = "")
    #training <- LDA(data1_dtm[folding != chain,], k = k,
    #                control = list(seed = SEED, estimate.alpha = FALSE))
    #testing <- LDA(data1_dtm[folding == chain,], model = training,
    #               control = list(estimate.beta = FALSE, seed = SEED))
    #save(training, testing, file = file.path("results", FILE))
    FILE <- paste("Gibbs_", k, "_", chain, ".rda", sep = "")
    training <- LDA(data1_dtm[folding != chain,], k = k,
                    control = list(seed = SEED, burnin = 1000, thin = 100,
                                   iter = 1000, best = FALSE), method = "Gibbs")
    best_training <- training@fitted[[which.max(logLik(training))]]
    testing <- LDA(data1_dtm[folding == chain,],
                   model = best_training, control = list(estimate.beta = FALSE,
                        seed = SEED, burnin = 1000, thin = 100, iter = 1000, best = FALSE))
    save(training, testing, file = file.path("results", FILE))
  }
}
