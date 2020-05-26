#example

#A.1. Simulation using 10-fold cross-validation
set.seed(0908)
topics <- 10 * c(1:5, 10, 20)
SEED <- 20080809
library("topicmodels")
data("AssociatedPress", package = "topicmodels")
D <- nrow(AssociatedPress)
folding <-
  sample(rep(seq_len(10), ceiling(D))[seq_len(D)])
for (k in topics) {
  for (chain in seq_len(10)) {
    FILE <- paste("VEM_", k, "_", chain, ".rda", sep = "")
    training <- LDA(AssociatedPress[folding != chain,], k = k,
                    control = list(seed = SEED))
    testing <- LDA(AssociatedPress[folding == chain,], model = training,
                   control = list(estimate.beta = FALSE, seed = SEED))
    save(training, testing, file = file.path("results", FILE))
    FILE <- paste("VEM_fixed_", k, "_", chain, ".rda", sep = "")
    training <- LDA(AssociatedPress[folding != chain,], k = k,
                    control = list(seed = SEED, estimate.alpha = FALSE))
    testing <- LDA(AssociatedPress[folding == chain,], model = training,
                   control = list(estimate.beta = FALSE, seed = SEED))
    save(training, testing, file = file.path("results", FILE))
    FILE <- paste("Gibbs_", k, "_", chain, ".rda", sep = "")
    training <- LDA(AssociatedPress[folding != chain,], k = k,
                    control = list(seed = SEED, burnin = 1000, thin = 100,
                                   iter = 1000, best = FALSE), method = "Gibbs")
    best_training <- training@fitted[[which.max(logLik(training))]]
    testing <- LDA(AssociatedPress[folding == chain,],
                   model = best_training, control = list(estimate.beta = FALSE,
                                                         seed = SEED, burnin = 1000, thin = 100, iter = 1000, best = FALSE))
    save(training, testing, file = file.path("results", FILE))
  }
}

#Summarizing the cross-validation simulation results

set.seed(0908)
topics <- 10 * c(1:5, 10)
library("topicmodels")
data("AssociatedPress", package = "topicmodels")
D <- nrow(AssociatedPress)
folding <-
  sample(rep(seq_len(10), ceiling(D))[seq_len(D)])
AP_test <- AP_alpha <- list()
for (method in c("VEM", "VEM_fixed", "Gibbs")) {
  AP_alpha[[method]] <- AP_test[[method]] <- matrix(NA,
                                                    nrow = length(topics), ncol = 10, dimnames = list(topics, seq_len(10)))
  for (fold in seq_len(10)) {
    for (i in seq_along(topics)) {
      T <- topics[i]
      FILE <- paste(method, "_", T, "_", fold, ".rda", sep = "")
      load(file.path("results", FILE))
      AP_alpha[[method]][paste(T),fold] <-
        if (is(training, "Gibbs_list")) training@fitted[[1]]@alpha
      else training@alpha
      AP_test[[method]][paste(T),fold] <- perplexity(testing,
                                                     AssociatedPress[folding == fold,], use_theta = FALSE)
    }
  } }
save(AP_alpha, AP_test, file = "AP.rda")


#===========load
load("AP.rda")
#save(mydata, file = "AP.rda")
list.files(pattern = ".rda")
data0 <- load("AP.rda")
data0
data1_alpha
data1_test

#==========plot
library("ggplot2")
library("lattice")
#library("scattersplot3d")

plot_VEM_alpha <- data1_alpha[1]$VEM
plot_VEM_ple <- data1_test[1]$VEM
xyplot(x = topics, data = plot_VEM)