#Summarizing the cross-validation simulation results

set.seed(0908)
topics <- 10 * c(1:5, 10, 20)
library("topicmodels")
#data("AssociatedPress", package = "topicmodels")
D <- nrow(data1_dtm)
#folding <- sample(rep(seq_len(10), ceiling(D))[seq_len(D)])
data1_test <- data1_alpha <- list()


#for (method in c("VEM", "VEM_fixed", "Gibbs")) {
#  data1_alpha[[method]] <- data1_test[[method]] <- matrix(NA,
#                                                    nrow = length(topics), ncol = 10, dimnames = list(topics, seq_len(10)))
#  for (fold in seq_len(10)) {
#    for (i in seq_along(topics)) {
#      T <- topics[i]
#      FILE <- paste(method, "_", T, "_", fold, ".rda", sep = "")
#      load(file.path("results", FILE))
#      data1_alpha[[method]][paste(T),fold] <-
#        if (is(training, "Gibbs_list")) training@fitted[[1]]@alpha
#        else training@alpha
#      data1_test[[method]][paste(T),fold] <- perplexity(testing,
#                                                     data1_dtm[folding == fold,], use_theta = FALSE)
#    }
#  } }

for (method in c("VEM", "VEM_fixed", "Gibbs")) {
  data1_alpha[[method]] <- data1_test[[method]] <- matrix(NA,
                                                          nrow = length(topics), ncol = 10, dimnames = list(topics, seq_len(10)))
  for (fold in seq_len(10)) {
    for (i in seq_along(topics)) {
      T <- topics[i]
      FILE <- paste(method, "_", T, "_", fold, ".rda", sep = "")
      load(file.path("results", FILE))
      data1_alpha[[method]][paste(T),fold] <-
        if (is(training, "Gibbs_list")) training@fitted[[1]]@alpha
    else training@alpha
      data1_test[[method]][paste(T),fold] <- 
        if (is(testing, "Gibbs_list")) testing@fitted[[1]]@loglikelihood
        else sum(testing@loglikelihood)
    }
  } }

save(data1_alpha, data1_test, file = "AP.rda")

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

