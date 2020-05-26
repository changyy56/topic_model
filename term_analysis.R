#term analysis

library("dplyr")
#packageVersion('Rcpp')

data_freq <- read.csv("result_1.csv",header = T, sep = ",")
freq_matrix <- t(data.matrix(data_freq[2:317]))

library(philentropy)
term_dis <- distance(freq_matrix, method = "cosine")

library(reticulate)
#py_available() #True: python is installed
os <- import("os")
os$listdir(".")
#calling python from R: https://rstudio.github.io/reticulate/articles/calling_python.html
