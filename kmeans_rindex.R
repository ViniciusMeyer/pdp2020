library('clValid')
#library("e1071")
#library(ggplot2)
#library(reshape2)
#library(gtable)
#library(scales)
#library(lattice)
#library(tidyverse)
#library("caret")
#library('ipred')
#library('dplyr')


source("./input_dataset.R")
folder_source <- "./training_dataset/"
total<-input_dataset(folder_source)


cl_total <- kmeans(total[1:7],3, nstart = 20)


#Rand-Index
#install.packages("fossil")
# load package
library(fossil)
true_label <- as.numeric(total$category)

# perform k-means clustering
my_kmeans <- kmeans(x = total[,-8], centers = 3)
# clustering results
#my_kmeans$cluster
# Rand index
rand.index(true_label, my_kmeans$cluster)


