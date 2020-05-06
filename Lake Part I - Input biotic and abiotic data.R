# Part I - Input biotic and abiotic data

setwd('XX') # input working path - if path is 'E:\', then XX should be 'E:\\'
library(vegan)

# Attention: Both biotic and abiotic matrix, the 'site' shoule be 'row names'
ben <- read.csv("XX.csv",header = T,row.names = 1)# XX is the file name

# group factors can be in the first column
env.src <- read.csv("XX.csv",header = T,row.names = 1)# XX is the file name

hea(ben)

env <- env.src[,-1]# Remove the first column 
head(env)

grp <- env.src[,1]
grp