## Read in cloud QA csv, create mask to use on extraction data

rm(list=ls())

qa <- read.csv("10970_i35_test_QA.csv", header = TRUE)
nqa <- read.csv("10970_i35_test.csv", header = TRUE)

## Might need this if strip off first column of rownumbers
#qa <- qa[,-1]

is.na(qa[,3])
9999 <- qa[is.na(qa)]

a <- as.data.frame(!is.na(qa))
a <- as.numeric(a[, 1:12])
a[, 1:12]

test <- a*nqa
