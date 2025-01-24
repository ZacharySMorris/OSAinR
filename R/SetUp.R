##Load packages##

library(ape)
library(igraph)
#library(animate)
library(caTools)


##This is just an example, installr is not needed?
# installing/loading the package:
if(!require(installr)) {
  install.packages("installr");
  require(installr)
} #load / install+load installr

##Set Working Directory##

setwd("/Users/zach/Dropbox/OSAinR")

getwd()
