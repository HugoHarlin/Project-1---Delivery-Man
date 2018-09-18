set.seed(10)
rm(list = ls())
setwd("~/GitHub/Project-1-Delivery-Man/")

library(DeliveryMan)
source("carmove.R")
source("appendSorted.R")
source("manhattanDist.R")
source("aStar.R")
options(error=traceback)

#runDeliveryMan(carReady = carmove)
testDM(myFunction = carmove)



