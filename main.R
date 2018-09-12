rm(list = ls())
library(DeliveryMan)
source("carmove.R")
source("appendSorted.R")
source("manhattanDist.R")
options(error=traceback)

runDeliveryMan(carReady = carmove)


