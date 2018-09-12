# rm(list = ls())
# source("manhattanDist.R")

carmove = function (roads, car, packages) 
{
  nextMove = 0;
  toGo = 0;
  offset = 0;
  
  if (car$load == 0) {
    packagesleft = which(packages[, 5] == 0)
    distance = abs(packages[,1] - car$x) + abs(packages[,2] - car$y)
    toGo = packagesleft[which.min(distance[packagesleft])]
    
    # We use A* to determine which package is the closest!
    #length = length(packagesleft[,1]);
    #show(length);
    #costPackages = list(1:length);
    #for (i in 1:length) {
    #}
    
  }
  else {
    toGo = car$load
    offset = 2
  }
  
 expanded = aStar(packages, toGo, offset,car,roads);
 
  #show(expanded)
  if(length(expanded$path) == 0){
    nextMove = 5;
  }
  else if (expanded$path[[1]]$x > car$x)
  {
    nextMove = 6;
  }
  else if(expanded$path[[1]]$x < car$x)
  {
    nextMove = 4;
  }
  
  else if(expanded$path[[1]]$y < car$y)
  {
    nextMove = 2;
  }
  else
  {
    nextMove = 8;
  }
  
  car$nextMove = nextMove;
  car$mem = list()
  return(car)
}