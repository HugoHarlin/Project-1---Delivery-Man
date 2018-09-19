# rm(list = ls())
# source("manhattanDist.R")

carmove = function (roads, car, packages) 
{
  nextMove = 0;
  toGo = 0;
  offset = 0;
  
  aStar= function (packages,toGo, offset,car,roads){
    #calculates the shortest distance from a starting point to a destination, and returns the corresponding node
    
    # we create our starting node and add it to the frontier list
    start_node = list(x=0, y=0, cost=0, heur=0, path=list());
    start_node$x = car$x;
    start_node$y = car$y; 
    start_node$cost = 0; 
    start_node$heur = manhattanDist(car$x,car$y, packages[toGo, 1+offset],packages[toGo, 2+offset]);
    
    frontier = list(start_node);
    
    
    # COMMENCE ASTAR!!
    flag = 1;
    while(flag)
    {
      expanded = frontier[[1]];
      frontier = frontier[-1];
      
      
      # if expanded node is the target node, we are done!
      if(expanded$x == packages[toGo, 1 + offset] && expanded$y == packages[toGo, 2 + offset])
      {
        flag = 0;
        optiPath = expanded$path; # optimal path to the goal
      }
      
      # current node is not the target, we expand the best path.
      else 
      {
        
        
        # checks if movement to the left is possible
        if(expanded$x > 1)
        {
          newNode =  list(x=0, y=0, cost=0, heur=0, path=list());
          newNode$x = expanded$x-1;
          newNode$y = expanded$y;
          newNode$cost = expanded$cost + roads$hroads[expanded$x-1,expanded$y];
          newNode$heur = manhattanDist(expanded$x-1,expanded$y, packages[toGo, 1+offset],packages[toGo, 2+offset]);
          newNode$path = expanded$path;
          newNode$path[[length(newNode$path)+1]] = list(x = expanded$x-1, y = expanded$y);
          
          # We add the new node to the frontier, sorted with respect to the total cost of the path
          flag2 = 1;
          length = length(expanded$path);
          if(length>0){
            for (i in 1:length(expanded$path)){
              if(expanded$path[[i]]$x == newNode$x && expanded$path[[i]]$y == newNode$y)
                flag2 = 0; 
            }
          }
          # We add the new node to the frontier, sorted with respect to the total cost of the path
          if(flag2){
            frontier = appendSorted(newNode,frontier);
          }
          
        }
        
        
        # checks if movement to the right is possible
        if(expanded$x < 10)
        {
          newNode =  list(x=0, y=0, cost=0, heur=0, path=list());
          newNode$x = expanded$x+1;
          newNode$y = expanded$y;
          newNode$cost = expanded$cost + roads$hroads[expanded$x,expanded$y];
          newNode$heur = manhattanDist(expanded$x+1,expanded$y, packages[toGo, 1+offset],packages[toGo, 2+offset]);
          newNode$path = expanded$path;
          newNode$path[[length(newNode$path)+1]] = list(x = expanded$x+1, y = expanded$y);
          
          flag2 = 1;
          length = length(expanded$path);
          if(length>0){
            for (i in 1:length(expanded$path)){
              if(expanded$path[[i]]$x == newNode$x && expanded$path[[i]]$y == newNode$y)
                flag2 = 0; 
            }
          }
          # We add the new node to the frontier, sorted with respect to the total cost of the path
          if(flag2){
            frontier = appendSorted(newNode,frontier);
          }
        }
        
        # checks if movement upp is possible
        if(expanded$y < 10)
        {
          newNode =  list(x=0, y=0, cost=0, heur=0, path=list());
          newNode$x = expanded$x;
          newNode$y = expanded$y+1;
          newNode$cost = expanded$cost + roads$vroads[expanded$x,expanded$y];
          newNode$heur = manhattanDist(expanded$x,expanded$y+1, packages[toGo, 1+offset],packages[toGo, 2+offset]);
          newNode$path = expanded$path;
          newNode$path[[length(newNode$path)+1]] = list(x = expanded$x, y = expanded$y+1);
          
          # We add the new node to the frontier, sorted with respect to the total cost of the path
          flag2 = 1;
          length = length(expanded$path);
          if(length>0){
            for (i in 1:length(expanded$path)){
              if(expanded$path[[i]]$x == newNode$x && expanded$path[[i]]$y == newNode$y)
                flag2 = 0; 
            }
          }
          # We add the new node to the frontier, sorted with respect to the total cost of the path
          if(flag2){
            frontier = appendSorted(newNode,frontier);
          }
          
        }
        
        # checks if movement upp is down
        if(expanded$y > 1)
        {
          newNode =  list(x=0, y=0, cost=0, heur=0, path=list());
          newNode$x = expanded$x;
          newNode$y = expanded$y-1;
          newNode$cost = expanded$cost + roads$vroads[expanded$x,expanded$y-1]; 
          newNode$heur = manhattanDist(expanded$x,expanded$y-1, packages[toGo, 1+offset],packages[toGo, 2+offset]);
          newNode$path = expanded$path;
          newNode$path[[length(newNode$path)+1]] = list(x = expanded$x, y = expanded$y-1);   
          # We add the new node to the frontier, sorted with respect to the total cost of the path
          
          flag2 = 1;
          length = length(expanded$path);
          if(length>0){
            for (i in 1:length(expanded$path)){
              if(expanded$path[[i]]$x == newNode$x && expanded$path[[i]]$y == newNode$y)
                flag2 = 0; 
            }
          }
          # We add the new node to the frontier, sorted with respect to the total cost of the path
          if(flag2){
            frontier = appendSorted(newNode,frontier);
          }
          
        }
        
        
      } 
      
    }   
    
    return(expanded)
  }
  
  appendSorted = function (newNode,frontier){
    # We add the new node to the frontier, sorted with respect to the total cost of the path
    length = length(frontier);
    totcost = newNode$heur + newNode$cost;
    
    
    if(length == 0)
    {
      frontier[1] = list(newNode);
    }
    else{
      
      for(i in 1:length){
        if(frontier[[i]]$x == newNode$x && frontier[[i]]$y == newNode$y){
          if(frontier[[i]]$cost + frontier[[i]]$heur >= totcost ){
            frontier = frontier[-i];
            length = length -1;
            break
          }else{
            return(frontier)
          }
        }
        
      }
      
      
      for (i in 1:length) {
        if(frontier[[i]]$cost + frontier[[i]]$heur >= totcost)
        {
          frontier = append(frontier,list(newNode), i-1);
          return(frontier)
        }
      }
      frontier[length+1] = list(newNode);
    }
    return (frontier)
  }
  
  permutations <- function(n){
    if(n==1){
      return(matrix(1))
    } else {
      sp <- permutations(n-1)
      p <- nrow(sp)
      A <- matrix(nrow=n*p,ncol=n)
      for(i in 1:n){
        A[(i-1)*p+1:p,] <- cbind(i,sp+(sp>=i))
      }
      return(A)
    }
  }
  
  manhattanDist = function (x,y,xt,yt){
    dist = abs(x-xt) + abs(y - yt);
    return (dist)
  }
  
  if (length(car$mem) == 0){
    
    a = permutations(nrow(packages));
    
    totalCost = 1000;
    pathNumber = 0;
    for (i in 1:length(a[,1])){
      pathCost = manhattanDist(car$x,car$y, packages[a[i,1], 1], packages[a[i,1], 2]);
      pathCost = pathCost + manhattanDist(packages[a[i,1], 1], packages[a[i,1], 2], packages[a[i,1], 3], packages[a[i,1], 4]);
      pathCost = pathCost + manhattanDist(packages[a[i,1], 3], packages[a[i,1], 4], packages[a[i,2], 1], packages[a[i,2], 2]);
      pathCost = pathCost + manhattanDist(packages[a[i,2], 1], packages[a[i,2], 2], packages[a[i,2], 3], packages[a[i,2], 4]);
      pathCost = pathCost + manhattanDist(packages[a[i,2], 3], packages[a[i,2], 4], packages[a[i,3], 1], packages[a[i,3], 2]);
      pathCost = pathCost + manhattanDist(packages[a[i,3], 1], packages[a[i,3], 2], packages[a[i,3], 3], packages[a[i,3], 4]);
      pathCost = pathCost + manhattanDist(packages[a[i,3], 3], packages[a[i,3], 4], packages[a[i,4], 1], packages[a[i,4], 2]);
      pathCost = pathCost + manhattanDist(packages[a[i,4], 1], packages[a[i,4], 2], packages[a[i,4], 3], packages[a[i,4], 4]);
      pathCost = pathCost + manhattanDist(packages[a[i,4], 3], packages[a[i,4], 4], packages[a[i,5], 1], packages[a[i,5], 2]);
      pathCost = pathCost + manhattanDist(packages[a[i,5], 1], packages[a[i,5], 2], packages[a[i,5], 3], packages[a[i,5], 4]);
      if (pathCost<totalCost){
        totalCost = pathCost;
        pathNumber = i;
      }
      
    }
      
    car$mem = a[pathNumber,];
  }
  
  if (packages[car$mem[[1]],5] == 2){
    car$mem = car$mem[-1];
  }
  
  if (car$load == 0) {
    toGo = car$mem[[1]];
  }
  else {
    toGo = car$load;
    offset = 2;
  }
  
  #if (car$load == 0) {
    #packagesleft = which(packages[, 5] == 0)
    #distance = abs(packages[,1] - car$x) + abs(packages[,2] - car$y)
    #toGo = packagesleft[which.min(distance[packagesleft])]
    
    # We use A* to determine which package is the closest!
    #length = length(packagesleft[,1]);
    #show(length);
    #costPackages = list(1:length);
    #for (i in 1:length) {
    #}
    
  #}
  #else {
   # toGo = car$load
    #offset = 2
  #}
  
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
  return(car)
  
  
}



