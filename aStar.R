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