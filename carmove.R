# rm(list = ls())
# source("manhattanDist.R")

carmove = function (roads, car, packages) 
{
  nextMove = 0;
  toGo = 0;
  offset = 0;
  
  #if no package is carried, we go for the first one in the list
  if (car$load == 0) { 
    toGo = which(packages[, 5] == 0)[1]
  }
  
  # if we already carry a package we head for its delivery point by setting offset to 2
  else { 
    toGo = car$load;
    offset = 2;
  }

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
    show("start of while loop")
    
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
        show("movement left")
        newNode =  list(x=0, y=0, cost=0, heur=0, path=list());
        
        newNode$x = expanded$x-1;
        newNode$y = expanded$y;
        newNode$cost = expanded$cost + roads$hroads[newNode$x-1,newNode$y];
        newNode$heur = manhattanDist(expanded$x-1,expanded$y, packages[toGo, 1+offset],packages[toGo, 2+offset]);
        newNode$path = append(expanded$path, list(x = expanded$x-1, y = expanded$y));
        
        
        # We add the new node to the frontier, sorted with respect to the total cost of the path
        frontier = appendSorted(newNode,frontier);
      }
      
      # checks if movement to the right is possible
      if(expanded$x < 10)
      {
        show("movement right")
        newNode =  list(x=0, y=0, cost=0, heur=0, path=list());
        
        
        newNode$x = expanded$x+1;
        newNode$y = expanded$y;
        newNode$cost = expanded$cost + roads$hroads[newNode$x,newNode$y];
        newNode$heur = manhattanDist(expanded$x+1,expanded$y, packages[toGo, 1+offset],packages[toGo, 2+offset]);
        newNode$path = append(expanded$path, list(x = expanded$x+1, y = expanded$y));

        
        # We add the new node to the frontier, sorted with respect to the total cost of the path
        frontier = appendSorted(newNode,frontier);
      }
      
      # checks if movement upp is possible
      if(expanded$y < 10)
      {
        show("movement upp")
        newNode =  list(x=0, y=0, cost=0, heur=0, path=list());
        
        newNode$x = expanded$x;
        newNode$y = expanded$y+1;
        newNode$cost = expanded$cost + roads$vroads[newNode$x,newNode$y];
        newNode$heur = manhattanDist(expanded$x,expanded$y+1, packages[toGo, 1+offset],packages[toGo, 2+offset]);
        newNode$path = append(expanded$path, list(x = expanded$x, y = expanded$y+1));

        
        # We add the new node to the frontier, sorted with respect to the total cost of the path
        frontier = appendSorted(newNode,frontier);
      }
      
      # checks if movement upp is down
      if(expanded$y > 1)
      {
        show("movement down")
        newNode =  list(x=0, y=0, cost=0, heur=0, path=list());

        newNode$x = expanded$x;
        newNode$y = expanded$y-1;
        newNode$cost = expanded$cost + roads$vroads[newNode$x,newNode$y-1]; # fel här??
      show("vroads")
        show(vroads)
        newNode$heur = manhattanDist(expanded$x,expanded$y-1, packages[toGo, 1+offset],packages[toGo, 2+offset]);
        newNode$path = append(expanded$path, list(x = expanded$x, y = expanded$y-1));
  
        
        # We add the new node to the frontier, sorted with respect to the total cost of the path
        frontier = appendSorted(newNode,frontier);
      }
      
      
    } 
      
  }   
  
  if(length(expanded) == 0){
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