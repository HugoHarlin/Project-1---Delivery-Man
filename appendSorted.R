appendSorted = function (newNode,frontier){
  # We add the new node to the frontier, sorted with respect to the total cost of the path
  length = length(frontier);
  totcost = newNode$heur + newNode$cost;
show("newNode$heur")
  show(newNode$heur)
  show("newNode$cost")
  show(newNode$cost)
  
  
  if(length == 0)
  {
    frontier = append(frontier,list(newNode));
    
  }
  else{
    for (i in 1:length) {
      if(frontier[[i]]$cost + frontier[[i]]$heur >= totcost)
      {
        frontier = append(frontier,list(newNode), i-1);
        return(frontier)
      }
    }
    frontier = append(frontier,list(newNode));
  }
  return (frontier)
}