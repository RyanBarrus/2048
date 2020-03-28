insert <- function(grid) {
  available = which(grid == 0)
  if (length(available) == 0) {
    return ("end")
  } else {
    placement = available[round(runif(1,1,length(available)))]
    grid[placement] = 2
    return (grid)
  }
}

rotate <- function(grid, t) {
  if (t == 0) return (grid)
  t = t - 1
  grid = t(apply(grid, 2, rev))
  return (rotate(grid,t))
}

locked <- function(grid) {
  available = which(grid == 0)
  if (length(available) != 0) {
    return (FALSE)
  }
  
  for (i in 1:4) {
    for (j in 1:4) {
      if (i < 4) {
        if (grid[i,j] == grid[i+1,j]) {
          return (FALSE)
        }
      }
      if (j < 4) {
        if (grid[i,j] == grid[i,j+1]) {
          return (FALSE)
        }
      }
    }
  }
  return (TRUE)
}

move <- function(grid,d) {
  
  t = switch(toString(d),
             '5' = 0,
             'w' = 0,
             '1' = 1,
             'a' = 1,
             '2' = 2,
             's' = 2,
             '3' = 3,
             'd' = 3)
  
  grid = rotate(grid,t)
  
  for (j in 1:4) {
    found <- FALSE
    i = 1
    while (i < 4) {
      for (k in (i+1):4) {
        if (grid[i,j] == grid[k,j] & grid[i,j] > 0) {
          grid[i,j] <- 0
          grid[k,j] <- grid[k,j] * 2
          found <- TRUE
          break
        } else if (grid[k,j] > 0) break
      }
      if (found) i = k
      i = i + 1
    }
    
    nums <- grid[,j]
    nums <- nums[which(nums>0)]
    while (length(nums) < 4) nums <- c(nums,0)
    grid[,j] <- nums
    
    t = switch(toString(d),
               '5' = 0,
               'w' = 0,
               '1' = 3,
               'a' = 3,
               '2' = 2,
               's' = 2,
               '3' = 1,
               'd' = 1)
  }
  
  grid = rotate(grid,t)
  return (grid)
}

go <- function() {
  grid <- matrix(0,ncol=4,nrow=4)
  grid = insert(grid)
  grid = insert(grid)
  d = 1
  clr = ifelse(.Platform$OS.type == 'windows','cls','clear')
  while (grid != "end" && d != "q") {
    shell(clr)
    cat('Input 5,1,2,3 or w,a,s,d to move\n')
    cat('Input q to quit\n\n')
    print(grid)
    d = readline()
    if (d %in% c(1,2,3,5,'w','a','s','d')) {
      pgrid = grid
      grid = move(grid,d)
      if (locked(grid)) {
        break
      } 
      if (!all(grid == pgrid)) {
        grid = insert(grid)
      }
    }
  }
  shell(clr)
  print(grid)
  cat("\n") 
  cat(paste('Game Over, highest tile:',max(grid)))
}


go()




