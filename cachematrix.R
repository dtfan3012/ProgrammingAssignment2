#This function creates a set of functions for recalling a cached Matrix inverse
makeCacheMatrix <- function(m = matrix()) {
  
  #Store empty variable for inverse
  i <- NULL
  #enclosed function to assign functions stored as m and i in parent environment  
  
    set <- function(matrix){
      
      m <<- matrix
      
      i <<- NULL
    
  }
  #This function produces the matrix m called during the set function  
  
    get <- function(){
    
      m
  }
  #This function stores inverse to i   
  
    setinverse <- function(inverse) {
    
      i <<- inverse
  }
  
  #Retrieve the inverse stored in the variable i  
  
    getinverse <- function() {
    
      i
  }
  
  #A list is created and references each method function  
  
    list(set=set, get=get, setinverse = setinverse,
       getinverse = getinverse)
}



## Write a short comment describing this function

cachesolve <- function(x, ...){
  
  #calls makecachematrix function getinverse and stores to m  
  m <- x$getinverse()
  
  #this if statement checks to see if m matrix has stored value and fetches it
  #with a brief message to indicate that it was stored previously
  if(!is.null(m)) {
    
    message("getting cache data")
    
    return(m)
  }
  #The matrix of the makeCacheMatrix function is called and stored to data  
    data <- x$get()
  
  #The solve function acts on the stored Matrix to find the inverse and matrix
  #math is preformed on the inverse and the data to give the identify matrix  
      m <- solve(data) %*% data
  
  #Once calculated, the setinverse function is called to assign m to i from above 
  #Reassigns the inverse if it differs    
      x$setinverse(m)
  #inverse is printed
    m
}

