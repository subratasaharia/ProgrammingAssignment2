## The makeCacheMatrix function uses the concepts of lexical scoping in R to create a list
## functions that can be used to set the matrix and get the matrix.The cacheSolve
## function firstly checks in the cache if the inverse of the matrix passed into 
## makeCacheMatrix exists already, if it exists, it prints the inverse without
## calculating the inverse again and hence saves time. If it does not exist in the
## cache, it uses the Solve function to calculate the inverse and prints the
## inverse ont the screen.

## The makeCacheMatrix function creates a list of functing for setting and
## getting the inverse of the matrix passed as argument into the function
## Lexical scoping is leveraged for easily passing variables from
## defining environment to parent environment. 

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  setmatrix<-function(y=matrix()){
    x<<-y  ## The << operator is used to push the matrix y into the parent env
    inv<<- NULL ## resets inv to NULL so that inverse is calculated again
  }	
  getmatrix<-function()x ## Leverages lexical scoping to get the matrix x
  setinverse<-function(inverse)inv<<-inverse ##Pushes inv variable into parent env
  getinverse<-function()inv ## Leverages lexical scoping to get the inv
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse, getinverse = getinverse)
  ## returns a list for easy calling of the functions from external env    
}


## The cacheSolve function is used to calculate the inverse of the matrix passed
## into the  makeCacheMatrix function. However, it calculates the inverse only
## if it is not existing in the cache and resues the calculated inverse in case
## the inverse exists in the cache.

cacheSolve <- function(x, ...) {
  inv<-x$getinverse()
  if(!is.null(inv)) { ## Firstly checks if the inverse is calculated
    message (" Getting cached inverse matrix data")
    return(inv) ## If the inverse exist, it directly prints without calculating
  }
  data<-x$getmatrix() ## If the inverse does not exist, it gets the data and calculates
  inv<-solve(data,...)
  x$setinverse(inv)   ## Uses the set function to push the inverse into the cache
  message("Presenting inverse matrix")
  inv
}
