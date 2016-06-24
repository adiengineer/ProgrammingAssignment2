# This function creates a list of helper functions
# to modify or retrieve the input matrix and its 
# inverse
makeCacheMatrix <- function(X = matrix()) {
   
  # X is the local matrix whose inverse will be computed
  Xinv <- NULL # reference to the inverse matrix
  
  #store references to each of the helper functions
  getX <- function()
  {
    X
  }
  
  setX <- function(newX)
  {
    # to assign the new value to X we need to use the <<- 
    # because with to this function, X is in the parent environment 
    
    X <<- newX
    Xinv <<- NULL # as X changes the old Xinv no longer stays valid, so better make it NULL  
  }
  
    getinv <- function()
    {
      Xinv  
    }
    
    setinv <- function(newinv)
    {
      Xinv <<- newinv
    }
    
    #list of functions returned
    list(setX=setX,getX=getX,setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function

# catchSolve is the function which checks if inverse is available
# saving computations if already present. If not a fresh computation
# is done. Care is taken that if matrix is changed then inverse is set
# to null. Thus if matrix has changed we are sure that we won't return
# any stale value of inverse.
cacheSolve <- function(functionList, ...) {
        ## Return a matrix that is the inverse of 'x'
        # here x is the list of functions to use
  
  inverse <- functionList$getinv()
  
  #if nothing's changed return the inverse
    if( !is.null(inverse) )
    {
      message("no need to compute again!")
      return(inverse) # apparently, return is a function in R and not a keyword!
    }
  
  # if we reach this point, then we have to recompute the inverse
  currMatrix <- functionList$get()  
  inverse<- solve(currMatrix,...)
  
  #update the value of inverse in the makeCacheMatric methods. Do attributes like 
  # xinv we set using our list method act like attribute variables in C which 
  # stick around till the object they belong to is valid?
  functionList$setinv(inverse)
  inverse # finally return the inverse to the calling function
}
