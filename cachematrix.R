## Find below makeCacheMatrix and cacheSolve function
## makeCacheMatrix defines methods for getting,setting matrix value and for getting and setting the matrix inverse
## cacheSolve checks if the inverse of a matrix is cached or not,if found then prints the cached value or else calculates the inverse and prints

## This function defines methods for getting,setting matrix value and for getting and setting the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  
  ## Set value to the matrix
  m <- NULL
  set <- function(y){
    ## For global/permanent assignment from within an function superassignment operator (<<-) is used
    x <<- y
    m <<- NULL
  }
  ## Get the value of the matrix 
  get <- function() x
  
  ## Set the inverse of the matrix
  setinversematrix <- function(solve) m <<- solve
  
  ## Get the inverse of the matrix
  getinversematrix <- function() m
  
  ## list the set,get,setinversematrix & getinversematrix methods 
  ## assigning names set,get,setinvmatrix & getinvmatrix respectively
  ## In a function the last evaluated expression is returned which in this case is the list call
  list(set = set, get = get,
       setinvmatrix = setinversematrix,
       getinvmatrix = getinversematrix)
}



## This functions checks if the inverse of matrix is cached, 
## if yes then returns the cached value or else calculates and returns the inverse

cacheSolve <- function(x = matrix(), ...) {
  
  ## Get the inverse of the matrix using getinvmatrix
  m <- x$getinvmatrix()
  
  ## Check if the returned value is not null, if true then return
  ## the precalculated cached value with a relevant display message and exit
  if (!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  ## If the inverse matrix value isnt cached they calculate it,set the value using setinversematrix and print it
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setinvmatrix(m)
  m
}
