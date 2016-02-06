## makeCacheMatrix creates a list of values needed to cache a matrix

## 1. Set matrix value
## 2. Get matrix value
## 3. Set value of inverse of matrix
## 4. Get value of inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(inverse) m <<- inverse
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## Computes the inverse of a matrix and sets value in cache
## if matrix is already present, it will skip the computation

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
  ## Return a matrix that is the inverse of 'x'
  m
}

## Extra does not to be graded
## Computes how long it takes to computer the inverse of a normal matrix
## then subtracts the compute time from a cached matrix to give the time
## time savings aquired by caching the matrix
how_much_faster = function(x){
  
  # creates temporary object to store cached inverse
  print('caching matrix')
  temp = makeCacheMatrix(x)
  cacheSolve(temp)
  
  ## computes time for normal matrix inversion and stores as object
  print('Solving inverse matrix')
  start.time = Sys.time()
  solve(x)
  dur1 = Sys.time() - start.time

  
  ## computes time to invert cached matrix and stores as object
  start.time = Sys.time()
  cacheSolve(temp)
  dur2 = Sys.time() - start.time
  
  ##returns time savings from cached matrix  
  dur1-dur2
}
