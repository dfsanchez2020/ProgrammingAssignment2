## Un par de funciones que se utilizan para crear un objeto especial que
## almacena una matriz y a su vez almacena en caché su inverso.

## Esta primera función crea un objeto especial "matriz" que se puede almacenar en el caché su inverso.

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    
    x <<- y
    i <<- NULL
    
  }
  
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Esta segunda función calcula el inverso de la "matriz" especial creada por makeCacheMatrix arriba. 
## Si ya se ha calculado el inverso y la matrix no ha cambiado, entonces se recuperara el inverso del caché.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  i <- x$getInverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  mat <- x$get()
  i <- solve(mat, ...)
  x$setInverse(i)
  i
}
