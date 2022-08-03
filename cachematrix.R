## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix 는 아래를 수행하는 특별한 행렬을 생성합니다.
## 값설정, 값얻기, inverse의 요소를 얻는 다.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## 위 특별한 행렬의 inverse를 구합니다.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}