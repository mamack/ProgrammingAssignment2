## The function take a matrix and make the inverse matrix. Because it takes a long time if the matrix
## is big, it makes the inverse in the cache.

## To test the functions the "mat"-matrix is created.
mat = matrix(c(5,1,0,3,-1,2,4,0,-1), nrow=3, byrow=TRUE)

## The function set the inverse NULL if it exists and make a Matrix in an other environment, in the cache.
makeCacheMatrix <- function(A = matrix()){
        I <- NULL
        set <- function(B){
                A <<- B
                I <<- NULL
        }
        get <- function() A
        setInverse <- function(solve) I <<- solve
        getInverse <- function() I
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## This function looks if there is an Inverse in the cache. Otherwise the function calculates the inverse if
## the matrix has an inverse.
cacheSolve <- function(A,...){
        I <- A$getInverse()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        else {
                data <- A$get()
                if(ncol(data)!=nrow(data)){
                        print("the matrix isn't square")
                }
                else if(det(data)==0){
                        print("the determinant is zero")
                }
                else{
                        I <- solve(data)
                        A$setInverse(I)
                        I
                }
        }
}

## With this code you can test the functions with the matrix mat. The second time "def" is called,
## the data come from the cache.
abc <- makeCacheMatrix(mat)
def <- cacheSolve(abc)
def <- cacheSolve(abc)