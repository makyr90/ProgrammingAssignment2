## The above two functions(makeCacheMatrix and cacheSolve) implememnts a solution for caching the inverted
## matrix of a given x matrix and eliminate the repeatedly calculation of the inverted matrix of x.
## This can have some benefit(in terms of calculation cost) especially for large  matrices.


##makeCacheMatrix function takes as an argument a square matrix(The matrix is stored to the function local
##environment as x) and then create a list with 4 elements. Each element of the above list is a function.
##By this way we can cache the inverted matrix of the initial matrix to the makeCacheMatrix function local
##environment(invert function).
##Also, we can retrive the initial matrix or the inverted matrix using the appropriate function of the list.
##Finally, we can change the initial matrix with a new one using the setmatrix function


makeCacheMatrix <- function(x = matrix()) {
       
       s <- NULL
        
        setmatrix <- function(y=matrix()) {
                x <<- y
                s <<- NULL
        }
        
        getmatrix <- function() x
           
        invert <- function(SolveMatrix=matrix()) s <<- SolveMatrix
        
        getinvertedmatrix <- function() s
        
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             invert = invert,
             getinvertedmatrix = getinvertedmatrix)
}

##cacheSolve function take as argument a list which has been created from makeCacheMatrix function. Then, it checks
## if the inverted matrix has been already calculated and cached. If the above statement is true then does not
## recalculate the inverted matrix and returns the cached matrix.
##If the inverted matrix is not be cached, then the function computes the inverted matrix, put it into the cache
##and then returns the inverted matrix.

cacheSolve <- function(x) {
   
        s <- x$getinvertedmatrix() 
        if(!is.null(s)) {
                message("getting cached matrix")
                return(s)
        }
        
        Matrice <- x$getmatrix()
        Solve <- solve(Matrice)
        x$invert(Solve)
        Solve
}