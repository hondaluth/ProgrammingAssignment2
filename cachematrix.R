#####################################################################################

## makeCacheMatrix(x) is just saving the inverse matrix to the "inv_matrix" name.
## cacheSolve(y) will first check if the inv_matrix is the inverse for the matrix 
## "y", if not it will then find y^-1

## makeCacheMatrix(x) is just saving the inverse matrix to the "inv_matrix" name.
m <- matrix(c(-1, -2, 1, 1), 2, 2) ## this is to check the functionality
m2 <-matrix(c(8, 9, 2, 3), 2, 2) ## this is to check the functionality

## Note, if m and m2 are not the same matrix sizes (i.e. 2x2, 3x3, etc.) this will return an error
## message. 

makeCacheMatrix <- function(x = matrix()) {
        id_matrix <<- diag(x = 1, nrow(x), ncol(x)) ## Making identiy matrix
        inv_matrix <<- solve(x, id_matrix) ## finding inverse matrix
}


## cacheSolve(y) will first check if the inv_matrix is the inverse for the matrix 
## "y", if not it will then find y^-1

cacheSolve <- function(x = matrix()) {
        if(identical(inv_matrix %*% x, id_matrix)){ ## checking to see if the cached inverse is good
                print(inv_matrix) 
        } else {                                ## if cached inverse is not good calculating new one
                id_matrix2 <- diag(x = 1, nrow(x), ncol(x))
                inv_matrix2 <<- solve(x, id_matrix2)
                print(inv_matrix2)        
                }
}
makeCacheMatrix(m)
cacheSolve(m)
cacheSolve(m2)
##---------------------------- end of assignment--------------------------------------
