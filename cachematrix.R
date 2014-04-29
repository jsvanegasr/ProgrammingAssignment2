##Scrip para alamcenar y calcular la inversa de una matriz##

makeCacheMatrix <- function(x = matrix()) { ##Creacion de objeto matrix##
        inverse <- NULL  ##Inicializacion de la matriz inversa##
        set <- function(y) { ##Funcion para obtener los valores de la matriz##
                x <<- y
                inverse <<- NULL
        }
        get <- function() x ##Retorno de la matriz##
        setinverse <- function(m) inverse <<- m  ##Se obtiene la matriz inversa##
        getinverse <- function() inverse ##Retorna la inversa de la matriz##
        list(set = set, get = get,  ##Lista de Funciones##
             setinverse = setinverse,
             getinverse = getinverse)
}

cacheSolve <- function(x, ...) { ##Funcion que calcula y retorna la inversa de la matriz x##
        inv <- x$getinverse()
        if(!is.null(inv)) {  ##Primero se obtiene la inversa de la matriz##
                message("getting cached data")
                return(inv)
        }
        data <- x$get() ##Si la inversa ya esta almacenada obtiene x ##
        inv <- solve(data) ##Calcula la inversa de la matriz x##
        x$setinverse(inv) ##Se obtiene la inversa del objeto x##
        inv ##Retorna la inversa##
}
