# tests for cacheMatrix

testsMatrix <- function() {
  matrix <- matrix(
    c(1:4),
    nrow = 2,
    ncol = 2,
    byrow = TRUE)
  
  matrixCached <- makeCacheMatrix(matrix)
  
  inverseMatrix = cacheSolve(matrixCached)
  
  # returns
  print("---- original")
  print(matrix)
  print("---- 'cached' matrix")
  print(matrixCached$get())
  
  print("----")
  print("---- Inverse with solve")
  print(solve(matrix))
  print("---- Inverse with function")
  print(cacheSolve(matrixCached))
  print("---- Inverse with function (must tell is cached)")
  print(cacheSolve(matrixCached))
  print("")
  print("")
  print("---- are values identical?")
  print(matrix == matrixCached$get())
  print("---- are values identical?")
  print(solve(matrix) == cacheSolve(matrixCached))
  
}