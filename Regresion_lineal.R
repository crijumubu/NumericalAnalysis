### INITIAL INFORMATION FOR LABORATORY SOLUTION


vectorx <- c(-1,3,8,16,17,22,24)
vectory <- c(-5,2,7,9,18,20,22)
n <- 4 ### FUNCTION ORDER
contx <- 0
conty <- 1
row_length <- n + 1                                        
column_length <- n + 2
matrix <- matrix(0:0, nrow <- row_length , ncol <- column_length)


### NEW VARIABLES DEFINITION


extension <- n * 2
sumx <- c(1:extension)
sumy <- 0
sumxy <- c(1:extension)
sumx[1] <- n


### CALCULATION AND CREATION OF THE MATRIX WITH THE GIVEN VECTORS


for (i in 1:extension){
  resultx <- 0
  resultxy <- 0
  sumy <- 0
  for (j in 1:length(vectorx)){
    resultx <- resultx + vectorx[j] ** (i)
    sumy <- sumy + vectory[j]
    resultxy <- resultxy + (vectorx[j] ** (i-1) * vectory[j])
  }
  sumxy[i] <- resultxy
  sumx[i+1] <- resultx
}

for (i in 1:row_length){
  for (j in 1:column_length){
    if (j != column_length){
      matrix[i,j] <- sumx[j+(i-1)]
    }else{
      matrix[i,j] <- sumxy[i]
    }
  }
}


cat("This is the matrix generated from the values of x and y defined in the vectors:\n\n")
print(matrix)


### GAUSS METHOD


for (i in 1:(n)){
  for (j in (i+1):row_length){
    scalar <- matrix[j,i]/matrix[i,i]
    for (k in 1:column_length){
      matrix[j,k] <- matrix[j,k] - matrix[i,k] * scalar
    }
  }
}

cat("\nThis is the resulting matrix after performing the Gauss method:\n\n")
print(matrix)


### GAUSS-JORDAN METHOD


for (i in 1:(n+1)){
  for (j in 1:(i-1)){
    if (i != 1){
      scalar <- matrix[j,i]/matrix[i,i]
      for (k in 1:column_length){
        matrix[j,k] <- matrix[j,k] - matrix[i,k] * scalar
      }
    }
  }
}


for (i in 1:row_length){
  scalar <- matrix[i,i]
  for (j in i:column_length){
    matrix[i,j] <- matrix[i,j] / scalar
  }
}


cat("\nFinally, to find out the result, we apply Gauss-Jordan method, this is the resulting matrix after performing this:\n\n")
print(matrix)


### STUDENT: CRISTIAN JULIAN MUNOZ BUENAHORA - 000430876