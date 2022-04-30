### INITIAL INFORMATION FOR LABORATORY SOLUTION

vectorx <- c(-1,3,8,16,17,22,24)
#vectorx <- c(-2,-1,0,1,2)
#vectory <- c(3,0,2,4,4)
vectory <- c(-5,2,7,9,18,20,22)
### FUNCTION THAT DOES THE ENTIRE REGRESION PROCESS

regresion_calculator <- function(n){
  
  ### VARIABLES AND VECTORS DEFINITION
  
  column_length <- n + 2
  contx <- 0
  conty <- 1
  determination_coefficient <- 0
  extension <- n * 2
  row_length <- n + 1     
  sumx <- c(1:extension)
  sumx[1] <- length(vectorx)
  sumxy <- c(1:extension)
  y_average <- 0
  
  matrix <- matrix(0:0, nrow <- row_length , ncol <- column_length)
  
  ### CALCULATION AND CREATION OF THE MATRIX WITH THE GIVEN VECTORS
  
  for (i in 1:extension){
    resultx <- 0
    resultxy <- 0
    y_average <- 0
    x_average <- 0
    for (j in 1:length(vectorx)){
      resultx <- resultx + vectorx[j] ** (i)
      y_average <- y_average + vectory[j]
      resultxy <- resultxy + (vectorx[j] ** (i-1) * vectory[j])
    }
    sumxy[i] <- resultxy
    sumx[i+1] <- resultx
  }
  y_average <- y_average / length(vectory)
  
  for (i in 1:row_length){
    for (j in 1:column_length){
      if (j != column_length){
        matrix[i,j] <- sumx[j+(i-1)]
      }else{
        matrix[i,j] <- sumxy[i]
      }
    }
  }
  
  ### GAUSS METHOD
  
  for (i in 1:(n)){
    for (j in (i+1):row_length){
      scalar <- matrix[j,i]/matrix[i,i]
      for (k in 1:column_length){
        matrix[j,k] <- matrix[j,k] - matrix[i,k] * scalar
      }
    }
  }
  
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
  
  ### SAVE THE RESULTS IN A NEW VECTOR
  
  resultVector <- c()
  string_result_vector <- ''
  for (i in 1:row_length){
    resultVector[i] <- matrix[(row_length - i) + 1,column_length]
    string_result_vector <- paste(string_result_vector, round(resultVector[i], digits = 5))
  }
  
  ### CALCULATE Y ESTIMATED
  
  yestimated <- c()
  
  for (i in 1:length(vectory)){
    yestimated[i] <- 0
    for (j in 1:length(resultVector)){
      yestimated[i] <- yestimated[i] + (resultVector[j]*(vectorx[i])**((length(resultVector) - j)))
    }
  }
  
  ### CALCULATE DETERMINATION COEFFICIENT
  
  y_minus_yaverage_squared <- 0
  y_minus_yestimated_squared <- 0
  
  for (i in 1:length(vectory)){
    y_minus_yaverage_squared <- y_minus_yaverage_squared + ((vectory[i]-y_average)**2)
    y_minus_yestimated_squared <- y_minus_yestimated_squared + ((vectory[i] - yestimated[i])**2)
  }
  
  determination_coefficient <- ((y_minus_yaverage_squared - y_minus_yestimated_squared) / y_minus_yaverage_squared) * 100
  
  ### IF DETERMINATION COEFFICIENT VALUE IS NAN, THEN CHANGE IT TO 0 FOR PRACTICAL PURPOSES
  
  if (is.nan(determination_coefficient)){
    determination_coefficient <- 0
  }
  
  ### RETURN THE DETERMINATION COEFFICIENT TOGETHER WITH THE COEFFICIENTS OF THE CALCULATED EQUATION
  
  result_function <- c(determination_coefficient, string_result_vector)
  
  return(result_function)
  
}


### SOLUTION TO THE PROPOSED POINTS


### 1) FIND A REGRESSION OF GRADE N BY LEAST SQUARES THAT GENERATES AT LEAST AN R^2 OF 70%

n <- 2 
while (TRUE){
  resultVector <- regresion_calculator(n)
  if (resultVector[1] < 70){
    n <- n + 1
  }else{
    cat("THE REGRESSION OF GRADE", n, "GENERATES AT LEAST AN R^2 OF 70%\n")
    cat("\tCOEFFICIENTS OF THE EQUATION ->", resultVector[2], "\n\n")
    break
  }
}

### 2) FIND THE BEST REGRESSION OF GRADE N BY LEAST SQUARES WHERE N RANGES FROM 2 TO 15

all_results_vector <- c()
for (i in 2:15){
  all_results_vector[i] <- regresion_calculator(i)[1]
}

best_r2 <- as.numeric(all_results_vector[2])
best_r2_position <- 2
for (i in 3:15){
  if (best_r2 < as.numeric(all_results_vector[i])){
    best_r2 <- as.numeric(all_results_vector[i])
    best_r2_position <- i
  }
}

cat("THE BEST REGRESSION IS OF ", best_r2_position, "TH GRADE WITH AN R^2 VALUE OF ", best_r2, "\n", sep = "")
cat('\tCOEFFICIENTS OF THE EQUATION ->', regresion_calculator(best_r2_position)[2])

### STUDENT: CRISTIAN JULIAN MUNOZ BUENAHORA - 000430876 