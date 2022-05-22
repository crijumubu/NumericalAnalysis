### INITIAL INFORMATION

x <- c(1,2,4,6,8)
y <- c(12,48,900,5112,17148)

### FUNCTIION CREATED TO INSERT A VALUE INTO THE VECTOR X BY ORDER

insertVectorx <- function(xValue){
  positionInsert <- 1
  for (i in 1:length(x)){
    if (xValue > x[i]){
      positionInsert <- i+1
    }
  }
  beforeValue <- xValue
  afterValue <- 0
  for (i in positionInsert:(length(x)+1)){
    afterValue <- x[i]
    x[i] <- beforeValue
    beforeValue <- afterValue
  }
  return(x)
}

### FUNCTIION CREATED TO INSERT A VALUE INTO THE VECTOR Y BY ORDER

insertVectory <- function(yValue){
  positionInsert <- 1
  for (i in 1:length(y)){
    if (yValue > y[i]){
      positionInsert <- i+1
    }
  }
  beforeValue <- yValue
  afterValue <- 0
  for (i in positionInsert:(length(y)+1)){
    afterValue <- y[i]
    y[i] <- beforeValue
    beforeValue <- afterValue
  }
  return(y)
}

### FUNCTIION CREATED TO DEVELOP THE LAGRANGE METHOD

lagrange <- function(xValue){
  resultValue <- 0
  for (i in 1:length(x)){
    if (y[i] != -1){
      functionValue <- 1
      for (j in 1:length(x)){
        numerator <- xValue - x[j]
        denominator <- x[i] - x[j]
        if (x[i] != x[j]){
          if (denominator != 0 && numerator != 0){
            functionValue <- functionValue * (numerator/denominator)
          }else{
            functionValue <- 0
            break
          }
        }else{
          next
        }
      }
      if (functionValue != 0){
        functionValue <- functionValue * y[i]
        resultValue <- resultValue + functionValue
      }
    }
  }
  return(resultValue)
}

### SOLUTION OUTPUT

#RESULT FOR X EQUAL TO 3
xUnknown <- 3
resultValue <- lagrange(xUnknown)
cat('Lagrange interpolation:', '\n\t', 'x:', xUnknown, '-> y:', resultValue, '\n\n')

#UPDATE THE VECTORS
x <- insertVectorx(xUnknown)
y <- insertVectory(resultValue)


#RESULT FOR X EQUAL TO 5
xUnknown <- 5
resultValue <- lagrange(xUnknown)
cat('Lagrange interpolation:', '\n\t', 'x:', xUnknown, '-> y:', resultValue, '\n\n')

#UPDATE THE VECTORS
x <- insertVectorx(xUnknown)
y <- insertVectory(resultValue)


#RESULT FOR X EQUAL TO 3
xUnknown <- 7
resultValue <- lagrange(xUnknown)
cat('Lagrange interpolation:', '\n\t', 'x:', xUnknown, '-> y:', resultValue, '\n\n')

#UPDATE THE VECTORS
x <- insertVectorx(xUnknown)
y <- insertVectory(resultValue)

cat('Finally, the vectors would be as follows:\n\tx ->', x, '\n\ty ->', y)