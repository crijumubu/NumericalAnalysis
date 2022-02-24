### INITIAL INFORMATION

xiMinusOne <- 0
xi <- 8
xiPlusOne <- 0 ### DEFAULT VALUE FOR XPLUSONE
presetEror <- 0.0000000001
percentageRelativeError <- 101 ### BY DEFAULT I SET PERCENTAGE RELATIVE ERROR VALUE TO 1000, THIS VALUE CAN BE WHATEVER NUMBER GREATER THAN PRESET ERROR VALUE
cont <- 1

evaluateFunction <- function(xValue) {
  return (sin(xValue)+cos(1+xValue**2)-1)
}

while (percentageRelativeError > presetEror){
  if (cont != 1){
    xiMinusOne <- xi
    xi <- xiPlusOne
  }
  Fxi <- evaluateFunction(xi)
  FxiMinusOne <- evaluateFunction(xiMinusOne)
  xiPlusOne <- xi - ((Fxi*(xiMinusOne - xi))/(FxiMinusOne - Fxi))
  percentageRelativeError <- abs((xiPlusOne - xi) / (xiPlusOne))*100
  
  ### PROCEDURAL OUTPUT
  
  cat("Iteration", cont, "\n")
  cat("xiPlusOne ->", xiPlusOne, "\n")
  cat("percentageRelativeError ->", percentageRelativeError, "\n\n")
  
  cont <- cont + 1
}

### sOLUTION OUTPUT

cat("The program has calculated the result, the value is", xiPlusOne, "\n")
cat("If we evaluate the result in the function it gives a value of", evaluateFunction(xiPlusOne))

### STUDENT: CRISTIAN JULIAN MUNOZ BUENAHORA - 000430876