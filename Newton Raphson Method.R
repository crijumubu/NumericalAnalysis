### INITIAL INFORMATION FOR LABORATORY SOLUTION

presetError <- 0.0001
xi <- 2 

### FUNCTIION CREATED TO EVALUATE THE PROPOSED FUNCTION GIVEN A X VALUE
evaluateFunction <- function(xValue) {
  return (7*xValue**3 + 2*xValue**2 - 3*xValue - 1)
}

### FUNCTIION CREATED TO EVALUATE THE DERIVATE OF THE PROPOSED FUNCTION GIVEN A X VALUE
evaluateDerivate <- function(xValue) {
  return (21*xValue**2 + 4*xValue - 3)
}

percentageRelativeError = 101
cont = 1 


while(percentageRelativeError >= presetError && cont <= 25){
  if (cont != 1){
    xi <- xiPlusOne
  }
  xiPlusOne <- xi - ((evaluateFunction(xi))/(evaluateDerivate(xi)))
  Fxi <- evaluateFunction(xi)
  FprimeXiPlusOne <- evaluateDerivate(xiPlusOne)
  percentageRelativeError <- abs((xiPlusOne - xi)/xiPlusOne)*100
  
  ### PROCEDURAL OUTPUT
  
  cat("Iteration", cont, "\n")
  cat("xi ->", xi, "\n")
  cat("xi plus one ->", xiPlusOne, "\n")
  cat("Fxi ->", Fxi, "\n")
  cat("FprimeXiPlusOne ->", FprimeXiPlusOne, "\n")
  cat("percentageRelativeError ->", percentageRelativeError, "\n\n")
  
  cont <- cont + 1
}

### SOLUTION OUTPUT

if (cont < 25){
  cat("The percentage real error is:", percentageRelativeError, "\n")
  cat("The result is finally:", xi)
}else{
  cat("The program has not been able to calculate the result, it has exceeded 25 iterations")
}


### STUDENT: CRISTIAN JULIAN MUNOZ BUENAHORA - 000430876